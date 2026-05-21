use crate::{
    ast::{parse::Parser, ty::TyCtx},
    desugar::DesugarPass,
    lower::LoweringPass,
    profile::Profiler,
    sema::{control::ControlPass, resolve::ResolverPass, typing::TypeChecker, SemaCtx},
};

use jcc_backend::{
    codemap::{color::ColorConfig, Diagnostic, Files, SimpleFiles},
    IdentInterner, TargetOs,
};

use anyhow::{Context, Result};

use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

#[derive(Debug)]
pub struct CompileOptions {
    pub path: PathBuf,
    pub profile: bool,
    pub target: TargetOs,
    pub libs: Vec<String>,
    pub stop_after: Option<Stage>,
    pub emit_options: EmitOptions,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct EmitOptions {
    pub ir: bool,
    pub diagnostics: bool,
    pub ast_graphviz: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    Assembly,
}

impl CompileOptions {
    pub fn new(target: TargetOs, path: PathBuf) -> Self {
        Self {
            path,
            target,
            profile: false,
            stop_after: None,
            libs: Vec::new(),
            emit_options: EmitOptions::default(),
        }
    }

    #[rustfmt::skip]
    pub fn help(mut out: impl std::io::Write) -> std::io::Result<()> {
        writeln!(out, "Usage: jcc [OPTIONS] <PATH>")?;
        writeln!(out)?;
        writeln!(out, "Options:")?;
        writeln!(out, "  -h, --help              Show this help message")?;
        writeln!(out, "  -l <lib>                Link with the given library")?;
        writeln!(out, "  --target <target>       Target OS (linux, macos) [default: linux]")?;
        writeln!(out, "  --lex                   Stop after lexing")?;
        writeln!(out, "  --parse                 Stop after parsing")?;
        writeln!(out, "  --profile               Run in profile mode")?;
        writeln!(out, "  --validate              Stop after validation")?;
        writeln!(out, "  --codegen               Stop after code generation")?;
        writeln!(out, "  --tacky                 Stop after Tacky IR generation")?;
        writeln!(out, "  -S, --assembly          Emit assembly file (.s)")?;
        writeln!(out, "  -c, --no-link           Compile and assemble, but do not link")?;
        writeln!(out, "  --emit-ast-graphviz     Emit AST Graphviz DOT file")?;
        writeln!(out, "  --emit-ir               Emit intermediate representation file")
    }

    pub fn from_env() -> Result<Option<Self>, jcc_args::error::Error> {
        let mut args = jcc_args::Args::from_env();
        if args.contains(["-h", "--help"]) {
            return Ok(None);
        }

        let lex = args.contains("--lex");
        let parse = args.contains("--parse");
        let tacky = args.contains("--tacky");
        let codegen = args.contains("--codegen");
        let validate = args.contains("--validate");
        let assembly = args.contains(["-c", "--no-link"]);
        let stop_after = if assembly {
            Some(Stage::Assembly)
        } else if codegen {
            Some(Stage::Codegen)
        } else if tacky {
            Some(Stage::Tacky)
        } else if validate {
            Some(Stage::Validate)
        } else if parse {
            Some(Stage::Parse)
        } else if lex {
            Some(Stage::Lex)
        } else {
            None
        };

        let libs = args.values_from_str("-l")?;
        let profile = args.contains("--profile");
        let target = args
            .opt_value_from_str("--target")?
            .unwrap_or(TargetOs::Linux);
        let emit_options = EmitOptions {
            ir: args.contains("--emit-ir"),
            diagnostics: !args.contains("--no-diagnostics"),
            ast_graphviz: args.contains("--emit-ast-graphviz"),
        };

        let path = args.free_from_fn(PathBuf::from_str)?;
        args.finish()?;

        Ok(Some(CompileOptions {
            path,
            libs,
            target,
            profile,
            stop_after,
            emit_options,
        }))
    }
}

impl CompileOptions {
    pub fn run(&self, profiler: &mut Profiler) -> Result<()> {
        let pp_path = self.path.with_extension("i");

        profiler.time("Preprocessor (gcc -E)", || {
            let out = std::process::Command::new("gcc")
                .args(["-E", "-P"])
                .arg(&self.path)
                .arg("-o")
                .arg(&pp_path)
                .output()
                .context("failed to invoke gcc -E")?;
            if !out.status.success() {
                if self.emit_options.diagnostics {
                    eprintln!("{}", String::from_utf8_lossy(&out.stderr));
                }
                return Err(anyhow::anyhow!("exiting due to preprocessor errors"));
            }
            Ok(())
        })?;

        let mut files = SimpleFiles::new();
        let fid = files
            .add_file(&pp_path)
            .context("failed to read preprocessed file")?;
        let file = files.get(fid).context("internal: file not in db")?;

        // Lexing and parsing
        let tys = TyCtx::new();
        let mut interner = IdentInterner::new();
        let r = profiler.time("Parser", || Parser::new(file, &tys, &mut interner).parse());
        check_pass(self, &mut files, "lexer", &r.lexer_diagnostics)?;
        if self.stop_after == Some(Stage::Lex) {
            return Ok(());
        }
        check_pass(self, &mut files, "parser", &r.parser_diagnostics)?;
        if r.ast.root.is_empty() {
            return Err(anyhow::anyhow!("exiting due to empty parse tree"));
        }
        if self.stop_after == Some(Stage::Parse) {
            return Ok(());
        }

        // Resolver, control flow analysis, and type checking
        let ast = r.ast;
        let r = profiler.time("Resolver", || ResolverPass::new(&ast).check());
        check_pass(self, &mut files, "resolver", &r.issues)?;
        let mut ctx = SemaCtx::new(&tys, r.symbol_count);
        let r = profiler.time("Control", || ControlPass::new(&ast, &mut ctx).check());
        check_pass(self, &mut files, "control", &r.issues)?;
        let r = profiler.time("Typer", || TypeChecker::new(&ast, &mut ctx).check());
        check_pass(self, &mut files, "typer", &r.issues)?;
        if self.stop_after == Some(Stage::Validate) {
            return Ok(());
        }

        // Desugaring and lowering
        let ast = profiler.time("Desugar", || DesugarPass::new(ast, r.actions).build());
        if self.emit_options.ast_graphviz {
            write_graphviz(&self.path, &ast, &interner)?;
        }
        let ssa = profiler.time("Lower", || {
            LoweringPass::new(&ast, &ctx, &mut interner).build()
        });
        if self.emit_options.ir {
            println!("{}", ssa.pretty(&interner));
        }
        if self.stop_after == Some(Stage::Tacky) {
            return Ok(());
        }

        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn check_pass(
    opts: &CompileOptions,
    files: &mut impl Files,
    pass: &str,
    diags: &[impl Into<Diagnostic> + Clone],
) -> Result<()> {
    if diags.is_empty() {
        return Ok(());
    }
    if opts.emit_options.diagnostics {
        let config = ColorConfig::default();
        for diag in diags {
            diag.clone().into().emit_colored_stderr(files, &config)?;
        }
        return Err(anyhow::anyhow!("exiting due to {} errors", pass));
    }
    Err(anyhow::anyhow!("{} {} error(s)", diags.len(), pass))
}

fn write_graphviz(path: &Path, ast: &crate::ast::Ast<'_>, interner: &IdentInterner) -> Result<()> {
    use std::io::Write as _;
    let dot_path = path.with_extension("dot");
    let mut f = std::fs::File::create(&dot_path)
        .with_context(|| format!("failed to create {}", dot_path.display()))?;
    write!(f, "{}", ast.graphviz(interner))
        .with_context(|| format!("failed to write {}", dot_path.display()))
}
