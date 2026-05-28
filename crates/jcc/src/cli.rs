use crate::{
    ast::{parse::Parser, ty::TyCtx},
    desugar::DesugarPass,
    lower::LoweringPass,
    profile::Profiler,
    sema::{control::ControlPass, resolve::ResolverPass, typing::TypeChecker, SemaCtx},
};

use jcc_backend::{
    codemap::{color::ColorConfig, simple::SimpleFiles, Diagnostic, Files},
    ir::{
        analysis::{cfg::ControlFlowGraph, dom::Dominance, liveness::Liveness, order::Order},
        passes::mem2reg::Mem2Reg,
    },
    x86_64::{emit::emit_program, regalloc::allocate},
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
    /// Produce a .o object file instead of a final executable (-c flag).
    pub object_only: bool,
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
            object_only: false,
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
        let object_only = args.contains(["-c", "--no-link"]);
        let emit_asm_only = args.contains(["-S", "--assembly"]);
        let stop_after = if emit_asm_only || object_only {
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
            object_only,
        }))
    }
}

impl CompileOptions {
    pub fn run(&self, profiler: &mut Profiler) -> Result<()> {
        let mut db = SimpleFiles::new();
        let file = db
            .add_file(&self.path)
            .context("failed to read source file")?;
        let file = db.get(file).context("internal: file not in db")?;

        // Lexing, preprocessing, and parsing
        let tys = TyCtx::new();
        let mut interner = IdentInterner::new();
        let r = profiler.time("Parser", || Parser::new(file, &tys, &mut interner).parse());
        check_pass(self, &mut db, "preprocessor", &r.prep_issues)?;
        if self.stop_after == Some(Stage::Lex) {
            return Ok(());
        }
        check_pass(self, &mut db, "parser", &r.parser_issues)?;
        if r.ast.root.is_empty() {
            return Err(anyhow::anyhow!("exiting due to empty parse tree"));
        }
        if self.stop_after == Some(Stage::Parse) {
            return Ok(());
        }

        // Resolver, control flow analysis, and type checking
        let ast = r.ast;
        let r = profiler.time("Resolver", || ResolverPass::new(&ast).check());
        check_pass(self, &mut db, "resolver", &r.issues)?;
        let mut ctx = SemaCtx::new(&tys, r.counter.len());
        let r = profiler.time("Control", || ControlPass::new(&ast, &mut ctx).check());
        check_pass(self, &mut db, "control", &r.issues)?;
        let r = profiler.time("Typer", || TypeChecker::new(&ast, &mut ctx).check());
        check_pass(self, &mut db, "typer", &r.issues)?;
        if self.stop_after == Some(Stage::Validate) {
            return Ok(());
        }

        // Desugaring and lowering
        let ast = profiler.time("Desugar", || DesugarPass::new(ast, r.actions).build());
        if self.emit_options.ast_graphviz {
            write_graphviz(&self.path, &ast, &interner)?;
        }
        let mut ssa = profiler.time("Lower", || {
            LoweringPass::new(&ast, &ctx, &mut interner).build()
        });
        if self.emit_options.ir {
            println!("{}", ssa.pretty(&interner));
        }
        if self.stop_after == Some(Stage::Tacky) {
            return Ok(());
        }

        // IR analysis passes (block structure is immutable from here on).
        let mut order = Order::new();
        profiler.time("Order", || order.compute(&ssa));

        let mut cfg = ControlFlowGraph::new();
        profiler.time("CFG", || cfg.compute(&ssa, &order));

        let mut dom = Dominance::new();
        profiler.time("Dominance", || dom.compute(&ssa, &order, &cfg));

        // mem2reg: promote stack allocations to SSA values.
        profiler.time("Mem2Reg", || {
            Mem2Reg::new().run(&mut ssa, &order, &cfg, &dom)
        });

        if self.stop_after == Some(Stage::Codegen) {
            // Validate that register allocation succeeds without writing output.
            for (_, func) in ssa.functions.iter() {
                if let Some(entry) = func.entry {
                    let liveness = Liveness::compute(entry, &ssa, &cfg, &order);
                    let _ = allocate(entry, &ssa, &order, &liveness);
                }
            }
            return Ok(());
        }

        // Emit x86-64 AT&T assembly.
        let asm = profiler.time("Codegen", || emit_program(&ssa, &order, &interner));
        let asm_path = self.path.with_extension("s");
        std::fs::write(&asm_path, &asm)
            .with_context(|| format!("failed to write {}", asm_path.display()))?;

        if self.stop_after == Some(Stage::Assembly) && !self.object_only {
            // -S / --assembly: leave the .s file on disk, done.
            return Ok(());
        }

        if self.object_only {
            // -c / --no-link: assemble to .o, remove .s, do not link.
            let obj_path = self.path.with_extension("o");
            let status = std::process::Command::new("gcc")
                .arg("-c")
                .arg(&asm_path)
                .arg("-o")
                .arg(&obj_path)
                .status()
                .context("failed to invoke gcc for assembly")?;
            std::fs::remove_file(&asm_path).ok();
            if !status.success() {
                return Err(anyhow::anyhow!("assembly failed"));
            }
            return Ok(());
        }

        // Assemble and link using the system C compiler.
        let output = self.path.with_extension("");
        let mut cmd = std::process::Command::new("gcc");
        cmd.arg("-o").arg(&output).arg(&asm_path);
        for lib in &self.libs {
            cmd.arg(format!("-l{lib}"));
        }
        let status = cmd.status().context("failed to invoke gcc for linking")?;
        if !status.success() {
            return Err(anyhow::anyhow!("assembly/linking failed"));
        }

        // Remove the intermediate assembly file.
        std::fs::remove_file(&asm_path).ok();

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
