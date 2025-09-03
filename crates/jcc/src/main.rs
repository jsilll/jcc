use jcc::{
    ast::{graphviz::AstGraphviz, parse::Parser},
    lower::LoweringPass,
    profile::Profiler,
    sema::{control::ControlPass, resolve::ResolverPass, ty::TyperPass, SemaCtx, TypeDict},
    ssa::SSABuilder,
    tok::lex::{Lexer, LexerDiagnosticKind},
};

use jcc_ssa::{
    self as ssa,
    amd64::{emit::AMD64Emitter, fix::AMD64Fixer},
    interner::Interner,
    sourcemap::{self, SourceDb, SourceMap},
};

use anyhow::{Context, Result};
use clap::Parser as ClapParser;

use std::{path::PathBuf, process::Command};

#[derive(ClapParser)]
struct Args {
    /// Run the compiler in verbose mode
    #[clap(long)]
    pub verbose: bool,
    /// Run the compiler in profile mode
    #[clap(long)]
    pub profile: bool,
    /// Run until the lexer and stop
    #[clap(long)]
    pub lex: bool,
    /// Run until the parser and stop
    #[clap(long)]
    pub parse: bool,
    /// Emit AST as Graphviz DOT file and stop
    #[clap(long)]
    pub emit_ast_graphviz: bool,
    /// Run until the semantic analyzer and stop
    #[clap(long)]
    pub validate: bool,
    /// Run until the tacky generator and stop
    #[clap(long)]
    pub tacky: bool,
    /// Run until the codegen and stop
    #[clap(long)]
    pub codegen: bool,
    /// Do not link the source files
    #[clap(long, short = 'c')]
    pub no_link: bool,
    /// Emit an assembly file but not an executable
    #[clap(long, short = 'S')]
    pub assembly: bool,
    /// The path to the source file to compile
    pub path: PathBuf,
}

fn main() {
    let args = Args::parse();
    let mut profiler = Profiler::new(args.profile);
    let r = try_main(&args, &mut profiler);
    profiler
        .report(&mut std::io::stdout())
        .expect("Failed to write profile report");
    if let Err(e) = r {
        println!("{:?}", e);
        std::process::exit(1)
    }
}

fn try_main(args: &Args, profiler: &mut Profiler<Pass>) -> Result<()> {
    // Run the preprocessor with `gcc -E -P`
    let pp_path = args.path.with_extension("i");
    profiler.time(Pass::Preprocessor, || -> Result<()> {
        let pp_output = Command::new("gcc")
            .arg("-E")
            .arg("-P")
            .arg(&args.path)
            .arg("-o")
            .arg(&pp_path)
            .output()
            .context("Failed to run preprocessor (gcc -E -P)")?;
        if !pp_output.status.success() {
            eprintln!("{}", String::from_utf8_lossy(&pp_output.stderr));
            return Err(anyhow::anyhow!("exiting due to preprocessor errors"));
        }
        Ok(())
    })?;

    // Initialize source database and interner
    let mut db = SourceDb::new();
    let mut interner = Interner::new();
    db.add(SourceMap::new(&pp_path).context(format!(
        "Failed to create source map for {}",
        pp_path.display()
    ))?);
    let file = db.files().last().context(format!(
        "No source files found in the database for {}",
        pp_path.display()
    ))?;

    // Lex file
    let mut r = profiler.time(Pass::Lexer, || Lexer::new(file).lex());
    if args.lex {
        r.diagnostics
            .retain(|d| !matches!(d.kind, LexerDiagnosticKind::UnbalancedToken(_)));
    }
    if !r.diagnostics.is_empty() {
        sourcemap::diag::report_batch_to_stderr(file, &r.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to lexer errors"));
    }
    if args.verbose {
        sourcemap::diag::report_batch_to_stderr(file, &r.diagnostics)?;
    }
    if args.lex {
        return Ok(());
    }

    // Parse tokens
    let mut dict = TypeDict::new();
    let r = profiler.time(Pass::Parser, || {
        Parser::new(file, &mut dict, &mut interner, r.tokens.iter()).parse()
    });
    if !r.diagnostics.is_empty() {
        sourcemap::diag::report_batch_to_stderr(file, &r.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to parser errors"));
    }
    if args.emit_ast_graphviz {
        let dot_path = args.path.with_extension("dot");
        let ast_graphviz = AstGraphviz::new(&r.ast, &interner);
        let dot = ast_graphviz.emit();
        std::fs::write(&dot_path, &dot).context("Failed to write AST graphviz file")?;
    }
    if args.parse {
        return Ok(());
    }

    // Resolve the AST
    let ast = r.ast;
    if ast.root().is_empty() {
        eprintln!("Error: no declarations in the source file");
        return Err(anyhow::anyhow!("exiting due to empty parse tree"));
    }
    let r = profiler.time(Pass::Resolver, || ResolverPass::new(&ast).check());
    if !r.diagnostics.is_empty() {
        sourcemap::diag::report_batch_to_stderr(file, &r.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to resolver errors"));
    }

    // Analyze the AST
    let mut ctx = SemaCtx::with_dict(&ast, dict);
    let r = profiler.time(Pass::Control, || ControlPass::new(&ast, &mut ctx).check());
    if !r.diagnostics.is_empty() {
        sourcemap::diag::report_batch_to_stderr(file, &r.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to control errors"));
    }
    let r = profiler.time(Pass::Typer, || TyperPass::new(&ast, &mut ctx).check());
    if !r.diagnostics.is_empty() {
        sourcemap::diag::report_batch_to_stderr(file, &r.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to typer errors"));
    }
    let ast = profiler.time(Pass::Lowering, || LoweringPass::new(ast, r.actions).build());
    if args.emit_ast_graphviz {
        let dot_path = args.path.with_extension("dot");
        let ast_graphviz = AstGraphviz::new(&ast, &interner);
        let dot = ast_graphviz.emit();
        std::fs::write(&dot_path, &dot).context("Failed to write AST graphviz file")?;
    }
    if args.validate {
        return Ok(());
    }

    // Generate SSA
    let ssa = profiler.time(Pass::SSABuild, || {
        SSABuilder::new(&ast, &ctx, &mut interner).build()
    });
    if args.verbose {
        println!("{}", ssa);
    }
    if args.tacky {
        return Ok(());
    }

    // Generate Assembly
    let mut amd64 = profiler.time(Pass::AMD64Build, || {
        ssa::amd64::build::Builder::new(&ssa).build()
    });
    if args.verbose {
        let asm = AMD64Emitter::new(&amd64, &interner).emit();
        println!("{}", asm);
    }
    profiler.time(Pass::AMD64Fixer, || AMD64Fixer::new().fix(&mut amd64));
    if args.verbose {
        let asm = AMD64Emitter::new(&amd64, &interner).emit();
        println!("{}", asm);
    }
    if args.codegen {
        return Ok(());
    }

    // Emit final assembly
    let asm_path = args.path.with_extension("s");
    let asm = profiler.time(Pass::AssemblyEmission, || {
        AMD64Emitter::new(&amd64, &interner).emit()
    });
    std::fs::write(&asm_path, &asm).context("Failed to write assembly file")?;
    if args.assembly {
        return Ok(());
    }

    // Run the assembler and linker with `gcc`
    profiler.time(Pass::AssemblerAndLinker, || -> Result<()> {
        let mut extension = "";
        let mut cmd = Command::new("gcc");
        if args.no_link {
            cmd.arg("-c");
            extension = "o";
        }
        cmd.arg(&asm_path)
            .arg("-o")
            .arg(args.path.with_extension(extension));
        let output = cmd.output()?;
        if !output.status.success() {
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
            return Err(anyhow::anyhow!("exiting due to assembler errors"));
        }
        Ok(())
    })?;

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Pass {
    Preprocessor,
    Lexer,
    Parser,
    Resolver,
    Control,
    Typer,
    Lowering,
    SSABuild,
    AMD64Build,
    AMD64Fixer,
    AssemblyEmission,
    AssemblerAndLinker,
}

impl std::fmt::Display for Pass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pass::Preprocessor => write!(f, "Preprocessor (gcc -E)"),
            Pass::Lexer => write!(f, "Lexer"),
            Pass::Parser => write!(f, "Parser"),
            Pass::Resolver => write!(f, "Resolver"),
            Pass::Control => write!(f, "Control Pass"),
            Pass::Typer => write!(f, "Typer Pass"),
            Pass::Lowering => write!(f, "Lowering Pass"),
            Pass::SSABuild => write!(f, "SSA Build"),
            Pass::AMD64Build => write!(f, "AMD64 Build"),
            Pass::AMD64Fixer => write!(f, "AMD64 Fixer"),
            Pass::AssemblyEmission => write!(f, "Assembly Emission"),
            Pass::AssemblerAndLinker => write!(f, "Assembler & Linker (gcc)"),
        }
    }
}
