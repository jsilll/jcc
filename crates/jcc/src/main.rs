use jcc::{
    ast::{graphviz::AstGraphviz, mermaid::AstMermaid, parse::Parser},
    sema::{control::ControlPass, resolve::ResolverPass, ty::TyperPass, SemaCtx},
    ssa::Builder,
    tok::lex::{Lexer, LexerDiagnosticKind},
};

use jcc_ssa::{
    self as ssa,
    amd64::{emit::AMD64Emitter, fix::AMD64Fixer},
    interner::Interner,
    sourcemap::{self, SourceDb, SourceMap},
    verify::SSAVerifier,
};

use anyhow::{Context, Result};
use clap::Parser as ClapParser;

use std::{path::PathBuf, process::Command};

#[derive(ClapParser)]
struct Args {
    /// Run the compiler in verbose mode
    #[clap(long)]
    pub verbose: bool,
    /// Run until the lexer and stop
    #[clap(long)]
    pub lex: bool,
    /// Run until the parser and stop
    #[clap(long)]
    pub parse: bool,
    /// Emit AST as Mermaid file and stop
    #[clap(long)]
    pub emit_ast_mermaid: bool,
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
    if let Err(e) = try_main() {
        println!("{:?}", e);
        std::process::exit(1)
    }
}

fn try_main() -> Result<()> {
    let args = Args::parse();

    // Run the preprocessor with `gcc -E -P`
    let pp_path = args.path.with_extension("i");
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
    let mut lexer_result = Lexer::new(file).lex();
    if args.lex {
        lexer_result
            .diagnostics
            .retain(|d| !matches!(d.kind, LexerDiagnosticKind::UnbalancedToken(_)));
    }
    if !lexer_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &lexer_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to lexer errors"));
    }
    if args.verbose {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &lexer_result.diagnostics)?;
    }
    if args.lex {
        return Ok(());
    }

    // Parse tokens
    let parser_result = Parser::new(file, &mut interner, lexer_result.tokens.iter()).parse();
    if !parser_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &parser_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to parser errors"));
    }
    if args.emit_ast_mermaid {
        let mmd_path = args.path.with_extension("mmd");
        let ast_mermaid = AstMermaid::new(&parser_result.ast, &interner);
        let dot = ast_mermaid.emit();
        std::fs::write(&mmd_path, &dot).context("Failed to write Mermaid file")?;
    }
    if args.emit_ast_graphviz {
        let dot_path = args.path.with_extension("dot");
        let ast_graphviz = AstGraphviz::new(&parser_result.ast, &interner);
        let dot = ast_graphviz.emit();
        std::fs::write(&dot_path, &dot).context("Failed to write AST graphviz file")?;
    }
    if args.parse {
        return Ok(());
    }
    if parser_result.ast.root().is_empty() {
        eprintln!("Error: no declarations in the source file");
        return Err(anyhow::anyhow!("exiting due to empty parse tree"));
    }

    // Resolve the AST
    let ast = parser_result.ast;
    let resolver_result = ResolverPass::new(&ast).check();
    if !resolver_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &resolver_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to resolver errors"));
    }
    if args.emit_ast_mermaid {
        let mmd_path = args.path.with_extension("mmd");
        let ast_mermaid = AstMermaid::new(&ast, &interner);
        let dot = ast_mermaid.emit();
        std::fs::write(&mmd_path, &dot).context("Failed to write Mermaid file")?;
    }
    if args.emit_ast_graphviz {
        let dot_path = args.path.with_extension("dot");
        let ast_graphviz = AstGraphviz::new(&ast, &interner);
        let dot = ast_graphviz.emit();
        std::fs::write(&dot_path, &dot).context("Failed to write AST graphviz file")?;
    }

    // Analyze the AST
    let mut ctx = SemaCtx::new(&ast);
    let control_result = ControlPass::new(&ast, &mut ctx).check();
    if !control_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &control_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to control errors"));
    }
    let typer_result = TyperPass::new(&ast, &mut ctx).check();
    if !typer_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &typer_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to typer errors"));
    }
    if args.validate {
        return Ok(());
    }

    // Generate SSA
    let ssa = Builder::new(&ast, &ctx, &mut interner).build();
    if args.verbose {
        println!("{}", ssa);
    }
    let verifier_result = SSAVerifier::new(&ssa).verify();
    if !verifier_result.diagnostics.is_empty() {
        // TODO: properly report ssa errors
        return Err(anyhow::anyhow!("exiting due to ssa verifier errors"));
    }
    if args.tacky {
        return Ok(());
    }

    // Generate Assembly
    let mut amd64 = ssa::amd64::build(&ssa);
    if args.verbose {
        let asm = AMD64Emitter::new(&amd64, &interner).emit();
        println!("{}", asm);
    }
    AMD64Fixer::new().fix(&mut amd64);
    if args.verbose {
        let asm = AMD64Emitter::new(&amd64, &interner).emit();
        println!("{}", asm);
    }
    if args.codegen {
        return Ok(());
    }

    // Emit final assembly
    let asm_path = args.path.with_extension("s");
    let asm = AMD64Emitter::new(&amd64, &interner).emit();
    std::fs::write(&asm_path, &asm).context("Failed to write assembly file")?;
    if args.assembly {
        return Ok(());
    }

    // Run the assembler and linker with `gcc`
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
}
