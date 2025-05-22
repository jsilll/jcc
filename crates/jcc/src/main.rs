use jcc::{
    ast::{graphviz::AstGraphviz, parse::Parser},
    lex::{Lexer, LexerDiagnosticKind},
    sema::{control::ControlPass, resolve::ResolverPass, ty::TyperPass, SemaCtx},
};

use jcc_ssa::{
    self as ssa,
    amd64::{emit::AMD64Emitter, fix::AMD64Fixer},
    sourcemap::{self, SourceDb, SourceMap},
    verify::SSAVerifier,
    interner::Interner,
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

    // Add file to db
    let mut db = SourceDb::new();
    db.add(SourceMap::new(&pp_path).context("Failed to read file")?);
    let file = db.files().last().context("Failed to store file in db")?;

    // Lex file
    let mut interner = Interner::new();
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
    if args.verbose {
        println!("{:#?}", parser_result.ast);
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

    let ast = parser_result.ast;

    // Analyze the AST
    let mut ctx = SemaCtx::new(&ast);
    let control_result = ControlPass::new(&mut ctx).check(&ast);
    if !control_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &control_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to control errors"));
    }
    let resolver_result = ResolverPass::new(&ast, &mut ctx).check();
    if !resolver_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(
            file,
            &mut std::io::stderr(),
            &resolver_result.diagnostics,
        )?;
        return Err(anyhow::anyhow!("exiting due to resolver errors"));
    }
    let typer_result = TyperPass::new(&ast, &mut ctx).check();
    if !typer_result.diagnostics.is_empty() {
        sourcemap::diag::report_batch(file, &mut std::io::stderr(), &typer_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to typer errors"));
    }
    if args.verbose {
        println!("{:#?}", ast);
    }
    if args.validate {
        return Ok(());
    }

    let ssa = jcc::ssa::build(&ast, &ctx, interner);
    if args.verbose {
        println!("{}", ssa);
    }

    let verifier_result = SSAVerifier::new(&ssa).verify();
    if !verifier_result.diagnostics.is_empty() {
        // TODO: properly report ssa errors
        return Err(anyhow::anyhow!("exiting due to ssa verifier errors"));
    }

    let mut amd64 = ssa::amd64::build(&ssa);
    let interner = ssa.take_interner();

    if args.verbose {
        println!("{}", amd64);
    }

    // Fix intructions
    AMD64Fixer::new().fix(&mut amd64);
    if args.verbose {
        println!("{}", amd64);
    }
    if args.codegen {
        return Ok(());
    }

    // Emit assembly
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
