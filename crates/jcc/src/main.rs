use jcc::{
    lex::{Lexer, LexerDiagnosticKind},
    parse::Parser,
    tacky::TackyBuilder,
};

use anyhow::{Context, Result};
use clap::Parser as ClapParser;

use tacky::{
    amd64::{build::AMD64Builder, emit::AMD64Emitter, fix::AMD64Fixer},
    source_file::{self, SourceDb, SourceFile},
    string_interner::StringInterner,
};

use std::{path::PathBuf, process::Command};

#[derive(ClapParser)]
struct Args {
    /// Run the compiler in verbose mode
    #[clap(long)]
    pub verbose: bool,
    /// Run the lexer, but stop before parsing
    #[clap(long)]
    pub lex: bool,
    /// Run the lexer and parser, but stop before tacky generation
    #[clap(long)]
    pub parse: bool,
    /// Run the lexer, parser, tacky generation, but stop code generation
    #[clap(long)]
    pub tacky: bool,
    /// Run the lexer, parser, tacky generation, code generation, but stop before assembly generation
    #[clap(long)]
    pub codegen: bool,
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
    let output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&args.path)
        .arg("-o")
        .arg(&pp_path)
        .output()?;
    if !output.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        return Err(anyhow::anyhow!("\nexiting due to preprocessor errors"));
    }

    // Add file to db
    let mut db = SourceDb::new();
    db.add(SourceFile::new(&pp_path).context("Failed to read file")?);
    let file = db.files().last().context("Failed to store file in db")?;

    // Lex file
    let mut interner = StringInterner::default();
    let mut lexer_result = Lexer::new(&file, &mut interner).lex();
    if args.lex {
        lexer_result
            .diagnostics
            .retain(|d| !matches!(d.kind, LexerDiagnosticKind::UnbalancedToken(_)));
    }
    if !lexer_result.diagnostics.is_empty() {
        source_file::diagnostic::report_batch(
            &file,
            &mut std::io::stderr(),
            &lexer_result.diagnostics,
        )?;
        return Err(anyhow::anyhow!("\nexiting due to lexer errors"));
    }
    if args.verbose {
        source_file::diagnostic::report_batch(
            &file,
            &mut std::io::stderr(),
            &lexer_result.diagnostics,
        )?;
    }
    if args.lex {
        return Ok(());
    }

    // Parse tokens
    let parser_result = Parser::new(&file, lexer_result.tokens.iter()).parse();
    if !parser_result.diagnostics.is_empty() {
        source_file::diagnostic::report_batch(
            &file,
            &mut std::io::stderr(),
            &parser_result.diagnostics,
        )?;
        return Err(anyhow::anyhow!("\nexiting due to parser errors"));
    }
    if args.verbose {
        println!("{:#?}", parser_result.ast);
    }
    if args.parse {
        return Ok(());
    }

    // TODO: Check the AST
    // let checker = Checker::new(&file, &interner, &ast);
    // checker.check()?;

    // Generate Tacky
    if parser_result.ast.items().is_empty() {
        eprintln!("Error: codegen was given an empty parse tree");
        return Err(anyhow::anyhow!("\nexiting due to codegen errors"));
    }
    let tacky = TackyBuilder::new(&parser_result.ast, &mut interner).build();
    if args.verbose {
        println!("{:#?}", tacky);
    }
    if args.tacky {
        return Ok(());
    }

    // Generate AMD64
    let mut amd64 = AMD64Builder::new(&tacky).build();
    if args.verbose {
        println!("{:#?}", amd64);
    }

    // Fix intructions
    AMD64Fixer::new().fix(&mut amd64);
    if args.verbose {
        println!("{:#?}", amd64);
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
    let output = Command::new("gcc")
        .arg(&asm_path)
        .arg("-o")
        .arg(&args.path.with_extension(""))
        .output()?;
    if !output.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        return Err(anyhow::anyhow!("\nexiting due to assembler errors"));
    }

    Ok(())
}
