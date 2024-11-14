use anyhow::{Context, Result};
// use bumpalo::Bump;
use clap::Parser;
use jcc::lexer::Lexer;
use source_file::{diagnostic::Diagnostic, SourceDb, SourceFile};
use std::{path::PathBuf, process::Command};
use string_interner::StringInterner;

#[derive(Parser)]
struct Args {
    /// Run the lexer, but stop before parsing
    #[clap(long)]
    pub lex: bool,
    /// Run the lexer and parser, but stop before assembly generation
    #[clap(long)]
    pub parse: bool,
    /// Run the lexer, parser, and assembly generation, but stop before code emission
    #[clap(long)]
    pub codegen: bool,
    /// Emit an assembly file but not an executable
    #[clap(long, short = 'S')]
    pub emit_asm: bool,
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

    // Global data structures
    let mut db = SourceDb::new();
    let mut interner = StringInterner::default();
    // let bump = Bump::new();

    // Add file to db
    db.add(SourceFile::new(&pp_path).context("Failed to read file")?);
    let file = db.files().last().context("Failed to store file in db")?;

    // Compile the file
    let lexer_result = Lexer::new(&file, &mut interner).lex();
    if !lexer_result.diagnostics.is_empty() {
        lexer_result.diagnostics.iter().try_for_each(|d| {
            let diag = Diagnostic::from(d.clone());
            diag.report(&file, &mut std::io::stderr())
        })?;
        return Err(anyhow::anyhow!("\nexiting due to lexer errors"));
    }
    lexer_result.tokens.iter().try_for_each(|t| {
        let diag = Diagnostic::note(t.span, "token", "here");
        diag.report(&file, &mut std::io::stdout())
    })?;
    if args.lex {
        return Ok(());
    }

    // let parser = Parser::new(&file, &interner, &tokens);
    if args.parse {
        return Ok(());
    }

    // let ast = parser.parse()?;
    // let checker = Checker::new(&file, &interner, &ast);
    // checker.check()?;

    // let codegen = Codegen::new(&file, &interner, &ast);
    // codegen.generate()?;
    if args.codegen {
        return Ok(());
    }

    // Make a dummy assembly file for now with a simple main function
    let asm_path = args.path.with_extension("s");
    std::fs::write(
        &asm_path,
        "
        .globl main
    main:
        movl $0, %eax
        ret
    ",
    )?;
    if args.emit_asm {
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
