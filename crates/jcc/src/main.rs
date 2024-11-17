use jcc::{
    ir::IrBuilder, lexer::{Lexer, LexerDiagnosticKind}, parser::Parser
};

use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use source_file::{diagnostic::Diagnostic, SourceDb, SourceFile};
use string_interner::StringInterner;

use std::{path::PathBuf, process::Command};

#[derive(ClapParser)]
struct Args {
    /// Run the compiler in verbose mode
    #[clap(long)]
    pub verbose: bool,
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
    /// The source file to compile
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

    // Add file to db
    db.add(SourceFile::new(&pp_path).context("Failed to read file")?);
    let file = db.files().last().context("Failed to store file in db")?;

    // Lex the file
    let mut lexer_result = Lexer::new(&file, &mut interner).lex();
    if args.lex {
        lexer_result.diagnostics.retain(|d| match d.kind {
            LexerDiagnosticKind::UnbalancedToken(_) => false,
            _ => true,
        });
    }
    if !lexer_result.diagnostics.is_empty() {
        let mut hint = 0;
        lexer_result.diagnostics.iter().try_for_each(|d| {
            let diag = Diagnostic::from(d.clone());
            diag.report_hint(&file, &mut hint, &mut std::io::stderr())
        })?;
        return Err(anyhow::anyhow!("\nexiting due to lexer errors"));
    }
    if args.verbose {
        let mut hint = 0;
        lexer_result.tokens.iter().try_for_each(|t| {
            let diag = Diagnostic::note(t.span, "token", "here");
            diag.report_hint(&file, &mut hint, &mut std::io::stdout())
        })?;
    }
    if args.lex {
        return Ok(());
    }

    // Parse the tokens
    let parser_result = Parser::new(&file, lexer_result.tokens.iter()).parse();
    if !parser_result.diagnostics.is_empty() {
        let mut hint = 0;
        parser_result.diagnostics.iter().try_for_each(|d| {
            let diag = Diagnostic::from(d.clone());
            diag.report_hint(&file, &mut hint, &mut std::io::stderr())
        })?;
        return Err(anyhow::anyhow!("\nexiting due to parser errors"));
    }
    if args.verbose {
        println!("{:#?}", parser_result.program);
    }
    if args.parse {
        return Ok(());
    }

    // let checker = Checker::new(&file, &interner, &ast);
    // checker.check()?;

    let ast = parser_result.program.context("Parsed an empty program")?;
    let ir_builder = IrBuilder::new(&ast);
    let ir = ir_builder.build();
    if args.verbose {
        println!("{:#?}", ir);
    }
    if args.codegen {
        return Ok(());
    }

    // Make a dummy assembly file for now with a simple main function
    let asm_path = args.path.with_extension("s");
    // TODO: Should be _main if on macOS
    // NOTE: Last line means that the stack is not executable
    std::fs::write(
        &asm_path,
        "
        .globl main
    main:
        movl $0, %eax
        ret
    .section .note.GNU-stack,\"\",@progbits
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
