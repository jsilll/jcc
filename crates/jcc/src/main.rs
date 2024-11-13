use anyhow::{Context, Result};
use clap::Parser;
use jcc::lexer::Lexer;
use source_file::{diagnostic::Diagnostic, SourceDb, SourceFile};
use std::path::PathBuf;
use string_interner::StringInterner;

#[derive(Parser)]
struct Args {
    pub path: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Global data structures
    let mut db = SourceDb::new();
    let mut interner = StringInterner::default();
    // let bump = Bumpalo::bump();

    // Add file to db
    db.add(SourceFile::new(args.path).context("Failed to read file")?);
    let file = db.files().last().context("Failed to store file in db")?;

    // Compile
    let lexer_result = Lexer::new(&file, &mut interner).lex();
    if !lexer_result.diagnostics.is_empty() {
        lexer_result.diagnostics.iter().try_for_each(|d| {
            let diag = Diagnostic::from(d.clone());
            diag.report(&file, &mut std::io::stderr())
        })?;
    }

    // let parser = Parser::new(&file, &interner, &tokens);
    // let ast = parser.parse()?;
    // let checker = Checker::new(&file, &interner, &ast);
    // checker.check()?;
    // let codegen = Codegen::new(&file, &interner, &ast);
    // codegen.generate()?;

    Ok(())
}
