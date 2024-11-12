use anyhow::{Context, Result};
use clap::Parser;
use jcc::{
    diagnostic::Diagnostic,
    source_file::{SourceDb, SourceFile},
};
use std::path::PathBuf;

#[derive(Parser)]
struct Args {
    pub path: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut db = SourceDb::new();
    db.add(SourceFile::new(args.path).context("Failed to read file")?);
    let file = db.files().last().context("Failed to store file in db")?;

    let span = file.span(2244..2416).context("Failed to create span")?;
    let diag = Diagnostic::error(span, "title", "message");
    diag.report(&file, &mut std::io::stderr())?;

    // let interner = StringInterner::default();
    // let lexer = Lexer::new(&file, &interner);
    // let tokens = lexer.collect::<Result<Vec<_>>>()?;
    // let parser = Parser::new(&file, &interner, &tokens);
    // let ast = parser.parse()?;
    // let checker = Checker::new(&file, &interner, &ast);
    // checker.check()?;
    // let codegen = Codegen::new(&file, &interner, &ast);
    // codegen.generate()?;

    Ok(())
}
