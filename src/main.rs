use jcc::{
    reporting::Diagnostic,
    source_file::{SourceFile, SourceSpan},
};

use anyhow::{Context, Result};
use clap::Parser;

use std::path::PathBuf;

#[derive(Parser)]
struct Args {
    pub path: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let file = SourceFile::new(args.path).context("Failed to read file")?;
    let span = SourceSpan::new(10, 13).context("Failed to create span")?;
    let diag = Diagnostic::new(span, "title", "message");
    diag.report(&mut std::io::stdout(), &file)?;

    Ok(())
}
