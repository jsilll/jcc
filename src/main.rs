use anyhow::{Context, Result};
use clap::Parser;
use jcc::{
    reporting::Diagnostic,
    source_file::{SourceFile, SourceSpan},
};
use std::path::PathBuf;

#[derive(Parser)]
struct Args {
    pub path: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let file = SourceFile::new(args.path).context("Failed to read file")?;
    let span = SourceSpan::new(499, 503).context("Failed to create span")?;
    let diag = Diagnostic::error(span, "title", "msg");
    diag.report(&file, &mut std::io::stderr())?;

    Ok(())
}
