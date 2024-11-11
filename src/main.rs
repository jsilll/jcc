use anyhow::Context;
use clap::Parser;
use jcc::{reporting::Diagnostic, source_file::SourceFile};
use std::path::PathBuf;

#[derive(Parser)]
struct Args {
    pub path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let file = SourceFile::new(args.path).context("Failed to read file")?;
    let span = file.span(1..200).context("Failed to create span")?;
    let diag = Diagnostic::error(span, "title", "msg");
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
