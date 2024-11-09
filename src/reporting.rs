use crate::source_file::{SourceFile, SourceSpan};

use std::io::{Result, Write};

#[derive(Debug)]
pub struct Diagnostic {
    span: SourceSpan,
    title: String,
    msg: String,
}

impl Diagnostic {
    pub fn new(span: SourceSpan, title: impl Into<String>, msg: impl Into<String>) -> Self {
        Self {
            span,
            title: title.into(),
            msg: msg.into(),
        }
    }

    pub fn report(&self, stream: &mut impl Write, file: &SourceFile) -> Result<()> {
        let location = file.locate(&self.span);
        let digit_count = location.line.checked_ilog10().unwrap_or(0) + 1;

        writeln!(stream, "Error: {}", self.title)?;
        writeln!(
            stream,
            "{}--> {}:{}:{}",
            " ".repeat(digit_count as usize),
            file.path().display(),
            location.line,
            location.column
        )?;
        writeln!(stream, "{} |", " ".repeat(digit_count as usize))?;
        writeln!(
            stream,
            "{} | {}",
            location.line,
            location.line_text.trim_end()
        )?;
        writeln!(
            stream,
            "{} |{}^{} {}",
            " ".repeat(digit_count as usize),
            " ".repeat(location.column as usize),
            "~".repeat(self.span.len() as usize - 1),
            self.msg
        )?;

        Ok(())
    }
}
