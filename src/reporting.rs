use crate::source_file::{SourceFile, SourceSpan};
use std::{
    fmt::{self, Display},
    io::{Result, Write},
};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    span: SourceSpan,
    title: String,
    msg: String,
    level: DiagnosticLevel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagnosticLevel {
    Error,
    Warning,
    Note,
}

impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Error => write!(f, "Error"),
            Self::Warning => write!(f, "Warning"),
            Self::Note => write!(f, "Note"),
        }
    }
}

impl Diagnostic {
    pub fn error(span: SourceSpan, title: impl Into<String>, msg: impl Into<String>) -> Self {
        Self {
            span,
            title: title.into(),
            msg: msg.into(),
            level: DiagnosticLevel::Error,
        }
    }

    pub fn warning(span: SourceSpan, title: impl Into<String>, msg: impl Into<String>) -> Self {
        Self {
            span,
            title: title.into(),
            msg: msg.into(),
            level: DiagnosticLevel::Warning,
        }
    }

    pub fn note(span: SourceSpan, title: impl Into<String>, msg: impl Into<String>) -> Self {
        Self {
            span,
            title: title.into(),
            msg: msg.into(),
            level: DiagnosticLevel::Note,
        }
    }

    pub fn report(&self, file: &SourceFile, buffer: &mut impl Write) -> Result<()> {
        let start_location = file.locate(&self.span).unwrap();
        let last_line = start_location.line + start_location.line_text.lines().count() as u32 - 1;
        let digit_count = last_line.checked_ilog10().unwrap_or(0) + 1;

        writeln!(buffer, "{}: {}", self.level, self.title)?;
        writeln!(
            buffer,
            "{}--> {}:{}:{}",
            " ".repeat(digit_count as usize),
            file.path().display(),
            start_location.line,
            start_location.column
        )?;
        writeln!(buffer, "{} |", " ".repeat(digit_count as usize))?;

        writeln!(
            buffer,
            "{}{} | {}",
            " ".repeat(
                (digit_count - start_location.line.checked_ilog10().unwrap_or(0) - 1) as usize
            ),
            start_location.line,
            start_location.line_text.lines().next().unwrap_or(""),
        )?;
        write!(
            buffer,
            "{} |{}^{}",
            " ".repeat(digit_count as usize),
            " ".repeat(start_location.column as usize),
            "~".repeat(std::cmp::min(
                self.span.len() as usize,
                start_location.line_text.lines().next().unwrap().len(),
            )),
        )?;

        for (i, line) in start_location.line_text.lines().skip(1).enumerate() {
            writeln!(
                buffer,
                "\n{}{} | {}",
                " ".repeat(
                    (digit_count - start_location.line.checked_ilog10().unwrap_or(0) - 1) as usize
                ),
                start_location.line + i as u32 + 1,
                line
            )?;
            write!(
                buffer,
                "{} | {}",
                " ".repeat(digit_count as usize),
                "~".repeat(line.len())
            )?;
        }

        writeln!(buffer, " {}", self.msg)?;

        Ok(())
    }
}
