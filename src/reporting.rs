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
pub enum DiagnosticLevel {
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

    pub fn report(&self, file: &SourceFile, stream: &mut impl Write) -> Result<()> {
        let location = file.locate(&self.span);
        let digit_count = location.line.checked_ilog10().unwrap_or(0) + 1;

        writeln!(stream, "{}: {}", self.level, self.title)?;
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
