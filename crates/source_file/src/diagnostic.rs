use crate::{SourceFile, SourceSpan};
use std::{
    fmt::{self, Display},
    io::{Result, Write},
};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    span: SourceSpan,
    title: String,
    message: String,
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
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
            Self::Note => write!(f, "note"),
        }
    }
}

impl Diagnostic {
    fn new(
        span: SourceSpan,
        title: impl Into<String>,
        message: impl Into<String>,
        level: DiagnosticLevel,
    ) -> Self {
        Self {
            span,
            title: title.into(),
            message: message.into(),
            level,
        }
    }
    pub fn error(span: SourceSpan, title: impl Into<String>, message: impl Into<String>) -> Self {
        Diagnostic::new(span, title, message, DiagnosticLevel::Error)
    }

    pub fn warning(span: SourceSpan, title: impl Into<String>, message: impl Into<String>) -> Self {
        Diagnostic::new(span, title, message, DiagnosticLevel::Warning)
    }

    pub fn note(span: SourceSpan, title: impl Into<String>, message: impl Into<String>) -> Self {
        Diagnostic::new(span, title, message, DiagnosticLevel::Note)
    }

    pub fn report(&self, file: &SourceFile, buffer: &mut impl Write) -> Result<()> {
        fn count_digits(n: u32) -> u32 {
            n.checked_ilog10().unwrap_or(0) + 1
        }
        fn count_leading_spaces(s: &str) -> usize {
            let mut spaces = 0;
            for c in s.chars() {
                if c == ' ' {
                    spaces += 1;
                } else {
                    break;
                }
            }
            spaces
        }

        let location = file.locate(self.span).unwrap_or_default();
        let lines = location.line_text.lines().collect::<Vec<_>>();
        let max_digits = count_digits(location.line + lines.len() as u32 - 1) as usize;

        // Title + Source File Location + First Line
        let line_text = lines.first().copied().unwrap_or("");
        let line_digits = count_digits(location.line) as usize;
        write!(
            buffer,
            "{}: {}\n{}--> {}:{}:{}\n{} |\n{}{} | {}\n{} |{}^{}",
            self.level,
            self.title,
            " ".repeat(max_digits),
            file.path().display(),
            location.line,
            location.column,
            " ".repeat(max_digits),
            " ".repeat(max_digits - line_digits),
            location.line,
            line_text,
            " ".repeat(max_digits),
            " ".repeat(location.column as usize),
            "^".repeat(std::cmp::min(
                self.span.len().checked_sub(1).unwrap_or(0),
                line_text
                    .len()
                    .checked_sub(location.column as usize)
                    .unwrap_or(0)
            )),
        )?;

        // Middle Lines
        lines
            .iter()
            .skip(1)
            .enumerate()
            .take(lines.len().checked_sub(2).unwrap_or(0))
            .map(|(index, line_text)| {
                let line = location.line + index as u32 + 1;
                (
                    line,
                    count_digits(line) as usize,
                    *line_text,
                    count_leading_spaces(line_text),
                )
            })
            .try_for_each(|(line, line_digits, line_text, line_leading_spaces)| {
                write!(
                    buffer,
                    "\n{}{} | {}\n{} | {}{}",
                    " ".repeat(max_digits - line_digits),
                    line,
                    line_text,
                    " ".repeat(max_digits),
                    " ".repeat(line_leading_spaces),
                    "^".repeat(line_text.len() - line_leading_spaces),
                )
            })?;

        // Last Line
        if lines.len() >= 2 {
            if let Some(line_text) = lines.last().copied() {
                let line = location.line + lines.len() as u32 - 1;
                let line_digits = count_digits(line) as usize;
                let line_text_leading_spaces = count_leading_spaces(line_text);
                let rtrim = location.line_text.len() - (location.column as usize + self.span.len());
                write!(
                    buffer,
                    "\n{}{} | {}\n{} | {}{}",
                    " ".repeat(max_digits - line_digits),
                    line,
                    line_text,
                    " ".repeat(max_digits),
                    " ".repeat(line_text_leading_spaces),
                    "^".repeat(
                        (line_text.len() - line_text_leading_spaces)
                            .checked_sub(rtrim)
                            .unwrap_or(0)
                    )
                )?;
            }
        }

        // Message
        writeln!(buffer, " {}", self.message)?;

        Ok(())
    }
}
