use crate::{SourceMap, SourceLocation, SourceSpan};

use std::{fmt::Display, io::Write};

// ---------------------------------------------------------------------------
// Support Functions
// ---------------------------------------------------------------------------

/// Reports a batch of diagnostics to a buffer.
///
/// The diagnostics must be from the same source file
/// and must be sorted in ascending order by their spans.
pub fn report_batch(
    file: &SourceMap,
    buffer: &mut impl Write,
    diagnostics: &[impl Into<Diagnostic> + Clone],
) -> std::io::Result<()> {
    let mut hint = 0;
    diagnostics
        .iter()
        .map(|d| d.clone().into())
        .try_for_each(|d| d.report_hint(file, &mut hint, buffer))
}

// ---------------------------------------------------------------------------
// Diagnostic
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    title: String,
    message: String,
    span: SourceSpan,
    level: DiagnosticLevel,
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
            level,
            title: title.into(),
            message: message.into(),
        }
    }

    pub fn note(span: SourceSpan, title: impl Into<String>, message: impl Into<String>) -> Self {
        Diagnostic::new(span, title, message, DiagnosticLevel::Note)
    }

    pub fn error(span: SourceSpan, title: impl Into<String>, message: impl Into<String>) -> Self {
        Diagnostic::new(span, title, message, DiagnosticLevel::Error)
    }

    pub fn warning(span: SourceSpan, title: impl Into<String>, message: impl Into<String>) -> Self {
        Diagnostic::new(span, title, message, DiagnosticLevel::Warning)
    }

    pub fn report(&self, file: &SourceMap, buffer: &mut impl Write) -> std::io::Result<()> {
        let location = file.locate(self.span).unwrap_or_default();
        self.report_internal(file, location, buffer)
    }

    pub fn report_hint(
        &self,
        file: &SourceMap,
        hint: &mut usize,
        buffer: &mut impl Write,
    ) -> std::io::Result<()> {
        let location = file.locate_hint(self.span, *hint).unwrap_or_default();
        *hint = location.line as usize - 1;
        self.report_internal(file, location, buffer)
    }

    fn report_internal(
        &self,
        file: &SourceMap,
        location: SourceLocation,
        buffer: &mut impl Write,
    ) -> std::io::Result<()> {
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
                self.span.len().saturating_sub(1),
                line_text.len().saturating_sub(location.column as usize)
            )),
        )?;

        // Middle Lines
        lines
            .iter()
            .skip(1)
            .enumerate()
            .take(lines.len().saturating_sub(2))
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
        if lines.len() > 1 {
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
                    "^".repeat((line_text.len() - line_text_leading_spaces).saturating_sub(rtrim))
                )?;
            }
        }

        // Message
        writeln!(buffer, " {}", self.message)?;

        Ok(())
    }
}

// ---------------------------------------------------------------------------
// DiagnosticLevel
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagnosticLevel {
    Note,
    Error,
    Warning,
}

impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Note => write!(f, "note"),
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
        }
    }
}
