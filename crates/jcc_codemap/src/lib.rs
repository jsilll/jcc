//! A lightweight diagnostic reporting library for compilers and interpreters.
//!
//! This crate provides everything you need to track source locations, manage multiple
//! source files, and emit beautiful, user-friendly diagnostic messages similar to
//! those produced by rustc and other modern compilers.
//!
//! # Features
//!
//! - **Source file management**: Track multiple source files with stable file IDs
//! - **Span tracking**: Represent regions of source code with byte positions
//! - **Location mapping**: Convert byte positions to human-readable line/column numbers
//! - **Diagnostic reporting**: Build and emit rich diagnostic messages with labels and notes
//! - **Unicode support**: Properly handles multi-byte UTF-8 characters in column calculations
//! - **Flexible output**: Emit diagnostics to any writer (stdout, stderr, strings, etc.)
//!
//! # Core Concepts
//!
//! ## Global Address Space
//!
//! All source files share a single virtual u32 address space. Each file is allocated a
//! unique, non-overlapping range. A [`Span`] therefore identifies both the file and the
//! byte range within it.
//!
//! **Limit**: the total size of all source files must not exceed 4 GiB ([`u32::MAX`] bytes).
//!
//! ## Spans
//!
//! A [`Span`] represents a contiguous region in the global address space using byte positions.
//! Spans are half-open intervals: `[start, end)`.
//!
//! ## Files
//!
//! The [`Files`] trait provides an interface for managing source files. The [`SimpleFiles`]
//! type is a basic in-memory implementation, but you can implement the trait yourself for
//! more advanced use cases like lazy loading or virtual file systems.
//!
//! ## Diagnostics
//!
//! A [`Diagnostic`] consists of:
//! - A severity level (error, warning, note, help)
//! - An optional error code
//! - A main message
//! - Zero or more labels pointing to spans in source files
//! - Zero or more additional notes
//!
//! ## Labels
//!
//! [`Label`]s attach messages to specific spans in your source code. They come in two styles:
//! - **Primary**: The main focus of the diagnostic (shown with `^^^`)
//! - **Secondary**: Additional context (shown with `---`)

pub mod byte;
pub mod file;
pub mod simple;
pub mod span;

#[cfg(test)]
pub mod testutil;

#[cfg(feature = "color")]
pub mod color;

#[cfg(feature = "color")]
use crate::color::ColorConfig;
use crate::{byte::BytePos, file::SourceFile, span::Span};

#[cfg(feature = "color")]
use termcolor::WriteColor;

use std::{collections::HashMap, io::Write};

/// A line and column position in a source file.
///
/// Both line and column are 1-indexed to match human expectations
/// and editor conventions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    /// 1-indexed line number
    pub line: u32,
    /// 1-indexed column number (in characters, not bytes)
    pub column: u32,
}

/// A human-readable location in a source file.
///
/// This type contains all the information needed to display
/// a diagnostic message, including the line and column numbers
/// and the text of the line(s) involved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location<'a> {
    /// The span within the source file
    pub span: Span,
    /// The line and column where the span starts
    pub line_col: LineCol,
    /// The text of the line(s) that the span covers
    pub line_text: &'a str,
}

/// A collection of source files, addressable by global byte position.
///
/// This is the main interface for managing multiple source files and looking up
/// location information for spans.
///
/// # Invariant for custom implementations
///
/// Every [`SourceFile`] returned by an implementation MUST have a unique,
/// non-overlapping range `[file.base(), file.base() + file.source().len())` in
/// the global address space. Violating this invariant produces incorrect diagnostic
/// output. See [`SimpleFiles`] for a correct reference implementation.
pub trait Files {
    /// Returns the source file whose global address range contains `pos`.
    ///
    /// Returns `None` if no file contains the given position (e.g., the position
    /// falls in a sentinel gap or beyond all loaded files).
    fn file_for_pos(&self, pos: BytePos) -> Option<&SourceFile>;

    /// Gets the source text slice for a global span.
    ///
    /// Returns `None` if no file contains the span's start position or the span
    /// is out of bounds within that file.
    fn slice(&self, span: Span) -> Option<&str> {
        self.file_for_pos(span.start())?.slice(span)
    }

    /// Gets a location for a global span.
    ///
    /// Returns `None` if no file contains the span's start position or the span
    /// is out of bounds within that file.
    fn location(&self, span: Span) -> Option<Location<'_>> {
        self.file_for_pos(span.start())?.location(span)
    }
}

/// The severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    /// A hint or suggestion.
    Help,
    /// An informational message.
    Note,
    /// A fatal error that prevents compilation.
    Error,
    /// A potential problem that should be addressed.
    Warning,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Severity::Help => write!(f, "help"),
            Severity::Note => write!(f, "note"),
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
        }
    }
}

/// The style of a label attached to a span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LabelStyle {
    /// The primary focus of the diagnostic.
    Primary,
    /// Additional context or related information.
    Secondary,
}

/// A label attached to a specific span in the source code.
///
/// Labels provide context about why a particular piece of code
/// is relevant to the diagnostic. The span must be a global span
/// (allocated through a [`Files`] database).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    /// The global span this label points to.
    pub span: Span,
    /// The style of this label (primary or secondary).
    pub style: LabelStyle,
    /// An optional message describing what's wrong with this span.
    pub message: Option<String>,
}

impl Label {
    /// Creates a new primary label at the given span.
    pub fn primary(span: Span) -> Self {
        Self {
            span,
            message: None,
            style: LabelStyle::Primary,
        }
    }

    /// Creates a new secondary label at the given span.
    pub fn secondary(span: Span) -> Self {
        Self {
            span,
            message: None,
            style: LabelStyle::Secondary,
        }
    }

    /// Adds a message to this label.
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }
}

/// A compiler-reported issue that has not yet been rendered into a user-facing diagnostic.
///
/// `Issue` represents the semantic form of a problem discovered during compilation.
/// It is independent of formatting concerns and is intended to be converted into a
/// [`Diagnostic`] for display to the user.
///
/// The span is a global span whose position implicitly encodes which file the
/// issue originates from.
///
/// The generic parameter `K` represents the kind of issue.
#[derive(Clone)]
pub struct Issue<K> {
    /// The kind of the issue.
    pub kind: K,
    /// The global span associated with this issue.
    pub span: Span,
}

impl<K> Issue<K> {
    /// Creates a new issue of the given kind at the given global span.
    #[inline]
    pub fn new(kind: K, span: Span) -> Self {
        Self { kind, span }
    }
}

/// A type that can be converted into a user-facing [`Diagnostic`].
///
/// Implement this trait for issue kinds to define how they are rendered
/// into structured diagnostic output.
pub trait IntoDiagnostic {
    /// Converts this issue kind into a fully rendered [`Diagnostic`],
    /// using the provided global source span.
    fn into_diagnostic(self, span: Span) -> Diagnostic;
}

impl<K: IntoDiagnostic> From<Issue<K>> for Diagnostic {
    /// Converts a semantic compiler [`Issue`] into a user-facing [`Diagnostic`].
    fn from(issue: Issue<K>) -> Self {
        issue.kind.into_diagnostic(issue.span)
    }
}

/// A diagnostic message with labels and notes.
///
/// Diagnostics use a builder pattern for easy construction
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    /// The main message describing the problem.
    pub message: String,
    /// The severity of this diagnostic.
    pub severity: Severity,
    /// Labels pointing to relevant spans in the source.
    pub labels: Vec<Label>,
    /// Additional notes that don't point to specific spans.
    pub notes: Vec<String>,
    /// An optional error code (e.g., "E0308").
    pub code: Option<String>,
}

impl Diagnostic {
    /// Creates a new diagnostic with the given severity.
    fn new(severity: Severity) -> Self {
        Self {
            severity,
            code: None,
            notes: Vec::new(),
            labels: Vec::new(),
            message: String::new(),
        }
    }

    /// Creates a new help diagnostic.
    pub fn help() -> Self {
        Self::new(Severity::Help)
    }

    /// Creates a new note diagnostic.
    pub fn note() -> Self {
        Self::new(Severity::Note)
    }

    /// Creates a new error diagnostic.
    pub fn error() -> Self {
        Self::new(Severity::Error)
    }

    /// Creates a new warning diagnostic.
    pub fn warning() -> Self {
        Self::new(Severity::Warning)
    }

    /// Adds a single label.
    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    /// Sets the error code.
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Adds a note.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Sets the main message.
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    /// Adds labels from any iterator.
    pub fn with_labels<I>(mut self, labels: I) -> Self
    where
        I: IntoIterator<Item = Label>,
    {
        self.labels.extend(labels);
        self
    }

    /// Adds notes from any iterator.
    pub fn with_notes<I, S>(mut self, notes: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator<Item = S>,
    {
        self.notes.extend(notes.into_iter().map(Into::into));
        self
    }

    /// Emits this diagnostic to stdout.
    ///
    /// # Errors
    ///
    /// If writing to stdout fails for any reason, an error is returned.
    pub fn emit_to_stdout<F: Files>(&self, files: &F) -> std::io::Result<()> {
        let mut stdout = std::io::stdout();
        self.emit(files, &mut stdout)
    }

    /// Emits this diagnostic to stderr.
    ///
    /// # Errors
    ///
    /// If writing to stderr fails for any reason, an error is returned.
    pub fn emit_to_stderr<F: Files>(&self, files: &F) -> std::io::Result<()> {
        let mut stderr = std::io::stderr();
        self.emit(files, &mut stderr)
    }

    /// Emits this diagnostic to a string using the provided files.
    ///
    /// # Errors
    ///
    /// If writing to the string fails for any reason, an error is returned.
    pub fn emit_to_string<F: Files>(&self, files: &F) -> Result<String, std::fmt::Error> {
        let mut output = Vec::new();
        self.emit(files, &mut output).map_err(|_| std::fmt::Error)?;
        String::from_utf8(output).map_err(|_| std::fmt::Error)
    }

    /// Emits this diagnostic to the given writer using the provided files.
    ///
    /// # Errors
    ///
    /// If writing to the writer fails for any reason, an error is returned.
    pub fn emit<F: Files>(&self, files: &F, writer: &mut impl Write) -> std::io::Result<()> {
        emit_diagnostic(files, self, writer)
    }

    /// Emits a diagnostic with colored output to stderr.
    ///
    /// # Errors
    ///
    /// If writing to stderr fails, an error is returned.
    #[cfg(feature = "color")]
    pub fn emit_colored_stdout<F: Files>(
        &self,
        files: &F,
        config: &ColorConfig,
    ) -> std::io::Result<()> {
        use termcolor::{ColorChoice, StandardStream};
        let mut stderr = StandardStream::stdout(ColorChoice::Auto);
        color::emit_diagnostic_colored(files, self, &mut stderr, config)
    }

    /// Emits a diagnostic with colored output to stderr.
    ///
    /// # Errors
    ///
    /// If writing to stderr fails, an error is returned.
    #[cfg(feature = "color")]
    pub fn emit_colored_stderr<F: Files>(
        &self,
        files: &F,
        config: &ColorConfig,
    ) -> std::io::Result<()> {
        use termcolor::{ColorChoice, StandardStream};
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        color::emit_diagnostic_colored(files, self, &mut stderr, config)
    }

    /// Emits this diagnostic with colored output to stderr.
    ///
    /// # Errors
    ///
    /// If writing to stderr fails, an error is returned.
    #[cfg(feature = "color")]
    pub fn emit_colored<F: Files>(
        &self,
        files: &F,
        writer: &mut impl WriteColor,
        config: &ColorConfig,
    ) -> std::io::Result<()> {
        color::emit_diagnostic_colored(files, self, writer, config)
    }
}

/// Emits a diagnostic to the given writer.
///
/// # Errors
///
/// If writing to the writer fails for any reason, an error is returned.
fn emit_diagnostic<F: Files>(
    files: &F,
    diagnostic: &Diagnostic,
    writer: &mut impl Write,
) -> std::io::Result<()> {
    write!(writer, "{}", diagnostic.severity)?;
    if let Some(code) = &diagnostic.code {
        write!(writer, "[{code}]")?;
    }
    writeln!(writer, ": {}", diagnostic.message)?;
    for (file, labels) in group_labels(files, &diagnostic.labels) {
        let name = labels
            .first()
            .and_then(|l| files.file_for_pos(l.span.start()))
            .map_or_else(|| format!("{file:?}"), |f| f.name().display().to_string());
        for label in labels {
            if let Some(location) = files.location(label.span) {
                emit_label(writer, &name, &location, label)?;
            }
        }
    }
    for note in &diagnostic.notes {
        writeln!(writer, "  = note: {note}")?;
    }
    Ok(())
}

/// Groups labels by file (keyed by a pointer to the source file) and sorts each group by span start.
pub fn group_labels<'a, F: Files>(
    files: &F,
    labels: &'a [Label],
) -> HashMap<*const SourceFile, Vec<&'a Label>> {
    let mut file_labels: HashMap<*const SourceFile, Vec<&'a Label>> = HashMap::new();
    for label in labels {
        if let Some(key) = files.file_for_pos(label.span.start()) {
            file_labels.entry(key).or_default().push(label);
        }
    }
    for labels in file_labels.values_mut() {
        labels.sort_by_key(|l| l.span.start());
    }
    file_labels
}

/// Emits a single label to the given writer.
///
/// # Errors
///
/// If writing to the writer fails for any reason, an error is returned.
fn emit_label(
    writer: &mut impl Write,
    file: &str,
    location: &Location,
    label: &Label,
) -> std::io::Result<()> {
    let LineCol { line, column } = location.line_col;
    let num_lines = location.line_text.lines().count();
    let last_line = line + u32::try_from(num_lines - 1).unwrap_or(0);
    let line_num_width = last_line.checked_ilog10().unwrap_or(0) as usize + 1;
    let empty = "";

    // Label header
    writeln!(
        writer,
        "  --> {file}:{line}:{column}\n{empty:line_num_width$} |",
    )?;

    let mut lines = location.line_text.lines().enumerate();

    if let Some((_, text)) = lines.next() {
        // First line: underline from column to end of span or line
        let col = column as usize;
        let underline_len = label
            .span
            .len()
            .max(1)
            .min(text.chars().count().saturating_sub(col) + 1);
        write!(
            writer,
            "{line:>line_num_width$} | {text}\n{empty:line_num_width$} |{empty:col$}"
        )?;
        match label.style {
            LabelStyle::Primary => writeln!(writer, "{empty:^>underline_len$}"),
            LabelStyle::Secondary => writeln!(writer, "{empty:->underline_len$}"),
        }?;
        if let Some(msg) = &label.message {
            writeln!(writer, "{empty:line_num_width$} |{empty:col$}{msg}")?;
        }
    }

    for (idx, text) in (&mut lines).take(num_lines.saturating_sub(2)) {
        // Middle lines: underline the entire line
        let trimmed_text = text.trim_start();
        let underline_len = trimmed_text.chars().count();
        let line_num = line + u32::try_from(idx).unwrap_or(0);
        let leading_spaces = text.len() - trimmed_text.len() + 1;
        write!(writer, "{line_num:>line_num_width$} | {text}\n{empty:line_num_width$} |{empty:leading_spaces$}")?;
        match label.style {
            LabelStyle::Primary => writeln!(writer, "{empty:^>underline_len$}"),
            LabelStyle::Secondary => writeln!(writer, "{empty:->underline_len$}"),
        }?;
    }

    if let Some((idx, text)) = lines.next() {
        // Last line: underline from start to end of span
        let line_num = line + u32::try_from(idx).unwrap_or(0);
        let span_end_index = (column as usize) + label.span.len();
        let leading_spaces = text.len() - text.trim_start().len();
        let line_start_index = location.line_text.len() - text.len();
        let underline_end_on_line = span_end_index.saturating_sub(line_start_index);
        let underline_len = underline_end_on_line
            .saturating_sub(leading_spaces)
            .min(text.chars().count() - leading_spaces)
            .max(1);
        let leading_spaces = leading_spaces + 1;
        write!(writer, "{line_num:>line_num_width$} | {text}\n{empty:line_num_width$} |{empty:leading_spaces$}")?;
        match label.style {
            LabelStyle::Primary => writeln!(writer, "{empty:^>underline_len$}"),
            LabelStyle::Secondary => writeln!(writer, "{empty:->underline_len$}"),
        }?;
    }

    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::testutil::create_test_files;

    #[test]
    fn severity_ordering() {
        assert!(Severity::Help < Severity::Note);
        assert!(Severity::Note < Severity::Error);
        assert!(Severity::Error < Severity::Warning);
    }

    #[test]
    fn severity_display() {
        assert_eq!(Severity::Help.to_string(), "help");
        assert_eq!(Severity::Note.to_string(), "note");
        assert_eq!(Severity::Error.to_string(), "error");
        assert_eq!(Severity::Warning.to_string(), "warning");
    }

    #[test]
    fn without_labels() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error().with_message("compilation failed");
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "error: compilation failed\n";
        assert_eq!(output, expected);
    }

    #[test]
    fn help() {
        let files = create_test_files();
        let diagnostic = Diagnostic::help()
            .with_message("this is a help message")
            .with_label(
                Label::primary(files.span(files.test_file, 4, 5))
                    .with_message("consider changing this"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
help: this is a help message
  --> test.rs:1:5
  |
1 | let x = 5;
  |     ^
  |     consider changing this
";
        assert_eq!(output, expected);
    }

    #[test]
    fn note() {
        let files = create_test_files();
        let diagnostic = Diagnostic::note()
            .with_message("this is a note message")
            .with_label(
                Label::primary(files.span(files.test_file, 4, 5)).with_message("just so you know"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
note: this is a note message
  --> test.rs:1:5
  |
1 | let x = 5;
  |     ^
  |     just so you know
";
        assert_eq!(output, expected);
    }

    #[test]
    fn error() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("variable not found")
            .with_label(
                Label::primary(files.span(files.test_file, 4, 5))
                    .with_message("not found in this scope"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error: variable not found
  --> test.rs:1:5
  |
1 | let x = 5;
  |     ^
  |     not found in this scope
";
        assert_eq!(output, expected);
    }

    #[test]
    fn warning() {
        let files = create_test_files();
        let diagnostic = Diagnostic::warning()
            .with_message("unused variable: `x`")
            .with_label(
                Label::primary(files.span(files.example_file, 20, 21))
                    .with_message("help: if this is intentional, prefix with underscore: `_x`"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
warning: unused variable: `x`
  --> example.rs:2:9
  |
2 |     let x = 5;
  |         ^
  |         help: if this is intentional, prefix with underscore: `_x`
";
        assert_eq!(output, expected);
    }

    #[test]
    fn with_code() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_code("E0425")
            .with_message("cannot find value `z` in this scope")
            .with_label(
                Label::primary(files.span(files.test_file, 4, 5))
                    .with_message("not found in this scope"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error[E0425]: cannot find value `z` in this scope
  --> test.rs:1:5
  |
1 | let x = 5;
  |     ^
  |     not found in this scope
";
        assert_eq!(output, expected);
    }

    #[test]
    fn with_notes() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("cannot borrow as mutable")
            .with_label(Label::primary(files.span(files.test_file, 0, 3)))
            .with_notes(vec![
                "consider changing this to be mutable: `mut x`",
                "see documentation on borrowing for more information",
            ]);
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error: cannot borrow as mutable
  --> test.rs:1:1
  |
1 | let x = 5;
  | ^^^
  = note: consider changing this to be mutable: `mut x`
  = note: see documentation on borrowing for more information
";
        assert_eq!(output, expected);
    }

    #[test]
    fn with_only_notes() {
        let files = create_test_files();
        let diagnostic = Diagnostic::warning()
            .with_message("potential issue detected")
            .with_note("this might cause problems")
            .with_note("consider reviewing your code");
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
warning: potential issue detected
  = note: this might cause problems
  = note: consider reviewing your code
";
        assert_eq!(output, expected);
    }

    #[test]
    fn empty_span() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("expected expression")
            .with_label(
                Label::primary(files.span(files.test_file, 3, 3))
                    .with_message("expected expression here"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error: expected expression
  --> test.rs:1:4
  |
1 | let x = 5;
  |    ^
  |    expected expression here
";
        assert_eq!(output, expected);
    }

    #[test]
    fn label_without_message() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("syntax error")
            .with_label(Label::primary(files.span(files.test_file, 4, 5)));
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error: syntax error
  --> test.rs:1:5
  |
1 | let x = 5;
  |     ^
";
        assert_eq!(output, expected);
    }

    #[test]
    fn unicode_handling() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("invalid character in identifier")
            .with_label(
                Label::primary(files.span(files.unicode_file, 4, 8))
                    .with_message("unexpected character"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error: invalid character in identifier
  --> unicode.rs:1:5
  |
1 | let café = \"☕\";
  |     ^^^^
  |     unexpected character
";
        assert_eq!(output, expected);
    }

    #[test]
    fn label_at_end_of_line() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("missing semicolon")
            .with_label(
                Label::primary(files.span(files.test_file, 9, 10))
                    .with_message("expected `;` here"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error: missing semicolon
  --> test.rs:1:10
  |
1 | let x = 5;
  |          ^
  |          expected `;` here
";
        assert_eq!(output, expected);
    }

    #[test]
    fn multiline_span() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("unclosed delimiter")
            .with_label(
                Label::primary(files.span(files.example_file, 0, 74))
                    .with_message("function body not closed"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        println!("{output}");
        let expected = "\
error: unclosed delimiter
  --> example.rs:1:1
  |
1 | fn main() {
  | ^^^^^^^^^^^
  | function body not closed
2 |     let x = 5;
  |     ^^^^^^^^^^
3 |     let y = x + \"hello\";
  |     ^^^^^^^^^^^^^^^^^^^^
4 |     println!(\"{}\", y);
  |     ^^^^^^^^^^^^^^^^^^
";
        assert_eq!(output, expected);
    }

    #[test]
    fn secondary_label() {
        let files = create_test_files();
        let diagnostic = Diagnostic::note()
            .with_message("related information")
            .with_label(
                Label::secondary(files.span(files.test_file, 4, 5)).with_message("defined here"),
            );
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
note: related information
  --> test.rs:1:5
  |
1 | let x = 5;
  |     -
  |     defined here
";
        assert_eq!(output, expected);
    }

    #[test]
    fn multiple_labels_same_file() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("mismatched types")
            .with_labels(vec![
                Label::primary(files.span(files.example_file, 37, 38))
                    .with_message("expected integer"),
                Label::secondary(files.span(files.example_file, 43, 50))
                    .with_message("this is a string"),
            ]);
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error: mismatched types
  --> example.rs:3:11
  |
3 |     let y = x + \"hello\";
  |           ^
  |           expected integer
  --> example.rs:3:17
  |
3 |     let y = x + \"hello\";
  |                 -------
  |                 this is a string
";
        assert_eq!(output, expected);
    }

    #[test]
    fn complex_diagnostic() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_code("E0308")
            .with_message("mismatched types")
            .with_labels(vec![
                Label::primary(files.span(files.example_file, 37, 38))
                    .with_message("expected `i32`, found `&str`"),
                Label::secondary(files.span(files.example_file, 20, 21))
                    .with_message("`x` is defined here as `i32`"),
            ])
            .with_note("expected type `i32`")
            .with_note("found type `&str`");
        let output = diagnostic.emit_to_string(&files.files).unwrap();
        let expected = "\
error[E0308]: mismatched types
  --> example.rs:2:9
  |
2 |     let x = 5;
  |         -
  |         `x` is defined here as `i32`
  --> example.rs:3:11
  |
3 |     let y = x + \"hello\";
  |           ^
  |           expected `i32`, found `&str`
  = note: expected type `i32`
  = note: found type `&str`
";
        assert_eq!(output, expected);
    }
}
