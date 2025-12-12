//! Colored diagnostic output support.
//!
//! This module provides colored terminal output for diagnostics, making errors and warnings
//! more visually distinct and easier to read. Colors are used to highlight severity levels,
//! labels, line numbers, and other diagnostic elements.
//!
//! # Feature Flag
//!
//! This module is only available when the `color` feature is enabled:
//!
//! ```toml
//! [dependencies]
//! jcc_codemap = { version = "...", features = ["color"] }
//! ```
//!
//! # Color Detection
//!
//! The library provides several preset configurations:
//!
//! - **Automatic detection**: Colors enabled if stderr is a TTY
//! - **Always enabled**: Colors always enabled
//! - **Colors disabled**: Colors always disabled
//!
//! # Color Scheme
//!
//! The default color scheme is designed to be readable and follows common conventions:
//!
//! - **Errors**: Red (bold)
//! - **Warnings**: Yellow (bold)
//! - **Notes**: Cyan (bold)
//! - **Help**: Green (bold)
//! - **Primary labels**: Red markers (`^^^`)
//! - **Secondary labels**: Blue markers (`---`)
//! - **Line numbers**: Blue (bold)
//!
//! # Terminal Compatibility
//!
//! This module uses the `termcolor` crate, which provides cross-platform support for
//! colored terminal output. It works correctly on:
//! - Unix terminals that support ANSI color codes
//! - Windows consoles (using native Windows Console API)
//! - Terminals that don't support colors (falls back to plain text)
//!
//! # Performance
//!
//! Colored output has minimal performance overhead compared to plain text output.
//! The main cost is the additional function calls to set colors, which are negligible
//! for typical diagnostic use cases.

#[cfg(feature = "color")]
use termcolor::{Color, ColorSpec, WriteColor};

#[cfg(feature = "color")]
use crate::{Diagnostic, Files, Label, Location};

/// Configuration for colored diagnostic output.
#[cfg(feature = "color")]
#[derive(Debug, Clone)]
pub struct ColorConfig {
    /// Color for error severity
    pub error: Color,
    /// Color for warning severity
    pub warning: Color,
    /// Color for note severity
    pub note: Color,
    /// Color for help severity
    pub help: Color,
    /// Color for primary labels
    pub primary: Color,
    /// Color for secondary labels
    pub secondary: Color,
    /// Color for line numbers
    pub line_number: Color,
}

#[cfg(feature = "color")]
impl Default for ColorConfig {
    fn default() -> Self {
        Self {
            note: Color::Cyan,
            error: Color::Red,
            help: Color::Green,
            primary: Color::Red,
            warning: Color::Yellow,
            secondary: Color::Blue,
            line_number: Color::Blue,
        }
    }
}

#[cfg(feature = "color")]
struct ColorSpecConfig {
    /// Color specification for error severity
    error: ColorSpec,
    /// Color specification for warning severity
    warning: ColorSpec,
    /// Color specification for note severity
    note: ColorSpec,
    /// Color specification for help severity
    help: ColorSpec,
    /// Color specification for primary labels
    primary: ColorSpec,
    /// Color specification for secondary labels
    secondary: ColorSpec,
    /// Color specification for line numbers
    line_number: ColorSpec,
}

impl From<&ColorConfig> for ColorSpecConfig {
    fn from(c: &ColorConfig) -> Self {
        let mut s = Self {
            note: ColorSpec::new(),
            help: ColorSpec::new(),
            error: ColorSpec::new(),
            warning: ColorSpec::new(),
            primary: ColorSpec::new(),
            secondary: ColorSpec::new(),
            line_number: ColorSpec::new(),
        };
        s.note.set_fg(Some(c.note)).set_bold(true);
        s.help.set_fg(Some(c.help)).set_bold(true);
        s.error.set_fg(Some(c.error)).set_bold(true);
        s.warning.set_fg(Some(c.warning)).set_bold(true);
        s.primary.set_fg(Some(c.primary)).set_bold(true);
        s.secondary.set_fg(Some(c.secondary)).set_bold(true);
        s.line_number.set_fg(Some(c.line_number)).set_bold(true);
        s
    }
}

/// Emits a diagnostic with colored output to a writer.
///
/// # Errors
///
/// If writing to the writer fails, an error is returned.
#[cfg(feature = "color")]
pub fn emit_diagnostic_colored<F: Files, W: WriteColor>(
    files: &F,
    diagnostic: &Diagnostic,
    writer: &mut W,
    config: &ColorConfig,
) -> std::io::Result<()> {
    use crate::{group_labels, Severity};
    let config = ColorSpecConfig::from(config);
    let severity_color = match diagnostic.severity {
        Severity::Note => &config.note,
        Severity::Help => &config.help,
        Severity::Error => &config.error,
        Severity::Warning => &config.warning,
    };
    writer.set_color(severity_color)?;
    write!(writer, "{}", diagnostic.severity)?;
    if let Some(code) = &diagnostic.code {
        write!(writer, "[{code}]")?;
    }
    writer.reset()?;
    writeln!(writer, ": {}", diagnostic.message)?;
    for (file, labels) in group_labels(&diagnostic.labels) {
        let name = files
            .name(file)
            .map_or_else(|| format!("{file:?}"), |p| p.display().to_string());
        for label in labels {
            if let Some(location) = files.location(file, label.span) {
                emit_label_colored(writer, &name, &location, label, &config)?;
            }
        }
    }
    for note in &diagnostic.notes {
        writer.set_color(&config.note)?;
        write!(writer, "  = note:")?;
        writer.reset()?;
        writeln!(writer, " {note}")?;
    }
    Ok(())
}

/// Emits a single label with colored output to a writer.
///
/// # Errors
///
/// If writing to the writer fails for any reason, an error is returned.
#[cfg(feature = "color")]
fn emit_label_colored<W: WriteColor>(
    writer: &mut W,
    file: &str,
    location: &Location,
    label: &Label,
    config: &ColorSpecConfig,
) -> std::io::Result<()> {
    use crate::{LabelStyle, LineCol};

    let LineCol { line, column } = location.line_col;
    let num_lines = location.line_text.lines().count();
    let last_line = line + u32::try_from(num_lines - 1).unwrap_or(0);
    let line_num_width = last_line.checked_ilog10().unwrap_or(0) as usize + 1;
    let empty = "";

    let marker_color = match label.style {
        LabelStyle::Primary => &config.primary,
        LabelStyle::Secondary => &config.secondary,
    };

    write!(writer, "  ")?;
    writer.set_color(&config.line_number)?;
    write!(writer, "-->")?;
    writer.reset()?;
    writeln!(writer, " {file}:{line}:{column}")?;

    writer.set_color(&config.line_number)?;
    writeln!(writer, "{empty:line_num_width$} |")?;
    writer.reset()?;

    let mut lines = location.line_text.lines().enumerate();

    if let Some((_, text)) = lines.next() {
        // First line: underline from column to end of span or line
        let col = column as usize;
        let underline_len = label
            .span
            .len()
            .max(1)
            .min(text.chars().count().saturating_sub(col) + 1);
        writer.set_color(&config.line_number)?;
        write!(writer, "{line:>line_num_width$} |")?;
        writer.reset()?;
        writeln!(writer, " {text}")?;
        writer.set_color(&config.line_number)?;
        write!(writer, "{empty:line_num_width$} |")?;
        writer.reset()?;
        write!(writer, "{empty:col$}")?;
        writer.set_color(marker_color)?;
        match label.style {
            LabelStyle::Primary => writeln!(writer, "{empty:^>underline_len$}")?,
            LabelStyle::Secondary => writeln!(writer, "{empty:->underline_len$}")?,
        }
        writer.reset()?;
        if let Some(msg) = &label.message {
            writer.set_color(&config.line_number)?;
            write!(writer, "{empty:line_num_width$} |")?;
            writer.reset()?;
            write!(writer, "{empty:col$}")?;
            writer.set_color(marker_color)?;
            writeln!(writer, "{msg}")?;
            writer.reset()?;
        }
    }

    for (idx, text) in (&mut lines).take(num_lines.saturating_sub(2)) {
        // Middle lines: underline the entire line
        let trimmed_text = text.trim_start();
        let underline_len = trimmed_text.chars().count();
        let line_num = line + u32::try_from(idx).unwrap_or(0);
        let leading_spaces = text.len() - trimmed_text.len() + 1;
        writer.set_color(&config.line_number)?;
        write!(writer, "{line_num:>line_num_width$} |")?;
        writer.reset()?;
        writeln!(writer, " {text}")?;
        writer.set_color(&config.line_number)?;
        write!(writer, "{empty:line_num_width$} |")?;
        writer.reset()?;
        write!(writer, "{empty:leading_spaces$}")?;
        writer.set_color(marker_color)?;
        match label.style {
            LabelStyle::Primary => writeln!(writer, "{empty:^>underline_len$}")?,
            LabelStyle::Secondary => writeln!(writer, "{empty:->underline_len$}")?,
        }
        writer.reset()?;
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
        writer.set_color(&config.line_number)?;
        write!(writer, "{line_num:>line_num_width$} |")?;
        writer.reset()?;
        writeln!(writer, " {text}")?;
        writer.set_color(&config.line_number)?;
        write!(writer, "{empty:line_num_width$} |")?;
        writer.reset()?;
        write!(writer, "{empty:leading_spaces$}")?;
        writer.set_color(marker_color)?;
        match label.style {
            LabelStyle::Primary => writeln!(writer, "{empty:^>underline_len$}")?,
            LabelStyle::Secondary => writeln!(writer, "{empty:->underline_len$}")?,
        }
        writer.reset()?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{file::FileId, span::Span, SimpleFiles};

    use termcolor::{Color, ColorSpec, WriteColor};

    use std::io;

    #[derive(Default)]
    struct MockWriter {
        output: String,
    }

    impl io::Write for MockWriter {
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }

        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.output.push_str(&String::from_utf8_lossy(buf));
            Ok(buf.len())
        }
    }

    impl WriteColor for MockWriter {
        fn supports_color(&self) -> bool {
            true
        }

        fn reset(&mut self) -> io::Result<()> {
            self.output.push_str("[Reset]");
            Ok(())
        }

        fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
            let color_name = if let Some(c) = spec.fg() {
                match c {
                    Color::Red => "Red",
                    Color::Green => "Green",
                    Color::Yellow => "Yellow",
                    Color::Blue => "Blue",
                    Color::Cyan => "Cyan",
                    Color::Magenta => "Magenta",
                    Color::White => "White",
                    Color::Black => "Black",
                    _ => "Color",
                }
            } else {
                "Default"
            };
            let style = if spec.bold() || spec.intense() {
                "!"
            } else {
                ""
            };
            self.output.push_str(&format!("[{}{}]", color_name, style));
            Ok(())
        }
    }

    struct TestFiles {
        files: SimpleFiles,
        test_file: FileId,
        example_file: FileId,
        unicode_file: FileId,
    }

    fn create_test_files() -> TestFiles {
        let mut files = SimpleFiles::new();
        let test_file = files.add("test.rs", "let x = 5;".to_string());
        let unicode_file = files.add("unicode.rs", "let café = \"☕\";".to_string());
        let example_file = files.add(
            "example.rs",
            r#"fn main() {
    let x = 5;
    let y = x + "hello";
    println!("{}", y);
}"#
            .to_string(),
        );
        TestFiles {
            files,
            test_file,
            example_file,
            unicode_file,
        }
    }

    fn emit_colored_to_string(files: &SimpleFiles, diagnostic: &Diagnostic) -> String {
        let config = ColorConfig::default();
        let mut writer = MockWriter::default();
        emit_diagnostic_colored(files, diagnostic, &mut writer, &config).unwrap();
        writer.output
    }

    #[test]
    fn test_without_labels() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error().with_message("compilation failed");
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "[Red!]error[Reset]: compilation failed\n";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_help() {
        let files = create_test_files();
        let diagnostic = Diagnostic::help()
            .with_message("this is a help message")
            .with_label(
                Label::primary(files.test_file, Span::new(4u32, 5u32).unwrap())
                    .with_message("consider changing this"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Green!]help[Reset]: this is a help message
  [Blue!]-->[Reset] test.rs:1:5
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]     [Red!]^
[Reset][Blue!]  |[Reset]     [Red!]consider changing this
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_note() {
        let files = create_test_files();
        let diagnostic = Diagnostic::note()
            .with_message("this is a note message")
            .with_label(
                Label::primary(files.test_file, Span::new(4u32, 5u32).unwrap())
                    .with_message("just so you know"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Cyan!]note[Reset]: this is a note message
  [Blue!]-->[Reset] test.rs:1:5
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]     [Red!]^
[Reset][Blue!]  |[Reset]     [Red!]just so you know
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_error() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("variable not found")
            .with_label(
                Label::primary(files.test_file, Span::new(4u32, 5u32).unwrap())
                    .with_message("not found in this scope"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: variable not found
  [Blue!]-->[Reset] test.rs:1:5
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]     [Red!]^
[Reset][Blue!]  |[Reset]     [Red!]not found in this scope
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_warning() {
        let files = create_test_files();
        let diagnostic = Diagnostic::warning()
            .with_message("unused variable: `x`")
            .with_label(
                Label::primary(files.example_file, Span::new(20u32, 21u32).unwrap())
                    .with_message("help: if this is intentional, prefix with underscore: `_x`"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Yellow!]warning[Reset]: unused variable: `x`
  [Blue!]-->[Reset] example.rs:2:9
[Blue!]  |
[Reset][Blue!]2 |[Reset]     let x = 5;
[Blue!]  |[Reset]         [Red!]^
[Reset][Blue!]  |[Reset]         [Red!]help: if this is intentional, prefix with underscore: `_x`
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_code() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_code("E0425")
            .with_message("cannot find value `z` in this scope")
            .with_label(
                Label::primary(files.test_file, Span::new(4u32, 5u32).unwrap())
                    .with_message("not found in this scope"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[E0425][Reset]: cannot find value `z` in this scope
  [Blue!]-->[Reset] test.rs:1:5
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]     [Red!]^
[Reset][Blue!]  |[Reset]     [Red!]not found in this scope
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_notes() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("cannot borrow as mutable")
            .with_label(Label::primary(
                files.test_file,
                Span::new(0u32, 3u32).unwrap(),
            ))
            .with_note("consider changing this to be mutable: `mut x`")
            .with_note("see documentation on borrowing for more information");
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: cannot borrow as mutable
  [Blue!]-->[Reset] test.rs:1:1
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset] [Red!]^^^
[Reset][Cyan!]  = note:[Reset] consider changing this to be mutable: `mut x`
[Cyan!]  = note:[Reset] see documentation on borrowing for more information
";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_only_notes() {
        let files = create_test_files();
        let diagnostic = Diagnostic::warning()
            .with_message("potential issue detected")
            .with_note("this might cause problems")
            .with_note("consider reviewing your code");
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Yellow!]warning[Reset]: potential issue detected
[Cyan!]  = note:[Reset] this might cause problems
[Cyan!]  = note:[Reset] consider reviewing your code
";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_empty_span() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("expected expression")
            .with_label(
                Label::primary(files.test_file, Span::new(3u32, 3u32).unwrap())
                    .with_message("expected expression here"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: expected expression
  [Blue!]-->[Reset] test.rs:1:4
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]    [Red!]^
[Reset][Blue!]  |[Reset]    [Red!]expected expression here
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_label_without_message() {
        let files = create_test_files();
        let diagnostic =
            Diagnostic::error()
                .with_message("syntax error")
                .with_label(Label::primary(
                    files.test_file,
                    Span::new(4u32, 5u32).unwrap(),
                ));
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: syntax error
  [Blue!]-->[Reset] test.rs:1:5
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]     [Red!]^
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_unicode_handling() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("invalid character in identifier")
            .with_label(
                Label::primary(files.unicode_file, Span::new(4u32, 8u32).unwrap())
                    .with_message("unexpected character"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: invalid character in identifier
  [Blue!]-->[Reset] unicode.rs:1:5
[Blue!]  |
[Reset][Blue!]1 |[Reset] let café = \"☕\";
[Blue!]  |[Reset]     [Red!]^^^^
[Reset][Blue!]  |[Reset]     [Red!]unexpected character
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_label_at_end_of_line() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("missing semicolon")
            .with_label(
                Label::primary(files.test_file, Span::new(9u32, 10u32).unwrap())
                    .with_message("expected `;` here"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: missing semicolon
  [Blue!]-->[Reset] test.rs:1:10
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]          [Red!]^
[Reset][Blue!]  |[Reset]          [Red!]expected `;` here
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_multiline_span() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("unclosed delimiter")
            .with_label(
                Label::primary(files.example_file, Span::new(0u32, 74u32).unwrap())
                    .with_message("function body not closed"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: unclosed delimiter
  [Blue!]-->[Reset] example.rs:1:1
[Blue!]  |
[Reset][Blue!]1 |[Reset] fn main() {
[Blue!]  |[Reset] [Red!]^^^^^^^^^^^
[Reset][Blue!]  |[Reset] [Red!]function body not closed
[Reset][Blue!]2 |[Reset]     let x = 5;
[Blue!]  |[Reset]     [Red!]^^^^^^^^^^
[Reset][Blue!]3 |[Reset]     let y = x + \"hello\";
[Blue!]  |[Reset]     [Red!]^^^^^^^^^^^^^^^^^^^^
[Reset][Blue!]4 |[Reset]     println!(\"{}\", y);
[Blue!]  |[Reset]     [Red!]^^^^^^^^^^^^^^^^^^
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_secondary_label() {
        let files = create_test_files();
        let diagnostic = Diagnostic::note()
            .with_message("related information")
            .with_label(
                Label::secondary(files.test_file, Span::new(4u32, 5u32).unwrap())
                    .with_message("defined here"),
            );
        let output = emit_colored_to_string(&files.files, &diagnostic);
        // Secondary labels usually use Cyan or Blue to distinguish from the primary color
        let expected = "\
[Cyan!]note[Reset]: related information
  [Blue!]-->[Reset] test.rs:1:5
[Blue!]  |
[Reset][Blue!]1 |[Reset] let x = 5;
[Blue!]  |[Reset]     [Blue!]-
[Reset][Blue!]  |[Reset]     [Blue!]defined here
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_multiple_labels_same_file() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_message("mismatched types")
            .with_labels(vec![
                Label::primary(files.example_file, Span::new(37u32, 38u32).unwrap())
                    .with_message("expected integer"),
                Label::secondary(files.example_file, Span::new(43u32, 50u32).unwrap())
                    .with_message("this is a string"),
            ]);
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[Reset]: mismatched types
  [Blue!]-->[Reset] example.rs:3:11
[Blue!]  |
[Reset][Blue!]3 |[Reset]     let y = x + \"hello\";
[Blue!]  |[Reset]           [Red!]^
[Reset][Blue!]  |[Reset]           [Red!]expected integer
[Reset]  [Blue!]-->[Reset] example.rs:3:17
[Blue!]  |
[Reset][Blue!]3 |[Reset]     let y = x + \"hello\";
[Blue!]  |[Reset]                 [Blue!]-------
[Reset][Blue!]  |[Reset]                 [Blue!]this is a string
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_complex_diagnostic() {
        let files = create_test_files();
        let diagnostic = Diagnostic::error()
            .with_code("E0308")
            .with_message("mismatched types")
            .with_labels(vec![
                Label::primary(files.example_file, Span::new(37u32, 38u32).unwrap())
                    .with_message("expected `i32`, found `&str`"),
                Label::secondary(files.example_file, Span::new(20u32, 21u32).unwrap())
                    .with_message("`x` is defined here as `i32`"),
            ])
            .with_note("expected type `i32`")
            .with_note("found type `&str`");
        let output = emit_colored_to_string(&files.files, &diagnostic);
        let expected = "\
[Red!]error[E0308][Reset]: mismatched types
  [Blue!]-->[Reset] example.rs:2:9
[Blue!]  |
[Reset][Blue!]2 |[Reset]     let x = 5;
[Blue!]  |[Reset]         [Blue!]-
[Reset][Blue!]  |[Reset]         [Blue!]`x` is defined here as `i32`
[Reset]  [Blue!]-->[Reset] example.rs:3:11
[Blue!]  |
[Reset][Blue!]3 |[Reset]     let y = x + \"hello\";
[Blue!]  |[Reset]           [Red!]^
[Reset][Blue!]  |[Reset]           [Red!]expected `i32`, found `&str`
[Reset][Cyan!]  = note:[Reset] expected type `i32`
[Cyan!]  = note:[Reset] found type `&str`
";
        assert_eq!(output, expected);
    }
}
