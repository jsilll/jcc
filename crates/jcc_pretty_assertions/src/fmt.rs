use crate::diff::DiffOp;

use termcolor::{Color, ColorSpec, WriteColor};

pub fn diff(buf: &mut dyn WriteColor, left: &str, right: &str) -> std::io::Result<()> {
    let mut spec = ColorSpec::new();
    let llines: Vec<&str> = left.lines().collect();
    let rlines: Vec<&str> = right.lines().collect();
    let diff = crate::diff::diff(&llines, &rlines);
    spec.set_bold(true);
    buf.set_color(&spec)?;
    writeln!(buf, "\nAssertion failed: left != right\n")?;
    for op in diff {
        match op {
            DiffOp::Equal(line) => {
                buf.reset()?;
                writeln!(buf, " {}", line)?;
            }
            DiffOp::Delete(line) => {
                spec.clear();
                spec.set_fg(Some(Color::Red));
                buf.set_color(&spec)?;
                writeln!(buf, "-{}", line)?;
            }
            DiffOp::Insert(line) => {
                spec.clear();
                spec.set_fg(Some(Color::Green));
                buf.set_color(&spec)?;
                writeln!(buf, "+{}", line)?;
            }
        }
    }
    buf.reset()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct MockWriter {
        output: String,
    }

    impl std::io::Write for MockWriter {
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }

        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.output.push_str(&String::from_utf8_lossy(buf));
            Ok(buf.len())
        }
    }

    impl WriteColor for MockWriter {
        fn supports_color(&self) -> bool {
            true
        }

        fn reset(&mut self) -> std::io::Result<()> {
            self.output.push_str("[Reset]");
            Ok(())
        }

        fn set_color(&mut self, spec: &ColorSpec) -> std::io::Result<()> {
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

    fn emit_diff_to_string(left: &str, right: &str) -> String {
        let mut mock = MockWriter::default();
        diff(&mut mock, left, right).expect("diff logic failed");
        mock.output
    }

    #[test]
    fn identical() {
        let left = "hello\nworld";
        let right = "hello\nworld";
        let output = emit_diff_to_string(left, right);
        let expected = "\
[Default!]
Assertion failed: left != right

[Reset] hello
[Reset] world
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn with_changes() {
        let left = "line1\nline2\nline3";
        let right = "line1\nchanged\nline3";
        let output = emit_diff_to_string(left, right);
        let expected = "\
[Default!]
Assertion failed: left != right

[Reset] line1
[Red]-line2
[Green]+changed
[Reset] line3
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn delete_only() {
        let left = "foo\nbar";
        let right = "foo";

        let output = emit_diff_to_string(left, right);

        let expected = "\
[Default!]
Assertion failed: left != right

[Reset] foo
[Red]-bar
[Reset]";
        assert_eq!(output, expected);
    }

    #[test]
    fn insert_only() {
        let left = "foo";
        let right = "foo\nbar";
        let output = emit_diff_to_string(left, right);
        let expected = "\
[Default!]
Assertion failed: left != right

[Reset] foo
[Green]+bar
[Reset]";
        assert_eq!(output, expected);
    }
}
