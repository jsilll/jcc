use std::{
    io,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    start: u32,
    end: u32,
}

impl SourceSpan {
    pub fn new(start: u32, end: u32) -> Option<Self> {
        match start <= end {
            false => None,
            true => Some(Self { start, end }),
        }
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

#[derive(Debug)]
pub struct SourceFile {
    data: String,
    path: PathBuf,
    lines: Vec<u32>,
}

#[derive(Debug, Clone)]
pub struct SourceLocation<'a> {
    pub line: u32,
    pub column: u32,
    pub line_text: &'a str,
}

impl SourceFile {
    pub fn new(path: impl AsRef<Path>) -> io::Result<Self> {
        let data = std::fs::read_to_string(path.as_ref())?;
        let mut lines = Vec::with_capacity(data.lines().count());
        lines.push(0);
        data.char_indices()
            .filter(|(_, c)| *c == '\n')
            .for_each(|(i, _)| {
                lines.push(i as u32 + 1);
            });
        Ok(Self {
            data,
            path: path.as_ref().to_path_buf(),
            lines,
        })
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    pub fn slice(&self, span: &SourceSpan) -> Option<&str> {
        self.data.get(span.start as usize..span.end as usize)
    }

    pub fn locate(&self, span: &SourceSpan) -> SourceLocation {
        let line = self
            .lines
            .binary_search(&span.start)
            .unwrap_or_else(|x| x - 1);
        let start = self.lines[line];
        let end = self
            .lines
            .get(line + 1)
            .copied()
            .unwrap_or(self.data.len() as u32);
        let column = span.start - start;
        let line_text = &self.data[start as usize..end as usize];
        SourceLocation {
            line: line as u32 + 1,
            column: column + 1,
            line_text,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_source_span() {
        assert!(SourceSpan::new(10, 5).is_none());
        let span = SourceSpan::new(5, 10).unwrap();
        assert_eq!(span.len(), 5);
        assert!(!span.is_empty());
    }

    #[test]
    fn test_source_file() -> io::Result<()> {
        let temp = NamedTempFile::new().unwrap();
        write(temp.path(), "Hello\nWorld\n").unwrap();

        let source = SourceFile::new(temp.path())?;
        assert_eq!(source.line_count(), 3);

        let span = SourceSpan::new(0, 5).unwrap();
        let location = source.locate(&span);
        assert_eq!(location.line, 1);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "Hello\n");

        let span = SourceSpan::new(6, 11).unwrap();
        let location = source.locate(&span);
        assert_eq!(location.line, 2);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "World\n");

        Ok(())
    }
}
