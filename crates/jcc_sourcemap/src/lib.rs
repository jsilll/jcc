pub mod diag;

use std::{
    ops::{Add, Range, Sub},
    path::{Path, PathBuf},
};

// ---------------------------------------------------------------------------
// SourceLocation
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation<'a> {
    pub line: u32,
    pub column: u32,
    pub line_text: &'a str,
}

impl<'a> Default for SourceLocation<'a> {
    fn default() -> Self {
        Self {
            line: 1,
            column: 1,
            line_text: "",
        }
    }
}

// ---------------------------------------------------------------------------
// SourceSpan
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SourceSpan {
    start: u32,
    end: u32,
}

impl Add<u32> for SourceSpan {
    type Output = Self;

    fn add(self, offset: u32) -> Self {
        Self {
            start: self.start + offset,
            end: self.end + offset,
        }
    }
}

impl Sub<u32> for SourceSpan {
    type Output = Self;

    fn sub(self, offset: u32) -> Self {
        Self {
            start: self.start.saturating_sub(offset),
            end: self.end.saturating_sub(offset),
        }
    }
}

impl From<SourceSpan> for Range<u32> {
    fn from(val: SourceSpan) -> Self {
        Range {
            start: val.start,
            end: val.end,
        }
    }
}

impl SourceSpan {
    fn new(range: impl Into<Range<u32>>) -> Option<Self> {
        let Range { start, end } = range.into();
        if start > end {
            None
        } else {
            Some(Self { start, end })
        }
    }

    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

// ---------------------------------------------------------------------------
// SourceMap
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMap {
    offset: u32,
    data: String,
    path: PathBuf,
    lines: Vec<u32>,
}

impl SourceMap {
    pub fn new(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let data = std::fs::read_to_string(path.as_ref())?;

        let estimated = data.len() / 80;
        let mut lines = Vec::with_capacity(estimated);

        lines.push(0);
        data.bytes()
            .enumerate()
            .filter(|(_, b)| *b == b'\n')
            .for_each(|(i, _)| {
                lines.push(i as u32 + 1);
            });
        lines.shrink_to_fit();

        Ok(Self {
            data,
            lines,
            offset: 0,
            path: path.as_ref().to_path_buf(),
        })
    }

    #[inline]
    pub fn data(&self) -> &str {
        &self.data
    }

    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[inline]
    pub fn end_span(&self) -> SourceSpan {
        let end = self.offset + self.data.len() as u32;
        SourceSpan {
            start: end.saturating_sub(1),
            end,
        }
    }

    #[inline]
    pub fn slice(&self, range: impl Into<Range<u32>>) -> Option<&str> {
        let range = range.into();
        self.data.get(range.start as usize..range.end as usize)
    }

    #[inline]
    pub fn span(&self, range: impl Into<Range<u32>>) -> Option<SourceSpan> {
        SourceSpan::new(range).map(|span| span + self.offset)
    }

    pub fn locate(&self, span: SourceSpan) -> Option<SourceLocation> {
        let span = span - self.offset;
        let index_first = self
            .lines
            .binary_search(&span.start)
            .unwrap_or_else(|x| x - 1);
        let index_last = self
            .lines
            .get(index_first..)?
            .binary_search(&span.end)
            .unwrap_or_else(|x| x)
            + index_first;
        self.locate_internal(span, index_first, index_last)
    }

    pub fn locate_hint(&self, span: SourceSpan, hint: usize) -> Option<SourceLocation> {
        let span = span - self.offset;
        let index_first = self
            .lines
            .get(hint..)?
            .binary_search(&span.start)
            .unwrap_or_else(|x| x - 1)
            + hint;
        let index_last = self
            .lines
            .get(index_first..)?
            .binary_search(&span.end)
            .unwrap_or_else(|x| x)
            + index_first;
        self.locate_internal(span, index_first, index_last)
    }

    fn locate_internal(
        &self,
        span: SourceSpan,
        index_first: usize,
        index_last: usize,
    ) -> Option<SourceLocation> {
        let start_offset = self.lines.get(index_first).copied()? as usize;
        let end_offset = self.lines.get(index_last).copied()? as usize;
        let column = self
            .data
            .get(start_offset..span.start as usize)?
            .chars()
            .count() as u32
            + 1;
        let line_text = &self.data.get(start_offset..end_offset)?;
        Some(SourceLocation {
            line: index_first as u32 + 1,
            column,
            line_text,
        })
    }
}

// ---------------------------------------------------------------------------
// SourceDb
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct SourceDb {
    files: Vec<SourceMap>,
}

impl SourceDb {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn files(&self) -> &[SourceMap] {
        self.files.as_slice()
    }

    pub fn add(&mut self, mut file: SourceMap) {
        file.offset = self
            .files
            .last()
            .map(|file| file.offset + file.data.len() as u32)
            .unwrap_or(0);
        self.files.push(file)
    }

    pub fn locate(&self, span: SourceSpan) -> Option<SourceLocation> {
        let index = self
            .files
            .binary_search_by_key(&span.start, |file| file.offset)
            .unwrap_or_else(|x| x - 1);
        self.files.get(index)?.locate(span)
    }

    pub fn locate_hint(&self, span: SourceSpan, hint: usize) -> Option<SourceLocation> {
        let index = self
            .files
            .binary_search_by_key(&span.start, |file| file.offset)
            .unwrap_or_else(|x| x - 1);
        self.files.get(index)?.locate_hint(span, hint)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_source_span() {
        assert!(SourceSpan::new(10..5).is_none());
        let span = SourceSpan::new(5..10).unwrap();
        assert_eq!(span.len(), 5);
    }

    #[test]
    fn test_source_file() {
        let temp = NamedTempFile::new().unwrap();
        write(temp.path(), "Hello\nWorld\n").unwrap();

        let source = SourceMap::new(temp.path()).unwrap();

        let span = source.span(0..5).unwrap();
        let location = source.locate(span).unwrap();
        assert_eq!(location.line, 1);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "Hello\n");

        let span = source.span(6..11).unwrap();
        let location = source.locate(span).unwrap();
        assert_eq!(location.line, 2);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "World\n");
    }

    #[test]
    fn test_source_db() {
        let temp1 = NamedTempFile::new().unwrap();
        write(temp1.path(), "Hello\nWorld\n").unwrap();

        let temp2 = NamedTempFile::new().unwrap();
        write(temp2.path(), "Rust\nForever\n").unwrap();

        let mut db = SourceDb::new();
        db.add(SourceMap::new(temp1.path()).unwrap());
        db.add(SourceMap::new(temp2.path()).unwrap());

        let source = db.files().first().unwrap();

        let span = source.span(0..5).unwrap();
        let location = source.locate(span).unwrap();
        assert_eq!(location.line, 1);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "Hello\n");
        assert_eq!(location, db.locate(span).unwrap());

        let span = source.span(6..11).unwrap();
        let location = source.locate(span).unwrap();
        assert_eq!(location.line, 2);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "World\n");
        assert_eq!(location, db.locate(span).unwrap());

        let span = source.span(0..30).unwrap();
        assert_eq!(source.locate(span), None);
        assert_eq!(db.locate(span), None);

        let source = db.files().last().unwrap();

        let span = source.span(0..4).unwrap();
        let location = source.locate(span).unwrap();
        assert_eq!(location.line, 1);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "Rust\n");
        assert_eq!(location, db.locate(span).unwrap());

        let span = source.span(5..12).unwrap();
        let location = source.locate(span).unwrap();
        assert_eq!(location.line, 2);
        assert_eq!(location.column, 1);
        assert_eq!(location.line_text, "Forever\n");
        assert_eq!(location, db.locate(span).unwrap());

        let span = source.span(0..30).unwrap();
        assert_eq!(source.locate(span), None);
        assert_eq!(db.locate(span), None);
    }
}
