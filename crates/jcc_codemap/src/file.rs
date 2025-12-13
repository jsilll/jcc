//! Source file representation and line/column mapping.
//!
//! This module provides the [`SourceFile`] type, which stores source code text along with
//! pre-computed line information for efficient position lookups. It handles the conversion
//! between byte positions and human-readable line/column coordinates.
//!
//! # Overview
//!
//! A [`SourceFile`] maintains:
//! - The complete source text as a string
//! - A file name or path for display purposes
//! - An index of byte positions where each line begins
//!
//! The line index is computed once when the file is created, making subsequent line/column
//! lookups extremely fast using binary search.
//!
//! # Unicode Handling
//!
//! Column numbers are counted in **characters** (Unicode scalar values), not bytes.
//! This ensures that the column numbers match what users see in their editors.

use std::{
    ops::Range,
    path::{Path, PathBuf},
};

use crate::{byte::BytePos, span::Span, LineCol, Location};

/// An opaque identifier for a source file.
///
/// This is used to reference files in a Files database.
/// File IDs are stable and can be stored in your AST or IR.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileId(u32);

impl FileId {
    /// Creates a new `FileId` from a u32.
    #[inline]
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    /// Returns the underlying u32 value.
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Returns the underlying usize value.
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

/// A single source file with line index information.
///
/// This stores the source text and pre-computed line start positions
/// for efficient line/column lookups.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    /// The unique identifier for this file
    id: FileId,
    /// The name or path of this file
    name: PathBuf,
    /// The source text
    source: String,
    /// Byte positions of the start of each line
    /// Always starts with 0 for the first line
    line_starts: Vec<BytePos>,
}

impl SourceFile {
    /// Creates a new source file from a path by reading it.
    ///
    /// # Errors
    ///
    /// If reading the file fails, an error is returned.
    pub fn new(id: FileId, path: impl AsRef<Path>) -> std::io::Result<Self> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(path)?;
        let line_starts = compute_line_starts(&source);
        Ok(Self {
            id,
            source,
            line_starts,
            name: path.to_path_buf(),
        })
    }

    /// Creates a source file from source text with the given name.
    pub fn from_source(id: FileId, name: impl Into<PathBuf>, source: String) -> Self {
        let line_starts = compute_line_starts(&source);
        Self {
            id,
            source,
            line_starts,
            name: name.into(),
        }
    }

    /// Returns the unique identifier for this file.
    #[inline]
    pub fn id(&self) -> FileId {
        self.id
    }

    /// Returns the name of this file.
    #[inline]
    pub fn name(&self) -> &Path {
        &self.name
    }

    /// Returns the source text.
    #[inline]
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Returns the number of lines in this file.
    #[inline]
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Gets the byte position at the end of the file.
    #[inline]
    pub fn end_pos(&self) -> BytePos {
        BytePos(self.source.len().try_into().unwrap_or(u32::MAX))
    }

    /// Gets a slice of the source text for the given span.
    #[inline]
    pub fn slice(&self, span: Span) -> Option<&str> {
        let range: Range<usize> = span.into();
        self.source.get(range)
    }

    /// Gets the line index (0-indexed) for a byte position.
    #[inline]
    fn line_index(&self, pos: BytePos) -> Option<usize> {
        match self.line_starts.binary_search(&pos) {
            Ok(idx) => Some(idx),
            Err(idx) => match idx {
                0 => None,
                _ => Some(idx - 1),
            },
        }
    }

    /// Converts a byte position to a line and column.
    ///
    /// Returns None if the position is out of bounds.
    pub fn line_col(&self, pos: BytePos) -> Option<LineCol> {
        if pos.to_usize() > self.source.len() {
            return None;
        }

        let line_index = self.line_index(pos)?;
        let line_start = self.line_starts.get(line_index).copied()?;
        let column = self
            .source
            .get(line_start.to_usize()..pos.to_usize())?
            .chars()
            .count();

        Some(LineCol {
            column: u32::try_from(column + 1).unwrap_or(u32::MAX),
            line: u32::try_from(line_index + 1).unwrap_or(u32::MAX),
        })
    }

    /// Gets a Location containing line/column info and line text for a span.
    ///
    /// Returns None if the span is out of bounds.
    pub fn location(&self, span: Span) -> Option<Location<'_>> {
        let line_col = self.line_col(span.start())?;
        let line_index = line_col.line as usize - 1;
        let end_line_index = self
            .line_index(span.end())
            .unwrap_or(self.line_starts.len() - 1);
        let start = self.line_starts.get(line_index)?.to_usize();
        let end = self
            .line_starts
            .get(end_line_index + 1)
            .map_or(self.source.len(), |pos| pos.to_usize());
        let line_text = self.source.get(start..end)?;
        Some(Location {
            span,
            line_col,
            line_text,
        })
    }
}

/// Computes byte positions where each line starts.
fn compute_line_starts(source: &str) -> Vec<BytePos> {
    let estimated_lines = source.len() / 80 + 1;
    let mut starts = Vec::with_capacity(estimated_lines);
    starts.push(BytePos::ZERO);
    for (pos, byte) in source.bytes().enumerate() {
        if byte == b'\n' {
            starts.push(BytePos(u32::try_from(pos + 1).unwrap_or(u32::MAX)));
        }
    }
    starts.shrink_to_fit();
    starts
}
