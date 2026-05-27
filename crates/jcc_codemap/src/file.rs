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
//! - A global base offset in the shared address space (see below)
//!
//! The line index is computed once when the file is created, making subsequent line/column
//! lookups extremely fast using binary search.
//!
//! # Global Address Space
//!
//! All source files share a single virtual address space managed by the file database
//! (e.g., [`crate::SimpleFiles`]). Each file is allocated a non-overlapping range
//! `[base, base + len)` in this space. Spans produced by lexing and parsing carry
//! global byte positions, so a [`crate::span::Span`] alone identifies both the file
//! and the location within it.
//!
//! Methods on [`SourceFile`] that accept positions or spans (`slice`, `line_col`,
//! `location`) expect global values and translate them to file-local offsets internally.
//!
//! # Unicode Handling
//!
//! Column numbers are counted in **characters** (Unicode scalar values), not bytes.
//! This ensures that the column numbers match what users see in their editors.

use std::path::{Path, PathBuf};

use crate::{byte::BytePos, span::Span, LineCol, Location};

/// A single source file with line index information.
///
/// This stores the source text and pre-computed line start positions
/// for efficient line/column lookups.
///
/// All positions and spans accepted by methods on this type are global
/// (i.e., offset from the start of the shared address space, not from the
/// start of this file). The file's [`base`](Self::base) is subtracted internally
/// before any string operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    /// Start of the file in the shared address space.
    base: BytePos,
    /// The name or path of this file.
    name: PathBuf,
    /// The source text.
    source: String,
    /// File-local byte positions of the start of each line.
    /// Always starts with `BytePos(0)` for the first line.
    line_starts: Vec<BytePos>,
}

impl SourceFile {
    /// Creates a new source file from a path by reading it.
    ///
    /// # Errors
    ///
    /// If reading the file fails, an error is returned.
    pub fn new(base: BytePos, path: impl AsRef<Path>) -> std::io::Result<Self> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(path)?;
        let line_starts = compute_line_starts(&source);
        Ok(Self {
            base,
            source,
            line_starts,
            name: path.to_path_buf(),
        })
    }

    /// Creates a source file from source text with the given name.
    pub fn from_source(base: BytePos, name: impl Into<PathBuf>, source: String) -> Self {
        let line_starts = compute_line_starts(&source);
        Self {
            base,
            source,
            line_starts,
            name: name.into(),
        }
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

    /// Returns the global base offset of this file.
    #[inline]
    pub fn base(&self) -> BytePos {
        self.base
    }

    /// Returns the number of lines in this file.
    #[inline]
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Returns the global byte position one past the last byte of this file.
    #[inline]
    pub fn end(&self) -> BytePos {
        self.base + u32::try_from(self.source.len()).unwrap_or(u32::MAX)
    }

    /// Returns the global byte position of the last byte in this file.
    #[inline]
    pub fn last(&self) -> BytePos {
        self.base + u32::try_from(self.source.len() - 1).unwrap_or(u32::MAX)
    }

    /// Gets a slice of the source text for the given a global span.
    ///
    /// Returns `None` if the span does not fall within this file's address range.
    #[inline]
    pub fn slice(&self, span: Span) -> Option<&str> {
        let base = self.base.to_usize();
        let start = span.start().to_usize() - base;
        let end = span.end().to_usize() - base;
        self.source.get(start..end)
    }

    /// Converts a file-local byte position to a global span of length 1.
    #[inline]
    pub fn single(&self, pos: impl Into<BytePos>) -> Span {
        let base = self.base.to_u32();
        let pos = pos.into() + base;
        Span::single(pos)
    }

    /// Creates a global [`Span`] from file-local byte offsets.
    ///
    /// This is a convenience method for constructing spans when you know the
    /// file-local start and end positions (e.g., in tests or when integrating
    /// with code that produces file-local offsets).
    ///
    /// Returns `None` if `local_start > local_end`.
    #[inline]
    pub fn span(&self, start: impl Into<BytePos>, end: impl Into<BytePos>) -> Option<Span> {
        let base = self.base.to_u32();
        let start = start.into() + base;
        let end = end.into() + base;
        Span::new(start, end)
    }

    /// Converts a global byte position to a line and column.
    ///
    /// Returns `None` if the position is outside this file's address range.
    pub fn line_col(&self, pos: BytePos) -> Option<LineCol> {
        let pos = BytePos(pos - self.base);
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

    /// Gets a Location containing line/column info and line text for a global span.
    ///
    /// Returns `None` if the span is outside this file's address range.
    pub fn location(&self, span: Span) -> Option<Location<'_>> {
        let line_col = self.line_col(span.start())?;
        let line_index = line_col.line as usize - 1;
        let end_line_index = self
            .line_index(BytePos(span.end() - self.base))
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

    #[inline]
    fn line_index(&self, pos: BytePos) -> Option<usize> {
        self.line_starts
            .partition_point(|&start| start <= pos)
            .checked_sub(1)
    }
}

/// Computes file-local byte positions where each line starts.
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
