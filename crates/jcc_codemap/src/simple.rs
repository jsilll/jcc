//! A simple in-memory [`crate::Files`] implementation.
//!
//! [`SimpleFiles`] maintains a flat list of source files, assigning each a
//! non-overlapping range in a global byte-address space. A one-byte sentinel
//! gap is inserted between files so that a [`crate::span::Span`] can never
//! accidentally straddle a file boundary. Lookup by position is O(log n) via
//! binary search.
//!
//! This is the default implementation. For more advanced use cases such as
//! lazy loading, virtual files, or IDE integration, implement the [`crate::Files`]
//! trait directly.

use crate::{byte::BytePos, file::SourceFile, Files};

use std::path::{Path, PathBuf};

/// An opaque identifier for a source file.
///
/// This is used to reference files in a Files database.
/// File IDs are stable and can be stored in your AST or IR.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileId(u32);

impl FileId {
    /// Returns the underlying usize value.
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

/// A simple in-memory collection of source files.
///
/// Files are allocated sequential, non-overlapping ranges in the global address
/// space, with a one-byte sentinel gap between each file so that spans cannot
/// accidentally straddle file boundaries.
///
/// This is the default implementation. For more advanced use cases (e.g., lazy
/// loading, virtual files, or integration with an IDE), implement the [`Files`] trait.
#[derive(Debug, Default, Clone)]
pub struct SimpleFiles {
    /// The next available global base address.
    next_base: BytePos,
    /// The list of source files, sorted by ascending base offset.
    files: Vec<SourceFile>,
}

impl Files for SimpleFiles {
    fn file_for_pos(&self, pos: BytePos) -> Option<&SourceFile> {
        let idx = self.files.partition_point(|f| f.base() <= pos);
        let file = self.files.get(idx.checked_sub(1)?)?;
        // Verify pos is not in the sentinel gap or past end of file.
        if pos < file.end() {
            Some(file)
        } else {
            None
        }
    }
}

impl SimpleFiles {
    /// Creates a new empty file collection.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of files.
    pub fn len(&self) -> usize {
        self.files.len()
    }

    /// Returns true if there are no files.
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    /// Adds a file from source text with the given name.
    pub fn add(&mut self, name: impl Into<PathBuf>, source: String) -> FileId {
        let id = FileId(u32::try_from(self.files.len()).unwrap_or(u32::MAX));
        let base = self.next_base;
        let len = u32::try_from(source.len()).unwrap_or(u32::MAX);
        self.next_base = BytePos::from(base.to_u32().saturating_add(len).saturating_add(1));
        self.files.push(SourceFile::from_source(base, name, source));
        id
    }

    /// Adds a file from a path by reading it.
    ///
    /// # Errors
    ///
    /// If reading the file fails, an error is returned.
    pub fn add_file(&mut self, path: impl AsRef<Path>) -> std::io::Result<FileId> {
        let id = FileId(u32::try_from(self.files.len()).unwrap_or(u32::MAX));
        let base = self.next_base;
        let file = SourceFile::new(base, path)?;
        let len = u32::try_from(file.source().len()).unwrap_or(u32::MAX);
        self.next_base = BytePos::from(base.to_u32().saturating_add(len).saturating_add(1));
        self.files.push(file);
        Ok(id)
    }

    /// Returns the name of the file with the given ID, or `None` if the ID is invalid.
    pub fn name(&self, file_id: FileId) -> Option<&Path> {
        self.get(file_id).map(SourceFile::name)
    }

    /// Returns the source file with the given ID, or `None` if the ID is invalid.
    pub fn get(&self, file_id: FileId) -> Option<&SourceFile> {
        self.files.get(file_id.as_usize())
    }

    /// Returns an iterator over all files.
    pub fn iter(&self) -> impl Iterator<Item = (FileId, &SourceFile)> {
        self.files
            .iter()
            .enumerate()
            .map(|(i, f)| (FileId(u32::try_from(i).unwrap_or(u32::MAX)), f))
    }
}
