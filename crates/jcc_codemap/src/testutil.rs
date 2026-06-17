//! Test utilities for building source file fixtures.

use crate::{
    simple::{FileId, SimpleFiles},
    span::Span,
};

/// A collection of pre-loaded source files for use in tests.
pub struct TestFiles {
    /// The underlying file store.
    pub files: SimpleFiles,
    /// A simple single-line source file (`test.rs`).
    pub test_file: FileId,
    /// A multi-line source file (`example.rs`).
    pub example_file: FileId,
    /// A source file containing non-ASCII characters (`unicode.rs`).
    pub unicode_file: FileId,
}

impl TestFiles {
    /// Returns a [`Span`] within `id` covering bytes `start..end`.
    ///
    /// # Panics
    ///
    /// Panics if the file ID is invalid or the range is out of bounds.
    pub fn span(&self, id: FileId, start: u32, end: u32) -> Span {
        self.files
            .get(id)
            .expect("invalid file id")
            .span(start, end)
            .expect("invalid span range")
    }
}

/// Creates a [`TestFiles`] instance with three pre-populated source files.
pub fn create_test_files() -> TestFiles {
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
