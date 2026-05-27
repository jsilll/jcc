use crate::prep::Preprocessor;

use jcc_backend::codemap::file::SourceFile;

use std::iter::{FusedIterator, Peekable};

// ---------------------------------------------------------------------------
// PrepStream
// ---------------------------------------------------------------------------

pub type PrepItem<'a> = <Preprocessor<'a> as Iterator>::Item;

pub struct PrepStream<'a> {
    lookahead: Option<PrepItem<'a>>,
    inner: Peekable<Preprocessor<'a>>,
}

impl<'a> FusedIterator for PrepStream<'a> {}
impl<'a> Iterator for PrepStream<'a> {
    type Item = PrepItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lookahead.take().or_else(|| self.inner.next())
    }
}

impl<'a> PrepStream<'a> {
    /// Creates a new `PrepStream` from the given source file.
    pub fn new(file: &'a SourceFile) -> Self {
        Self {
            lookahead: None,
            inner: Preprocessor::new(file).peekable(),
        }
    }

    /// Returns the next token without consuming it.
    pub fn peek(&mut self) -> Option<&PrepItem<'a>> {
        if self.lookahead.is_none() {
            self.lookahead = self.inner.next();
        }
        self.lookahead.as_ref()
    }

    /// Returns the second token without consuming either token.
    ///
    /// Returns `None` if fewer than two tokens remain.
    pub fn peek2(&mut self) -> Option<&PrepItem<'a>> {
        self.peek()?;
        self.inner.peek()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{testutil::file, token::TokenKind};

    #[test]
    fn peek_does_not_consume() {
        let f = file("int return");
        let mut s = PrepStream::new(&f);

        let peek = s.peek().and_then(|r| r.as_ref().ok()).map(|t| t.kind);
        assert_eq!(peek, Some(TokenKind::KwInt));

        let next = s.next().and_then(|r| r.ok()).map(|t| t.kind);
        assert_eq!(next, Some(TokenKind::KwInt));
    }

    #[test]
    fn peek2_returns_none() {
        let f = file("int");
        let mut s = PrepStream::new(&f);

        assert!(s.peek2().is_none());

        let next = s.next().and_then(|r| r.ok()).map(|t| t.kind);
        assert_eq!(next, Some(TokenKind::KwInt));
    }

    #[test]
    fn peek2_returns_second_token() {
        let f = file("int return");
        let mut s = PrepStream::new(&f);

        let second = s.peek2().and_then(|r| r.as_ref().ok()).map(|t| t.kind);
        assert_eq!(second, Some(TokenKind::KwReturn));

        let first = s.next().and_then(|r| r.ok()).map(|t| t.kind);
        assert_eq!(first, Some(TokenKind::KwInt));

        let second = s.next().and_then(|r| r.ok()).map(|t| t.kind);
        assert_eq!(second, Some(TokenKind::KwReturn));
    }
}
