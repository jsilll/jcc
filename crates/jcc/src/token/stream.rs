use jcc_backend::codemap::file::SourceFile;

use crate::token::{lex::Lexer, TokenKind};

use std::iter::{FusedIterator, Peekable};

// ---------------------------------------------------------------------------
// TokenStream
// ---------------------------------------------------------------------------

/// A parser-oriented token stream wrapper around [`Lexer`].
///
/// Newline tokens are consumed internally and are never exposed through the
/// public iterator interface. The current source line is tracked automatically as tokens are consumed.
///
/// Lines are 1-indexed.
#[derive(Clone)]
pub struct TokenStream<'a> {
    line: u32,
    inner: Peekable<Lexer<'a>>,
}

impl<'a> From<Lexer<'a>> for TokenStream<'a> {
    fn from(inner: Lexer<'a>) -> Self {
        Self {
            line: 1,
            inner: inner.peekable(),
        }
    }
}

impl<'a> FusedIterator for TokenStream<'a> {}
impl<'a> Iterator for TokenStream<'a> {
    type Item = <Lexer<'a> as Iterator>::Item;

    /// Returns the next non-newline token from the stream.
    ///
    /// Any pending newline tokens are consumed internally.
    fn next(&mut self) -> Option<Self::Item> {
        self.consume_newlines();
        self.inner.next()
    }
}

impl<'a> TokenStream<'a> {
    /// Creates a new token stream from the given source file.
    pub fn new(file: &'a SourceFile) -> Self {
        Self::from(Lexer::new(file))
    }

    /// Returns a reference to the next non-newline token without consuming it.
    ///
    /// Any pending newline tokens are consumed internally before peeking.
    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        self.consume_newlines();
        self.inner.peek()
    }

    /// Returns the next token only if it is on the current line.
    ///
    /// If consuming pending newline tokens advances the stream onto the next line, `None` is returned instead.
    pub fn next_on_line(&mut self) -> Option<<Self as Iterator>::Item> {
        let curr = self.line;
        self.peek()?;
        if self.line == curr {
            self.next()
        } else {
            None
        }
    }

    /// Skips all remaining tokens on the current line.
    ///
    /// The terminating newline token is consumed if present, advancing the stream to the beginning of the next line.
    pub fn skip_line(&mut self) {
        for token in self.inner.by_ref().flatten() {
            if token.kind == TokenKind::NewLine {
                self.line += 1;
                break;
            }
        }
    }

    /// Consumes all pending newline tokens and advances the current line counter accordingly.
    fn consume_newlines(&mut self) {
        while let Some(Ok(_)) = self
            .inner
            .next_if(|item| matches!(item, Ok(token) if token.kind == TokenKind::NewLine))
        {
            self.line += 1;
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{testutil::file, token::lex::Lexer};

    fn kinds(src: &str) -> Vec<TokenKind> {
        let file = file(src);
        TokenStream::from(Lexer::new(&file))
            .filter_map(|r| r.ok())
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn empty_source() {
        assert_eq!(kinds(""), vec![]);
    }

    #[test]
    fn newlines_are_hidden() {
        assert_eq!(
            kinds("int\nreturn"),
            vec![TokenKind::KwInt, TokenKind::KwReturn]
        );
    }

    #[test]
    fn consecutive_newlines_are_hidden() {
        assert_eq!(
            kinds("int\n\n\nreturn"),
            vec![TokenKind::KwInt, TokenKind::KwReturn]
        );
    }

    #[test]
    fn single_line_tokens() {
        assert_eq!(
            kinds("int x = 42;"),
            [
                TokenKind::KwInt,
                TokenKind::Identifier,
                TokenKind::Eq,
                TokenKind::NumInt,
                TokenKind::Semi,
            ]
        );
    }

    #[test]
    fn peek_does_not_consume() {
        let f = file("int");
        let mut s = TokenStream::from(Lexer::new(&f));

        let peeked = s.peek().and_then(|r| r.as_ref().ok()).map(|t| t.kind);
        assert_eq!(peeked, Some(TokenKind::KwInt));

        let nexted = s.next().and_then(|r| r.ok()).map(|t| t.kind);
        assert_eq!(nexted, Some(TokenKind::KwInt));
        assert!(s.next().is_none());
    }

    #[test]
    fn peek_skips_newlines() {
        let f = file("\n\nint");
        let mut s = TokenStream::from(Lexer::new(&f));

        let kind = s.peek().and_then(|r| r.as_ref().ok()).map(|t| t.kind);
        assert_eq!(kind, Some(TokenKind::KwInt));
    }

    #[test]
    fn next_on_line_returns_token() {
        let f = file("int x");
        let mut s = TokenStream::from(Lexer::new(&f));

        s.next(); // consume "int"
        let kind = s.next_on_line().and_then(|r| r.ok()).map(|t| t.kind);
        assert_eq!(kind, Some(TokenKind::Identifier));
    }

    #[test]
    fn next_on_line_returns_none() {
        let f = file("int\nreturn");
        let mut s = TokenStream::from(Lexer::new(&f));

        s.next(); // consume "int"
        assert!(s.next_on_line().is_none());
    }

    #[test]
    fn skip_line_discards_remaining() {
        let f = file("int x y\nreturn");
        let mut s = TokenStream::from(Lexer::new(&f));

        s.skip_line(); // skip entire first line
        let kind = s.next().and_then(|r| r.ok()).map(|t| t.kind);
        assert_eq!(kind, Some(TokenKind::KwReturn));
    }

    #[test]
    fn skip_line_on_empty_last_line() {
        let f = file("int");
        let mut s = TokenStream::from(Lexer::new(&f));

        s.skip_line(); // no newline — should not panic
        assert!(s.next().is_none());
    }
}
