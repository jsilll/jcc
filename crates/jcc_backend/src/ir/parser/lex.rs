use crate::ir::parser::token::{Token, TokenKind};

use jcc_codemap::{file::SourceFile, span::Span, Diagnostic, IntoDiagnostic, Issue, Label};

use std::{iter::FusedIterator, iter::Peekable, str::CharIndices};

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

pub struct Lexer<'a> {
    pos: usize,
    file: &'a SourceFile,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> FusedIterator for Lexer<'a> {}
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, Issue<LexerIssue>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((_, c)) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.chars.next();
        }
        let (idx, c) = self.chars.next()?;
        self.pos = idx;
        Some(match c {
            c if c.is_ascii_digit() => Ok(self.number()),
            c if c.is_ascii_alphabetic() => self.word(TokenKind::BlockRef),
            '@' => self.word(TokenKind::AtIdent),
            '%' => self.word(TokenKind::ValueRef),
            '#' => self.word(TokenKind::ParamIndex),
            '-' => Ok(self.match1('>', TokenKind::Arrow, TokenKind::Minus)),
            ',' => Ok(Token::new(TokenKind::Comma, self.file.single(self.pos))),
            ':' => Ok(Token::new(TokenKind::Colon, self.file.single(self.pos))),
            '=' => Ok(Token::new(TokenKind::Assign, self.file.single(self.pos))),
            '{' => Ok(Token::new(TokenKind::LBrace, self.file.single(self.pos))),
            '}' => Ok(Token::new(TokenKind::RBrace, self.file.single(self.pos))),
            '[' => Ok(Token::new(TokenKind::LBrack, self.file.single(self.pos))),
            ']' => Ok(Token::new(TokenKind::RBrack, self.file.single(self.pos))),
            '(' => Ok(Token::new(TokenKind::LParen, self.file.single(self.pos))),
            ')' => Ok(Token::new(TokenKind::RParen, self.file.single(self.pos))),
            _ => Err(Issue {
                kind: LexerIssue::UnexpectedChar,
                span: self.file.single(self.pos),
            }),
        })
    }
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile) -> Self {
        Self {
            file,
            pos: 0,
            chars: file.source().char_indices().peekable(),
        }
    }

    /// Creates a global span of `len` bytes starting at the current position.
    #[inline]
    fn span(&self, len: usize) -> Span {
        self.file.span(self.pos, self.pos + len).unwrap()
    }

    #[inline]
    fn match1(&mut self, ch: char, kind: TokenKind, fall: TokenKind) -> Token {
        match self.chars.peek() {
            Some((_, ch2)) if *ch2 == ch => {
                self.chars.next();
                Token::new(kind, self.span(2))
            }
            _ => Token::new(fall, self.span(1)),
        }
    }

    fn word(&mut self, kind: TokenKind) -> Result<Token, Issue<LexerIssue>> {
        let end = self.next_while(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.');
        let span = self.file.span(self.pos, end).unwrap();
        let word = &self.file.source()[self.pos..end];
        match TokenKind::from_keyword(word) {
            None => Ok(Token::new(kind, span)),
            Some(kind) => Ok(Token::new(kind, span)),
        }
    }

    fn number(&mut self) -> Token {
        let mut kind = TokenKind::Integer;
        let mut end = self.next_while(|c| c.is_ascii_digit());
        if matches!(self.chars.peek(), Some((_, '.'))) {
            self.chars.next();
            kind = TokenKind::Float;
            end = self.next_while(|c| c.is_ascii_digit());
        }
        if matches!(self.chars.peek(), Some((_, 'e' | 'E'))) {
            self.chars.next();
            if matches!(self.chars.peek(), Some((_, '+' | '-'))) {
                self.chars.next();
            }
            end = self.next_while(|c| c.is_ascii_digit());
            kind = TokenKind::Float;
        }
        Token::new(kind, self.file.span(self.pos, end).unwrap())
    }

    #[inline]
    fn next_while<F>(&mut self, mut predicate: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        while let Some(&(_, c)) = self.chars.peek() {
            if !predicate(c) {
                break;
            }
            self.chars.next();
        }
        self.chars
            .peek()
            .map(|(idx, _)| *idx)
            .unwrap_or(self.file.source().len())
    }
}

// ---------------------------------------------------------------------------
// LexerIssue
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerIssue {
    UnexpectedChar,
    InvalidEntityRef,
    UnknownIdentifier,
    UnknownAtIdentifier,
}

impl IntoDiagnostic for LexerIssue {
    fn into_diagnostic(self, span: Span) -> Diagnostic {
        let (msg, note) = match self {
            LexerIssue::UnexpectedChar => (
                "unexpected character",
                "this character is not part of the IR syntax",
            ),
            LexerIssue::InvalidEntityRef => (
                "invalid entity reference",
                "expected decimal digits after '%' or '#'",
            ),
            LexerIssue::UnknownIdentifier => (
                "unknown identifier",
                "this identifier is not a recognized IR keyword",
            ),
            LexerIssue::UnknownAtIdentifier => (
                "unknown '@' identifier",
                "expected '@global<N>' or '@function<N>'",
            ),
        };
        Diagnostic::error()
            .with_label(Label::primary(span).with_message(msg))
            .with_note(note)
    }
}
