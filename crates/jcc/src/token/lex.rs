use super::{Token, TokenKind};

use jcc_backend::codemap::{
    file::SourceFile, span::Span, Diagnostic, IntoDiagnostic, Issue, Label,
};

use std::{
    iter::{FusedIterator, Peekable},
    str::CharIndices,
};

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub struct Lexer<'a> {
    pos: usize,
    line_start: bool,
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
            self.line_start = *c == '\n';
            let idx = self.chars.next()?.0;
            if self.line_start {
                self.pos = idx;
                return Some(Ok(Token::new(TokenKind::NewLine, self.span(1))));
            }
        }
        let can_emit_hash = self.line_start;
        let (idx, c) = self.chars.next()?;
        self.line_start = false;
        self.pos = idx;
        Some(match c {
            c if c.is_ascii_digit() => self.number(false),
            c if c.is_ascii_alphabetic() || c == '_' => Ok(self.word()),
            '#' if can_emit_hash => Ok(Token::new(TokenKind::DirectiveHash, self.span(1))),
            '.' => match self.chars.peek() {
                Some((_, c)) if c.is_ascii_digit() => self.number(true),
                _ => Err(Issue::new(
                    LexerIssue::UnexpectedCharacter,
                    self.file.single(self.pos),
                )),
            },
            ';' => Ok(Token::new(TokenKind::Semi, self.span(1))),
            ',' => Ok(Token::new(TokenKind::Comma, self.span(1))),
            ':' => Ok(Token::new(TokenKind::Colon, self.span(1))),
            '~' => Ok(Token::new(TokenKind::Tilde, self.span(1))),
            '(' => Ok(Token::new(TokenKind::LParen, self.span(1))),
            '{' => Ok(Token::new(TokenKind::LBrace, self.span(1))),
            '[' => Ok(Token::new(TokenKind::LBrack, self.span(1))),
            ')' => Ok(Token::new(TokenKind::RParen, self.span(1))),
            '}' => Ok(Token::new(TokenKind::RBrace, self.span(1))),
            ']' => Ok(Token::new(TokenKind::RBrack, self.span(1))),
            '?' => Ok(Token::new(TokenKind::Question, self.span(1))),
            '=' => Ok(self.match1('=', TokenKind::EqEq, TokenKind::Eq)),
            '!' => Ok(self.match1('=', TokenKind::BangEq, TokenKind::Bang)),
            '*' => Ok(self.match1('=', TokenKind::StarEq, TokenKind::Star)),
            '^' => Ok(self.match1('=', TokenKind::CaretEq, TokenKind::Caret)),
            '%' => Ok(self.match1('=', TokenKind::PercentEq, TokenKind::Percent)),
            '+' => Ok(self.match2(
                ('=', TokenKind::PlusEq),
                ('+', TokenKind::PlusPlus),
                TokenKind::Plus,
            )),
            '-' => Ok(self.match2(
                ('=', TokenKind::MinusEq),
                ('-', TokenKind::MinusMinus),
                TokenKind::Minus,
            )),
            '&' => Ok(self.match2(
                ('=', TokenKind::AmpEq),
                ('&', TokenKind::AmpAmp),
                TokenKind::Amp,
            )),
            '|' => Ok(self.match2(
                ('=', TokenKind::PipeEq),
                ('|', TokenKind::PipePipe),
                TokenKind::Pipe,
            )),
            '<' => Ok(self.match3(
                ('=', TokenKind::LtEq),
                ('<', TokenKind::LtLt),
                ('=', TokenKind::LtLtEq),
                TokenKind::Lt,
            )),
            '>' => Ok(self.match3(
                ('=', TokenKind::GtEq),
                ('>', TokenKind::GtGt),
                ('=', TokenKind::GtGtEq),
                TokenKind::Gt,
            )),
            '/' => match self.chars.peek() {
                Some((_, '*')) => self.comment_block(),
                Some((_, '/')) => Ok(self.comment_inline()),
                _ => Ok(self.match1('=', TokenKind::SlashEq, TokenKind::Slash)),
            },
            _ => Err(Issue::new(
                LexerIssue::UnexpectedCharacter,
                self.file.single(self.pos),
            )),
        })
    }
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile) -> Self {
        Self {
            file,
            pos: 0,
            line_start: true,
            chars: file.source().char_indices().peekable(),
        }
    }

    /// Creates a global span of `len` bytes starting at the current position.
    #[inline]
    fn span(&self, len: usize) -> Span {
        self.file.span(self.pos, self.pos + len).unwrap()
    }

    #[inline]
    fn match1(&mut self, ch: char, kind: TokenKind, fallback: TokenKind) -> Token {
        match self.chars.peek() {
            Some((_, ch2)) if *ch2 == ch => {
                self.chars.next();
                Token::new(kind, self.span(2))
            }
            _ => Token::new(fallback, self.span(1)),
        }
    }

    #[inline]
    fn match2(
        &mut self,
        pair1: (char, TokenKind),
        pair2: (char, TokenKind),
        fallback: TokenKind,
    ) -> Token {
        let (kind, len) = match self.chars.peek() {
            Some((_, c)) if *c == pair1.0 => {
                self.chars.next();
                (pair1.1, 2)
            }
            Some((_, c)) if *c == pair2.0 => {
                self.chars.next();
                (pair2.1, 2)
            }
            _ => (fallback, 1),
        };
        Token::new(kind, self.span(len))
    }

    #[inline]
    fn match3(
        &mut self,
        kind1: (char, TokenKind),
        kind2: (char, TokenKind),
        kind3: (char, TokenKind),
        fallback: TokenKind,
    ) -> Token {
        match self.chars.peek() {
            Some((_, c)) if *c == kind1.0 => {
                self.chars.next();
                Token::new(kind1.1, self.span(2))
            }
            Some((_, c)) if *c == kind2.0 => {
                self.chars.next();
                self.match1(kind3.0, kind3.1, kind2.1)
            }
            _ => Token::new(fallback, self.span(1)),
        }
    }

    fn comment_inline(&mut self) -> Token {
        while let Some((_, c)) = self.chars.peek() {
            if *c == '\n' {
                break;
            }
            self.chars.next();
        }
        let end = self
            .chars
            .peek()
            .map(|(idx, _)| *idx as u32)
            .unwrap_or(self.file.source().len() as u32);
        Token::new(
            TokenKind::CommentInline,
            self.file.span(self.pos, end).unwrap(),
        )
    }

    fn comment_block(&mut self) -> Result<Token, Issue<LexerIssue>> {
        self.chars.next(); // Skip '*'
        let mut closed = false;
        while let Some((_, c)) = self.chars.next() {
            if c == '*' && matches!(self.chars.peek(), Some((_, '/'))) {
                self.chars.next();
                closed = true;
                break;
            }
        }
        let end = self
            .chars
            .peek()
            .map(|(idx, _)| *idx as u32)
            .unwrap_or(self.file.source().len() as u32);
        let span = self.file.span(self.pos, end).unwrap();
        if closed {
            Ok(Token {
                span,
                kind: TokenKind::CommentBlock,
            })
        } else {
            Err(Issue::new(LexerIssue::UnclosedBlockComment, span))
        }
    }

    fn number(&mut self, started_with_dot: bool) -> Result<Token, Issue<LexerIssue>> {
        let mut is_float = started_with_dot;
        self.next_while(|c| c.is_ascii_digit());

        if let Some((_, '.')) = self.chars.peek() {
            if !started_with_dot {
                is_float = true;
                self.chars.next();
                self.next_while(|c| c.is_ascii_digit());
            }
        }

        if let Some((_, 'e' | 'E')) = self.chars.peek() {
            is_float = true;
            self.chars.next();
            if let Some((_, '+' | '-')) = self.chars.peek() {
                self.chars.next();
            }
            match self.chars.peek() {
                Some((_, c)) if c.is_ascii_digit() => {
                    self.next_while(|c| c.is_ascii_digit());
                }
                _ => {
                    let end = self
                        .chars
                        .peek()
                        .map(|(idx, _)| *idx)
                        .unwrap_or(self.file.source().len());
                    return Err(Issue::new(
                        LexerIssue::EmptyExponent,
                        self.file.span(self.pos, end).unwrap(),
                    ));
                }
            }
        }

        let kind = if is_float {
            TokenKind::NumFloat
        } else {
            match self.chars.peek() {
                Some((_, 'u' | 'U')) => {
                    self.chars.next();
                    match self.chars.peek() {
                        Some((_, 'l' | 'L')) => {
                            self.chars.next();
                            TokenKind::NumULong
                        }
                        _ => TokenKind::NumUInt,
                    }
                }
                Some((_, 'l' | 'L')) => {
                    self.chars.next();
                    match self.chars.peek() {
                        Some((_, 'u' | 'U')) => {
                            self.chars.next();
                            TokenKind::NumULong
                        }
                        _ => TokenKind::NumLong,
                    }
                }
                _ => TokenKind::NumInt,
            }
        };

        let end = match self.chars.peek() {
            None => self.file.source().len(),
            Some((end, c)) => {
                if c.is_ascii_alphanumeric() || *c == '_' || *c == '.' {
                    return Err(Issue::new(
                        LexerIssue::InvalidNumberSuffix,
                        self.file.span(self.pos, *end).unwrap(),
                    ));
                }
                *end
            }
        };

        Ok(Token {
            kind,
            span: self.file.span(self.pos, end).unwrap(),
        })
    }

    fn word(&mut self) -> Token {
        let end = self.next_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let ident = self.file.source().get(self.pos..end).unwrap_or_default();
        Token {
            span: self.file.span(self.pos, end).unwrap(),
            kind: TokenKind::from_keyword(ident).unwrap_or(TokenKind::Identifier),
        }
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
            .unwrap_or(self.pos + 1)
    }
}

// ---------------------------------------------------------------------------
// LexerIssue
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LexerIssue {
    EmptyExponent,
    InvalidNumberSuffix,
    UnexpectedCharacter,
    UnclosedBlockComment,
}

impl IntoDiagnostic for LexerIssue {
    fn into_diagnostic(self, span: Span) -> Diagnostic {
        let (msg, note) = match self {
            LexerIssue::EmptyExponent => (
                "empty exponent",
                "exponents must contain at least one digit",
            ),
            LexerIssue::UnexpectedCharacter => (
                "unexpected character",
                "the character is not recognized by the lexer",
            ),
            LexerIssue::UnclosedBlockComment => (
                "unterminated block comment",
                "block comments must be closed with '*/'",
            ),
            LexerIssue::InvalidNumberSuffix => (
                "invalid number suffix",
                "numbers cannot be followed by a letter, digit, underscore, or dot",
            ),
        };
        Diagnostic::error()
            .with_label(Label::primary(span).with_message(msg))
            .with_note(note)
    }
}
