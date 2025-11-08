use super::{Token, TokenKind};

use jcc_ssa::sourcemap::{diag::Diagnostic, SourceMap, SourceSpan};

use std::{iter::Peekable, str::CharIndices};

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub struct Lexer<'a> {
    idx: u32,
    file: &'a SourceMap,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerDiagnostic>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((_, c)) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.chars.next();
        }
        self.chars.next().map(|(idx, c)| {
            self.idx = idx as u32;
            match c {
                c if c.is_ascii_digit() => self.number(),
                c if c.is_ascii_alphabetic() || c == '_' => Ok(self.word()),
                ';' => Ok(self.token(TokenKind::Semi, 1)),
                ',' => Ok(self.token(TokenKind::Comma, 1)),
                ':' => Ok(self.token(TokenKind::Colon, 1)),
                '~' => Ok(self.token(TokenKind::Tilde, 1)),
                '(' => Ok(self.token(TokenKind::LParen, 1)),
                '{' => Ok(self.token(TokenKind::LBrace, 1)),
                '[' => Ok(self.token(TokenKind::LBrack, 1)),
                ')' => Ok(self.token(TokenKind::RParen, 1)),
                '}' => Ok(self.token(TokenKind::RBrace, 1)),
                ']' => Ok(self.token(TokenKind::RBrack, 1)),
                '?' => Ok(self.token(TokenKind::Question, 1)),
                '=' => Ok(self.match1('=', TokenKind::EqEq, TokenKind::Eq)),
                '!' => Ok(self.match1('=', TokenKind::BangEq, TokenKind::Bang)),
                '*' => Ok(self.match1('=', TokenKind::StarEq, TokenKind::Star)),
                '/' => Ok(self.match1('=', TokenKind::SlashEq, TokenKind::Slash)),
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
                _ => Err(LexerDiagnostic {
                    kind: LexerDiagnosticKind::UnexpectedCharacter,
                    span: self.file.span(self.idx..self.idx + 1).unwrap_or_default(),
                }),
            }
        })
    }
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceMap) -> Self {
        Self {
            file,
            idx: 0,
            chars: file.data().char_indices().peekable(),
        }
    }

    #[inline]
    fn token(&mut self, kind: TokenKind, len: u32) -> Token {
        Token {
            kind,
            span: self.file.span(self.idx..self.idx + len).unwrap_or_default(),
        }
    }

    #[inline]
    fn match1(&mut self, ch: char, kind: TokenKind, fallback: TokenKind) -> Token {
        match self.chars.peek() {
            Some((_, ch2)) if *ch2 == ch => {
                self.chars.next();
                self.token(kind, 2)
            }
            _ => self.token(fallback, 1),
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
        self.token(kind, len)
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
                self.token(kind1.1, 2)
            }
            Some((_, c)) if *c == kind2.0 => {
                self.chars.next();
                self.match1(kind3.0, kind3.1, kind2.1)
            }
            _ => self.token(fallback, 1),
        }
    }

    fn number(&mut self) -> Result<Token, LexerDiagnostic> {
        self.next_while(|c| c.is_ascii_digit());
        let kind = match self.chars.peek() {
            Some((_, 'l' | 'L')) => {
                self.chars.next();
                TokenKind::LongIntNumber
            }
            _ => TokenKind::IntNumber,
        };
        let end = self
            .chars
            .peek()
            .map(|(idx, _)| *idx as u32)
            .unwrap_or(self.file.data().len() as u32);
        if let Some((_, c)) = self.chars.peek() {
            if c.is_ascii_alphabetic() {
                return Err(LexerDiagnostic {
                    kind: LexerDiagnosticKind::IdentifierStartsWithDigit,
                    span: self.file.span(self.idx..end).unwrap_or_default(),
                });
            }
        }
        let span = self.file.span(self.idx..end).unwrap_or_default();
        Ok(Token { kind, span })
    }

    fn word(&mut self) -> Token {
        static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map! {
            "break" => TokenKind::KwBreak,
            "case" => TokenKind::KwCase,
            "continue" => TokenKind::KwContinue,
            "default" => TokenKind::KwDefault,
            "do" => TokenKind::KwDo,
            "else" => TokenKind::KwElse,
            "extern" => TokenKind::KwExtern,
            "for" => TokenKind::KwFor,
            "goto" => TokenKind::KwGoto,
            "if" => TokenKind::KwIf,
            "int" => TokenKind::KwInt,
            "long" => TokenKind::KwLong,
            "return" => TokenKind::KwReturn,
            "static" => TokenKind::KwStatic,
            "switch" => TokenKind::KwSwitch,
            "void" => TokenKind::KwVoid,
            "while" => TokenKind::KwWhile,
        };
        let end = self.next_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let ident = self.file.slice(self.idx..end).unwrap_or_default();
        let kind = KEYWORDS
            .get(ident)
            .copied()
            .unwrap_or(TokenKind::Identifier);
        let span = self.file.span(self.idx..end).unwrap_or_default();
        Token { kind, span }
    }

    #[inline]
    fn next_while<F>(&mut self, mut predicate: F) -> u32
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
            .map(|(idx, _)| *idx as u32)
            .unwrap_or(self.idx + 1)
    }
}

// ---------------------------------------------------------------------------
// LexerDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct LexerDiagnostic {
    pub span: SourceSpan,
    pub kind: LexerDiagnosticKind,
}

// ---------------------------------------------------------------------------
// LexerDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LexerDiagnosticKind {
    UnexpectedCharacter,
    IdentifierStartsWithDigit,
}

impl From<LexerDiagnostic> for Diagnostic {
    fn from(diagnostic: LexerDiagnostic) -> Self {
        match diagnostic.kind {
            LexerDiagnosticKind::UnexpectedCharacter => Diagnostic::error(
                diagnostic.span,
                "unexpected character",
                "expected a valid character",
            ),
            LexerDiagnosticKind::IdentifierStartsWithDigit => Diagnostic::error(
                diagnostic.span,
                "identifier starts with digit",
                "identifiers cannot start with a digit",
            ),
        }
    }
}
