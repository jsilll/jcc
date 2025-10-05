use crate::PassResult;

use super::{Token, TokenKind};

use jcc_ssa::sourcemap::{diag::Diagnostic, SourceMap, SourceSpan};

use std::{iter::Peekable, str::CharIndices};

// ---------------------------------------------------------------------------
// LexerResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub diagnostics: Vec<LexerDiagnostic>,
}

impl PassResult for LexerResult {
    fn diagnostics(&self) -> &[impl Into<jcc_ssa::sourcemap::diag::Diagnostic> + Clone] {
        &self.diagnostics
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
// Lexer
// ---------------------------------------------------------------------------

pub struct Lexer<'a> {
    file: &'a SourceMap,
    idx: u32,
    result: LexerResult,
    nesting: Vec<TokenKind>,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceMap) -> Self {
        Self {
            file,
            idx: 0,
            result: LexerResult::default(),
            nesting: Vec::with_capacity(16),
            chars: file.data().char_indices().peekable(),
        }
    }

    pub fn lex(mut self) -> LexerResult {
        while let Some((idx, c)) = self.chars.next() {
            self.idx = idx as u32;
            match c {
                c if c.is_whitespace() => continue,
                c if c.is_ascii_digit() => self.handle_number(),
                c if c.is_ascii_alphabetic() || c == '_' => self.handle_word(),
                ';' => self.push_token(TokenKind::Semi, 1),
                ',' => self.push_token(TokenKind::Comma, 1),
                ':' => self.push_token(TokenKind::Colon, 1),
                '~' => self.push_token(TokenKind::Tilde, 1),
                '?' => self.push_token(TokenKind::Question, 1),
                '=' => self.try_match('=', TokenKind::EqEq, TokenKind::Eq),
                '!' => self.try_match('=', TokenKind::BangEq, TokenKind::Bang),
                '*' => self.try_match('=', TokenKind::StarEq, TokenKind::Star),
                '/' => self.try_match('=', TokenKind::SlashEq, TokenKind::Slash),
                '^' => self.try_match('=', TokenKind::CaretEq, TokenKind::Caret),
                '%' => self.try_match('=', TokenKind::PercentEq, TokenKind::Percent),
                '(' => self.handle_nesting_open(TokenKind::LParen, TokenKind::RParen),
                '{' => self.handle_nesting_open(TokenKind::LBrace, TokenKind::RBrace),
                '[' => self.handle_nesting_open(TokenKind::LBrack, TokenKind::RBrack),
                ')' => self.handle_nesting_close(TokenKind::LParen, TokenKind::RParen),
                '}' => self.handle_nesting_close(TokenKind::LBrace, TokenKind::RBrace),
                ']' => self.handle_nesting_close(TokenKind::LBrack, TokenKind::RBrack),
                '+' => self.try_match2(
                    ('=', TokenKind::PlusEq),
                    ('+', TokenKind::PlusPlus),
                    TokenKind::Plus,
                ),
                '-' => self.try_match2(
                    ('=', TokenKind::MinusEq),
                    ('-', TokenKind::MinusMinus),
                    TokenKind::Minus,
                ),
                '&' => self.try_match2(
                    ('=', TokenKind::AmpEq),
                    ('&', TokenKind::AmpAmp),
                    TokenKind::Amp,
                ),
                '|' => self.try_match2(
                    ('=', TokenKind::PipeEq),
                    ('|', TokenKind::PipePipe),
                    TokenKind::Pipe,
                ),
                '<' => self.try_match3(
                    ('=', TokenKind::LtEq),
                    ('<', TokenKind::LtLt),
                    ('=', TokenKind::LtLtEq),
                    TokenKind::Lt,
                ),
                '>' => self.try_match3(
                    ('=', TokenKind::GtEq),
                    ('>', TokenKind::GtGt),
                    ('=', TokenKind::GtGtEq),
                    TokenKind::Gt,
                ),
                _ => self.result.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::UnexpectedCharacter,
                    span: self.file.span(self.idx..self.idx + 1).unwrap_or_default(),
                }),
            }
        }
        if !self.nesting.is_empty() {
            for i in 0..self.nesting.len() {
                self.push_unbalanced_token(self.nesting[i], true);
            }
        }
        self.result
    }

    #[inline]
    fn push_token(&mut self, kind: TokenKind, len: u32) {
        self.result.tokens.push(Token {
            kind,
            span: self.file.span(self.idx..self.idx + len).unwrap_or_default(),
        })
    }

    fn push_unbalanced_token(&mut self, kind: TokenKind, end: bool) {
        let span = if end {
            self.file.end_span()
        } else {
            self.file.span(self.idx..self.idx + 1).unwrap_or_default()
        };
        self.result.tokens.push(Token { kind, span });
        self.result.diagnostics.push(LexerDiagnostic {
            kind: LexerDiagnosticKind::UnbalancedToken(kind),
            span,
        });
    }

    fn try_match(&mut self, ch: char, kind: TokenKind, fallback: TokenKind) {
        match self.chars.peek() {
            Some((_, ch2)) if *ch2 == ch => {
                self.chars.next();
                self.result.tokens.push(Token {
                    kind,
                    span: self.file.span(self.idx..self.idx + 2).unwrap_or_default(),
                });
            }
            _ => self.result.tokens.push(Token {
                kind: fallback,
                span: self.file.span(self.idx..self.idx + 1).unwrap_or_default(),
            }),
        }
    }

    fn try_match2(
        &mut self,
        kind1: (char, TokenKind),
        kind2: (char, TokenKind),
        fallback: TokenKind,
    ) {
        let (token, len) = match self.chars.peek() {
            Some((_, c)) if *c == kind1.0 => {
                self.chars.next();
                (kind1.1, 2)
            }
            Some((_, c)) if *c == kind2.0 => {
                self.chars.next();
                (kind2.1, 2)
            }
            _ => (fallback, 1),
        };
        self.result.tokens.push(Token {
            kind: token,
            span: self.file.span(self.idx..self.idx + len).unwrap_or_default(),
        });
    }

    fn try_match3(
        &mut self,
        kind1: (char, TokenKind),
        kind2: (char, TokenKind),
        kind3: (char, TokenKind),
        fallback: TokenKind,
    ) {
        match self.chars.peek() {
            Some((_, c)) if *c == kind1.0 => {
                self.chars.next();
                self.push_token(kind1.1, 1);
            }
            Some((_, c)) if *c == kind2.0 => {
                self.chars.next();
                self.try_match(kind3.0, kind3.1, kind2.1)
            }
            _ => self.push_token(fallback, 1),
        }
    }

    fn handle_number(&mut self) {
        self.consume_while(|c| c.is_ascii_digit());
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
                self.result.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::IdentifierStartsWithDigit,
                    span: self.file.span(self.idx..end).unwrap_or_default(),
                });
            }
        }
        self.result.tokens.push(Token {
            kind,
            span: self.file.span(self.idx..end).unwrap_or_default(),
        });
    }

    fn handle_word(&mut self) {
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
        let end = self.consume_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let ident = self.file.slice(self.idx..end).unwrap_or_default();
        let kind = KEYWORDS
            .get(ident)
            .copied()
            .unwrap_or(TokenKind::Identifier);
        self.result.tokens.push(Token {
            kind,
            span: self.file.span(self.idx..end).unwrap_or_default(),
        });
    }

    #[inline]
    fn handle_nesting_open(&mut self, open: TokenKind, close: TokenKind) {
        self.nesting.push(close);
        self.push_token(open, 1);
    }

    fn handle_nesting_close(&mut self, open: TokenKind, close: TokenKind) {
        let mut matched = false;
        while let Some(kind) = self.nesting.pop() {
            if kind == close {
                matched = true;
                break;
            }
            self.push_unbalanced_token(kind, false);
        }
        if !matched {
            self.push_unbalanced_token(open, false);
        }
        self.result.tokens.push(Token {
            kind: close,
            span: self.file.span(self.idx..self.idx + 1).unwrap_or_default(),
        });
    }

    #[inline]
    fn consume_while<F>(&mut self, mut predicate: F) -> u32
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
// LexerDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LexerDiagnosticKind {
    UnexpectedCharacter,
    IdentifierStartsWithDigit,
    UnbalancedToken(TokenKind),
}

impl From<LexerDiagnostic> for Diagnostic {
    fn from(diagnostic: LexerDiagnostic) -> Self {
        match diagnostic.kind {
            LexerDiagnosticKind::UnexpectedCharacter => Diagnostic::error(
                diagnostic.span,
                "unexpected character",
                "expected a valid character",
            ),
            LexerDiagnosticKind::UnbalancedToken(token) => Diagnostic::error(
                diagnostic.span,
                "unbalanced token",
                format!("expected a matching {token}"),
            ),
            LexerDiagnosticKind::IdentifierStartsWithDigit => Diagnostic::error(
                diagnostic.span,
                "identifier starts with digit",
                "identifiers cannot start with a digit",
            ),
        }
    }
}
