use peeking_take_while::PeekableExt;
use source_file::{diagnostic::Diagnostic, SourceFile, SourceSpan};
use string_interner::{DefaultStringInterner, DefaultSymbol};

use std::{fmt, iter::Peekable, str::CharIndices};

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map! {
    "int" => TokenKind::KwInt,
    "void" => TokenKind::KwVoid,
    "return" => TokenKind::KwReturn,
};

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

pub struct Lexer<'a> {
    file: &'a SourceFile,
    chars: Peekable<CharIndices<'a>>,
    interner: &'a mut DefaultStringInterner,
    tokens: Vec<Token>,
    nesting: Vec<TokenKind>,
    diagnostics: Vec<LexerDiagnostic>,
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile, interner: &'a mut DefaultStringInterner) -> Self {
        Self {
            file,
            chars: file.data().char_indices().peekable(),
            interner,
            tokens: Vec::with_capacity(128),
            nesting: Vec::with_capacity(16),
            diagnostics: Vec::new(),
        }
    }

    pub fn lex(mut self) -> LexerResult {
        while let Some((begin, c)) = self.chars.next().map(|(begin, c)| (begin as u32, c)) {
            match c {
                c if c.is_whitespace() => continue,
                c if c.is_digit(10) => self.lex_number(begin),
                c if c.is_ascii_alphabetic() => self.lex_keyword_or_identifier(begin),
                ';' => self.lex_char(begin, TokenKind::Semi),
                '(' => self.handle_nesting_open(begin, TokenKind::LParen, TokenKind::RParen),
                '{' => self.handle_nesting_open(begin, TokenKind::LBrace, TokenKind::RBrace),
                '[' => self.handle_nesting_open(begin, TokenKind::LBrack, TokenKind::RBrack),
                ')' => self.handle_nesting_close(begin, TokenKind::LParen, TokenKind::RParen),
                '}' => self.handle_nesting_close(begin, TokenKind::LBrace, TokenKind::RBrace),
                ']' => self.handle_nesting_close(begin, TokenKind::LBrack, TokenKind::RBrack),
                _ => self.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::UnexpectedCharacter,
                    span: self.file.span(begin..begin + 1).unwrap_or_default(),
                }),
            }
        }
        if !self.nesting.is_empty() {
            self.nesting.clone().into_iter().for_each(|kind| {
                self.insert_unbalanced(kind, None);
            });
        }
        LexerResult {
            tokens: self.tokens,
            diagnostics: self.diagnostics,
        }
    }

    fn lex_char(&mut self, begin: u32, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            span: self.file.span(begin..begin + 1).unwrap_or_default(),
        })
    }

    fn lex_number(&mut self, begin: u32) {
        let end = self
            .chars
            .peeking_take_while(|(_, c)| c.is_digit(10))
            .last()
            .map(|(end, _)| end as u32)
            .unwrap_or_else(|| begin)
            + 1;
        let number = self.file.slice(begin..end).unwrap_or_default();
        let number = number.parse().unwrap_or_default();
        if let Some((_, c)) = self.chars.peek() {
            if c.is_ascii_alphabetic() {
                self.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::IdentifierStartsWithDigit,
                    span: self.file.span(begin..end).unwrap_or_default(),
                });
            }
        }
        self.tokens.push(Token {
            kind: TokenKind::Number(number),
            span: self.file.span(begin..end).unwrap_or_default(),
        });
    }

    fn lex_keyword_or_identifier(&mut self, begin: u32) {
        let end = self
            .chars
            .peeking_take_while(|(_, c)| c.is_ascii_alphanumeric())
            .last()
            .map(|(end, _)| end as u32)
            .unwrap_or_else(|| begin)
            + 1;
        let ident = self.file.slice(begin..end).unwrap_or_default();
        let kind = KEYWORDS
            .get(ident)
            .copied()
            .unwrap_or(TokenKind::Identifier(self.interner.get_or_intern(ident)));
        self.tokens.push(Token {
            kind,
            span: self.file.span(begin..end).unwrap_or_default(),
        });
    }

    fn handle_nesting_open(&mut self, begin: u32, open: TokenKind, close: TokenKind) {
        self.nesting.push(close);
        self.lex_char(begin, open);
    }

    fn handle_nesting_close(&mut self, begin: u32, open: TokenKind, close: TokenKind) {
        let mut matched = false;
        while let Some(kind) = self.nesting.pop() {
            if kind == close {
                matched = true;
                break;
            }
            self.insert_unbalanced(kind, Some(begin));
        }
        if !matched {
            self.insert_unbalanced(open, Some(begin));
        }
        self.tokens.push(Token {
            kind: close,
            span: self.file.span(begin..begin + 1).unwrap_or_default(),
        });
    }

    fn insert_unbalanced(&mut self, kind: TokenKind, begin: Option<u32>) {
        let span = match begin {
            Some(begin) => self.file.span(begin..begin + 1).unwrap_or_default(),
            None => self.file.end_span(),
        };
        self.tokens.push(Token { kind, span });
        self.diagnostics.push(LexerDiagnostic {
            kind: LexerDiagnosticKind::UnbalancedToken(kind),
            span,
        });
    }
}

// ---------------------------------------------------------------------------
// LexerResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub diagnostics: Vec<LexerDiagnostic>,
}

// ---------------------------------------------------------------------------
// LexerDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct LexerDiagnostic {
    pub kind: LexerDiagnosticKind,
    pub span: SourceSpan,
}

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
            LexerDiagnosticKind::IdentifierStartsWithDigit => Diagnostic::error(
                diagnostic.span,
                "identifier starts with digit",
                "identifiers cannot start with a digit",
            ),
            LexerDiagnosticKind::UnbalancedToken(token) => Diagnostic::error(
                diagnostic.span,
                "unbalanced token",
                format!("expected a matching {token}"),
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Token
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Semi,

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,

    KwInt,
    KwVoid,
    KwReturn,

    Number(u32),
    Identifier(DefaultSymbol),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Semi => write!(f, "';'"),

            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBrack => write!(f, "'['"),
            TokenKind::RBrack => write!(f, "']'"),

            TokenKind::KwInt => write!(f, "'int'"),
            TokenKind::KwVoid => write!(f, "'void'"),
            TokenKind::KwReturn => write!(f, "'return'"),

            TokenKind::Number(_) => write!(f, "a number"),
            TokenKind::Identifier(_) => write!(f, "an identifier"),
        }
    }
}
