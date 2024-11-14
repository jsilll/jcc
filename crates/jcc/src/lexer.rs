use source_file::{diagnostic::Diagnostic, SourceFile, SourceSpan};
use std::{iter::Peekable, str::CharIndices};
use string_interner::{backend::Backend, StringInterner};

#[derive(Clone, PartialEq, Eq)]
pub struct Token<StringBackend: Backend> {
    pub kind: TokenKind<StringBackend>,
    pub span: SourceSpan,
}

pub struct Lexer<'a, StringBackend: Backend> {
    file: &'a SourceFile,
    chars: Peekable<CharIndices<'a>>,
    tokens: Vec<Token<StringBackend>>,
    diagnostics: Vec<LexerDiagnostic>,
    nesting: Vec<TokenKind<StringBackend>>,
    interner: &'a mut StringInterner<StringBackend>,
}

impl<'a, StringBackend: Backend> Lexer<'a, StringBackend> {
    pub fn new(file: &'a SourceFile, interner: &'a mut StringInterner<StringBackend>) -> Self {
        Self {
            file,
            chars: file.data().char_indices().peekable(),
            tokens: Vec::new(),
            diagnostics: Vec::new(),
            nesting: Vec::new(),
            interner,
        }
    }

    pub fn lex(mut self) -> LexerResult<StringBackend> {
        self.nesting.push(TokenKind::RBrace);
        self.tokens.push(Token {
            span: self.file.span(0..1).unwrap_or_default(),
            kind: TokenKind::Identifier(self.interner.get_or_intern("Hello World")),
        });
        // self.diagnostics.push(LexerDiagnostic {
        //     kind: LexerDiagnosticKind::UnexpectedCharacter,
        //     span: self.file.span(555..565).unwrap(),
        // });
        while let Some((begin, c)) = self.chars.next().map(|(begin, c)| (begin as u32, c)) {
            match c {
                // Nesting tokens
                '(' => {
                    self.nesting.push(TokenKind::RParen);
                    self.lex_char(TokenKind::LParen, begin)
                }
                '{' => {
                    self.nesting.push(TokenKind::RBrace);
                    self.lex_char(TokenKind::LBrace, begin)
                }
                '[' => {
                    self.nesting.push(TokenKind::RBrack);
                    self.lex_char(TokenKind::LBrack, begin)
                }
                ')' => self.lex_char(TokenKind::RParen, begin),
                '}' => self.lex_char(TokenKind::RBrace, begin),
                ']' => self.lex_char(TokenKind::RBrack, begin),
                _ => {}
            }
        }
        LexerResult {
            tokens: self.tokens,
            diagnostics: self.diagnostics,
        }
    }

    fn lex_char(&mut self, kind: TokenKind<StringBackend>, begin: u32) {
        self.tokens.push(Token {
            kind,
            span: self.file.span(begin..begin + 1).unwrap_or_default(),
        })
    }
}

#[derive(Clone)]
pub struct LexerResult<StringBackend: Backend> {
    pub tokens: Vec<Token<StringBackend>>,
    pub diagnostics: Vec<LexerDiagnostic>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct LexerDiagnostic {
    pub kind: LexerDiagnosticKind,
    pub span: SourceSpan,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LexerDiagnosticKind {
    UnexpectedCharacter,
}

impl From<LexerDiagnostic> for Diagnostic {
    fn from(diagnostic: LexerDiagnostic) -> Self {
        match diagnostic.kind {
            LexerDiagnosticKind::UnexpectedCharacter => Diagnostic::error(
                diagnostic.span,
                "unexpected character",
                "expected a valid character",
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<StringBackend: Backend> {
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Identifier(StringBackend::Symbol),
}
