use source_file::{diagnostic::Diagnostic, SourceFile, SourceSpan};
use string_interner::{backend::Backend, StringInterner};

#[derive(Clone, PartialEq, Eq)]
pub struct Token<StringBackend: Backend> {
    pub kind: TokenKind<StringBackend>,
    pub span: SourceSpan,
}

pub struct Lexer<'a, StringBackend: Backend> {
    file: &'a SourceFile,
    tokens: Vec<Token<StringBackend>>,
    diagnostics: Vec<LexerDiagnostic>,
    interner: &'a mut StringInterner<StringBackend>,
}

impl<'a, StringBackend: Backend> Lexer<'a, StringBackend> {
    pub fn new(file: &'a SourceFile, interner: &'a mut StringInterner<StringBackend>) -> Self {
        Self {
            file,
            tokens: Vec::new(),
            diagnostics: Vec::new(),
            interner,
        }
    }

    pub fn lex(mut self) -> LexerResult<StringBackend> {
        self.tokens.push(Token {
            span: self.file.span(0..1).unwrap_or_default(),
            kind: TokenKind::Identifier(self.interner.get_or_intern("Hello World")),
        });
        self.diagnostics.push(LexerDiagnostic {
            kind: LexerDiagnosticKind::UnexpectedCharacter,
            span: self.file.span(555..565).unwrap(),
        });
        // use anyhow::{Context, Result};
        LexerResult {
            tokens: self.tokens,
            diagnostics: self.diagnostics,
        }
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<StringBackend: Backend> {
    Identifier(StringBackend::Symbol),
}
