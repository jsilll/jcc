use crate::{
    ir::{
        parser::{lex::LexerIssue, token::TokenKind},
        Program,
    },
    IdentInterner,
};

use jcc_codemap::{file::SourceFile, span::Span, Diagnostic, IntoDiagnostic, Issue, Label};

use std::borrow::Cow;

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a> {
    _file: &'a SourceFile,
    _interner: &'a mut IdentInterner,
}

impl<'a> Parser<'a> {
    pub fn new(_file: &'a SourceFile, _interner: &'a mut IdentInterner) -> Self {
        Self { _file, _interner }
    }

    pub fn parse(self) -> ParserResult {
        ParserResult::default()
    }
}

// ---------------------------------------------------------------------------
// ParserResult
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct ParserResult {
    pub program: Program,
    pub lexer_issues: Vec<Issue<LexerIssue>>,
    pub parser_issues: Vec<Issue<ParserIssue>>,
}

// ---------------------------------------------------------------------------
// ParserIssue
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum ParserIssue {
    UnexpectedEof,
    InvalidLiteral,
    UnexpectedToken(TokenKind),
    IndexMismatch { found: u32, expected: u32 },
}

impl IntoDiagnostic for ParserIssue {
    fn into_diagnostic(self, span: Span) -> Diagnostic {
        let (msg, note): (Cow<str>, Cow<str>) = match self {
            ParserIssue::UnexpectedEof => (
                "unexpected end of file".into(),
                "the IR text ended before it was complete".into(),
            ),
            ParserIssue::InvalidLiteral => (
                "invalid literal".into(),
                "this token could not be parsed as a numeric literal".into(),
            ),
            ParserIssue::UnexpectedToken(expected) => (
                "unexpected token".into(),
                format!("expected {expected}").into(),
            ),
            ParserIssue::IndexMismatch { found, expected } => (
                "index mismatch".into(),
                format!("found index {found} but expected {expected}").into(),
            ),
        };
        Diagnostic::error()
            .with_label(Label::primary(span).with_message(msg))
            .with_note(note)
    }
}
