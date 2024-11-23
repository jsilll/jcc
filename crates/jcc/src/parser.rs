use crate::lexer::{Token, TokenKind};

use source_file::{diagnostic::Diagnostic, SourceFile, SourceSpan};
use string_interner::{DefaultSymbol, Symbol};

use std::{iter::Peekable, slice::Iter};

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a> {
    file: &'a SourceFile,
    iter: Peekable<Iter<'a, Token>>,
    diagnostics: Vec<ParserDiagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, iter: Iter<'a, Token>) -> Self {
        Self {
            file,
            iter: iter.peekable(),
            diagnostics: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        let program = self.parse_program();
        if !self.iter.peek().is_none() {
            self.diagnostics.push(ParserDiagnostic {
                kind: ParserDiagnosticKind::UnexpectedToken(TokenKind::KwInt),
                span: self.file.end_span(),
            })
        }
        ParserResult {
            program,
            diagnostics: self.diagnostics,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Option<&Token> {
        let token = self.iter.peek().or_else(|| {
            self.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })?;
        let matches = match (token.kind, kind) {
            (TokenKind::Number(_), TokenKind::Number(_))
            | (TokenKind::Identifier(_), TokenKind::Identifier(_)) => true,
            _ => token.kind == kind,
        };
        if matches {
            self.iter.next()
        } else {
            self.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::UnexpectedToken(kind),
            });
            None
        }
    }

    fn expect_number(&mut self) -> Option<(SourceSpan, u32)> {
        let token = self.iter.peek().or_else(|| {
            self.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })?;
        if let TokenKind::Number(n) = token.kind {
            let span = token.span;
            self.iter.next();
            Some((span, n))
        } else {
            self.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::UnexpectedToken(TokenKind::Number(0)),
            });
            None
        }
    }

    fn expect_identifier(&mut self) -> Option<(SourceSpan, DefaultSymbol)> {
        let token = self.iter.peek().or_else(|| {
            self.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })?;
        if let TokenKind::Identifier(s) = token.kind {
            let span = token.span;
            self.iter.next();
            Some((span, s))
        } else {
            self.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::UnexpectedToken(TokenKind::Identifier(
                    DefaultSymbol::try_from_usize(0).unwrap(),
                )),
            });
            None
        }
    }

    fn parse_program(&mut self) -> Option<Program> {
        match self.iter.peek()? {
            Token {
                kind: TokenKind::KwInt,
                ..
            } => self.parse_fn_def().map(Program),
            Token { span, .. } => {
                self.diagnostics.push(ParserDiagnostic {
                    kind: ParserDiagnosticKind::UnexpectedToken(TokenKind::KwInt),
                    span: *span,
                });
                None
            }
        }
    }

    fn parse_fn_def(&mut self) -> Option<FnDef> {
        self.expect(TokenKind::KwInt)?;
        let (span, name) = self.expect_identifier()?;
        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::KwVoid)?;
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::LBrace)?;
        let body = self.parse_stmt()?;
        self.expect(TokenKind::RBrace)?;
        Some(FnDef { span, name, body })
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        self.expect(TokenKind::KwReturn)?;
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semi)?;
        Some(Stmt::Return(expr))
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        let (span, value) = self.expect_number()?;
        Some(Expr::Constant { span, value })
    }
}

// ---------------------------------------------------------------------------
// ParserResult
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct ParserResult {
    pub program: Option<Program>,
    pub diagnostics: Vec<ParserDiagnostic>,
}

// ---------------------------------------------------------------------------
// ParserDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct ParserDiagnostic {
    pub kind: ParserDiagnosticKind,
    pub span: SourceSpan,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParserDiagnosticKind {
    UnexpectedEof,
    UnexpectedToken(TokenKind),
}

impl From<ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: ParserDiagnostic) -> Self {
        match diagnostic.kind {
            ParserDiagnosticKind::UnexpectedEof => Diagnostic::error(
                diagnostic.span,
                "unexpected end of file",
                "expected more tokens",
            ),
            ParserDiagnosticKind::UnexpectedToken(token) => Diagnostic::error(
                diagnostic.span,
                "unexpected token",
                format!("expected {token}"),
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// AST
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct Program(pub FnDef);

#[derive(Debug)]
pub struct FnDef {
    pub span: SourceSpan,
    pub name: DefaultSymbol,
    pub body: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Return(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Constant { span: SourceSpan, value: u32 },
}
