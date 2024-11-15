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
        while let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::KwInt => {
                    let program = self.parse_fn_def().map(|f| Program(f));
                    return ParserResult {
                        program,
                        diagnostics: self.diagnostics,
                    };
                }
                _ => self.diagnostics.push(ParserDiagnostic {
                    kind: ParserDiagnosticKind::ExpectedToken(TokenKind::KwInt),
                    span: token.span,
                }),
            }
        }
        ParserResult {
            program: None,
            diagnostics: self.diagnostics,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Option<Token> {
        match self.iter.peek() {
            Some(token) => {
                let eq = match (token.kind, kind) {
                    (TokenKind::Identifier(_), TokenKind::Identifier(_)) => true,
                    (TokenKind::Number(_), TokenKind::Number(_)) => true,
                    _ => token.kind == kind,
                };
                if eq {
                    self.iter.next().cloned()
                } else {
                    self.diagnostics.push(ParserDiagnostic {
                        span: token.span,
                        kind: ParserDiagnosticKind::ExpectedToken(kind),
                    });
                    None
                }
            }
            None => {
                self.diagnostics.push(ParserDiagnostic {
                    span: self.file.end_span(),
                    kind: ParserDiagnosticKind::UnexpectedEof,
                });
                None
            }
        }
    }

    fn expect_number(&mut self) -> Option<(SourceSpan, u32)> {
        match self.iter.peek() {
            Some(token) => {
                if let TokenKind::Number(_) = token.kind {
                    self.iter.next().map(|t| match t.kind {
                        TokenKind::Number(n) => (t.span, n),
                        _ => unreachable!(),
                    })
                } else {
                    self.diagnostics.push(ParserDiagnostic {
                        span: token.span,
                        kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Number(0)),
                    });
                    None
                }
            }
            None => {
                self.diagnostics.push(ParserDiagnostic {
                    span: self.file.end_span(),
                    kind: ParserDiagnosticKind::UnexpectedEof,
                });
                None
            }
        }
    }

    fn expect_identifier(&mut self) -> Option<(SourceSpan, DefaultSymbol)> {
        match self.iter.peek() {
            Some(token) => {
                if let TokenKind::Identifier(_) = token.kind {
                    self.iter.next().map(|t| match t.kind {
                        TokenKind::Identifier(s) => (t.span, s),
                        _ => unreachable!(),
                    })
                } else {
                    self.diagnostics.push(ParserDiagnostic {
                        span: token.span,
                        kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Identifier(
                            DefaultSymbol::try_from_usize(0).unwrap(),
                        )),
                    });
                    None
                }
            }
            None => {
                self.diagnostics.push(ParserDiagnostic {
                    span: self.file.end_span(),
                    kind: ParserDiagnosticKind::UnexpectedEof,
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
    ExpectedToken(TokenKind),
}

impl From<ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: ParserDiagnostic) -> Self {
        match diagnostic.kind {
            ParserDiagnosticKind::UnexpectedEof => Diagnostic::error(
                diagnostic.span,
                "unexpected end of file",
                "expected more tokens",
            ),
            ParserDiagnosticKind::ExpectedToken(t) => Diagnostic::error(
                diagnostic.span,
                "expected token",
                format!("expected {}", t),
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
    Constant{ span: SourceSpan, value: u32 },
}
