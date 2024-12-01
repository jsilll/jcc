use crate::lexer::{Token, TokenKind};

use string_interner::{DefaultSymbol, Symbol};
use tacky::source_file::{diagnostic::Diagnostic, SourceFile, SourceSpan};

use std::{fmt, iter::Peekable, slice::Iter};

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a> {
    file: &'a SourceFile,
    iter: Peekable<Iter<'a, Token>>,
    ast: Ast,
    diagnostics: Vec<ParserDiagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, iter: Iter<'a, Token>) -> Self {
        Self {
            file,
            iter: iter.peekable(),
            ast: Ast::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        if let Some(item) = self.parse_item() {
            self.ast.items.push(item);
        }
        if !self.iter.peek().is_none() {
            self.diagnostics.push(ParserDiagnostic {
                kind: ParserDiagnosticKind::UnexpectedToken,
                span: self.file.end_span(),
            })
        }
        ParserResult {
            ast: self.ast,
            diagnostics: self.diagnostics,
        }
    }

    fn parse_item(&mut self) -> Option<Item> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        self.eat(TokenKind::LParen)?;
        self.eat(TokenKind::KwVoid)?;
        self.eat(TokenKind::RParen)?;
        self.eat(TokenKind::LBrace)?;
        let body = self.parse_stmt()?;
        self.eat(TokenKind::RBrace)?;
        Some(Item { span, name, body })
    }

    fn parse_stmt(&mut self) -> Option<StmtRef> {
        let span = self.eat(TokenKind::KwReturn)?.span;
        let expr = self.parse_expr()?;
        self.eat(TokenKind::Semi)?;
        Some(self.ast.push_stmt(Stmt::Return(expr), span))
    }

    fn parse_expr(&mut self) -> Option<ExprRef> {
        let token = self.iter.peek().cloned()?;
        match token.kind {
            TokenKind::Number(value) => {
                self.iter.next();
                Some(self.ast.push_expr(Expr::Constant(value), token.span))
            }
            TokenKind::Identifier(name) => {
                self.iter.next();
                Some(self.ast.push_expr(Expr::Variable(name), token.span))
            }
            TokenKind::LParen => {
                self.iter.next();
                let expr = self.parse_expr()?;
                self.eat(TokenKind::RParen)?;
                Some(self.ast.push_expr(Expr::Grouped(expr), token.span))
            }
            TokenKind::Minus => {
                self.iter.next();
                let expr = self.parse_expr()?;
                Some(self.ast.push_expr(
                    Expr::Unary {
                        op: UnaryOp::Minus,
                        expr,
                    },
                    token.span,
                ))
            }
            TokenKind::Tilde => {
                self.iter.next();
                let expr = self.parse_expr()?;
                Some(self.ast.push_expr(
                    Expr::Unary {
                        op: UnaryOp::BitwiseNot,
                        expr,
                    },
                    token.span,
                ))
            }
            _ => {
                self.diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::UnexpectedToken,
                });
                None
            }
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Option<&Token> {
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
                kind: ParserDiagnosticKind::ExpectedToken(kind),
            });
            None
        }
    }

    fn eat_identifier(&mut self) -> Option<(SourceSpan, DefaultSymbol)> {
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
                kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Identifier(
                    DefaultSymbol::try_from_usize(0).unwrap(),
                )),
            });
            None
        }
    }
}

// ---------------------------------------------------------------------------
// ParserResult
// ---------------------------------------------------------------------------

pub struct ParserResult {
    pub ast: Ast,
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
    UnexpectedToken,
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
            ParserDiagnosticKind::UnexpectedToken => Diagnostic::error(
                diagnostic.span,
                "unexpected token",
                "expected a different token",
            ),
            ParserDiagnosticKind::ExpectedToken(token) => Diagnostic::error(
                diagnostic.span,
                "unexpected token",
                format!("expected {token}"),
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Ast
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct Ast {
    items: Vec<Item>,
    exprs: Vec<Expr>,
    stmts: Vec<Stmt>,
    exprs_span: Vec<SourceSpan>,
    stmts_span: Vec<SourceSpan>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ExprRef(u32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StmtRef(u32);

impl Ast {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn items(&self) -> &[Item] {
        &self.items
    }

    pub fn get_expr(&self, expr: ExprRef) -> &Expr {
        &self.exprs[expr.0 as usize]
    }

    pub fn get_stmt(&self, stmt: StmtRef) -> &Stmt {
        &self.stmts[stmt.0 as usize]
    }

    pub fn get_expr_mut(&mut self, expr: ExprRef) -> &mut Expr {
        &mut self.exprs[expr.0 as usize]
    }

    pub fn get_stmt_mut(&mut self, stmt: StmtRef) -> &mut Stmt {
        &mut self.stmts[stmt.0 as usize]
    }

    pub fn get_expr_span(&self, expr: ExprRef) -> &SourceSpan {
        &self.exprs_span[expr.0 as usize]
    }

    pub fn get_stmt_span(&self, stmt: StmtRef) -> &SourceSpan {
        &self.stmts_span[stmt.0 as usize]
    }

    fn push_expr(&mut self, expr: Expr, span: SourceSpan) -> ExprRef {
        let r = ExprRef(self.exprs.len() as u32);
        self.exprs.push(expr);
        self.exprs_span.push(span);
        r
    }

    fn push_stmt(&mut self, stmt: Stmt, span: SourceSpan) -> StmtRef {
        let r = StmtRef(self.stmts.len() as u32);
        self.stmts.push(stmt);
        self.stmts_span.push(span);
        r
    }
}

impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ast")
            .field("items", &self.items)
            .field("exprs", &self.exprs)
            .field("stmts", &self.stmts)
            .finish()
    }
}

// ---------------------------------------------------------------------------
// Ast Nodes
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct Item {
    pub span: SourceSpan,
    pub name: DefaultSymbol,
    pub body: StmtRef,
}

#[derive(Debug)]
pub enum Stmt {
    /// A return statement.
    Return(ExprRef),
}

#[derive(Debug)]
pub enum Expr {
    /// A constant integer value.
    Constant(u32),
    /// A variable reference.
    Variable(DefaultSymbol),
    /// A grouped expression.
    Grouped(ExprRef),
    /// An unary operation.
    Unary { op: UnaryOp, expr: ExprRef },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    /// The `-` operator.
    Minus,
    /// The `~` operator.
    BitwiseNot,
}

impl From<UnaryOp> for tacky::UnaryOp {
    fn from(op: UnaryOp) -> Self {
        match op {
            UnaryOp::Minus => tacky::UnaryOp::Minus,
            UnaryOp::BitwiseNot => tacky::UnaryOp::BitwiseNot,
        }
    }
}
