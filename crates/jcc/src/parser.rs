use crate::lexer::{Token, TokenKind};

use string_interner::{DefaultSymbol, Symbol};
use tacky::source_file::{diagnostic::Diagnostic, SourceFile, SourceSpan};

use std::{iter::Peekable, slice::Iter};

// ---------------------------------------------------------------------------
// Precedence
// ---------------------------------------------------------------------------

struct Precedence {
    op: BinaryOp,
    plhs: u8,
    prhs: u8,
}

impl From<TokenKind> for Option<Precedence> {
    fn from(token: TokenKind) -> Self {
        match token {
            TokenKind::Plus => Some(Precedence {
                op: BinaryOp::Add,
                plhs: 0,
                prhs: 1,
            }),
            TokenKind::Minus => Some(Precedence {
                op: BinaryOp::Sub,
                plhs: 0,
                prhs: 1,
            }),
            TokenKind::Star => Some(Precedence {
                op: BinaryOp::Mul,
                plhs: 1,
                prhs: 2,
            }),
            TokenKind::Slash => Some(Precedence {
                op: BinaryOp::Div,
                plhs: 1,
                prhs: 2,
            }),
            TokenKind::Percent => Some(Precedence {
                op: BinaryOp::Rem,
                plhs: 1,
                prhs: 2,
            }),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a> {
    file: &'a SourceFile,
    iter: Peekable<Iter<'a, Token>>,
    res: ParserResult,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, iter: Iter<'a, Token>) -> Self {
        Self {
            file,
            iter: iter.peekable(),
            res: ParserResult::default(),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        if let Some(item) = self.parse_item() {
            self.res.ast.items.push(item);
        }
        if let Some(Token { span, .. }) = self.iter.next().cloned() {
            if self.res.diagnostics.is_empty() {
                self.res.diagnostics.push(ParserDiagnostic {
                    kind: ParserDiagnosticKind::UnexpectedToken,
                    span,
                })
            }
        }
        self.res
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
        let expr = self.parse_expr(0)?;
        self.eat(TokenKind::Semi)?;
        Some(self.res.ast.push_stmt(Stmt::Return(expr), span))
    }

    fn parse_expr(&mut self, min_prec: u8) -> Option<ExprRef> {
        let mut lhs = self.parse_expr_prefix()?;
        while let Some(Token { kind, span }) = self.iter.peek() {
            match Option::<Precedence>::from(*kind) {
                Some(Precedence { op, plhs, prhs }) => {
                    if plhs < min_prec {
                        break;
                    }
                    self.iter.next();
                    let rhs = self.parse_expr(prhs)?;
                    lhs = self.res.ast.push_expr(Expr::Binary { op, lhs, rhs }, *span);
                }
                None => break,
            }
        }
        Some(lhs)
    }

    fn parse_expr_prefix(&mut self) -> Option<ExprRef> {
        let token = self.iter.peek().cloned()?;
        match token.kind {
            TokenKind::Number(value) => {
                self.iter.next();
                Some(self.res.ast.push_expr(Expr::Constant(value), token.span))
            }
            TokenKind::Identifier(name) => {
                self.iter.next();
                Some(self.res.ast.push_expr(Expr::Variable(name), token.span))
            }
            TokenKind::LParen => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                Some(self.res.ast.push_expr(Expr::Grouped(expr), token.span))
            }
            TokenKind::Minus => {
                self.iter.next();
                let expr = self.parse_expr_prefix()?;
                Some(self.res.ast.push_expr(
                    Expr::Unary {
                        op: UnaryOp::Neg,
                        expr,
                    },
                    token.span,
                ))
            }
            TokenKind::Tilde => {
                self.iter.next();
                let expr = self.parse_expr_prefix()?;
                Some(self.res.ast.push_expr(
                    Expr::Unary {
                        op: UnaryOp::BitNot,
                        expr,
                    },
                    token.span,
                ))
            }
            _ => {
                self.res.diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::UnexpectedToken,
                });
                None
            }
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Option<&Token> {
        let token = self.iter.peek().or_else(|| {
            self.res.diagnostics.push(ParserDiagnostic {
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
            self.res.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::ExpectedToken(kind),
            });
            None
        }
    }

    fn eat_identifier(&mut self) -> Option<(SourceSpan, DefaultSymbol)> {
        let token = self.iter.peek().or_else(|| {
            self.res.diagnostics.push(ParserDiagnostic {
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
            self.res.diagnostics.push(ParserDiagnostic {
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

#[derive(Default, Clone, PartialEq, Eq)]
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

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Ast {
    items: Vec<Item>,
    stmts: Vec<Stmt>,
    exprs: Vec<Expr>,
    stmts_span: Vec<SourceSpan>,
    exprs_span: Vec<SourceSpan>,
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

    pub fn get_stmt(&self, stmt: StmtRef) -> &Stmt {
        &self.stmts[stmt.0 as usize]
    }

    pub fn get_expr(&self, expr: ExprRef) -> &Expr {
        &self.exprs[expr.0 as usize]
    }

    pub fn get_stmt_mut(&mut self, stmt: StmtRef) -> &mut Stmt {
        &mut self.stmts[stmt.0 as usize]
    }

    pub fn get_expr_mut(&mut self, expr: ExprRef) -> &mut Expr {
        &mut self.exprs[expr.0 as usize]
    }

    pub fn get_stmt_span(&self, stmt: StmtRef) -> &SourceSpan {
        &self.stmts_span[stmt.0 as usize]
    }

    pub fn get_expr_span(&self, expr: ExprRef) -> &SourceSpan {
        &self.exprs_span[expr.0 as usize]
    }

    fn push_stmt(&mut self, stmt: Stmt, span: SourceSpan) -> StmtRef {
        let r = StmtRef(self.stmts.len() as u32);
        self.stmts.push(stmt);
        self.stmts_span.push(span);
        r
    }

    fn push_expr(&mut self, expr: Expr, span: SourceSpan) -> ExprRef {
        let r = ExprRef(self.exprs.len() as u32);
        self.exprs.push(expr);
        self.exprs_span.push(span);
        r
    }
}

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ast")
            .field("items", &self.items)
            .field("stmts", &self.stmts)
            .field("exprs", &self.exprs)
            .finish()
    }
}

// ---------------------------------------------------------------------------
// Ast Nodes
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item {
    pub span: SourceSpan,
    pub name: DefaultSymbol,
    pub body: StmtRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    /// A return statement.
    Return(ExprRef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// A constant integer value.
    Constant(u32),
    /// A variable reference.
    Variable(DefaultSymbol),
    /// A grouped expression.
    Grouped(ExprRef),
    /// An unary expression.
    Unary { op: UnaryOp, expr: ExprRef },
    /// A binary expression.
    Binary {
        op: BinaryOp,
        lhs: ExprRef,
        rhs: ExprRef,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// The `-` operator.
    Neg,
    /// The `~` operator.
    BitNot,
}

impl From<UnaryOp> for tacky::UnaryOp {
    fn from(op: UnaryOp) -> Self {
        match op {
            UnaryOp::Neg => tacky::UnaryOp::Neg,
            UnaryOp::BitNot => tacky::UnaryOp::Not,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // The `+` operator.
    Add,
    // The `-` operator.
    Sub,
    // The `*` operator.
    Mul,
    // The `/` operator.
    Div,
    // The `%` operator.
    Rem,
}
