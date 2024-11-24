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
    ast: Ast,
    diagnostics: Vec<ParserDiagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, iter: Iter<'a, Token>) -> Self {
        let ast = Ast {
            items: Vec::new(),
            exprs: Vec::new(),
            stmts: Vec::new(),
        };
        Self {
            file,
            iter: iter.peekable(),
            ast,
            diagnostics: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        if let Some(fn_def) = self.parse_fn_def() {
            self.ast.items.push(fn_def);
        }
        if !self.iter.peek().is_none() {
            self.diagnostics.push(ParserDiagnostic {
                kind: ParserDiagnosticKind::UnexpectedToken(TokenKind::KwInt),
                span: self.file.end_span(),
            })
        }
        ParserResult {
            ast: self.ast,
            diagnostics: self.diagnostics,
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

    fn parse_expr(&mut self) -> Option<ExprRef> {
        let (span, value) = self.expect_number()?;
        Some(self.ast.push_expr(Expr::Constant { span, value }))
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

pub struct Ast {
    pub items: Vec<FnDef>,
    exprs: Vec<Expr>,
    stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ExprRef(u32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StmtRef(u32);

impl Ast {
    fn push_expr(&mut self, expr: Expr) -> ExprRef {
        let r = ExprRef(self.exprs.len() as u32);
        self.exprs.push(expr);
        r
    }

    // fn push_stmt(&mut self, stmt: Stmt) -> StmtRef {
    //     let r = StmtRef(self.exprs.len() as u32);
    //     self.stmts.push(stmt);
    //     r
    // }

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
}

#[derive(Debug)]
pub struct FnDef {
    pub span: SourceSpan,
    pub name: DefaultSymbol,
    pub body: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Return(ExprRef),
}

#[derive(Debug)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum Expr {
    Constant {
        span: SourceSpan,
        value: u32,
    },
    Unary {
        span: SourceSpan,
        op: UnOp,
        expr: ExprRef,
    },
}
