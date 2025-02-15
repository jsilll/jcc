use crate::lex::{Token, TokenKind};

use tacky::{
    source_file::{diagnostic::Diagnostic, SourceFile, SourceSpan},
    string_interner::{DefaultSymbol, Symbol},
};

use std::{iter::Peekable, slice::Iter};

// ---------------------------------------------------------------------------
// Ast
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Ast {
    items: Vec<Item>,
    decls: Vec<Decl>,
    stmts: Vec<Stmt>,
    exprs: Vec<Expr>,
    items_span: Vec<SourceSpan>,
    decls_span: Vec<SourceSpan>,
    stmts_span: Vec<SourceSpan>,
    exprs_span: Vec<SourceSpan>,
}

impl Ast {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn items(&self) -> &[Item] {
        &self.items
    }

    pub fn get_item(&self, item: ItemRef) -> &Item {
        &self.items[item.0 as usize]
    }

    pub fn get_decl(&self, decl: DeclRef) -> &Decl {
        &self.decls[decl.0 as usize]
    }

    pub fn get_stmt(&self, stmt: StmtRef) -> &Stmt {
        &self.stmts[stmt.0 as usize]
    }

    pub fn get_expr(&self, expr: ExprRef) -> &Expr {
        &self.exprs[expr.0 as usize]
    }

    pub fn get_item_mut(&mut self, item: ItemRef) -> &mut Item {
        &mut self.items[item.0 as usize]
    }

    pub fn get_decl_mut(&mut self, decl: DeclRef) -> &mut Decl {
        &mut self.decls[decl.0 as usize]
    }

    pub fn get_stmt_mut(&mut self, stmt: StmtRef) -> &mut Stmt {
        &mut self.stmts[stmt.0 as usize]
    }

    pub fn get_expr_mut(&mut self, expr: ExprRef) -> &mut Expr {
        &mut self.exprs[expr.0 as usize]
    }

    pub fn get_item_span(&self, item: ItemRef) -> &SourceSpan {
        &self.items_span[item.0 as usize]
    }

    pub fn get_decl_span(&self, decl: DeclRef) -> &SourceSpan {
        &self.decls_span[decl.0 as usize]
    }

    pub fn get_stmt_span(&self, stmt: StmtRef) -> &SourceSpan {
        &self.stmts_span[stmt.0 as usize]
    }

    pub fn get_expr_span(&self, expr: ExprRef) -> &SourceSpan {
        &self.exprs_span[expr.0 as usize]
    }

    pub fn items_iter_refs(&self) -> impl Iterator<Item = ItemRef> {
        (0..self.items.len() as u32).map(ItemRef)
    }

    pub fn items_iter_both(&self) -> impl Iterator<Item = (ItemRef, &Item)> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, item)| (ItemRef(i as u32), item))
    }

    pub fn push_item(&mut self, item: Item, span: SourceSpan) -> ItemRef {
        let r = ItemRef(self.items.len() as u32);
        self.items.push(item);
        self.items_span.push(span);
        r
    }

    pub fn push_decl(&mut self, decl: Decl, span: SourceSpan) -> DeclRef {
        let r = DeclRef(self.decls.len() as u32);
        self.decls.push(decl);
        self.decls_span.push(span);
        r
    }

    pub fn push_stmt(&mut self, stmt: Stmt, span: SourceSpan) -> StmtRef {
        let r = StmtRef(self.stmts.len() as u32);
        self.stmts.push(stmt);
        self.stmts_span.push(span);
        r
    }

    pub fn push_expr(&mut self, expr: Expr, span: SourceSpan) -> ExprRef {
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
            .field("decls", &self.decls)
            .field("stmts", &self.stmts)
            .field("exprs", &self.exprs)
            .finish()
    }
}

// ---------------------------------------------------------------------------
// Ast Nodes
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ItemRef(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item {
    pub name: DefaultSymbol,
    pub body: Vec<BlockItem>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BlockItem {
    /// A declaration.
    Decl(DeclRef),
    /// A statement.
    Stmt(StmtRef),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DeclRef(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl {
    /// A variable declaration.
    Var {
        name: DefaultSymbol,
        value: Option<ExprRef>,
    },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StmtRef(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    /// An empty statement.
    Empty,
    /// An expression statement.
    Expr(ExprRef),
    /// A return statement.
    Return(ExprRef),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ExprRef(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// A constant integer value.
    Constant(u32),
    /// A variable reference.
    Var {
        name: DefaultSymbol,
        decl: Option<DeclRef>,
    },
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
    /// The `!` operator.
    LogicalNot,
}

impl From<UnaryOp> for tacky::UnaryOp {
    fn from(op: UnaryOp) -> Self {
        match op {
            UnaryOp::Neg => tacky::UnaryOp::Neg,
            UnaryOp::BitNot => tacky::UnaryOp::BitNot,
            UnaryOp::LogicalNot => tacky::UnaryOp::Not,
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
    // The `|` operator.
    BitOr,
    // The `&` operator.
    BitAnd,
    // The `^` operator.
    BitXor,
    // The `<<` operator.
    BitLsh,
    // The `>>` operator.
    BitRsh,
    // The `||` operator.
    LogicalOr,
    // The `&&` operator.
    LogicalAnd,
    // The `==` operator.
    Equal,
    // The `!=` operator.
    NotEqual,
    // The `<` operator.
    LessThan,
    // The `<=` operator.
    LessEqual,
    // The `>` operator.
    GreaterThan,
    // The `>=` operator.
    GreaterEqual,
    // The `=` operator.
    Assign,
}

impl TryFrom<BinaryOp> for tacky::BinaryOp {
    type Error = ();

    fn try_from(op: BinaryOp) -> Result<tacky::BinaryOp, Self::Error> {
        match op {
            BinaryOp::Add => Ok(tacky::BinaryOp::Add),
            BinaryOp::Sub => Ok(tacky::BinaryOp::Sub),
            BinaryOp::Mul => Ok(tacky::BinaryOp::Mul),
            BinaryOp::Div => Ok(tacky::BinaryOp::Div),
            BinaryOp::Rem => Ok(tacky::BinaryOp::Rem),
            BinaryOp::BitOr => Ok(tacky::BinaryOp::BitOr),
            BinaryOp::BitAnd => Ok(tacky::BinaryOp::BitAnd),
            BinaryOp::BitXor => Ok(tacky::BinaryOp::BitXor),
            BinaryOp::BitLsh => Ok(tacky::BinaryOp::BitShl),
            BinaryOp::BitRsh => Ok(tacky::BinaryOp::BitShr),
            BinaryOp::Equal => Ok(tacky::BinaryOp::Equal),
            BinaryOp::NotEqual => Ok(tacky::BinaryOp::NotEqual),
            BinaryOp::LessThan => Ok(tacky::BinaryOp::LessThan),
            BinaryOp::LessEqual => Ok(tacky::BinaryOp::LessEqual),
            BinaryOp::GreaterThan => Ok(tacky::BinaryOp::GreaterThan),
            BinaryOp::GreaterEqual => Ok(tacky::BinaryOp::GreaterEqual),
            _ => Err(()),
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
    pub span: SourceSpan,
    pub kind: ParserDiagnosticKind,
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a> {
    result: ParserResult,
    file: &'a SourceFile,
    iter: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, iter: Iter<'a, Token>) -> Self {
        Self {
            file,
            iter: iter.peekable(),
            result: ParserResult::default(),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        self.parse_item();
        if let Some(Token { span, .. }) = self.iter.next().cloned() {
            if self.result.diagnostics.is_empty() {
                self.result.diagnostics.push(ParserDiagnostic {
                    kind: ParserDiagnosticKind::UnexpectedToken,
                    span,
                })
            }
        }
        self.result
    }

    fn parse_item(&mut self) -> Option<ItemRef> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        self.eat(TokenKind::LParen)?;
        self.eat(TokenKind::KwVoid)?;
        self.eat(TokenKind::RParen)?;
        self.eat(TokenKind::LBrace)?;
        let body = self.parse_body()?;
        self.eat(TokenKind::RBrace)?;
        Some(self.result.ast.push_item(Item { name, body }, span))
    }

    fn parse_body(&mut self) -> Option<Vec<BlockItem>> {
        let mut body = Vec::new();
        while let Some(Token { kind, span }) = self.iter.peek() {
            match kind {
                TokenKind::RBrace => break,
                TokenKind::Semi => {
                    self.iter.next();
                    body.push(BlockItem::Stmt(
                        self.result.ast.push_stmt(Stmt::Empty, *span),
                    ));
                }
                TokenKind::KwReturn => {
                    self.iter.next();
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    let stmt = self.result.ast.push_stmt(Stmt::Return(expr), *span);
                    body.push(BlockItem::Stmt(stmt));
                }
                TokenKind::KwInt => {
                    self.iter.next();
                    let (span, name) = self.eat_identifier()?;
                    let value = match self.iter.peek() {
                        Some(Token {
                            kind: TokenKind::Eq,
                            ..
                        }) => {
                            self.iter.next();
                            Some(self.parse_expr(0)?)
                        }
                        _ => None,
                    };
                    self.eat(TokenKind::Semi)?;
                    body.push(BlockItem::Decl(
                        self.result.ast.push_decl(Decl::Var { name, value }, span),
                    ));
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    body.push(BlockItem::Stmt(
                        self.result.ast.push_stmt(Stmt::Expr(expr), *span),
                    ));
                }
            }
        }
        Some(body)
    }

    fn parse_expr(&mut self, min_prec: u8) -> Option<ExprRef> {
        let mut lhs = self.parse_expr_prefix()?;
        while let Some(Token { kind, span }) = self.iter.peek() {
            match Option::<Precedence>::from(*kind) {
                None => break,
                Some(Precedence { op, prec, assoc }) => {
                    if prec < min_prec {
                        break;
                    }
                    self.iter.next();
                    let rhs = match assoc {
                        Associativity::Right => self.parse_expr(prec)?,
                        Associativity::Left => self.parse_expr(prec + 1)?,
                    };
                    lhs = self
                        .result
                        .ast
                        .push_expr(Expr::Binary { op, lhs, rhs }, *span);
                }
            }
        }
        Some(lhs)
    }

    fn parse_expr_prefix(&mut self) -> Option<ExprRef> {
        let token = self.iter.peek().cloned()?;
        match token.kind {
            TokenKind::Number(value) => {
                self.iter.next();
                Some(self.result.ast.push_expr(Expr::Constant(value), token.span))
            }
            TokenKind::Identifier(name) => {
                self.iter.next();
                Some(
                    self.result
                        .ast
                        .push_expr(Expr::Var { name, decl: None }, token.span),
                )
            }
            TokenKind::LParen => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                Some(self.result.ast.push_expr(Expr::Grouped(expr), token.span))
            }
            TokenKind::Minus => {
                self.iter.next();
                let expr = self.parse_expr_prefix()?;
                Some(self.result.ast.push_expr(
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
                Some(self.result.ast.push_expr(
                    Expr::Unary {
                        op: UnaryOp::BitNot,
                        expr,
                    },
                    token.span,
                ))
            }
            TokenKind::Bang => {
                self.iter.next();
                let expr = self.parse_expr_prefix()?;
                Some(self.result.ast.push_expr(
                    Expr::Unary {
                        op: UnaryOp::LogicalNot,
                        expr,
                    },
                    token.span,
                ))
            }
            _ => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::UnexpectedToken,
                });
                None
            }
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Option<&Token> {
        let token = self.iter.peek().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
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
            self.result.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::ExpectedToken(kind),
            });
            None
        }
    }

    fn eat_identifier(&mut self) -> Option<(SourceSpan, DefaultSymbol)> {
        let token = self.iter.peek().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
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
            self.result.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Identifier(
                    // TODO: This is a hack, fix it.
                    DefaultSymbol::try_from_usize(0).expect("could not convert 0 to DefaultSymbol"),
                )),
            });
            None
        }
    }
}

// ---------------------------------------------------------------------------
// ParserDiagnosticKind
// ---------------------------------------------------------------------------

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
                format!("expected {token} instead"),
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Associativity
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

// ---------------------------------------------------------------------------
// Precedence
// ---------------------------------------------------------------------------

struct Precedence {
    op: BinaryOp,
    prec: u8,
    assoc: Associativity,
}

impl From<TokenKind> for Option<Precedence> {
    fn from(token: TokenKind) -> Self {
        match token {
            // Group: Right-to-left Associativity
            TokenKind::Eq => Some(Precedence {
                op: BinaryOp::Assign,
                prec: 0,
                assoc: Associativity::Right,
            }),
            // Group: Left-to-right Associativity
            TokenKind::PipePipe => Some(Precedence {
                op: BinaryOp::LogicalOr,
                prec: 1,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::AmpAmp => Some(Precedence {
                op: BinaryOp::LogicalAnd,
                prec: 2,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::Pipe => Some(Precedence {
                op: BinaryOp::BitOr,
                prec: 3,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::Caret => Some(Precedence {
                op: BinaryOp::BitXor,
                prec: 4,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::Amp => Some(Precedence {
                op: BinaryOp::BitAnd,
                prec: 5,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::EqEq => Some(Precedence {
                op: BinaryOp::Equal,
                prec: 6,
                assoc: Associativity::Left,
            }),
            TokenKind::BangEq => Some(Precedence {
                op: BinaryOp::NotEqual,
                prec: 6,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::Lt => Some(Precedence {
                op: BinaryOp::LessThan,
                prec: 7,
                assoc: Associativity::Left,
            }),
            TokenKind::Gt => Some(Precedence {
                op: BinaryOp::GreaterThan,
                prec: 7,
                assoc: Associativity::Left,
            }),
            TokenKind::LtEq => Some(Precedence {
                op: BinaryOp::LessEqual,
                prec: 7,
                assoc: Associativity::Left,
            }),
            TokenKind::GtEq => Some(Precedence {
                op: BinaryOp::GreaterEqual,
                prec: 7,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::LtLt => Some(Precedence {
                op: BinaryOp::BitLsh,
                prec: 8,
                assoc: Associativity::Left,
            }),
            TokenKind::GtGt => Some(Precedence {
                op: BinaryOp::BitRsh,
                prec: 8,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::Plus => Some(Precedence {
                op: BinaryOp::Add,
                prec: 9,
                assoc: Associativity::Left,
            }),
            TokenKind::Minus => Some(Precedence {
                op: BinaryOp::Sub,
                prec: 9,
                assoc: Associativity::Left,
            }),
            // Group: Left-to-right Associativity
            TokenKind::Star => Some(Precedence {
                op: BinaryOp::Mul,
                prec: 10,
                assoc: Associativity::Left,
            }),
            TokenKind::Slash => Some(Precedence {
                op: BinaryOp::Div,
                prec: 10,
                assoc: Associativity::Left,
            }),
            TokenKind::Percent => Some(Precedence {
                op: BinaryOp::Rem,
                prec: 10,
                assoc: Associativity::Left,
            }),
            _ => None,
        }
    }
}
