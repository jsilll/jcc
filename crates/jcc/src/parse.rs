use crate::lex::{Token, TokenKind};

use tacky::{
    source_file::{diag::Diagnostic, SourceFile, SourceSpan},
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

    pub fn items_iter_refs(&self) -> impl Iterator<Item = ItemRef> {
        (0..self.items.len() as u32).map(ItemRef)
    }

    pub fn items_iter_both(&self) -> impl Iterator<Item = (ItemRef, &Item)> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, item)| (ItemRef(i as u32), item))
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
        init: Option<ExprRef>,
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
    /// A goto statement.
    Goto(DefaultSymbol),
    /// A compound statement.
    Compound(Vec<BlockItem>),
    /// A label statement.
    Label { label: DefaultSymbol, stmt: StmtRef },
    /// An if statement.
    If {
        cond: ExprRef,
        then: StmtRef,
        otherwise: Option<StmtRef>,
    },
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
    /// A ternary expression.
    Ternary {
        cond: ExprRef,
        then: ExprRef,
        otherwise: ExprRef,
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
    /// The prefix `++` operator.
    PreInc,
    /// The prefix `--` operator.
    PreDec,
    /// The postfix `++` operator.
    PostInc,
    /// The postfix `--` operator.
    PostDec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// The `||` operator.
    LogicalOr,
    /// The `&&` operator.
    LogicalAnd,
    /// The `==` operator.
    Equal,
    /// The `!=` operator.
    NotEqual,
    /// The `<` operator.
    LessThan,
    /// The `<=` operator.
    LessEqual,
    /// The `>` operator.
    GreaterThan,
    /// The `>=` operator.
    GreaterEqual,
    /// The `+` operator.
    Add,
    /// The `-` operator.
    Sub,
    /// The `*` operator.
    Mul,
    /// The `/` operator.
    Div,
    /// The `%` operator.
    Rem,
    /// The `|` operator.
    BitOr,
    /// The `&` operator.
    BitAnd,
    /// The `^` operator.
    BitXor,
    /// The `<<` operator.
    BitLsh,
    /// The `>>` operator.
    BitRsh,
    /// The `=` operator.
    Assign,
    /// The `+=` operator.
    AddAssign,
    /// The `-=` operator.
    SubAssign,
    /// The `*=` operator.
    MulAssign,
    /// The `/=` operator.
    DivAssign,
    /// The `%=` operator.
    RemAssign,
    /// The `|=` operator.
    BitOrAssign,
    /// The `&=` operator.
    BitAndAssign,
    /// The `^=` operator.
    BitXorAssign,
    /// The `<<=` operator.
    BitLshAssign,
    /// The `>>=` operator.
    BitRshAssign,
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
        if let Some(Token { span, .. }) = self.iter.next() {
            if self.result.diagnostics.is_empty() {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: *span,
                    kind: ParserDiagnosticKind::UnexpectedToken,
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
        while let Some(Token { kind, .. }) = self.iter.peek() {
            match kind {
                TokenKind::RBrace => break,
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
                        self.result
                            .ast
                            .push_decl(Decl::Var { name, init: value }, span),
                    ));
                }
                _ => body.push(BlockItem::Stmt(self.parse_stmt()?)),
            }
        }
        Some(body)
    }

    fn parse_stmt(&mut self) -> Option<StmtRef> {
        let Token { kind, span } = self.iter.peek()?;
        match kind {
            TokenKind::Semi => {
                self.iter.next();
                Some(self.result.ast.push_stmt(Stmt::Empty, *span))
            }
            TokenKind::KwReturn => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.push_stmt(Stmt::Return(expr), *span))
            }
            TokenKind::KwGoto => {
                self.iter.next();
                let (_, name) = self.eat_identifier()?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.push_stmt(Stmt::Goto(name), *span))
            }
            TokenKind::LBrace => {
                self.iter.next();
                let body = self.parse_body()?;
                self.eat(TokenKind::RBrace)?;
                Some(self.result.ast.push_stmt(Stmt::Compound(body), *span))
            }
            TokenKind::KwIf => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let then = self.parse_stmt()?;
                let otherwise = match self.iter.peek() {
                    Some(Token {
                        kind: TokenKind::KwElse,
                        ..
                    }) => {
                        self.iter.next();
                        Some(self.parse_stmt()?)
                    }
                    _ => None,
                };
                Some(self.result.ast.push_stmt(
                    Stmt::If {
                        cond,
                        then,
                        otherwise,
                    },
                    *span,
                ))
            }
            TokenKind::Identifier(name) => match self.iter.clone().skip(1).next() {
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    self.iter.next();
                    self.iter.next();
                    let stmt = self.parse_stmt()?;
                    Some(
                        self.result
                            .ast
                            .push_stmt(Stmt::Label { label: *name, stmt }, *span),
                    )
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    Some(self.result.ast.push_stmt(Stmt::Expr(expr), *span))
                }
            },
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.push_stmt(Stmt::Expr(expr), *span))
            }
        }
    }

    fn parse_expr(&mut self, min_prec: u8) -> Option<ExprRef> {
        let mut lhs = self.parse_expr_prefix()?;
        while let Some(Token { kind, span }) = self.iter.peek() {
            match Option::<Precedence>::from(*kind) {
                None => break,
                Some(Precedence { token, prec, assoc }) => {
                    if prec < min_prec {
                        break;
                    }
                    self.iter.next();
                    let prec = match assoc {
                        Associativity::Right => prec,
                        Associativity::Left => prec + 1,
                    };
                    match token {
                        InfixToken::Ternary => {
                            let then = self.parse_expr(0)?;
                            self.eat(TokenKind::Colon)?;
                            let otherwise = self.parse_expr(prec)?;
                            lhs = self.result.ast.push_expr(
                                Expr::Ternary {
                                    cond: lhs,
                                    then,
                                    otherwise,
                                },
                                *span,
                            );
                        }
                        InfixToken::Binary(op) => {
                            let rhs = self.parse_expr(prec)?;
                            lhs = self
                                .result
                                .ast
                                .push_expr(Expr::Binary { op, lhs, rhs }, *span);
                        }
                    }
                }
            }
        }
        Some(lhs)
    }

    fn parse_expr_prefix(&mut self) -> Option<ExprRef> {
        let Token { kind, span } = self.iter.peek()?;
        match kind {
            TokenKind::Number(value) => {
                self.iter.next();
                Some(self.result.ast.push_expr(Expr::Constant(*value), *span))
            }
            TokenKind::Identifier(name) => {
                self.iter.next();
                let expr = self.result.ast.push_expr(
                    Expr::Var {
                        name: *name,
                        decl: None,
                    },
                    *span,
                );
                Some(self.parse_expr_postfix(expr))
            }
            TokenKind::LParen => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let expr = self.result.ast.push_expr(Expr::Grouped(expr), *span);
                Some(self.parse_expr_postfix(expr))
            }
            TokenKind::Minus => self.parse_prefix_operator(UnaryOp::Neg, span),
            TokenKind::Tilde => self.parse_prefix_operator(UnaryOp::BitNot, span),
            TokenKind::Bang => self.parse_prefix_operator(UnaryOp::LogicalNot, span),
            TokenKind::PlusPlus => self.parse_prefix_operator(UnaryOp::PreInc, span),
            TokenKind::MinusMinus => self.parse_prefix_operator(UnaryOp::PreDec, span),
            _ => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: *span,
                    kind: ParserDiagnosticKind::UnexpectedToken,
                });
                None
            }
        }
    }

    fn parse_prefix_operator(&mut self, op: UnaryOp, span: &SourceSpan) -> Option<ExprRef> {
        self.iter.next();
        let expr = self.parse_expr_prefix()?;
        Some(self.result.ast.push_expr(Expr::Unary { op, expr }, *span))
    }

    fn parse_expr_postfix(&mut self, mut expr: ExprRef) -> ExprRef {
        while let Some(Token { kind, span }) = self.iter.peek() {
            match kind {
                TokenKind::PlusPlus => {
                    self.parse_postfix_operator(&mut expr, UnaryOp::PostInc, span)
                }
                TokenKind::MinusMinus => {
                    self.parse_postfix_operator(&mut expr, UnaryOp::PostDec, span)
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_postfix_operator(&mut self, expr: &mut ExprRef, op: UnaryOp, span: &SourceSpan) {
        self.iter.next();
        *expr = self
            .result
            .ast
            .push_expr(Expr::Unary { op, expr: *expr }, *span);
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
// InfixToken
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixToken {
    Ternary,
    Binary(BinaryOp),
}

// ---------------------------------------------------------------------------
// Precedence
// ---------------------------------------------------------------------------

struct Precedence {
    prec: u8,
    token: InfixToken,
    assoc: Associativity,
}

impl From<TokenKind> for Option<Precedence> {
    fn from(token: TokenKind) -> Self {
        match token {
            // Group: Right-to-left Associativity
            TokenKind::Eq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::Assign),
            }),
            TokenKind::PlusEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::AddAssign),
            }),
            TokenKind::MinusEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::SubAssign),
            }),
            TokenKind::StarEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::MulAssign),
            }),
            TokenKind::SlashEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::DivAssign),
            }),
            TokenKind::PercentEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::RemAssign),
            }),
            TokenKind::AmpEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitAndAssign),
            }),
            TokenKind::PipeEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitOrAssign),
            }),
            TokenKind::CaretEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitXorAssign),
            }),
            TokenKind::LtLtEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitLshAssign),
            }),
            TokenKind::GtGtEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitRshAssign),
            }),
            // Group: Right-to-left Associativity
            TokenKind::Question => Some(Precedence {
                prec: 1,
                token: InfixToken::Ternary,
                assoc: Associativity::Right,
            }),
            // Group: Left-to-right Associativity
            TokenKind::PipePipe => Some(Precedence {
                prec: 2,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LogicalOr),
            }),
            // Group: Left-to-right Associativity
            TokenKind::AmpAmp => Some(Precedence {
                prec: 3,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LogicalAnd),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Pipe => Some(Precedence {
                prec: 4,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitOr),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Caret => Some(Precedence {
                prec: 5,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitXor),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Amp => Some(Precedence {
                prec: 6,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitAnd),
            }),
            // Group: Left-to-right Associativity
            TokenKind::EqEq => Some(Precedence {
                prec: 7,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Equal),
            }),
            TokenKind::BangEq => Some(Precedence {
                prec: 7,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::NotEqual),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Lt => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LessThan),
            }),
            TokenKind::Gt => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::GreaterThan),
            }),
            TokenKind::LtEq => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LessEqual),
            }),
            TokenKind::GtEq => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::GreaterEqual),
            }),
            // Group: Left-to-right Associativity
            TokenKind::LtLt => Some(Precedence {
                prec: 9,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitLsh),
            }),
            TokenKind::GtGt => Some(Precedence {
                prec: 9,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitRsh),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Plus => Some(Precedence {
                prec: 10,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Add),
            }),
            TokenKind::Minus => Some(Precedence {
                prec: 10,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Sub),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Star => Some(Precedence {
                prec: 11,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Mul),
            }),
            TokenKind::Slash => Some(Precedence {
                prec: 11,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Div),
            }),
            TokenKind::Percent => Some(Precedence {
                prec: 11,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Rem),
            }),
            _ => None,
        }
    }
}
