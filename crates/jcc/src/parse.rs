use crate::lex::{Token, TokenKind};

use tacky::{
    source_file::{diag::Diagnostic, SourceFile, SourceSpan},
    Interner, Symbol,
};

use std::{iter::Peekable, num::NonZeroU32, slice::Iter};

// ---------------------------------------------------------------------------
// Ast
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Ast {
    items: Vec<Item>,
    decls: Vec<Decl>,
    stmts: Vec<Stmt>,
    exprs: Vec<Expr>,
    args: Vec<ExprRef>,
    block_items: Vec<BlockItem>,
    items_span: Vec<SourceSpan>,
    decls_span: Vec<SourceSpan>,
    stmts_span: Vec<SourceSpan>,
    exprs_span: Vec<SourceSpan>,
}

impl Default for Ast {
    fn default() -> Self {
        Ast {
            args: Default::default(),
            block_items: Default::default(),
            items: vec![Default::default()],
            decls: vec![Default::default()],
            stmts: vec![Default::default()],
            exprs: vec![Default::default()],
            items_span: vec![Default::default()],
            decls_span: vec![Default::default()],
            stmts_span: vec![Default::default()],
            exprs_span: vec![Default::default()],
        }
    }
}

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ast")
            .field("items", &&self.items[1..])
            .field("decls", &&self.decls[1..])
            .field("stmts", &&self.stmts[1..])
            .field("exprs", &&self.exprs[1..])
            .field("args", &self.args)
            .field("block_items", &self.block_items)
            .finish()
    }
}

impl Ast {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn items(&self) -> &[Item] {
        &self.items[1..]
    }

    #[inline]
    pub fn item(&self, item: ItemRef) -> &Item {
        &self.items[item.0.get() as usize]
    }

    #[inline]
    pub fn decl(&self, decl: DeclRef) -> &Decl {
        &self.decls[decl.0.get() as usize]
    }

    #[inline]
    pub fn stmt(&self, stmt: StmtRef) -> &Stmt {
        &self.stmts[stmt.0.get() as usize]
    }

    #[inline]
    pub fn expr(&self, expr: ExprRef) -> &Expr {
        &self.exprs[expr.0.get() as usize]
    }

    #[inline]
    pub fn item_span(&self, item: ItemRef) -> &SourceSpan {
        &self.items_span[item.0.get() as usize]
    }

    #[inline]
    pub fn decl_span(&self, decl: DeclRef) -> &SourceSpan {
        &self.decls_span[decl.0.get() as usize]
    }

    #[inline]
    pub fn stmt_span(&self, stmt: StmtRef) -> &SourceSpan {
        &self.stmts_span[stmt.0.get() as usize]
    }

    #[inline]
    pub fn expr_span(&self, expr: ExprRef) -> &SourceSpan {
        &self.exprs_span[expr.0.get() as usize]
    }

    #[inline]
    pub fn item_mut(&mut self, item: ItemRef) -> &mut Item {
        &mut self.items[item.0.get() as usize]
    }

    #[inline]
    pub fn decl_mut(&mut self, decl: DeclRef) -> &mut Decl {
        &mut self.decls[decl.0.get() as usize]
    }

    #[inline]
    pub fn stmt_mut(&mut self, stmt: StmtRef) -> &mut Stmt {
        &mut self.stmts[stmt.0.get() as usize]
    }

    #[inline]
    pub fn expr_mut(&mut self, expr: ExprRef) -> &mut Expr {
        &mut self.exprs[expr.0.get() as usize]
    }

    pub fn args(&self, slice: ArgsSlice) -> &[ExprRef] {
        &self.args[slice.begin as usize..slice.end as usize]
    }

    #[inline]
    pub fn block_items(&self, slice: BlockItemSlice) -> &[BlockItem] {
        &self.block_items[slice.begin as usize..slice.end as usize]
    }

    #[inline]
    pub fn new_item(&mut self, item: Item, span: SourceSpan) -> ItemRef {
        let r = ItemRef(NonZeroU32::new(self.items.len() as u32).unwrap());
        self.items.push(item);
        self.items_span.push(span);
        r
    }

    #[inline]
    pub fn new_decl(&mut self, decl: Decl, span: SourceSpan) -> DeclRef {
        let r = DeclRef(NonZeroU32::new(self.decls.len() as u32).unwrap());
        self.decls.push(decl);
        self.decls_span.push(span);
        r
    }

    #[inline]
    pub fn new_stmt(&mut self, stmt: Stmt, span: SourceSpan) -> StmtRef {
        let r = StmtRef(NonZeroU32::new(self.stmts.len() as u32).unwrap());
        self.stmts.push(stmt);
        self.stmts_span.push(span);
        r
    }

    #[inline]
    pub fn new_expr(&mut self, expr: Expr, span: SourceSpan) -> ExprRef {
        let r = ExprRef(NonZeroU32::new(self.exprs.len() as u32).unwrap());
        self.exprs.push(expr);
        self.exprs_span.push(span);
        r
    }

    #[inline]
    pub fn new_args(&mut self, args: impl IntoIterator<Item = ExprRef>) -> ArgsSlice {
        let begin = self.args.len() as u32;
        self.args.extend(args);
        let end = self.args.len() as u32;
        ArgsSlice { begin, end }
    }

    #[inline]
    pub fn new_block_items(
        &mut self,
        items: impl IntoIterator<Item = BlockItem>,
    ) -> BlockItemSlice {
        let begin = self.block_items.len() as u32;
        self.block_items.extend(items);
        let end = self.block_items.len() as u32;
        BlockItemSlice { begin, end }
    }

    #[inline]
    pub fn items_iter(&self) -> impl Iterator<Item = ItemRef> {
        unsafe { (1..self.items.len()).map(|i| ItemRef(NonZeroU32::new_unchecked(i as u32))) }
    }

    #[inline]
    pub fn items_iter2(&self) -> impl Iterator<Item = (ItemRef, &Item)> {
        self.items_iter().zip(self.items().iter())
    }
}

// ---------------------------------------------------------------------------
// Ast Nodes
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ItemRef(NonZeroU32);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Item {
    pub name: Symbol,
    pub body: BlockItemSlice,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DeclRef(NonZeroU32);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Decl {
    pub name: Symbol,
    pub init: Option<ExprRef>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StmtRef(NonZeroU32);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    /// An empty statement.
    #[default]
    Empty,
    /// A break statement.
    Break,
    /// A continue statement.
    Continue,
    /// A goto statement.
    Goto(Symbol),
    /// An expression statement.
    Expr(ExprRef),
    /// A return statement.
    Return(ExprRef),
    /// A default statement.
    Default(StmtRef),
    /// A compound statement.
    Compound(BlockItemSlice),
    /// A case statement.
    Case { expr: ExprRef, stmt: StmtRef },
    /// A label statement.
    Label { label: Symbol, stmt: StmtRef },
    /// A switch statement.
    Switch { cond: ExprRef, body: StmtRef },
    /// An if statement.
    If {
        cond: ExprRef,
        then: StmtRef,
        otherwise: Option<StmtRef>,
    },
    /// A while statement.
    While { cond: ExprRef, body: StmtRef },
    /// A do-while statement.
    DoWhile { body: StmtRef, cond: ExprRef },
    /// A for statement.
    For {
        init: Option<ForInit>,
        cond: Option<ExprRef>,
        step: Option<ExprRef>,
        body: StmtRef,
    },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ExprRef(NonZeroU32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Expr {
    /// A constant integer value.
    Const(i64),
    /// A variable reference.
    Var(Symbol),
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
    /// A function call expression.
    Call { name: Symbol, args: ArgsSlice },
}

impl Default for Expr {
    fn default() -> Self {
        Self::Const(0)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
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
    BitShl,
    /// The `>>` operator.
    BitShr,
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
    BitShlAssign,
    /// The `>>=` operator.
    BitShrAssign,
}

// ---------------------------------------------------------------------------
// Support structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum ForInit {
    /// A declaration.
    Decl(DeclRef),
    /// An expression.
    Expr(ExprRef),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BlockItem {
    /// A declaration.
    Decl(DeclRef),
    /// A statement.
    Stmt(StmtRef),
}

#[derive(Default, Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockItemSlice {
    begin: u32,
    end: u32,
}

#[derive(Default, Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ArgsSlice {
    pub begin: u32,
    pub end: u32,
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
    file: &'a SourceFile,
    interner: &'a mut Interner,
    iter: Peekable<Iter<'a, Token>>,
    result: ParserResult,
    block_item_stack: Vec<BlockItem>,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, interner: &'a mut Interner, iter: Iter<'a, Token>) -> Self {
        Self {
            file,
            interner,
            iter: iter.peekable(),
            result: ParserResult::default(),
            block_item_stack: Vec::with_capacity(16),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        self.parse_item();
        assert!(self.block_item_stack.is_empty());
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
        let body = self.parse_body();
        self.eat(TokenKind::RBrace)?;
        Some(self.result.ast.new_item(Item { name, body }, span))
    }

    fn parse_body(&mut self) -> BlockItemSlice {
        let base = self.block_item_stack.len();
        while let Some(Token { kind, .. }) = self.iter.peek() {
            match kind {
                TokenKind::RBrace => break,
                TokenKind::KwInt => match self.parse_decl() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(decl) => self.block_item_stack.push(BlockItem::Decl(decl)),
                },
                _ => match self.parse_stmt() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(expr) => self.block_item_stack.push(BlockItem::Stmt(expr)),
                },
            }
        }
        self.result
            .ast
            .new_block_items(self.block_item_stack.drain(base..))
    }

    fn parse_decl(&mut self) -> Option<DeclRef> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        let init = match self.iter.peek() {
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
        Some(self.result.ast.new_decl(Decl { name, init }, span))
    }

    fn parse_stmt(&mut self) -> Option<StmtRef> {
        let Token { kind, span } = self.iter.peek()?;
        match kind {
            TokenKind::Semi => {
                self.iter.next();
                Some(self.result.ast.new_stmt(Stmt::Empty, *span))
            }
            TokenKind::KwBreak => {
                self.iter.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Break, *span))
            }
            TokenKind::KwContinue => {
                self.iter.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Continue, *span))
            }
            TokenKind::KwReturn => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Return(expr), *span))
            }
            TokenKind::KwDefault => {
                self.iter.next();
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::Default(stmt), *span))
            }
            TokenKind::KwGoto => {
                self.iter.next();
                let (_, name) = self.eat_identifier()?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Goto(name), *span))
            }
            TokenKind::LBrace => {
                self.iter.next();
                let body = self.parse_body();
                self.eat(TokenKind::RBrace)?;
                Some(self.result.ast.new_stmt(Stmt::Compound(body), *span))
            }
            TokenKind::KwCase => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::Case { expr, stmt }, *span))
            }
            TokenKind::KwSwitch => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::Switch { cond, body }, *span))
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
                Some(self.result.ast.new_stmt(
                    Stmt::If {
                        cond,
                        then,
                        otherwise,
                    },
                    *span,
                ))
            }
            TokenKind::KwWhile => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::While { cond, body }, *span))
            }
            TokenKind::KwDo => {
                self.iter.next();
                let body = self.parse_stmt()?;
                self.eat(TokenKind::KwWhile)?;
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                self.eat(TokenKind::Semi)?;
                Some(
                    self.result
                        .ast
                        .new_stmt(Stmt::DoWhile { body, cond }, *span),
                )
            }
            TokenKind::KwFor => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let init = if let Some(Token {
                    kind: TokenKind::KwInt,
                    ..
                }) = self.iter.peek()
                {
                    Some(ForInit::Decl(self.parse_decl()?))
                } else {
                    self.parse_optional_expr(TokenKind::Semi).map(ForInit::Expr)
                };
                let cond = self.parse_optional_expr(TokenKind::Semi);
                let step = self.parse_optional_expr(TokenKind::RParen);
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(
                    Stmt::For {
                        init,
                        cond,
                        step,
                        body,
                    },
                    *span,
                ))
            }
            TokenKind::Identifier => match self.iter.clone().skip(1).next() {
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    self.iter.next();
                    self.iter.next();
                    let stmt = self.parse_stmt()?;
                    let label = self.intern_span(span);
                    Some(self.result.ast.new_stmt(Stmt::Label { label, stmt }, *span))
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    Some(self.result.ast.new_stmt(Stmt::Expr(expr), *span))
                }
            },
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Expr(expr), *span))
            }
        }
    }

    fn parse_optional_expr(&mut self, delim: TokenKind) -> Option<ExprRef> {
        match self.iter.peek() {
            Some(Token { kind, .. }) if *kind == delim => {
                self.iter.next();
                None
            }
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(delim)?;
                Some(expr)
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
                            lhs = self.result.ast.new_expr(
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
                                .new_expr(Expr::Binary { op, lhs, rhs }, *span);
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
            TokenKind::Number => {
                self.iter.next();
                let number = self.file.slice(*span).expect("expected span to be valid");
                let number = number.parse().expect("expected number to be valid");
                Some(self.result.ast.new_expr(Expr::Const(number), *span))
            }
            TokenKind::Identifier => {
                self.iter.next();
                let name = self.intern_span(span);
                let expr = self.result.ast.new_expr(Expr::Var(name), *span);
                Some(self.parse_expr_postfix(expr))
            }
            TokenKind::LParen => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let expr = self.result.ast.new_expr(Expr::Grouped(expr), *span);
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
        Some(self.result.ast.new_expr(Expr::Unary { op, expr }, *span))
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
            .new_expr(Expr::Unary { op, expr: *expr }, *span);
    }

    fn sync(&mut self, eat: TokenKind, stop: TokenKind) {
        while let Some(token) = self.iter.peek() {
            if token.kind == eat {
                self.iter.next();
                break;
            }
            if token.kind == stop {
                break;
            }
            self.iter.next();
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
        if token.kind == kind {
            self.iter.next()
        } else {
            self.result.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::ExpectedToken(kind),
            });
            None
        }
    }

    fn eat_identifier(&mut self) -> Option<(SourceSpan, Symbol)> {
        let token = self.iter.peek().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })?;
        match token.kind {
            TokenKind::Identifier => {
                let span = token.span;
                self.iter.next();
                let symbol = self.intern_span(&span);
                Some((span, symbol))
            }
            _ => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Identifier),
                });
                None
            }
        }
    }

    fn intern_span(&mut self, span: &SourceSpan) -> Symbol {
        self.interner
            .get_or_intern(self.file.slice(*span).expect("expected span to be valid"))
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
                token: InfixToken::Binary(BinaryOp::BitShlAssign),
            }),
            TokenKind::GtGtEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitShrAssign),
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
                token: InfixToken::Binary(BinaryOp::BitShl),
            }),
            TokenKind::GtGt => Some(Precedence {
                prec: 9,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitShr),
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
