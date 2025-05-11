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
    root: Vec<DeclRef>,
    decls: Vec<Decl>,
    stmts: Vec<Stmt>,
    exprs: Vec<Expr>,
    decls_span: Vec<SourceSpan>,
    stmts_span: Vec<SourceSpan>,
    exprs_span: Vec<SourceSpan>,
    sliced_args: Vec<ExprRef>,
    sliced_params: Vec<DeclRef>,
    sliced_block_items: Vec<BlockItem>,
}

impl Default for Ast {
    fn default() -> Self {
        Ast {
            root: Default::default(),
            decls: vec![Default::default()],
            stmts: vec![Default::default()],
            exprs: vec![Default::default()],
            decls_span: vec![Default::default()],
            stmts_span: vec![Default::default()],
            exprs_span: vec![Default::default()],
            sliced_args: Default::default(),
            sliced_params: Default::default(),
            sliced_block_items: Default::default(),
        }
    }
}

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ast")
            .field("root", &self.root)
            .field("decls", &&self.decls[1..])
            .field("stmts", &&self.stmts[1..])
            .field("exprs", &&self.exprs[1..])
            .field("sliced_args", &self.sliced_args)
            .field("sliced_params", &self.sliced_params)
            .field("sliced_block_items", &self.sliced_block_items)
            .finish()
    }
}

impl Ast {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn root(&self) -> &[DeclRef] {
        &self.root
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

    #[inline]
    pub fn args(&self, slice: Slice<ExprRef>) -> &[ExprRef] {
        &self.sliced_args[slice.0 as usize..slice.1 as usize]
    }

    #[inline]
    pub fn params(&self, slice: Slice<DeclRef>) -> &[DeclRef] {
        &self.sliced_params[slice.0 as usize..slice.1 as usize]
    }

    #[inline]
    pub fn block_items(&self, slice: Slice<BlockItem>) -> &[BlockItem] {
        &self.sliced_block_items[slice.0 as usize..slice.1 as usize]
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
    pub fn new_args(&mut self, args: impl IntoIterator<Item = ExprRef>) -> Slice<ExprRef> {
        let begin = self.sliced_args.len() as u32;
        self.sliced_args.extend(args);
        let end = self.sliced_args.len() as u32;
        Slice::new(begin, end)
    }

    #[inline]
    pub fn new_params(&mut self, params: impl IntoIterator<Item = DeclRef>) -> Slice<DeclRef> {
        let begin = self.sliced_params.len() as u32;
        self.sliced_params.extend(params);
        let end = self.sliced_params.len() as u32;
        Slice::new(begin, end)
    }

    #[inline]
    pub fn new_block_items(
        &mut self,
        items: impl IntoIterator<Item = BlockItem>,
    ) -> Slice<BlockItem> {
        let begin = self.sliced_block_items.len() as u32;
        self.sliced_block_items.extend(items);
        let end = self.sliced_block_items.len() as u32;
        Slice::new(begin, end)
    }
}

// ---------------------------------------------------------------------------
// Ast Nodes
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DeclRef(pub(crate)NonZeroU32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Decl {
    /// A variable declaration.
    Var { name: Symbol, init: Option<ExprRef> },
    /// A function declaration.
    Func {
        name: Symbol,
        params: Slice<DeclRef>,
        body: Option<Slice<BlockItem>>,
    },
}

impl Default for Decl {
    fn default() -> Self {
        Self::Var {
            name: Default::default(),
            init: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StmtRef(pub(crate)NonZeroU32);

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
    Compound(Slice<BlockItem>),
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
pub struct ExprRef(pub(crate)NonZeroU32);

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
    Call { name: Symbol, args: Slice<ExprRef> },
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
// Auxiliary structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Slice<T>(u32, u32, std::marker::PhantomData<T>);

impl<T> Slice<T> {
    #[inline]
    pub fn new(begin: u32, end: u32) -> Self {
        Slice(begin, end, std::marker::PhantomData)
    }
}

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Slice(0, 0, std::marker::PhantomData)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BlockItem {
    /// A declaration.
    Decl(DeclRef),
    /// A statement.
    Stmt(StmtRef),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum ForInit {
    /// An expression.
    Expr(ExprRef),
    /// A variable declaration.
    VarDecl(DeclRef),
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
    args_stack: Vec<ExprRef>,
    params_stack: Vec<DeclRef>,
    block_items_stack: Vec<BlockItem>,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, interner: &'a mut Interner, iter: Iter<'a, Token>) -> Self {
        Self {
            file,
            interner,
            iter: iter.peekable(),
            result: ParserResult::default(),
            args_stack: Vec::with_capacity(16),
            params_stack: Vec::with_capacity(16),
            block_items_stack: Vec::with_capacity(16),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        if let Some(main) = self.parse_func_decl() {
            self.result.ast.root.push(main);
        }
        assert!(self.block_items_stack.is_empty());
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

    #[inline]
    fn intern_span(&mut self, span: &SourceSpan) -> Symbol {
        self.interner
            .get_or_intern(self.file.slice(*span).expect("expected span to be valid"))
    }

    fn parse_var_decl(&mut self) -> Option<DeclRef> {
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
        Some(self.result.ast.new_decl(Decl::Var { name, init }, span))
    }

    fn parse_func_decl(&mut self) -> Option<DeclRef> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        self.eat(TokenKind::LParen)?;
        let params = self.parse_params();
        self.eat(TokenKind::RParen)?;
        let body = if let Some(Token {
            kind: TokenKind::Semi,
            ..
        }) = self.iter.peek()
        {
            self.iter.next();
            None
        } else {
            self.eat(TokenKind::LBrace)?;
            let body = self.parse_body();
            self.eat(TokenKind::RBrace)?;
            Some(body)
        };
        Some(
            self.result
                .ast
                .new_decl(Decl::Func { name, params, body }, span),
        )
    }

    fn parse_params(&mut self) -> Slice<DeclRef> {
        let Some(token) = self.iter.peek() else {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            return Slice::default();
        };
        match token.kind {
            TokenKind::RParen => Slice::default(),
            TokenKind::KwVoid => {
                self.iter.next();
                Slice::default()
            }
            TokenKind::KwInt => {
                self.iter.next();
                if let Some(decl) = self.parse_var_decl() {
                    self.params_stack.push(decl);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.iter.peek()
                {
                    self.iter.next();
                    if let Some(decl) = self.parse_var_decl() {
                        self.params_stack.push(decl);
                    }
                }
                self.result.ast.new_params(self.params_stack.drain(0..))
            }
            _ => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::UnexpectedToken,
                });
                Slice::default()
            }
        }
    }

    fn parse_body(&mut self) -> Slice<BlockItem> {
        while let Some(Token { kind, .. }) = self.iter.peek() {
            match kind {
                TokenKind::RBrace => break,
                TokenKind::KwInt => match self.parse_var_decl() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(decl) => self.block_items_stack.push(BlockItem::Decl(decl)),
                },
                _ => match self.parse_stmt() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(expr) => self.block_items_stack.push(BlockItem::Stmt(expr)),
                },
            }
        }
        self.result
            .ast
            .new_block_items(self.block_items_stack.drain(0..))
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
            TokenKind::KwFor => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let init = if let Some(Token {
                    kind: TokenKind::KwInt,
                    ..
                }) = self.iter.peek()
                {
                    Some(ForInit::VarDecl(self.parse_var_decl()?))
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
                        InfixToken::Binary(op) => {
                            let rhs = self.parse_expr(prec)?;
                            lhs = self
                                .result
                                .ast
                                .new_expr(Expr::Binary { op, lhs, rhs }, *span);
                        }
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
                    }
                }
            }
        }
        Some(lhs)
    }

    fn parse_expr_prefix(&mut self) -> Option<ExprRef> {
        let Token { kind, span } = self.iter.peek()?;
        match kind {
            TokenKind::LParen => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let expr = self.result.ast.new_expr(Expr::Grouped(expr), *span);
                Some(self.parse_expr_postfix(expr))
            }
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
                match self.iter.peek() {
                    Some(Token {
                        kind: TokenKind::LParen,
                        ..
                    }) => {
                        self.iter.next();
                        let args = self.parse_args();
                        self.eat(TokenKind::RParen)?;
                        Some(self.result.ast.new_expr(Expr::Call { name, args }, *span))
                    }
                    _ => Some(self.parse_expr_postfix(expr)),
                }
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

    #[inline]
    fn parse_prefix_operator(&mut self, op: UnaryOp, span: &SourceSpan) -> Option<ExprRef> {
        self.iter.next();
        let expr = self.parse_expr_prefix()?;
        Some(self.result.ast.new_expr(Expr::Unary { op, expr }, *span))
    }

    fn parse_expr_postfix(&mut self, mut expr: ExprRef) -> ExprRef {
        fn parse_postfix_operator(
            parser: &mut Parser,
            expr: &mut ExprRef,
            op: UnaryOp,
            span: &SourceSpan,
        ) {
            parser.iter.next();
            *expr = parser
                .result
                .ast
                .new_expr(Expr::Unary { op, expr: *expr }, *span);
        }
        while let Some(Token { kind, span }) = self.iter.peek() {
            match kind {
                TokenKind::PlusPlus => {
                    parse_postfix_operator(self, &mut expr, UnaryOp::PostInc, span)
                }
                TokenKind::MinusMinus => {
                    parse_postfix_operator(self, &mut expr, UnaryOp::PostDec, span)
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_args(&mut self) -> Slice<ExprRef> {
        let Some(token) = self.iter.peek() else {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            return Slice::default();
        };
        match token.kind {
            TokenKind::RParen => Slice::default(),
            _ => {
                if let Some(expr) = self.parse_expr(0) {
                    self.args_stack.push(expr);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.iter.peek()
                {
                    self.iter.next();
                    if let Some(expr) = self.parse_expr(0) {
                        self.args_stack.push(expr);
                    }
                }
                self.eat(TokenKind::RParen);
                self.result.ast.new_args(self.args_stack.drain(0..))
            }
        }
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
