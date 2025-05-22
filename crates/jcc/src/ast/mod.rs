pub mod parse;

pub mod graphviz;

use ssa::{sourcemap::SourceSpan, Symbol};

use std::num::NonZeroU32;

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
        self.root.as_slice()
    }

    #[inline]
    pub fn decls(&self) -> &[Decl] {
        &self.decls[1..]
    }

    #[inline]
    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts[1..]
    }

    #[inline]
    pub fn exprs(&self) -> &[Expr] {
        &self.exprs[1..]
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
pub struct DeclRef(pub(crate) NonZeroU32);

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
pub struct StmtRef(pub(crate) NonZeroU32);

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
pub struct ExprRef(pub(crate) NonZeroU32);

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

    #[inline]
    pub fn len(&self) -> u32 {
        self.1 - self.0
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0 == self.1
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
