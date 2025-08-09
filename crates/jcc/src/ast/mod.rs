pub mod graphviz;
pub mod mermaid;
pub mod parse;

use crate::sema::{SemaSymbol, Type};

use jcc_ssa::{interner::Symbol, sourcemap::SourceSpan};

use std::{cell::Cell, num::NonZeroU32};

// ---------------------------------------------------------------------------
// Ast
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Ast {
    root: Vec<DeclRef>,
    decls: Vec<Decl>,
    stmts: Vec<Stmt>,
    exprs: Vec<Expr>,
    sliced_decls: Vec<DeclRef>,
    sliced_exprs: Vec<ExprRef>,
    sliced_bitems: Vec<BlockItem>,
    last_symbol: Cell<Option<NonZeroU32>>,
}

impl Default for Ast {
    fn default() -> Self {
        Ast::new()
    }
}

impl Ast {
    pub fn new() -> Self {
        Self::with_capacity(256)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        #[inline]
        fn default_vec_with_capacity<T: Default>(capacity: usize) -> Vec<T> {
            let mut v = Vec::with_capacity(capacity);
            v.push(Default::default());
            v
        }
        Ast {
            last_symbol: Cell::new(None),
            root: Vec::with_capacity(capacity),
            decls: default_vec_with_capacity(capacity),
            stmts: default_vec_with_capacity(capacity),
            exprs: default_vec_with_capacity(capacity),
            sliced_decls: Vec::with_capacity(capacity),
            sliced_exprs: Vec::with_capacity(capacity),
            sliced_bitems: Vec::with_capacity(capacity),
        }
    }

    // ---------------------------------------------------------------------------
    // Accessors
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn decls_len(&self) -> usize {
        self.decls.len() - 1
    }

    #[inline]
    pub fn stmts_len(&self) -> usize {
        self.stmts.len() - 1
    }

    #[inline]
    pub fn exprs_len(&self) -> usize {
        self.exprs.len() - 1
    }

    #[inline]
    pub fn symbols_len(&self) -> usize {
        self.last_symbol.get().map_or(1, |s| s.get() as usize) - 1
    }

    #[inline]
    pub fn root(&self) -> &[DeclRef] {
        self.root.as_slice()
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
    pub fn exprs(&self, slice: Slice<ExprRef>) -> &[ExprRef] {
        &self.sliced_exprs[slice.0 as usize..slice.1 as usize]
    }

    #[inline]
    pub fn decls(&self, slice: Slice<DeclRef>) -> &[DeclRef] {
        &self.sliced_decls[slice.0 as usize..slice.1 as usize]
    }

    #[inline]
    pub fn bitems(&self, slice: Slice<BlockItem>) -> &[BlockItem] {
        &self.sliced_bitems[slice.0 as usize..slice.1 as usize]
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

    // ---------------------------------------------------------------------------
    // Setters
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn set_last_symbol(&self, count: NonZeroU32) {
        self.last_symbol.set(Some(count));
    }

    // ---------------------------------------------------------------------------
    // Creation
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn new_decl(&mut self, decl: Decl) -> DeclRef {
        let r = DeclRef(NonZeroU32::new(self.decls.len() as u32).unwrap());
        self.decls.push(decl);
        r
    }

    #[inline]
    pub fn new_stmt(&mut self, stmt: Stmt) -> StmtRef {
        let r = StmtRef(NonZeroU32::new(self.stmts.len() as u32).unwrap());
        self.stmts.push(stmt);
        r
    }

    #[inline]
    pub fn new_expr(&mut self, expr: Expr) -> ExprRef {
        let r = ExprRef(NonZeroU32::new(self.exprs.len() as u32).unwrap());
        self.exprs.push(expr);
        r
    }

    #[inline]
    pub fn new_exprs(&mut self, args: impl IntoIterator<Item = ExprRef>) -> Slice<ExprRef> {
        let begin = self.sliced_exprs.len() as u32;
        self.sliced_exprs.extend(args);
        let end = self.sliced_exprs.len() as u32;
        Slice::new(begin, end)
    }

    #[inline]
    pub fn new_decls(&mut self, params: impl IntoIterator<Item = DeclRef>) -> Slice<DeclRef> {
        let begin = self.sliced_decls.len() as u32;
        self.sliced_decls.extend(params);
        let end = self.sliced_decls.len() as u32;
        Slice::new(begin, end)
    }

    #[inline]
    pub fn new_bitems(&mut self, items: impl IntoIterator<Item = BlockItem>) -> Slice<BlockItem> {
        let begin = self.sliced_bitems.len() as u32;
        self.sliced_bitems.extend(items);
        let end = self.sliced_bitems.len() as u32;
        Slice::new(begin, end)
    }
}

// ---------------------------------------------------------------------------
// Ast Nodes
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DeclRef(pub(crate) NonZeroU32);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Decl {
    pub ty: Cell<Type>,
    pub kind: DeclKind,
    pub name: AstSymbol,
    pub span: SourceSpan,
    pub storage: Option<StorageClass>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum DeclKind {
    /// A variable declaration.
    Var(Option<ExprRef>),
    /// A function declaration.
    Func {
        params: Slice<DeclRef>,
        body: Option<Slice<BlockItem>>,
    },
}

impl Default for DeclKind {
    fn default() -> Self {
        DeclKind::Var(None)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StmtRef(pub(crate) NonZeroU32);

impl Default for StmtRef {
    fn default() -> Self {
        Self(NonZeroU32::new(u32::MAX).unwrap())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: SourceSpan,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    /// An empty statement.
    #[default]
    Empty,
    /// An expression statement.
    Expr(ExprRef),
    /// A return statement.
    Return(ExprRef),
    /// A default statement.
    Default(StmtRef),
    /// A break statement.
    Break(Cell<StmtRef>),
    /// A continue statement.
    Continue(Cell<StmtRef>),
    /// A compound statement.
    Compound(Slice<BlockItem>),
    /// A case statement.
    Case { expr: ExprRef, stmt: StmtRef },
    /// A label statement.
    Label { label: Symbol, stmt: StmtRef },
    /// A switch statement.
    Switch { cond: ExprRef, body: StmtRef },
    /// A while statement.
    While { cond: ExprRef, body: StmtRef },
    /// A do-while statement.
    DoWhile { body: StmtRef, cond: ExprRef },
    /// A goto statement.
    Goto { label: Symbol, stmt: Cell<StmtRef> },
    /// An if statement.
    If {
        cond: ExprRef,
        then: StmtRef,
        otherwise: Option<StmtRef>,
    },
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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Expr {
    pub ty: Cell<Type>,
    pub kind: ExprKind,
    pub span: SourceSpan,
}

impl Expr {
    #[inline]
    pub fn new(kind: ExprKind, span: SourceSpan) -> Self {
        Expr {
            kind,
            span,
            ..Default::default()
        }
    }
}

impl Default for ExprKind {
    fn default() -> Self {
        Self::Const(0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    /// A constant integer value.
    Const(i64),
    /// A variable reference.
    Var(AstSymbol),
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
    Call {
        name: AstSymbol,
        args: Slice<ExprRef>,
    },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum StorageClass {
    /// Extern storage class.
    Extern,
    /// Static storage class.
    Static,
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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AstSymbol {
    pub raw: Symbol,
    pub sema: Cell<SemaSymbol>,
}

impl AstSymbol {
    pub fn new(name: Symbol) -> Self {
        AstSymbol {
            raw: name,
            ..Default::default()
        }
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
// Slice
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Slice<T>(u32, u32, std::marker::PhantomData<T>);

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Slice(0, 0, std::marker::PhantomData)
    }
}

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
