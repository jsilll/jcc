pub mod graphviz;
pub mod parse;
pub mod slice;
pub mod ty;

pub use ty::{Ty, TyKind};

use crate::{ast::slice::Slice, sema::SemaSymbol};

use jcc_ssa::{interner::Symbol, ir::ConstValue, sourcemap::SourceSpan};

use std::{cell::Cell, num::NonZeroU32};

// ---------------------------------------------------------------------------
// Ast
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Ast<'ctx> {
    root: Vec<DeclRef>,
    stmts: Vec<Stmt>,
    decls: Vec<Decl<'ctx>>,
    exprs: Vec<Expr<'ctx>>,
    sliced_decls: Vec<DeclRef>,
    sliced_exprs: Vec<ExprRef>,
    sliced_items: Vec<BlockItem>,
}

impl<'ctx> Default for Ast<'ctx> {
    fn default() -> Self {
        Ast::new()
    }
}

impl<'ctx> Ast<'ctx> {
    pub fn new() -> Self {
        Self::with_capacity(256)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Ast {
            root: Vec::with_capacity(capacity),
            decls: Vec::with_capacity(capacity),
            stmts: Vec::with_capacity(capacity),
            exprs: Vec::with_capacity(capacity),
            sliced_decls: Vec::with_capacity(capacity),
            sliced_exprs: Vec::with_capacity(capacity),
            sliced_items: Vec::with_capacity(capacity),
        }
    }

    // ---------------------------------------------------------------------------
    // Accessors
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn decls_len(&self) -> usize {
        self.decls.len()
    }

    #[inline]
    pub fn stmts_len(&self) -> usize {
        self.stmts.len()
    }

    #[inline]
    pub fn exprs_len(&self) -> usize {
        self.exprs.len()
    }

    #[inline]
    pub fn root(&self) -> &[DeclRef] {
        self.root.as_slice()
    }

    #[inline]
    pub fn decl(&self, decl: DeclRef) -> &Decl<'ctx> {
        &self.decls[decl.0.get() as usize - 1]
    }

    #[inline]
    pub fn stmt(&self, stmt: StmtRef) -> &Stmt {
        &self.stmts[stmt.0.get() as usize - 1]
    }

    #[inline]
    pub fn expr(&self, expr: ExprRef) -> &Expr<'ctx> {
        &self.exprs[expr.0.get() as usize - 1]
    }

    #[inline]
    pub fn decl_mut(&mut self, decl: DeclRef) -> &mut Decl<'ctx> {
        &mut self.decls[decl.0.get() as usize - 1]
    }

    #[inline]
    pub fn stmt_mut(&mut self, stmt: StmtRef) -> &mut Stmt {
        &mut self.stmts[stmt.0.get() as usize - 1]
    }

    #[inline]
    pub fn expr_mut(&mut self, expr: ExprRef) -> &mut Expr<'ctx> {
        &mut self.exprs[expr.0.get() as usize - 1]
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
    pub fn items(&self, slice: Slice<BlockItem>) -> &[BlockItem] {
        &self.sliced_items[slice.0 as usize..slice.1 as usize]
    }

    // ---------------------------------------------------------------------------
    // Creation
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn new_decl(&mut self, decl: Decl<'ctx>) -> DeclRef {
        let r = DeclRef(NonZeroU32::new(self.decls.len() as u32 + 1).unwrap());
        self.decls.push(decl);
        r
    }

    #[inline]
    pub fn new_stmt(&mut self, stmt: Stmt) -> StmtRef {
        let r = StmtRef(NonZeroU32::new(self.stmts.len() as u32 + 1).unwrap());
        self.stmts.push(stmt);
        r
    }

    #[inline]
    pub fn new_expr(&mut self, expr: Expr<'ctx>) -> ExprRef {
        let r = ExprRef(NonZeroU32::new(self.exprs.len() as u32 + 1).unwrap());
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
    pub fn new_items(&mut self, items: impl IntoIterator<Item = BlockItem>) -> Slice<BlockItem> {
        let begin = self.sliced_items.len() as u32;
        self.sliced_items.extend(items);
        let end = self.sliced_items.len() as u32;
        Slice::new(begin, end)
    }
}

// ---------------------------------------------------------------------------
// Decl
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DeclRef(pub(crate) NonZeroU32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl<'ctx> {
    pub ty: Ty<'ctx>,
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

// ---------------------------------------------------------------------------
// Stmt
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Expr
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ExprRef(pub(crate) NonZeroU32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr<'ctx> {
    pub span: SourceSpan,
    pub ty: Cell<Ty<'ctx>>,
    pub kind: ExprKind<'ctx>,
}

impl<'ctx> Expr<'ctx> {
    #[inline]
    pub fn new(kind: ExprKind<'ctx>, span: SourceSpan, ty: Ty<'ctx>) -> Self {
        Expr {
            kind,
            span,
            ty: Cell::new(ty),
        }
    }
}

impl<'ctx> Default for ExprKind<'ctx> {
    fn default() -> Self {
        Self::Const(ConstValue::Int32(0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind<'ctx> {
    /// A variable reference.
    Var(AstSymbol),
    /// A constant integer value.
    Const(ConstValue),
    /// A grouped expression.
    Grouped(ExprRef),
    /// A cast expression.
    Cast { ty: Ty<'ctx>, expr: ExprRef },
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
        other: ExprRef,
    },
    /// A function call expression.
    Call {
        name: AstSymbol,
        args: Slice<ExprRef>,
    },
}

// ---------------------------------------------------------------------------
// Support enums
// ---------------------------------------------------------------------------

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
