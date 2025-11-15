pub mod graphviz;
pub mod parse;
pub mod ty;

pub use ty::{Ty, TyKind};

use crate::sema::SemaSymbol;

use jcc_entity::{EntityList, EntityRef, ListPool, PrimaryMap};
use jcc_ssa::{interner::Symbol, ir::ConstValue, sourcemap::SourceSpan};

use std::cell::Cell;

// ---------------------------------------------------------------------------
// Ast
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct DeclMarker;
pub type DeclRef = EntityRef<DeclMarker>;
pub type DeclList = EntityList<DeclRef>;

#[derive(Debug)]
pub struct ExprMarker;
pub type ExprRef = EntityRef<ExprMarker>;
pub type ExprList = EntityList<ExprRef>;

#[derive(Debug)]
pub struct StmtMarker;
pub type StmtRef = EntityRef<StmtMarker>;
pub type Block = EntityList<BlockItem>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BlockItem {
    /// A declaration.
    Decl(DeclRef),
    /// A statement.
    Stmt(StmtRef),
}

#[derive(Default)]
pub struct Ast<'ctx> {
    root: Vec<DeclRef>,
    pub sliced_decls: ListPool<DeclRef>,
    pub sliced_exprs: ListPool<ExprRef>,
    pub sliced_items: ListPool<BlockItem>,
    pub stmts: PrimaryMap<StmtMarker, Stmt>,
    pub decls: PrimaryMap<DeclMarker, Decl<'ctx>>,
    pub exprs: PrimaryMap<ExprMarker, Expr<'ctx>>,
}

impl<'ctx> Ast<'ctx> {
    pub fn new() -> Self {
        Self::with_capacity(256)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Ast {
            root: Vec::with_capacity(capacity),
            decls: PrimaryMap::with_capacity(capacity),
            stmts: PrimaryMap::with_capacity(capacity),
            exprs: PrimaryMap::with_capacity(capacity),
            sliced_decls: ListPool::with_capacity(capacity),
            sliced_exprs: ListPool::with_capacity(capacity),
            sliced_items: ListPool::with_capacity(capacity),
        }
    }

    pub fn root(&self) -> &[DeclRef] {
        self.root.as_slice()
    }
}

// ---------------------------------------------------------------------------
// Decl
// ---------------------------------------------------------------------------

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
        params: DeclList,
        body: Option<Block>,
    },
}

// ---------------------------------------------------------------------------
// Stmt
// ---------------------------------------------------------------------------

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
    /// A compound statement.
    Compound(Block),
    /// A default statement.
    Default(StmtRef),
    /// A break statement.
    Break(Cell<Option<StmtRef>>),
    /// A continue statement.
    Continue(Cell<Option<StmtRef>>),
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
    Goto {
        label: Symbol,
        stmt: Cell<Option<StmtRef>>,
    },
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
    Call { name: AstSymbol, args: ExprList },
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
pub enum ForInit {
    /// An expression.
    Expr(ExprRef),
    /// A variable declaration.
    VarDecl(DeclRef),
}
