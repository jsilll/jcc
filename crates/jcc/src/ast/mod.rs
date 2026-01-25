pub mod constant;
pub mod graphviz;
pub mod parse;
pub mod ty;

use crate::{ast::constant::Constant, sema};
pub use ty::{Ty, TyKind};

use jcc_entity::{entity_impl, EntityList, ListPool, PrimaryMap};
use jcc_ssa::{
    codemap::{file::FileId, span::Span},
    Ident,
};

use std::cell::Cell;

//============================================================================
// Ast
//============================================================================

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Decl(u32);
entity_impl!(Decl, "decl");
pub type DeclList = EntityList<Decl>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Expr(u32);
entity_impl!(Expr, "expr");
pub type ExprList = EntityList<Expr>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Stmt(u32);
entity_impl!(Stmt, "stmt");
pub type Block = EntityList<BlockItem>;

pub struct Ast<'ctx> {
    pub file: FileId,
    pub root: Vec<Decl>,
    pub decls: ListPool<Decl>,
    pub exprs: ListPool<Expr>,
    pub items: ListPool<BlockItem>,
    pub stmt: PrimaryMap<Stmt, StmtData>,
    pub decl: PrimaryMap<Decl, DeclData<'ctx>>,
    pub expr: PrimaryMap<Expr, ExprData<'ctx>>,
}

impl<'ctx> Ast<'ctx> {
    pub fn new(file: FileId) -> Self {
        Self::with_capacity(file, 256)
    }

    pub fn with_capacity(file: FileId, capacity: usize) -> Self {
        Ast {
            file,
            root: Vec::with_capacity(capacity),
            decls: ListPool::with_capacity(capacity),
            exprs: ListPool::with_capacity(capacity),
            items: ListPool::with_capacity(capacity),
            decl: PrimaryMap::with_capacity(capacity),
            stmt: PrimaryMap::with_capacity(capacity),
            expr: PrimaryMap::with_capacity(capacity),
        }
    }
}

//============================================================================
// Data
//============================================================================

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: Ident,
    pub sema: Cell<Option<sema::Symbol>>,
}

#[derive(Debug, Clone, Copy)]
pub enum BlockItem {
    /// A declaration.
    Decl(Decl),
    /// A statement.
    Stmt(Stmt),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    /// An expression.
    Expr(Expr),
    /// A variable declaration.
    VarDecl(Decl),
}

#[derive(Debug, Clone)]
pub enum StorageClass {
    /// Extern storage class.
    Extern,
    /// Static storage class.
    Static,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    /// The `-` operator.
    Neg,
    /// The `~` operator.
    BitNot,
    /// The `!` operator.
    LogNot,
    /// The prefix `++` operator.
    PreInc,
    /// The prefix `--` operator.
    PreDec,
    /// The postfix `++` operator.
    PostInc,
    /// The postfix `--` operator.
    PostDec,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    /// The `||` operator.
    LogOr,
    /// The `&&` operator.
    LogAnd,
    /// The `==` operator.
    Eq,
    /// The `!=` operator.
    Ne,
    /// The `<` operator.
    Lt,
    /// The `<=` operator.
    Le,
    /// The `>` operator.
    Gt,
    /// The `>=` operator.
    Ge,
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

#[derive(Debug, Clone)]
pub struct DeclData<'ctx> {
    pub span: Span,
    pub name: Symbol,
    pub ty: Ty<'ctx>,
    pub kind: DeclKind,
    pub storage: Option<StorageClass>,
}

#[derive(Debug, Clone)]
pub enum DeclKind {
    /// A variable declaration.
    Var(Option<Expr>),
    /// A function declaration.
    Func {
        params: DeclList,
        body: Option<Block>,
    },
}

#[derive(Debug, Clone)]
pub struct StmtData {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// An empty statement.
    Empty,
    /// An expression statement.
    Expr(Expr),
    /// A return statement.
    Return(Expr),
    /// A default statement.
    Default(Stmt),
    /// A compound statement.
    Compound(Block),
    /// A break statement.
    Break(Cell<Option<Stmt>>),
    /// A continue statement.
    Continue(Cell<Option<Stmt>>),
    /// A case statement.
    Case { expr: Expr, stmt: Stmt },
    /// A label statement.
    Label { label: Ident, stmt: Stmt },
    /// A switch statement.
    Switch { cond: Expr, body: Stmt },
    /// A while statement.
    While { cond: Expr, body: Stmt },
    /// A do-while statement.
    DoWhile { body: Stmt, cond: Expr },
    /// A goto statement.
    Goto {
        label: Ident,
        stmt: Cell<Option<Stmt>>,
    },
    /// An if statement.
    If {
        cond: Expr,
        then: Stmt,
        otherwise: Option<Stmt>,
    },
    /// A for statement.
    For {
        init: Option<ForInit>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Stmt,
    },
}

#[derive(Debug, Clone)]
pub struct ExprData<'ctx> {
    pub span: Span,
    pub ty: Cell<Ty<'ctx>>,
    pub kind: ExprKind<'ctx>,
}

impl<'ctx> ExprData<'ctx> {
    #[inline]
    pub fn new(kind: ExprKind<'ctx>, span: Span, ty: Ty<'ctx>) -> Self {
        ExprData {
            kind,
            span,
            ty: Cell::new(ty),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind<'ctx> {
    /// A variable reference.
    Var(Symbol),
    /// A constant integer value.
    Const(Constant),
    /// A grouped expression.
    Grouped(Expr),
    /// A cast expression.
    Cast { ty: Ty<'ctx>, expr: Expr },
    /// An unary expression.
    Unary { op: UnaryOp, expr: Expr },
    /// A binary expression.
    Binary { op: BinaryOp, lhs: Expr, rhs: Expr },
    /// A ternary expression.
    Ternary { cond: Expr, then: Expr, other: Expr },
    /// A function call expression.
    Call { name: Symbol, args: ExprList },
}

impl ExprKind<'_> {
    pub fn is_lvalue(&self, ctx: &Ast) -> bool {
        match self {
            Self::Var { .. } => true,
            Self::Grouped(expr) => ctx.expr[*expr].kind.is_lvalue(ctx),
            Self::Unary {
                op: UnaryOp::PreInc | UnaryOp::PreDec,
                ..
            } => true,
            _ => false,
        }
    }
}
