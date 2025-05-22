use crate::{
    ast::{
        Ast, BinaryOp, BlockItem, Decl, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef, UnaryOp,
    },
    sema::{SemaCtx, Type},
};

use jcc_ssa::{
    sourcemap::{diag::Diagnostic, SourceSpan},
    interner::Symbol,
};

use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
// TyperResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct TyperResult {
    pub diagnostics: Vec<TyperDiagnostic>,
}

// ---------------------------------------------------------------------------
// TyperDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct TyperDiagnostic {
    pub span: SourceSpan,
    pub kind: TyperDiagnosticKind,
}

// ---------------------------------------------------------------------------
// TyperPass
// ---------------------------------------------------------------------------

pub struct TyperPass<'ctx> {
    ast: &'ctx Ast,
    ctx: &'ctx mut SemaCtx,
    result: TyperResult,
    switch_cases: HashSet<Expr>,
    functions: HashMap<Symbol, FuncEntry>,
}

impl<'ctx> TyperPass<'ctx> {
    pub fn new(ast: &'ctx Ast, ctx: &'ctx mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            functions: HashMap::new(),
            switch_cases: HashSet::new(),
            result: TyperResult::default(),
        }
    }

    pub fn check(mut self) -> TyperResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| self.visit_decl(*decl));
        self.result
    }

    #[inline]
    fn assert_is_lvalue(&mut self, expr: ExprRef) -> bool {
        if !is_lvalue(self.ast, expr) {
            self.result.diagnostics.push(TyperDiagnostic {
                span: *self.ast.expr_span(expr),
                kind: TyperDiagnosticKind::InvalidLValue,
            });
            return false;
        }
        true
    }

    fn visit_decl(&mut self, decl: DeclRef) {
        match self.ast.decl(decl) {
            Decl::Var { init, .. } => {
                *self.ctx.decl_type_mut(decl) = Type::Int;
                if let Some(init) = init {
                    self.visit_expr(*init);
                    if self.ctx.decl_type(decl) != self.ctx.expr_type(*init) {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: *self.ast.expr_span(*init),
                            kind: TyperDiagnosticKind::InitializerTypeMismatch,
                        });
                    }
                }
            }
            Decl::Func { name, params, body } => {
                self.ast
                    .params(*params)
                    .iter()
                    .for_each(|param| self.visit_decl(*param));

                let ty = Type::Func(params.len());
                *self.ctx.decl_type_mut(decl) = ty;

                let entry = self
                    .functions
                    .entry(*name)
                    .or_insert_with(|| FuncEntry::without_definition(ty));

                if entry.ty != ty {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: *self.ast.decl_span(decl),
                        kind: TyperDiagnosticKind::FunctionTypeMismatch,
                    });
                }

                if let Some(body) = body {
                    if entry.is_defined {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: *self.ast.decl_span(decl),
                            kind: TyperDiagnosticKind::FunctionAlreadyDefined,
                        });
                    }
                    entry.is_defined = true;
                    self.ast
                        .block_items(*body)
                        .iter()
                        .for_each(|item| match item {
                            BlockItem::Decl(decl) => self.visit_decl(*decl),
                            BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        });
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: StmtRef) {
        match self.ast.stmt(stmt) {
            Stmt::Empty | Stmt::Break | Stmt::Continue | Stmt::Goto(_) => {}
            Stmt::Expr(expr) => self.visit_expr(*expr),
            Stmt::Return(expr) => self.visit_expr(*expr),
            Stmt::Default(stmt) => self.visit_stmt(*stmt),
            Stmt::Label { stmt, .. } => self.visit_stmt(*stmt),
            Stmt::Compound(items) => {
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(decl) => self.visit_decl(*decl),
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                    });
            }
            Stmt::Case { expr, stmt } => {
                self.visit_expr(*expr);
                self.visit_stmt(*stmt);
            }
            Stmt::While { cond, body } => {
                self.visit_expr(*cond);
                self.visit_stmt(*body);
            }
            Stmt::DoWhile { body, cond } => {
                self.visit_stmt(*body);
                self.visit_expr(*cond);
            }
            Stmt::If {
                cond,
                then,
                otherwise,
            } => {
                self.visit_expr(*cond);
                self.visit_stmt(*then);
                if let Some(otherwise) = otherwise {
                    self.visit_stmt(*otherwise);
                }
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => self.visit_expr(*expr),
                        ForInit::VarDecl(decl) => self.visit_decl(*decl),
                    }
                }
                if let Some(cond) = cond {
                    self.visit_expr(*cond);
                }
                if let Some(step) = step {
                    self.visit_expr(*step);
                }
                self.visit_stmt(*body);
            }
            Stmt::Switch { cond, body } => {
                if let Some(switch) = self.ctx.switches.get(&stmt) {
                    switch
                        .cases
                        .iter()
                        .for_each(|stmt| match self.ast.stmt(*stmt) {
                            Stmt::Case { expr, .. } => {
                                if !is_constant(self.ast, *expr) {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        span: *self.ast.expr_span(*expr),
                                        kind: TyperDiagnosticKind::NotConstant,
                                    });
                                } else if !self.switch_cases.insert(*self.ast.expr(*expr)) {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        span: *self.ast.expr_span(*expr),
                                        kind: TyperDiagnosticKind::DuplicateSwitchCase,
                                    });
                                }
                            }
                            _ => panic!("unexpected statement in switch case"),
                        });
                }
                self.switch_cases.clear();
                self.visit_expr(*cond);
                self.visit_stmt(*body);
            }
        }
    }

    fn visit_expr(&mut self, expr: ExprRef) {
        *self.ctx.expr_type_mut(expr) = Type::Int;
        match self.ast.expr(expr) {
            Expr::Const(_) => {}
            Expr::Grouped(expr) => self.visit_expr(*expr),
            Expr::Var(_) => {
                let decl = self.ctx.names.get(&expr).expect("decl not found");
                if let Type::Func(_) = self.ctx.decl_type(*decl) {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: *self.ast.expr_span(expr),
                        kind: TyperDiagnosticKind::FunctionUsedAsVariable,
                    });
                }
            }
            Expr::Unary { op, expr } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                    self.assert_is_lvalue(*expr);
                    self.visit_expr(*expr);
                }
                _ => self.visit_expr(*expr),
            },
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.visit_expr(*cond);
                self.visit_expr(*then);
                self.visit_expr(*otherwise);
            }
            Expr::Binary { op, lhs, rhs } => match op {
                BinaryOp::Assign
                | BinaryOp::AddAssign
                | BinaryOp::SubAssign
                | BinaryOp::MulAssign
                | BinaryOp::DivAssign
                | BinaryOp::RemAssign
                | BinaryOp::BitOrAssign
                | BinaryOp::BitAndAssign
                | BinaryOp::BitXorAssign
                | BinaryOp::BitShlAssign
                | BinaryOp::BitShrAssign => {
                    self.assert_is_lvalue(*lhs);
                    self.visit_expr(*lhs);
                    self.visit_expr(*rhs);
                }
                _ => {
                    self.visit_expr(*lhs);
                    self.visit_expr(*rhs);
                }
            },
            Expr::Call { args, .. } => {
                let decl = self.ctx.names.get(&expr).expect("decl not found");
                match self.ctx.decl_type(*decl) {
                    Type::Func(arity) => {
                        if *arity != args.len() {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: *self.ast.expr_span(expr),
                                kind: TyperDiagnosticKind::FunctionTypeMismatch,
                            });
                        }
                        self.ast
                            .args(*args)
                            .iter()
                            .for_each(|arg| self.visit_expr(*arg));
                    }
                    _ => {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: *self.ast.expr_span(expr),
                            kind: TyperDiagnosticKind::VariableUsedAsFunction,
                        });
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// TyperDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum TyperDiagnosticKind {
    NotConstant,
    InvalidLValue,
    DuplicateSwitchCase,
    FunctionTypeMismatch,
    FunctionAlreadyDefined,
    FunctionUsedAsVariable,
    InitializerTypeMismatch,
    VariableUsedAsFunction,
}

impl From<TyperDiagnostic> for Diagnostic {
    fn from(diagnostic: TyperDiagnostic) -> Self {
        match diagnostic.kind {
            TyperDiagnosticKind::NotConstant => Diagnostic::error(
                diagnostic.span,
                "not a constant",
                "this expression is not a constant",
            ),
            TyperDiagnosticKind::InvalidLValue => Diagnostic::error(
                diagnostic.span,
                "invalid lvalue",
                "this expression is not a valid lvalue",
            ),
            TyperDiagnosticKind::DuplicateSwitchCase => Diagnostic::error(
                diagnostic.span,
                "duplicate switch case",
                "this case value is already defined",
            ),
            TyperDiagnosticKind::FunctionTypeMismatch => Diagnostic::error(
                diagnostic.span,
                "function type mismatch",
                "this function has a different type than expected",
            ),
            TyperDiagnosticKind::FunctionAlreadyDefined => Diagnostic::error(
                diagnostic.span,
                "function already defined",
                "this function has already been defined",
            ),
            TyperDiagnosticKind::FunctionUsedAsVariable => Diagnostic::error(
                diagnostic.span,
                "function used as variable",
                "this function is being used as a variable",
            ),
            TyperDiagnosticKind::InitializerTypeMismatch => Diagnostic::error(
                diagnostic.span,
                "initializer type mismatch",
                "this initializer has a different type than expected",
            ),
            TyperDiagnosticKind::VariableUsedAsFunction => Diagnostic::error(
                diagnostic.span,
                "variable used as function",
                "this variable is being used as a function",
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary structures
// ---------------------------------------------------------------------------

struct FuncEntry {
    ty: Type,
    is_defined: bool,
}

impl FuncEntry {
    fn without_definition(ty: Type) -> Self {
        Self {
            ty,
            is_defined: false,
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary functions
// ---------------------------------------------------------------------------

fn is_lvalue(ast: &Ast, expr: ExprRef) -> bool {
    match ast.expr(expr) {
        Expr::Var { .. } => true,
        Expr::Grouped(expr) => is_lvalue(ast, *expr),
        Expr::Const(_)
        | Expr::Unary { .. }
        | Expr::Binary { .. }
        | Expr::Ternary { .. }
        | Expr::Call { .. } => false,
    }
}

fn is_constant(ast: &Ast, expr: ExprRef) -> bool {
    match ast.expr(expr) {
        Expr::Const(_) => true,
        Expr::Grouped(expr) => is_constant(ast, *expr),
        Expr::Var { .. }
        | Expr::Unary { .. }
        | Expr::Binary { .. }
        | Expr::Ternary { .. }
        | Expr::Call { .. } => false,
    }
}
