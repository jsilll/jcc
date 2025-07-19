use crate::{
    ast::{
        Ast, BinaryOp, BlockItem, DeclKind, DeclRef, ExprKind, ExprRef, ForInit, StmtKind, StmtRef,
        StorageClass, UnaryOp,
    },
    sema::{Attribute, SemaCtx, StaticValue, SymbolInfo, Type},
};

use jcc_ssa::sourcemap::{diag::Diagnostic, SourceSpan};

use std::collections::HashSet;

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
    switch_cases: HashSet<i64>,
}

impl<'ctx> TyperPass<'ctx> {
    pub fn new(ast: &'ctx Ast, ctx: &'ctx mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            switch_cases: HashSet::new(),
            result: TyperResult::default(),
        }
    }

    pub fn check(mut self) -> TyperResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| self.visit_file_scope_decl(*decl));
        self.result
    }

    #[inline]
    fn assert_is_lvalue(&mut self, expr: ExprRef) -> bool {
        if !is_lvalue(self.ast, expr) {
            self.result.diagnostics.push(TyperDiagnostic {
                span: self.ast.expr(expr).span,
                kind: TyperDiagnosticKind::InvalidLValue,
            });
            return false;
        }
        true
    }

    fn visit_file_scope_decl(&mut self, decl_ref: DeclRef) {
        let decl = self.ast.decl(decl_ref);
        match decl.kind {
            DeclKind::Func { .. } => self.visit_func_decl(decl_ref),
            DeclKind::Var(init) => {
                let ty = Type::Int;
                *self.ctx.decl_type_mut(decl_ref) = ty;
                let decl_is_global = decl.storage != Some(StorageClass::Static);
                let decl_init = match init {
                    None => match decl.storage {
                        Some(StorageClass::Extern) => StaticValue::NoInitializer,
                        _ => StaticValue::Tentative,
                    },
                    Some(init) => {
                        self.visit_expr(init);
                        if self.ctx.decl_type(decl_ref) != self.ctx.expr_type(init) {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: self.ast.expr(init).span,
                                kind: TyperDiagnosticKind::InitializerTypeMismatch,
                            });
                        }
                        match eval_constant(self.ast, init) {
                            Some(value) => StaticValue::Initialized(value),
                            None => {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: self.ast.expr(init).span,
                                    kind: TyperDiagnosticKind::NotConstant,
                                });
                                StaticValue::NoInitializer
                            }
                        }
                    }
                };

                let info = self.ctx.symbol_mut(decl.name.sema.get());
                let occupied = info.is_some();
                let info = info.get_or_insert(SymbolInfo::statik(ty, decl_is_global, decl_init));

                if info.ty != ty {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: decl.span,
                        kind: TyperDiagnosticKind::DeclarationTypeMismatch,
                    });
                }

                if let Attribute::Static {
                    is_global,
                    ref mut init,
                } = info.attr
                {
                    if decl.storage != Some(StorageClass::Extern) && (is_global != decl_is_global) {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: decl.span,
                            kind: TyperDiagnosticKind::DeclarationVisibilityMismatch,
                        });
                    }
                    match init {
                        StaticValue::NoInitializer => *init = decl_init,
                        StaticValue::Tentative => {
                            if matches!(decl_init, StaticValue::Initialized(_)) {
                                *init = decl_init
                            }
                        }
                        StaticValue::Initialized(_) => {
                            if occupied && matches!(decl_init, StaticValue::Initialized(_)) {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: decl.span,
                                    kind: TyperDiagnosticKind::MultipleInitializers,
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    fn visit_block_scope_decl(&mut self, decl_ref: DeclRef) {
        let decl = self.ast.decl(decl_ref);
        match decl.kind {
            DeclKind::Func { .. } => self.visit_func_decl(decl_ref),
            DeclKind::Var(init) => {
                let ty = Type::Int;
                *self.ctx.decl_type_mut(decl_ref) = ty;
                match decl.storage {
                    None => {
                        *self.ctx.symbol_mut(decl.name.sema.get()) = Some(SymbolInfo::local(ty));
                        if let Some(init) = init {
                            self.visit_expr(init);
                        }
                    }
                    Some(StorageClass::Extern) => match init {
                        Some(init) => {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: self.ast.expr(init).span,
                                kind: TyperDiagnosticKind::ExternLocalInitialized,
                            });
                        }
                        None => {
                            let info = self.ctx.symbol_mut(decl.name.sema.get()).get_or_insert(
                                SymbolInfo::statik(ty, true, StaticValue::NoInitializer),
                            );
                            if info.ty != ty {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: decl.span,
                                    kind: TyperDiagnosticKind::DeclarationTypeMismatch,
                                });
                            }
                        }
                    },
                    Some(StorageClass::Static) => {
                        let decl_init = match init {
                            None => StaticValue::Initialized(0),
                            Some(init) => {
                                self.visit_expr(init);
                                if self.ctx.decl_type(decl_ref) != self.ctx.expr_type(init) {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        span: self.ast.expr(init).span,
                                        kind: TyperDiagnosticKind::InitializerTypeMismatch,
                                    });
                                }
                                match eval_constant(self.ast, init) {
                                    Some(value) => StaticValue::Initialized(value),
                                    None => {
                                        self.result.diagnostics.push(TyperDiagnostic {
                                            span: self.ast.expr(init).span,
                                            kind: TyperDiagnosticKind::NotConstant,
                                        });
                                        StaticValue::Initialized(0)
                                    }
                                }
                            }
                        };
                        *self.ctx.symbol_mut(decl.name.sema.get()) =
                            Some(SymbolInfo::statik(ty, false, decl_init));
                    }
                }
            }
        }
    }

    fn visit_func_decl(&mut self, decl_ref: DeclRef) {
        let decl = self.ast.decl(decl_ref);
        if let DeclKind::Func { params, body } = decl.kind {
            let ty = Type::Func(params.len());
            *self.ctx.decl_type_mut(decl_ref) = ty;
            let decl_is_global = decl.storage != Some(StorageClass::Static);

            let entry = self
                .ctx
                .symbol_mut(decl.name.sema.get())
                .get_or_insert(SymbolInfo::function(ty, decl_is_global, false));

            if entry.ty != ty {
                self.result.diagnostics.push(TyperDiagnostic {
                    span: decl.span,
                    kind: TyperDiagnosticKind::DeclarationTypeMismatch,
                });
            }

            if let Attribute::Function {
                is_global,
                ref mut is_defined,
            } = entry.attr
            {
                if is_global && !decl_is_global {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: decl.span,
                        kind: TyperDiagnosticKind::DeclarationVisibilityMismatch,
                    });
                }
                if body.is_some() {
                    assert!(!*is_defined, "function already defined");
                    *is_defined = true;
                }
                self.ast.params(params).iter().for_each(|param| {
                    if self.ast.decl(*param).storage.is_some() {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: self.ast.decl(*param).span,
                            kind: TyperDiagnosticKind::StorageClassesDisallowed,
                        });
                    }
                    self.visit_block_scope_decl(*param)
                });
                self.ast
                    .block_items(body.unwrap_or_default())
                    .iter()
                    .for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
            }
        }
    }

    fn visit_stmt(&mut self, stmt_ref: StmtRef) {
        let stmt = self.ast.stmt(stmt_ref);
        match &stmt.kind {
            StmtKind::Empty | StmtKind::Break(_) | StmtKind::Continue(_) | StmtKind::Goto(_) => {}
            StmtKind::Expr(expr) => self.visit_expr(*expr),
            StmtKind::Return(expr) => self.visit_expr(*expr),
            StmtKind::Default(stmt) => self.visit_stmt(*stmt),
            StmtKind::Case { stmt, .. } => self.visit_stmt(*stmt),
            StmtKind::Label { stmt, .. } => self.visit_stmt(*stmt),
            StmtKind::While { cond, body } => {
                self.visit_expr(*cond);
                self.visit_stmt(*body);
            }
            StmtKind::DoWhile { body, cond } => {
                self.visit_stmt(*body);
                self.visit_expr(*cond);
            }
            StmtKind::If {
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
            StmtKind::Compound(items) => {
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
            }
            StmtKind::For {
                init,
                cond,
                step,
                body,
            } => {
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => self.visit_expr(*expr),
                        ForInit::VarDecl(decl) => {
                            if self.ast.decl(*decl).storage.is_some() {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: self.ast.decl(*decl).span,
                                    kind: TyperDiagnosticKind::StorageClassesDisallowed,
                                });
                            }
                            self.visit_block_scope_decl(*decl);
                        }
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
            StmtKind::Switch { cond, body } => {
                if let Some(switch) = self.ctx.switches.get(&stmt_ref) {
                    switch
                        .cases
                        .iter()
                        .for_each(|stmt| match &self.ast.stmt(*stmt).kind {
                            StmtKind::Case { expr, .. } => match eval_constant(self.ast, *expr) {
                                None => {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        span: self.ast.expr(*expr).span,
                                        kind: TyperDiagnosticKind::NotConstant,
                                    });
                                }
                                Some(value) => {
                                    if !self.switch_cases.insert(value) {
                                        self.result.diagnostics.push(TyperDiagnostic {
                                            span: self.ast.expr(*expr).span,
                                            kind: TyperDiagnosticKind::DuplicateSwitchCase,
                                        });
                                    }
                                }
                            },
                            _ => panic!("unexpected statement in switch case"),
                        });
                    self.switch_cases.clear();
                }
                self.visit_expr(*cond);
                self.visit_stmt(*body);
            }
        }
    }

    fn visit_expr(&mut self, expr_ref: ExprRef) {
        *self.ctx.expr_type_mut(expr_ref) = Type::Int;
        let expr = self.ast.expr(expr_ref);
        match &expr.kind {
            ExprKind::Const(_) => {}
            ExprKind::Grouped(expr) => self.visit_expr(*expr),
            ExprKind::Var(name) => {
                let info = self
                    .ctx
                    .symbol(name.sema.get())
                    .expect("symbol info not found");
                if let Type::Func(_) = info.ty {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: expr.span,
                        kind: TyperDiagnosticKind::FunctionUsedAsVariable,
                    });
                }
            }
            ExprKind::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.visit_expr(*cond);
                self.visit_expr(*then);
                self.visit_expr(*otherwise);
            }
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                    self.assert_is_lvalue(*expr);
                    self.visit_expr(*expr);
                }
                _ => self.visit_expr(*expr),
            },
            ExprKind::Binary { op, lhs, rhs } => match op {
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
            ExprKind::Call { name, args } => {
                let info = self
                    .ctx
                    .symbol(name.sema.get())
                    .expect("symbol info not found");
                match info.ty {
                    Type::Func(arity) => {
                        if arity != args.len() {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: expr.span,
                                kind: TyperDiagnosticKind::DeclarationTypeMismatch,
                            });
                        }
                        self.ast
                            .args(*args)
                            .iter()
                            .for_each(|arg| self.visit_expr(*arg));
                    }
                    _ => {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: expr.span,
                            kind: TyperDiagnosticKind::VariableUsedAsFunction,
                        });
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary functions
// ---------------------------------------------------------------------------

fn is_lvalue(ast: &Ast, expr: ExprRef) -> bool {
    match ast.expr(expr).kind {
        ExprKind::Var { .. } => true,
        ExprKind::Grouped(expr) => is_lvalue(ast, expr),
        ExprKind::Const(_)
        | ExprKind::Unary { .. }
        | ExprKind::Binary { .. }
        | ExprKind::Ternary { .. }
        | ExprKind::Call { .. } => false,
    }
}

fn eval_constant(ast: &Ast, expr: ExprRef) -> Option<i64> {
    match ast.expr(expr).kind {
        ExprKind::Const(value) => Some(value),
        ExprKind::Grouped(expr) => eval_constant(ast, expr),
        ExprKind::Var { .. }
        | ExprKind::Unary { .. }
        | ExprKind::Binary { .. }
        | ExprKind::Ternary { .. }
        | ExprKind::Call { .. } => None,
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
    MultipleInitializers,
    ExternLocalInitialized,
    VariableUsedAsFunction,
    InitializerTypeMismatch,
    DeclarationTypeMismatch,
    FunctionUsedAsVariable,
    StorageClassesDisallowed,
    DeclarationVisibilityMismatch,
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
            TyperDiagnosticKind::MultipleInitializers => Diagnostic::error(
                diagnostic.span,
                "multiple initializers",
                "this variable is already initialized",
            ),
            TyperDiagnosticKind::ExternLocalInitialized => Diagnostic::error(
                diagnostic.span,
                "local extern variable initialized",
                "local extern variable cannot have an initializer",
            ),
            TyperDiagnosticKind::DeclarationTypeMismatch => Diagnostic::error(
                diagnostic.span,
                "declaration type mismatch",
                "this declaration has a different type than expected",
            ),
            TyperDiagnosticKind::FunctionUsedAsVariable => Diagnostic::error(
                diagnostic.span,
                "function used as variable",
                "this function is being used as a variable",
            ),
            TyperDiagnosticKind::VariableUsedAsFunction => Diagnostic::error(
                diagnostic.span,
                "variable used as function",
                "this variable is being used as a function",
            ),
            TyperDiagnosticKind::InitializerTypeMismatch => Diagnostic::error(
                diagnostic.span,
                "initializer type mismatch",
                "this initializer has a different type than expected",
            ),
            TyperDiagnosticKind::StorageClassesDisallowed => Diagnostic::error(
                diagnostic.span,
                "storage classes disallowed",
                "storage classes are disallowed in this context",
            ),
            TyperDiagnosticKind::DeclarationVisibilityMismatch => Diagnostic::error(
                diagnostic.span,
                "declaration visibility mismatch",
                "this declaration has a different visibility than expected",
            ),
        }
    }
}
