use crate::{
    ast::{
        Ast, BinaryOp, BlockItem, DeclKind, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef,
        StorageClass, UnaryOp,
    },
    sema::{SemaCtx, Type},
};

use jcc_ssa::{
    interner::SymbolTable,
    sourcemap::{diag::Diagnostic, SourceSpan},
};

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
    switch_cases: HashSet<Expr>,
    symbols: SymbolTable<SymbolEntry>,
}

impl<'ctx> TyperPass<'ctx> {
    pub fn new(ast: &'ctx Ast, ctx: &'ctx mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            symbols: SymbolTable::new(),
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
                span: *self.ast.expr_span(expr),
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
                                span: *self.ast.expr_span(init),
                                kind: TyperDiagnosticKind::InitializerTypeMismatch,
                            });
                        }
                        match eval_constant(self.ast, init) {
                            Some(value) => StaticValue::Initialized(value),
                            None => {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: *self.ast.expr_span(init),
                                    kind: TyperDiagnosticKind::NotConstant,
                                });
                                StaticValue::NoInitializer
                            }
                        }
                    }
                };

                let entry = self.symbols.get(&decl.name).cloned();
                let occupied = entry.is_some();
                let mut entry =
                    entry.unwrap_or(SymbolEntry::static_(ty, decl_is_global, decl_init));

                if entry.ty != ty {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: *self.ast.decl_span(decl_ref),
                        kind: TyperDiagnosticKind::DeclarationTypeMismatch,
                    });
                }

                if let Attribute::Static {
                    is_global,
                    ref mut init,
                } = entry.attr
                {
                    if decl.storage != Some(StorageClass::Extern) && (is_global != decl_is_global) {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: *self.ast.decl_span(decl_ref),
                            kind: TyperDiagnosticKind::FunctionVisibilityMismatch,
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
                                    span: *self.ast.decl_span(decl_ref),
                                    kind: TyperDiagnosticKind::MultipleInitializers,
                                });
                            }
                        }
                    }
                }

                self.symbols.insert(decl.name, entry);
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
                        self.symbols.insert(decl.name, SymbolEntry::local(ty));
                        if let Some(init) = init {
                            self.visit_expr(init);
                        }
                    }
                    Some(StorageClass::Extern) => match init {
                        Some(init) => {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: *self.ast.expr_span(init),
                                kind: TyperDiagnosticKind::ExternLocalInitialized,
                            });
                        }
                        None => {
                            let entry = self.symbols.entry_global(decl.name).or_insert(
                                SymbolEntry::static_(ty, true, StaticValue::NoInitializer),
                            );
                            if entry.ty != ty {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: *self.ast.decl_span(decl_ref),
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
                                        span: *self.ast.expr_span(init),
                                        kind: TyperDiagnosticKind::InitializerTypeMismatch,
                                    });
                                }
                                match eval_constant(self.ast, init) {
                                    Some(value) => StaticValue::Initialized(value),
                                    None => {
                                        self.result.diagnostics.push(TyperDiagnostic {
                                            span: *self.ast.expr_span(init),
                                            kind: TyperDiagnosticKind::NotConstant,
                                        });
                                        StaticValue::Initialized(0)
                                    }
                                }
                            }
                        };
                        self.symbols
                            .insert_global(decl.name, SymbolEntry::static_(ty, false, decl_init));
                    }
                }
            }
        }
    }

    #[inline]
    fn visit_func_decl(&mut self, decl_ref: DeclRef) {
        let decl = self.ast.decl(decl_ref);
        if let DeclKind::Func { params, body } = decl.kind {
            let ty = Type::Func(params.len());
            *self.ctx.decl_type_mut(decl_ref) = ty;
            let decl_is_global = decl.storage != Some(StorageClass::Static);

            let entry = self
                .symbols
                .entry_global(decl.name)
                .or_insert(SymbolEntry::function(ty, decl_is_global, false));

            if entry.ty != ty {
                self.result.diagnostics.push(TyperDiagnostic {
                    span: *self.ast.decl_span(decl_ref),
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
                        span: *self.ast.decl_span(decl_ref),
                        kind: TyperDiagnosticKind::FunctionVisibilityMismatch,
                    });
                }
                if body.is_some() {
                    assert!(!*is_defined, "function already defined");
                    *is_defined = true;
                }
                self.symbols.push_scope();
                self.ast.params(params).iter().for_each(|param| {
                    if let Some(_) = self.ast.decl(*param).storage {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: *self.ast.decl_span(*param),
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
                self.symbols.pop_scope();
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
            Stmt::Compound(items) => {
                self.symbols.push_scope();
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
                self.symbols.pop_scope();
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                self.symbols.push_scope();
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => self.visit_expr(*expr),
                        ForInit::VarDecl(decl) => {
                            if let Some(_) = self.ast.decl(*decl).storage {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: *self.ast.decl_span(*decl),
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
                self.symbols.pop_scope();
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
                let decl = self.ctx.vars.get(&expr).expect("decl not found");
                if let Type::Func(_) = self.ctx.decl_type(*decl) {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: *self.ast.expr_span(expr),
                        kind: TyperDiagnosticKind::FunctionUsedAsVariable,
                    });
                }
            }
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.visit_expr(*cond);
                self.visit_expr(*then);
                self.visit_expr(*otherwise);
            }
            Expr::Unary { op, expr } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                    self.assert_is_lvalue(*expr);
                    self.visit_expr(*expr);
                }
                _ => self.visit_expr(*expr),
            },
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
                let decl = self.ctx.vars.get(&expr).expect("decl not found");
                match self.ctx.decl_type(*decl) {
                    Type::Func(arity) => {
                        if *arity != args.len() {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: *self.ast.expr_span(expr),
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
// Auxiliary structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct SymbolEntry {
    ty: Type,
    attr: Attribute,
}

impl SymbolEntry {
    #[inline]
    fn local(ty: Type) -> Self {
        Self {
            ty,
            attr: Attribute::Local,
        }
    }

    #[inline]
    fn function(ty: Type, is_global: bool, is_defined: bool) -> Self {
        Self {
            ty,
            attr: Attribute::Function {
                is_global,
                is_defined,
            },
        }
    }

    #[inline]
    fn static_(ty: Type, is_global: bool, init: StaticValue) -> Self {
        Self {
            ty,
            attr: Attribute::Static { is_global, init },
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Attribute {
    #[default]
    Local,
    Function {
        is_global: bool,
        is_defined: bool,
    },
    Static {
        is_global: bool,
        init: StaticValue,
    },
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum StaticValue {
    #[default]
    NoInitializer,
    Tentative,
    Initialized(i64),
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

fn eval_constant(ast: &Ast, expr: ExprRef) -> Option<i64> {
    match ast.expr(expr) {
        Expr::Const(value) => Some(*value),
        Expr::Grouped(expr) => eval_constant(ast, *expr),
        Expr::Var { .. }
        | Expr::Unary { .. }
        | Expr::Binary { .. }
        | Expr::Ternary { .. }
        | Expr::Call { .. } => None,
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
    FunctionVisibilityMismatch,
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
            TyperDiagnosticKind::FunctionVisibilityMismatch => Diagnostic::error(
                diagnostic.span,
                "function visibility mismatch",
                "this function has a different visibility than expected",
            ),
        }
    }
}
