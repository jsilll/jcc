use crate::{
    ast::{
        Ast, BinaryOp, BlockItem, Decl, DeclKind, DeclRef, ExprKind, ExprRef, ForInit, StmtKind,
        StmtRef, StorageClass, UnaryOp,
    },
    lower::LoweringActions,
    sema::{Attribute, CompoundType, SemaCtx, StaticValue, SymbolInfo, Type},
    PassResult,
};

use jcc_ssa::{
    sourcemap::{diag::Diagnostic, SourceSpan},
    ConstValue,
};

use std::collections::HashSet;

// ---------------------------------------------------------------------------
// TyperResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct TyperResult {
    pub actions: LoweringActions,
    pub diagnostics: Vec<TyperDiagnostic>,
}

impl PassResult for TyperResult {
    fn diagnostics(&self) -> &[impl Into<jcc_ssa::sourcemap::diag::Diagnostic> + Clone] {
        &self.diagnostics
    }
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

pub struct TyperPass<'a> {
    ast: &'a Ast,
    ctx: &'a mut SemaCtx,
    curr_ret: Type,
    result: TyperResult,
    switch_cases: HashSet<ConstValue>,
}

impl<'a> TyperPass<'a> {
    pub fn new(ast: &'a Ast, ctx: &'a mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            curr_ret: Type::default(),
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

    fn visit_file_scope_decl(&mut self, decl_ref: DeclRef) {
        let decl = self.ast.decl(decl_ref);
        match decl.kind {
            DeclKind::Func { .. } => self.visit_func_decl(decl),
            DeclKind::Var(init) => {
                let decl_is_global = decl.storage != Some(StorageClass::Static);
                let decl_init = match init {
                    None => match decl.storage {
                        Some(StorageClass::Extern) => StaticValue::NoInitializer,
                        _ => StaticValue::Tentative,
                    },
                    Some(init) => {
                        let ty = self.visit_expr(init);
                        if ty != decl.ty {
                            self.result.actions.schedule_cast(init, decl.ty);
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
                let info =
                    info.get_or_insert(SymbolInfo::statik(decl.ty, decl_is_global, decl_init));

                if info.ty != decl.ty {
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
            DeclKind::Func { .. } => self.visit_func_decl(decl),
            DeclKind::Var(init) => match decl.storage {
                None => {
                    let _ = self
                        .ctx
                        .symbol_mut(decl.name.sema.get())
                        .insert(SymbolInfo::local(decl.ty));
                    if let Some(init) = init {
                        let ty = self.visit_expr(init);
                        if ty != decl.ty {
                            self.result.actions.schedule_cast(init, decl.ty);
                        }
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
                            SymbolInfo::statik(decl.ty, true, StaticValue::NoInitializer),
                        );
                        if info.ty != decl.ty {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: decl.span,
                                kind: TyperDiagnosticKind::DeclarationTypeMismatch,
                            });
                        }
                    }
                },
                Some(StorageClass::Static) => {
                    let decl_init = match init {
                        None => StaticValue::Tentative,
                        Some(init) => {
                            let ty = self.visit_expr(init);
                            if ty != decl.ty {
                                self.result.actions.schedule_cast(init, decl.ty);
                            }
                            match eval_constant(self.ast, init) {
                                Some(value) => StaticValue::Initialized(value),
                                None => {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        span: self.ast.expr(init).span,
                                        kind: TyperDiagnosticKind::NotConstant,
                                    });
                                    StaticValue::Tentative
                                }
                            }
                        }
                    };
                    *self.ctx.symbol_mut(decl.name.sema.get()) =
                        Some(SymbolInfo::statik(decl.ty, false, decl_init));
                }
            },
        }
    }

    fn visit_func_decl(&mut self, decl: &Decl) {
        if let DeclKind::Func { params, body } = decl.kind {
            let decl_is_global = decl.storage != Some(StorageClass::Static);
            let entry = self
                .ctx
                .symbol_mut(decl.name.sema.get())
                .get_or_insert(SymbolInfo::function(decl.ty, decl_is_global, false));

            if entry.ty != decl.ty {
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
                self.ast.decls(params).iter().for_each(|param| {
                    if self.ast.decl(*param).storage.is_some() {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: self.ast.decl(*param).span,
                            kind: TyperDiagnosticKind::StorageClassesDisallowed,
                        });
                    }
                    self.visit_block_scope_decl(*param)
                });
                if let Some(body) = body {
                    self.curr_ret = self.get_return_type(decl).unwrap_or_default();
                    self.ast.bitems(body).iter().for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt_ref: StmtRef) {
        let stmt = self.ast.stmt(stmt_ref);
        match &stmt.kind {
            StmtKind::Empty
            | StmtKind::Break(_)
            | StmtKind::Continue(_)
            | StmtKind::Goto { .. } => {}
            StmtKind::Expr(expr) => {
                self.visit_expr(*expr);
            }
            StmtKind::Return(expr) => {
                let ty = self.visit_expr(*expr);
                if ty != self.curr_ret {
                    self.result.actions.schedule_cast(*expr, self.curr_ret);
                }
            }
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
                self.ast.bitems(*items).iter().for_each(|item| match item {
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
                        ForInit::Expr(expr) => {
                            self.visit_expr(*expr);
                        }
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
                let cty = self.visit_expr(*cond);
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
                                Some(ConstValue::Int32(v)) => {
                                    let v = match cty {
                                        Type::Int => ConstValue::Int32(v),
                                        Type::Long => ConstValue::Int64(v as i64),
                                        _ => panic!("invalid switch condition type"),
                                    };
                                    if !self.switch_cases.insert(v) {
                                        self.result.diagnostics.push(TyperDiagnostic {
                                            span: self.ast.expr(*expr).span,
                                            kind: TyperDiagnosticKind::DuplicateSwitchCase,
                                        });
                                    }
                                }
                                Some(ConstValue::Int64(v)) => {
                                    let v = match cty {
                                        Type::Int => ConstValue::Int32(v as i32),
                                        Type::Long => ConstValue::Int64(v),
                                        _ => panic!("invalid switch condition type"),
                                    };
                                    if !self.switch_cases.insert(v) {
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
                self.visit_stmt(*body);
            }
        }
    }

    fn visit_expr(&mut self, expr_ref: ExprRef) -> Type {
        let expr = self.ast.expr(expr_ref);
        let ty = match &expr.kind {
            ExprKind::Const(ConstValue::Int32(_)) => Type::Int,
            ExprKind::Const(ConstValue::Int64(_)) => Type::Long,
            ExprKind::Grouped(expr) => self.visit_expr(*expr),
            ExprKind::Cast { ty, expr } => {
                self.visit_expr(*expr);
                *ty
            }
            ExprKind::Ternary { cond, then, other } => {
                self.visit_expr(*cond);
                let tty = self.visit_expr(*then);
                let oty = self.visit_expr(*other);
                let common = self.get_common_type(tty, oty, expr.span);
                if tty != common {
                    self.result.actions.schedule_cast(*then, common);
                }
                if oty != common {
                    self.result.actions.schedule_cast(*other, common);
                }
                common
            }
            ExprKind::Unary { op, expr } => {
                let ty = self.visit_expr(*expr);
                match op {
                    UnaryOp::LogicalNot => Type::Int,
                    UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                        self.assert_is_lvalue(*expr);
                        ty
                    }
                    _ => ty,
                }
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lty = self.visit_expr(*lhs);
                let rty = self.visit_expr(*rhs);
                match op {
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => Type::Int,
                    BinaryOp::BitShl | BinaryOp::BitShr => lty,
                    BinaryOp::BitShlAssign | BinaryOp::BitShrAssign => {
                        self.assert_is_lvalue(*lhs);
                        lty
                    }
                    BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::LessThan
                    | BinaryOp::LessEqual
                    | BinaryOp::GreaterThan
                    | BinaryOp::GreaterEqual => {
                        let common = self.get_common_type(lty, rty, expr.span);
                        if lty != common {
                            self.result.actions.schedule_cast(*lhs, common);
                        }
                        if rty != common {
                            self.result.actions.schedule_cast(*rhs, common);
                        }
                        Type::Int
                    }
                    BinaryOp::Assign
                    | BinaryOp::AddAssign
                    | BinaryOp::SubAssign
                    | BinaryOp::MulAssign
                    | BinaryOp::DivAssign
                    | BinaryOp::RemAssign
                    | BinaryOp::BitOrAssign
                    | BinaryOp::BitAndAssign
                    | BinaryOp::BitXorAssign => {
                        self.assert_is_lvalue(*lhs);
                        let common = self.get_common_type(lty, rty, expr.span);
                        if rty != common {
                            self.result.actions.schedule_cast(*rhs, common);
                        }
                        lty
                    }
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Rem
                    | BinaryOp::BitOr
                    | BinaryOp::BitAnd
                    | BinaryOp::BitXor => {
                        let common = self.get_common_type(lty, rty, expr.span);
                        if lty != common {
                            self.result.actions.schedule_cast(*lhs, common);
                        }
                        if rty != common {
                            self.result.actions.schedule_cast(*rhs, common);
                        }
                        common
                    }
                }
            }
            ExprKind::Var(name) => {
                let ty = self
                    .ctx
                    .symbol(name.sema.get())
                    .expect("symbol info not found")
                    .ty;
                if matches!(ty, Type::Void | Type::Compound(_)) {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: expr.span,
                        kind: TyperDiagnosticKind::VariableUsedAsFunction,
                    });
                }
                ty
            }
            ExprKind::Call { name, args } => {
                self.ast.exprs(*args).iter().for_each(|arg| {
                    self.visit_expr(*arg);
                });
                let ty = self
                    .ctx
                    .symbol(name.sema.get())
                    .expect("symbol info not found")
                    .ty;
                match ty {
                    Type::Compound(r) => match self.ctx.dict.get_compound(r) {
                        CompoundType::Ptr(_) => todo!("handle pointer type"),
                        CompoundType::Func { ret, params } => {
                            if args.len() != params.len() {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: expr.span,
                                    kind: TyperDiagnosticKind::FunctionCalledWithWrongArgsNumber,
                                });
                            }
                            self.ast
                                .exprs(*args)
                                .iter()
                                .zip(params)
                                .for_each(|(arg, ty)| {
                                    if self.ast.expr(*arg).ty.get() != *ty {
                                        self.result.actions.schedule_cast(*arg, *ty);
                                    }
                                });
                            *ret
                        }
                    },
                    _ => {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: expr.span,
                            kind: TyperDiagnosticKind::VariableUsedAsFunction,
                        });
                        Type::default()
                    }
                }
            }
        };
        expr.ty.set(ty);
        ty
    }

    // ---------------------------------------------------------------------------
    // Auxiliary methods
    // ---------------------------------------------------------------------------

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

    #[inline]
    fn get_return_type(&self, decl: &Decl) -> Option<Type> {
        match decl.ty {
            Type::Compound(r) => match self.ctx.dict.get_compound(r) {
                CompoundType::Func { ret, .. } => Some(*ret),
                _ => None,
            },
            _ => None,
        }
    }

    #[inline]
    fn get_common_type(&mut self, lhs: Type, rhs: Type, span: SourceSpan) -> Type {
        match (lhs, rhs) {
            (Type::Int, Type::Int) => Type::Int,
            (Type::Long, Type::Long) => Type::Long,
            (Type::Int, Type::Long) | (Type::Long, Type::Int) => Type::Long,
            _ => {
                self.result.diagnostics.push(TyperDiagnostic {
                    span,
                    kind: TyperDiagnosticKind::TypeMismatch,
                });
                Type::Void
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
        ExprKind::Cast { .. } => todo!("handle cast expressions"),
        ExprKind::Grouped(expr) => is_lvalue(ast, expr),
        ExprKind::Const(_)
        | ExprKind::Unary { .. }
        | ExprKind::Binary { .. }
        | ExprKind::Ternary { .. }
        | ExprKind::Call { .. } => false,
    }
}

fn eval_constant(ast: &Ast, expr: ExprRef) -> Option<ConstValue> {
    match ast.expr(expr).kind {
        ExprKind::Const(value) => Some(value),
        ExprKind::Cast { .. } => todo!("handle cast expressions"),
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
    DeclarationTypeMismatch,
    DeclarationVisibilityMismatch,
    DuplicateSwitchCase,
    ExternLocalInitialized,
    FunctionCalledWithWrongArgsNumber,
    FunctionUsedAsVariable,
    InvalidLValue,
    MultipleInitializers,
    NotConstant,
    StorageClassesDisallowed,
    TypeMismatch,
    VariableUsedAsFunction,
}

impl From<TyperDiagnostic> for Diagnostic {
    fn from(diagnostic: TyperDiagnostic) -> Self {
        match diagnostic.kind {
            TyperDiagnosticKind::DeclarationTypeMismatch => Diagnostic::error(
                diagnostic.span,
                "declaration type mismatch",
                "this declaration has a different type than expected",
            ),
            TyperDiagnosticKind::DeclarationVisibilityMismatch => Diagnostic::error(
                diagnostic.span,
                "declaration visibility mismatch",
                "this declaration has a different visibility than expected",
            ),
            TyperDiagnosticKind::DuplicateSwitchCase => Diagnostic::error(
                diagnostic.span,
                "duplicate switch case",
                "this case value is already defined",
            ),
            TyperDiagnosticKind::ExternLocalInitialized => Diagnostic::error(
                diagnostic.span,
                "local extern variable initialized",
                "local extern variable cannot have an initializer",
            ),
            TyperDiagnosticKind::FunctionCalledWithWrongArgsNumber => Diagnostic::error(
                diagnostic.span,
                "function called with wrong number of arguments",
                "this function is being called with a wrong number of arguments",
            ),
            TyperDiagnosticKind::FunctionUsedAsVariable => Diagnostic::error(
                diagnostic.span,
                "function used as variable",
                "this function is being used as a variable",
            ),
            TyperDiagnosticKind::InvalidLValue => Diagnostic::error(
                diagnostic.span,
                "invalid lvalue",
                "this expression is not a valid lvalue",
            ),
            TyperDiagnosticKind::MultipleInitializers => Diagnostic::error(
                diagnostic.span,
                "multiple initializers",
                "this variable is already initialized",
            ),
            TyperDiagnosticKind::NotConstant => Diagnostic::error(
                diagnostic.span,
                "not a constant",
                "this expression is not a constant",
            ),
            TyperDiagnosticKind::StorageClassesDisallowed => Diagnostic::error(
                diagnostic.span,
                "storage classes disallowed",
                "storage classes are disallowed in this context",
            ),
            TyperDiagnosticKind::TypeMismatch => Diagnostic::error(
                diagnostic.span,
                "type mismatch",
                "this expression has a different type than expected",
            ),
            TyperDiagnosticKind::VariableUsedAsFunction => Diagnostic::error(
                diagnostic.span,
                "variable used as function",
                "this variable is being used as a function",
            ),
        }
    }
}
