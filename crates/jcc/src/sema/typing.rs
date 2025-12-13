use crate::{
    ast::{
        ty::{Ty, TyKind},
        Ast, BinaryOp, BlockItem, Decl, DeclData, DeclKind, Expr, ExprKind, ForInit, Stmt,
        StmtKind, StorageClass, UnaryOp,
    },
    lower::LoweringActions,
    sema::{Attribute, SemaCtx, StaticValue, SymbolInfo},
};

use jcc_ssa::{
    codemap::{file::FileId, span::Span, Diagnostic, Label},
    ir::ConstValue,
};

use std::collections::HashSet;

// ---------------------------------------------------------------------------
// TypeChecker
// ---------------------------------------------------------------------------

pub struct TypeChecker<'a, 'ctx> {
    /// The AST being analyzed
    ast: &'a Ast<'ctx>,
    /// The semantic analysis context
    ctx: &'a mut SemaCtx<'ctx>,
    /// The current function return type
    curr_ret: Ty<'ctx>,
    /// The result of the type checking
    result: TyperResult<'ctx>,
    /// The set of switch case values encountered
    switch_cases: HashSet<ConstValue>,
}

impl<'a, 'ctx> TypeChecker<'a, 'ctx> {
    pub fn new(ast: &'a Ast<'ctx>, ctx: &'a mut SemaCtx<'ctx>) -> Self {
        let curr_ret = ctx.tys.void_ty;
        Self {
            ast,
            ctx,
            curr_ret,
            switch_cases: HashSet::new(),
            result: TyperResult::default(),
        }
    }

    pub fn check(mut self) -> TyperResult<'ctx> {
        self.ast
            .root
            .iter()
            .for_each(|decl| self.visit_file_scope_decl(*decl));
        self.result
    }

    fn visit_file_scope_decl(&mut self, decl: Decl) {
        let data = &self.ast.decl[decl];
        match data.kind {
            DeclKind::Func { .. } => self.visit_func_decl(data),
            DeclKind::Var(init) => {
                let decl_is_global = !matches!(data.storage, Some(StorageClass::Static));
                let decl_init = match init {
                    None => match data.storage {
                        Some(StorageClass::Extern) => StaticValue::NoInit,
                        _ => StaticValue::Tentative,
                    },
                    Some(init) => {
                        let ty = self.visit_expr(init);
                        if ty != data.ty {
                            self.result.actions.cast(data.ty, init);
                        }
                        match eval_constant(self.ast, init) {
                            Some(value) => StaticValue::Init(value),
                            None => {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    file: self.ast.file,
                                    span: self.ast.expr[init].span,
                                    kind: TyperDiagnosticKind::NotConstant,
                                });
                                StaticValue::NoInit
                            }
                        }
                    }
                };

                let info =
                    &mut self.ctx.symbols[data.name.sema.get().expect("sema symbol not set")];
                let occupied = info.is_some();
                let info =
                    info.get_or_insert(SymbolInfo::statik(data.ty, decl_is_global, decl_init));

                if info.ty != data.ty {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: data.span,
                        file: self.ast.file,
                        kind: TyperDiagnosticKind::DeclarationTypeMismatch,
                    });
                }

                if let Attribute::Static {
                    is_global,
                    ref mut init,
                } = info.attr
                {
                    if (is_global != decl_is_global)
                        && !matches!(data.storage, Some(StorageClass::Extern))
                    {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: data.span,
                            file: self.ast.file,
                            kind: TyperDiagnosticKind::DeclarationVisibilityMismatch,
                        });
                    }
                    match init {
                        StaticValue::NoInit => *init = decl_init,
                        StaticValue::Tentative => {
                            if matches!(decl_init, StaticValue::Init(_)) {
                                *init = decl_init
                            }
                        }
                        StaticValue::Init(_) => {
                            if occupied && matches!(decl_init, StaticValue::Init(_)) {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    span: data.span,
                                    file: self.ast.file,
                                    kind: TyperDiagnosticKind::MultipleInitializers,
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    fn visit_block_scope_decl(&mut self, decl: Decl) {
        let data = &self.ast.decl[decl];
        match data.kind {
            DeclKind::Func { .. } => self.visit_func_decl(data),
            DeclKind::Var(init) => match data.storage {
                None => {
                    let _ = self.ctx.symbols[data.name.sema.get().expect("sema symbol not set")]
                        .insert(SymbolInfo::local(data.ty));
                    if let Some(init) = init {
                        let ty = self.visit_expr(init);
                        if ty != data.ty {
                            self.result.actions.cast(data.ty, init);
                        }
                    }
                }
                Some(StorageClass::Extern) => match init {
                    Some(init) => {
                        self.result.diagnostics.push(TyperDiagnostic {
                            file: self.ast.file,
                            span: self.ast.expr[init].span,
                            kind: TyperDiagnosticKind::ExternLocalInitialized,
                        });
                    }
                    None => {
                        let info = self.ctx.symbols
                            [data.name.sema.get().expect("sema symbol not set")]
                        .get_or_insert(SymbolInfo::statik(data.ty, true, StaticValue::NoInit));
                        if info.ty != data.ty {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: data.span,
                                file: self.ast.file,
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
                            if ty != data.ty {
                                self.result.actions.cast(data.ty, init);
                            }
                            match eval_constant(self.ast, init) {
                                Some(value) => StaticValue::Init(value),
                                None => {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        file: self.ast.file,
                                        span: self.ast.expr[init].span,
                                        kind: TyperDiagnosticKind::NotConstant,
                                    });
                                    StaticValue::Tentative
                                }
                            }
                        }
                    };
                    self.ctx.symbols[data.name.sema.get().expect("sema symbol not set")] =
                        Some(SymbolInfo::statik(data.ty, false, decl_init));
                }
            },
        }
    }

    fn visit_func_decl(&mut self, decl: &DeclData<'ctx>) {
        if let DeclKind::Func { params, body } = decl.kind {
            let decl_is_global = !matches!(decl.storage, Some(StorageClass::Static));
            let entry = self.ctx.symbols[decl.name.sema.get().expect("sema symbol not set")]
                .get_or_insert(SymbolInfo::function(decl.ty, decl_is_global, false));

            if entry.ty != decl.ty {
                self.result.diagnostics.push(TyperDiagnostic {
                    span: decl.span,
                    file: self.ast.file,
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
                        file: self.ast.file,
                        kind: TyperDiagnosticKind::DeclarationVisibilityMismatch,
                    });
                }
                if body.is_some() {
                    assert!(!*is_defined, "function already defined");
                    *is_defined = true;
                }
                self.ast.decls[params].iter().for_each(|param| {
                    if self.ast.decl[*param].storage.is_some() {
                        self.result.diagnostics.push(TyperDiagnostic {
                            file: self.ast.file,
                            span: self.ast.decl[*param].span,
                            kind: TyperDiagnosticKind::StorageClassesDisallowed,
                        });
                    }
                    self.visit_block_scope_decl(*param)
                });
                if let Some(body) = body {
                    self.curr_ret = self.get_return_type(decl).unwrap_or(self.ctx.tys.void_ty);
                    self.ast.items[body].iter().for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: Stmt) {
        let data = &self.ast.stmt[stmt];
        match &data.kind {
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
                    self.result.actions.cast(self.curr_ret, *expr);
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
                self.ast.items[*items].iter().for_each(|item| match item {
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
                            if self.ast.decl[*decl].storage.is_some() {
                                self.result.diagnostics.push(TyperDiagnostic {
                                    file: self.ast.file,
                                    span: self.ast.decl[*decl].span,
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
                if let Some(switch) = self.ctx.switches.get(&stmt) {
                    switch
                        .cases
                        .iter()
                        .for_each(|stmt| match &self.ast.stmt[*stmt].kind {
                            StmtKind::Case { expr, .. } => match eval_constant(self.ast, *expr) {
                                None => {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        file: self.ast.file,
                                        span: self.ast.expr[*expr].span,
                                        kind: TyperDiagnosticKind::NotConstant,
                                    });
                                }
                                Some(ConstValue::Int32(v)) => {
                                    let v = match *cty {
                                        TyKind::Int => ConstValue::Int32(v),
                                        TyKind::Long => ConstValue::Int64(v as i64),
                                        _ => panic!("invalid switch condition type"),
                                    };
                                    if !self.switch_cases.insert(v) {
                                        self.result.diagnostics.push(TyperDiagnostic {
                                            file: self.ast.file,
                                            span: self.ast.expr[*expr].span,
                                            kind: TyperDiagnosticKind::DuplicateSwitchCase,
                                        });
                                    }
                                }
                                Some(ConstValue::Int64(v)) => {
                                    let v = match *cty {
                                        TyKind::Int => ConstValue::Int32(v as i32),
                                        TyKind::Long => ConstValue::Int64(v),
                                        _ => panic!("invalid switch condition type"),
                                    };
                                    if !self.switch_cases.insert(v) {
                                        self.result.diagnostics.push(TyperDiagnostic {
                                            file: self.ast.file,
                                            span: self.ast.expr[*expr].span,
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

    fn visit_expr(&mut self, expr: Expr) -> Ty<'ctx> {
        let data = &self.ast.expr[expr];
        let ty = match &data.kind {
            ExprKind::Const(ConstValue::Int32(_)) => self.ctx.tys.int_ty,
            ExprKind::Const(ConstValue::Int64(_)) => self.ctx.tys.long_ty,
            ExprKind::Grouped(expr) => self.visit_expr(*expr),
            ExprKind::Cast { ty, expr } => {
                self.visit_expr(*expr);
                *ty
            }
            ExprKind::Ternary { cond, then, other } => {
                self.visit_expr(*cond);
                let tty = self.visit_expr(*then);
                let oty = self.visit_expr(*other);
                let common = self.get_common_type(tty, oty, data.span);
                if tty != common {
                    self.result.actions.cast(common, *then);
                }
                if oty != common {
                    self.result.actions.cast(common, *other);
                }
                common
            }
            ExprKind::Unary { op, expr } => {
                let ty = self.visit_expr(*expr);
                match op {
                    UnaryOp::LogicalNot => self.ctx.tys.int_ty,
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
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => self.ctx.tys.int_ty,
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
                        let common = self.get_common_type(lty, rty, data.span);
                        if lty != common {
                            self.result.actions.cast(common, *lhs);
                        }
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        self.ctx.tys.int_ty
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
                        let common = self.get_common_type(lty, rty, data.span);
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
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
                        let common = self.get_common_type(lty, rty, data.span);
                        if lty != common {
                            self.result.actions.cast(common, *lhs);
                        }
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        common
                    }
                }
            }
            ExprKind::Var(name) => {
                let ty = self.ctx.symbols[name.sema.get().expect("sema symbol not set")]
                    .expect("symbol info not found")
                    .ty;
                if matches!(*ty, TyKind::Void | TyKind::Func { .. }) {
                    self.result.diagnostics.push(TyperDiagnostic {
                        span: data.span,
                        file: self.ast.file,
                        kind: TyperDiagnosticKind::VariableUsedAsFunction,
                    });
                }
                ty
            }
            ExprKind::Call { name, args } => {
                self.ast.exprs[*args].iter().for_each(|arg| {
                    self.visit_expr(*arg);
                });
                let ty = self.ctx.symbols[name.sema.get().expect("sema symbol not set")]
                    .expect("symbol info not found")
                    .ty;
                match *ty {
                    TyKind::Func { ret, ref params } => {
                        if args.len() != params.len() {
                            self.result.diagnostics.push(TyperDiagnostic {
                                span: data.span,
                                file: self.ast.file,
                                kind: TyperDiagnosticKind::FunctionCalledWithWrongArgsNumber,
                            });
                        }
                        self.ast.exprs[*args]
                            .iter()
                            .zip(params)
                            .for_each(|(arg, ty)| {
                                if self.ast.expr[*arg].ty.get() != *ty {
                                    self.result.actions.cast(*ty, *arg);
                                }
                            });
                        ret
                    }
                    _ => {
                        self.result.diagnostics.push(TyperDiagnostic {
                            span: data.span,
                            file: self.ast.file,
                            kind: TyperDiagnosticKind::VariableUsedAsFunction,
                        });
                        self.ctx.tys.void_ty
                    }
                }
            }
        };
        data.ty.set(ty);
        ty
    }

    // ---------------------------------------------------------------------------
    // Auxiliary methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn assert_is_lvalue(&mut self, expr: Expr) -> bool {
        if !is_lvalue(self.ast, expr) {
            self.result.diagnostics.push(TyperDiagnostic {
                file: self.ast.file,
                span: self.ast.expr[expr].span,
                kind: TyperDiagnosticKind::InvalidLValue,
            });
            return false;
        }
        true
    }

    #[inline]
    fn get_return_type(&self, decl: &DeclData<'ctx>) -> Option<Ty<'ctx>> {
        match *decl.ty {
            TyKind::Func { ret, .. } => Some(ret),
            _ => None,
        }
    }

    #[inline]
    fn get_common_type(&mut self, lhs: Ty<'ctx>, rhs: Ty<'ctx>, span: Span) -> Ty<'ctx> {
        match (&*lhs, &*rhs) {
            (TyKind::Int, TyKind::Int) => self.ctx.tys.int_ty,
            (TyKind::Long, TyKind::Long) => self.ctx.tys.long_ty,
            (TyKind::Int, TyKind::Long) | (TyKind::Long, TyKind::Int) => self.ctx.tys.long_ty,
            _ => {
                self.result.diagnostics.push(TyperDiagnostic {
                    span,
                    file: self.ast.file,
                    kind: TyperDiagnosticKind::TypeMismatch,
                });
                self.ctx.tys.void_ty
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary functions
// ---------------------------------------------------------------------------

fn is_lvalue(ast: &Ast, expr: Expr) -> bool {
    match ast.expr[expr].kind {
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

fn eval_constant(ast: &Ast, expr: Expr) -> Option<ConstValue> {
    match ast.expr[expr].kind {
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
            TyperDiagnosticKind::DeclarationTypeMismatch => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("type doesn't match previous declaration"),
                )
                .with_note("redeclared with a different type than the original declaration"),
            TyperDiagnosticKind::DeclarationVisibilityMismatch => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("visibility doesn't match previous declaration"),
                )
                .with_note("redeclared with a different visibility (static/extern) than the original declaration"),
            TyperDiagnosticKind::DuplicateSwitchCase => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("duplicate case value"),
                )
                .with_note("this case value already appears earlier in the switch statement"),
            TyperDiagnosticKind::ExternLocalInitialized => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("extern variable cannot have an initializer"),
                )
                .with_note("variables with 'extern' storage class are declarations only and cannot be initialized"),
            TyperDiagnosticKind::FunctionCalledWithWrongArgsNumber => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("incorrect number of arguments"),
                )
                .with_note("the number of arguments doesn't match the function's parameter count"),
            TyperDiagnosticKind::FunctionUsedAsVariable => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("cannot use function as a value"),
                )
                .with_note("functions cannot be used in value contexts; did you forget the function call parentheses?"),
            TyperDiagnosticKind::InvalidLValue => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("cannot assign to this expression"),
                )
                .with_note("only variables and dereferenced pointers can appear on the left side of an assignment"),
            TyperDiagnosticKind::MultipleInitializers => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("variable already has an initializer"),
                )
                .with_note("variables can only be initialized once in their declaration"),
            TyperDiagnosticKind::NotConstant => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("expected a constant expression"),
                )
                .with_note("this context requires a compile-time constant value"),
            TyperDiagnosticKind::StorageClassesDisallowed => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("storage class not allowed here"),
                )
                .with_note("storage class specifiers (static, extern, auto, register) cannot be used in this context"),
            TyperDiagnosticKind::TypeMismatch => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("type mismatch"),
                )
                .with_note("the type of this expression doesn't match what was expected"),
            TyperDiagnosticKind::VariableUsedAsFunction => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("cannot call a non-function"),
                )
                .with_note("only functions can be called; this is a variable"),
        }
    }
}

// ---------------------------------------------------------------------------
// TyperResult
// ---------------------------------------------------------------------------

#[derive(Default, PartialEq, Eq)]
pub struct TyperResult<'ctx> {
    pub actions: LoweringActions<'ctx>,
    pub diagnostics: Vec<TyperDiagnostic>,
}

// ---------------------------------------------------------------------------
// TyperDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct TyperDiagnostic {
    pub span: Span,
    pub file: FileId,
    pub kind: TyperDiagnosticKind,
}
