use crate::{
    ast::{
        constant::Constant,
        ty::{Ty, TyKind},
        Ast, BinaryOp, BlockItem, Decl, DeclData, DeclKind, Expr, ExprKind, ForInit, Stmt,
        StmtKind, StorageClass, UnaryOp,
    },
    desugar::DesugarActions,
    sema::{Attribute, SemaCtx, StaticValue, SymbolInfo},
};

use jcc_backend::codemap::{file::FileId, span::Span, Diagnostic, IntoDiagnostic, Issue, Label};

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
    switch_cases: HashSet<Constant>,
}

impl<'a, 'ctx> TypeChecker<'a, 'ctx> {
    pub fn new(ast: &'a Ast<'ctx>, ctx: &'a mut SemaCtx<'ctx>) -> Self {
        let curr_ret = ctx.ty.void_ty;
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
                    Some(init) => match eval_constant(self.ast, init) {
                        Some(value) => match value.cast(data.ty) {
                            Some(v) => StaticValue::Init(v),
                            None => {
                                self.emit(self.ast.expr[init].span, TypeIssue::ConstantOutOfRange);
                                StaticValue::NoInit
                            }
                        },
                        None => {
                            self.emit(self.ast.expr[init].span, TypeIssue::NotConstant);
                            StaticValue::NoInit
                        }
                    },
                };

                let info =
                    &mut self.ctx.symbols[data.name.sema.get().expect("sema symbol not set")];
                let occupied = info.is_some();
                let info =
                    info.get_or_insert(SymbolInfo::statik(data.ty, decl_is_global, decl_init));

                if info.ty != data.ty {
                    self.result.issues.push(Issue::new(
                        TypeIssue::DeclarationTypeMismatch,
                        self.ast.file,
                        data.span,
                    ));
                }

                if let Attribute::Static {
                    is_global,
                    ref mut init,
                } = info.attr
                {
                    if (is_global != decl_is_global)
                        && !matches!(data.storage, Some(StorageClass::Extern))
                    {
                        self.result.issues.push(Issue::new(
                            TypeIssue::DeclarationVisibilityMismatch,
                            self.ast.file,
                            data.span,
                        ));
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
                                self.emit(data.span, TypeIssue::MultipleInitializers);
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
                Some(StorageClass::Extern) => {
                    if let Some(init) = init {
                        self.emit(self.ast.expr[init].span, TypeIssue::ExternLocalInitialized);
                    }
                    let info = self.ctx.symbols[data.name.sema.get().expect("sema symbol not set")]
                        .get_or_insert(SymbolInfo::statik(data.ty, true, StaticValue::NoInit));
                    if info.ty != data.ty {
                        self.emit(data.span, TypeIssue::DeclarationTypeMismatch);
                    }
                }
                Some(StorageClass::Static) => {
                    let decl_init = match init {
                        None => StaticValue::Tentative,
                        Some(init) => {
                            let ty = self.visit_expr(init);
                            if ty != data.ty {
                                self.result.actions.cast(data.ty, init);
                            }
                            match eval_constant(self.ast, init) {
                                Some(value) => match value.cast(data.ty) {
                                    Some(v) => StaticValue::Init(v),
                                    None => {
                                        self.emit(
                                            self.ast.expr[init].span,
                                            TypeIssue::ConstantOutOfRange,
                                        );
                                        StaticValue::Tentative
                                    }
                                },
                                None => {
                                    self.emit(self.ast.expr[init].span, TypeIssue::NotConstant);
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
                self.result.issues.push(Issue::new(
                    TypeIssue::DeclarationTypeMismatch,
                    self.ast.file,
                    decl.span,
                ));
            }

            if let Attribute::Function {
                is_global,
                ref mut is_defined,
            } = entry.attr
            {
                if is_global && !decl_is_global {
                    self.result.issues.push(Issue::new(
                        TypeIssue::DeclarationVisibilityMismatch,
                        self.ast.file,
                        decl.span,
                    ));
                }
                if body.is_some() {
                    if *is_defined {
                        self.emit(decl.span, TypeIssue::FunctionAlreadyDefined);
                        return;
                    }
                    *is_defined = true;
                }
                self.ast.decls[params].iter().for_each(|param| {
                    if self.ast.decl[*param].storage.is_some() {
                        self.result.issues.push(Issue::new(
                            TypeIssue::StorageClassesDisallowed,
                            self.ast.file,
                            self.ast.decl[*param].span,
                        ));
                    }
                    self.visit_block_scope_decl(*param)
                });
                if let Some(body) = body {
                    self.curr_ret = decl.ty.ret().unwrap_or(self.ctx.ty.void_ty);
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
                                self.result.issues.push(Issue::new(
                                    TypeIssue::StorageClassesDisallowed,
                                    self.ast.file,
                                    self.ast.decl[*decl].span,
                                ));
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
                                    self.result.issues.push(Issue::new(
                                        TypeIssue::NotConstant,
                                        self.ast.file,
                                        self.ast.expr[*expr].span,
                                    ));
                                }
                                Some(c) => match c.cast(cty) {
                                    Some(val) => {
                                        if !self.switch_cases.insert(val) {
                                            self.result.issues.push(Issue::new(
                                                TypeIssue::DuplicateSwitchCase,
                                                self.ast.file,
                                                self.ast.expr[*expr].span,
                                            ));
                                        }
                                    }
                                    None => {
                                        self.result.issues.push(Issue::new(
                                            TypeIssue::ConstantOutOfRange,
                                            self.ast.file,
                                            self.ast.expr[*expr].span,
                                        ));
                                    }
                                },
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
            ExprKind::Const(Constant::Int(_)) => self.ctx.ty.int_ty,
            ExprKind::Const(Constant::UInt(_)) => self.ctx.ty.uint_ty,
            ExprKind::Const(Constant::Long(_)) => self.ctx.ty.long_ty,
            ExprKind::Const(Constant::ULong(_)) => self.ctx.ty.ulong_ty,
            ExprKind::Const(Constant::Double(_)) => self.ctx.ty.double_ty,
            ExprKind::Grouped(expr) => self.visit_expr(*expr),
            ExprKind::Cast { ty, expr } => {
                self.visit_expr(*expr);
                *ty
            }
            ExprKind::Ternary { cond, then, other } => {
                self.visit_expr(*cond);
                let tty = self.visit_expr(*then);
                let oty = self.visit_expr(*other);
                let common = self.assert_common_type(tty, oty, data.span);
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
                    UnaryOp::LogNot => self.ctx.ty.int_ty,
                    UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                        self.assert_is_lvalue(*expr);
                        ty
                    }
                    UnaryOp::BitNot => {
                        if !ty.is_integer() {
                            self.emit(data.span, TypeIssue::InvalidBitwiseOperand);
                        }
                        ty
                    }
                    _ => ty,
                }
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lty = self.visit_expr(*lhs);
                let rty = self.visit_expr(*rhs);
                match op {
                    BinaryOp::LogAnd | BinaryOp::LogOr => self.ctx.ty.int_ty,
                    BinaryOp::BitShl | BinaryOp::BitShr => {
                        if !lty.is_integer() || !rty.is_integer() {
                            self.emit(data.span, TypeIssue::InvalidBitwiseOperand);
                        }
                        lty
                    }
                    BinaryOp::BitShlAssign | BinaryOp::BitShrAssign => {
                        self.assert_is_lvalue(*lhs);
                        if !lty.is_integer() || !rty.is_integer() {
                            self.emit(data.span, TypeIssue::InvalidBitwiseOperand);
                        }
                        lty
                    }
                    BinaryOp::Rem => {
                        if !lty.is_integer() || !rty.is_integer() {
                            self.emit(data.span, TypeIssue::InvalidRemainderOperand);
                        }
                        let common = self.assert_common_type(lty, rty, data.span);
                        if lty != common {
                            self.result.actions.cast(common, *lhs);
                        }
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        common
                    }
                    BinaryOp::RemAssign => {
                        self.assert_is_lvalue(*lhs);
                        if !lty.is_integer() || !rty.is_integer() {
                            self.emit(data.span, TypeIssue::InvalidRemainderOperand);
                        }
                        let common = self.assert_common_type(lty, rty, data.span);
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        lty
                    }
                    BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Lt
                    | BinaryOp::Le
                    | BinaryOp::Gt
                    | BinaryOp::Ge => {
                        let common = self.assert_common_type(lty, rty, data.span);
                        if lty != common {
                            self.result.actions.cast(common, *lhs);
                        }
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        self.ctx.ty.int_ty
                    }
                    BinaryOp::Assign
                    | BinaryOp::AddAssign
                    | BinaryOp::SubAssign
                    | BinaryOp::MulAssign
                    | BinaryOp::DivAssign => {
                        self.assert_is_lvalue(*lhs);
                        let common = self.assert_common_type(lty, rty, data.span);
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        lty
                    }
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        let common = self.assert_common_type(lty, rty, data.span);
                        if lty != common {
                            self.result.actions.cast(common, *lhs);
                        }
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        common
                    }
                    BinaryOp::BitOr | BinaryOp::BitAnd | BinaryOp::BitXor => {
                        if !lty.is_integer() || !rty.is_integer() {
                            self.emit(data.span, TypeIssue::InvalidBitwiseOperand);
                        }
                        let common = self.assert_common_type(lty, rty, data.span);
                        if lty != common {
                            self.result.actions.cast(common, *lhs);
                        }
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        common
                    }
                    BinaryOp::BitOrAssign | BinaryOp::BitAndAssign | BinaryOp::BitXorAssign => {
                        self.assert_is_lvalue(*lhs);
                        if !lty.is_integer() || !rty.is_integer() {
                            self.emit(data.span, TypeIssue::InvalidBitwiseOperand);
                        }
                        let common = self.assert_common_type(lty, rty, data.span);
                        if rty != common {
                            self.result.actions.cast(common, *rhs);
                        }
                        lty
                    }
                }
            }
            ExprKind::Var(name) => {
                let ty = self.ctx.symbols[name.sema.get().expect("sema symbol not set")]
                    .expect("symbol info not found")
                    .ty;
                if matches!(*ty, TyKind::Void | TyKind::Func { .. }) {
                    self.emit(data.span, TypeIssue::VariableUsedAsFunction);
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
                        if self.ast.exprs.len_of(*args) != params.len() {
                            self.emit(data.span, TypeIssue::FunctionCalledWithWrongArgsNumber);
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
                        self.emit(data.span, TypeIssue::VariableUsedAsFunction);
                        self.ctx.ty.void_ty
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

    fn emit(&mut self, span: Span, kind: TypeIssue) {
        self.result
            .issues
            .push(Issue::new(kind, self.ast.file, span));
    }

    fn assert_is_lvalue(&mut self, expr: Expr) -> bool {
        if !self.ast.expr[expr].kind.is_lvalue(self.ast) {
            self.emit(self.ast.expr[expr].span, TypeIssue::InvalidLValue);
            return false;
        }
        true
    }

    fn assert_common_type(&mut self, lhs: Ty<'ctx>, rhs: Ty<'ctx>, span: Span) -> Ty<'ctx> {
        TyKind::common(lhs, rhs).unwrap_or_else(|| {
            self.emit(span, TypeIssue::TypeMismatch);
            self.ctx.ty.void_ty
        })
    }
}

// ---------------------------------------------------------------------------
// Auxiliary functions
// ---------------------------------------------------------------------------

fn eval_constant(ast: &Ast, expr: Expr) -> Option<Constant> {
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
pub enum TypeIssue {
    ConstantOutOfRange,
    DeclarationTypeMismatch,
    DeclarationVisibilityMismatch,
    DuplicateSwitchCase,
    ExternLocalInitialized,
    FunctionAlreadyDefined,
    FunctionCalledWithWrongArgsNumber,
    FunctionUsedAsVariable,
    InvalidBitwiseOperand,
    InvalidLValue,
    InvalidRemainderOperand,
    MultipleInitializers,
    NotConstant,
    StorageClassesDisallowed,
    TypeMismatch,
    VariableUsedAsFunction,
}

impl IntoDiagnostic for TypeIssue {
    fn into_diagnostic(self, file: FileId, span: Span) -> Diagnostic {
        let (msg, note) = match self {
            TypeIssue::ConstantOutOfRange => (
                "constant value is out of range for the target type",
                "the value cannot be represented in the target type without overflow",
            ),
            TypeIssue::DeclarationTypeMismatch => (
                "type doesn't match previous declaration",
                "redeclared with a different type than the original declaration",
            ),
            TypeIssue::DeclarationVisibilityMismatch => (
                "visibility doesn't match previous declaration",
                "redeclared with a different visibility (static/extern) than the original declaration",
            ),
            TypeIssue::DuplicateSwitchCase => (
                "duplicate case value",
                "this case value already appears earlier in the switch statement",
            ),
            TypeIssue::ExternLocalInitialized => (
                "extern variable cannot have an initializer",
                "variables with 'extern' storage class are declarations only and cannot be initialized",
            ),
            TypeIssue::FunctionAlreadyDefined => (
                "function already has a definition",
                "a function can only be defined once; a previous definition already exists",
            ),
            TypeIssue::FunctionCalledWithWrongArgsNumber => (
                "incorrect number of arguments",
                "the number of arguments doesn't match the function's parameter count",
            ),
            TypeIssue::FunctionUsedAsVariable => (
                "cannot use function as a value",
                "functions cannot be used in value contexts; did you forget the function call parentheses?",
            ),
            TypeIssue::InvalidBitwiseOperand => (
                "invalid operand type for bitwise operator",
                "bitwise operators (<<, >>, &, |, ^, ~) only accept integer operands",
            ),
            TypeIssue::InvalidLValue => (
                "cannot assign to this expression",
                "only variables and dereferenced pointers can appear on the left side of an assignment",
            ),
            TypeIssue::InvalidRemainderOperand => (
                "invalid operand type for remainder operator",
                "the remainder operator (%) only accepts integer operands",
            ),
            TypeIssue::MultipleInitializers => (
                "variable already has an initializer",
                "variables can only be initialized once in their declaration",
            ),
            TypeIssue::NotConstant => (
                "expected a constant expression",
                "this context requires a compile-time constant value",
            ),
            TypeIssue::StorageClassesDisallowed => (
                "storage class not allowed here",
                "storage class specifiers (static, extern, register) cannot be used in this context",
            ),
            TypeIssue::TypeMismatch => (
                "type mismatch",
                "the type of this expression doesn't match what was expected",
            ),
            TypeIssue::VariableUsedAsFunction => (
                "cannot call a non-function",
                "only functions can be called; this is a variable",
            ),
        };
        Diagnostic::error()
            .with_label(Label::primary(file, span).with_message(msg))
            .with_note(note)
    }
}

// ---------------------------------------------------------------------------
// TyperResult
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct TyperResult<'ctx> {
    pub actions: DesugarActions<'ctx>,
    pub issues: Vec<Issue<TypeIssue>>,
}
