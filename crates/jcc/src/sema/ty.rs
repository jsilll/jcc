use crate::{
    parse::{
        Ast, BinaryOp, BlockItem, Decl, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef, UnaryOp,
    },
    sema::SemaCtx,
};

use tacky::source_file::{diag::Diagnostic, SourceSpan};

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

    pub fn analyze(mut self) -> TyperResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| match self.ast.decl(*decl) {
                Decl::Var { .. } => todo!("handle variable declarations"),
                Decl::Func { body, .. } => {
                    let body = body.expect("expected a function body");
                    self.ast
                        .block_items(body)
                        .iter()
                        .for_each(|block_item| match block_item {
                            BlockItem::Decl(decl) => self.analyze_decl(*decl),
                            BlockItem::Stmt(stmt) => self.analyze_stmt(*stmt),
                        });
                }
            });
        self.result
    }

    fn analyze_decl(&mut self, decl: DeclRef) {
        match self.ast.decl(decl) {
            Decl::Var { init, .. } => {
                if let Some(init) = init {
                    self.analyze_expr(*init);
                }
            }
            Decl::Func { .. } => todo!("handle function declarations"),
        }
    }

    fn analyze_stmt(&mut self, stmt: StmtRef) {
        match self.ast.stmt(stmt) {
            Stmt::Empty | Stmt::Break | Stmt::Continue | Stmt::Goto(_) => {}
            Stmt::Expr(expr) => self.analyze_expr(*expr),
            Stmt::Return(expr) => self.analyze_expr(*expr),
            Stmt::Default(stmt) => self.analyze_stmt(*stmt),
            Stmt::Label { stmt, .. } => self.analyze_stmt(*stmt),
            Stmt::Compound(items) => {
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(decl) => self.analyze_decl(*decl),
                        BlockItem::Stmt(stmt) => self.analyze_stmt(*stmt),
                    });
            }
            Stmt::Case { expr, stmt } => {
                self.analyze_expr(*expr);
                self.analyze_stmt(*stmt);
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
                self.analyze_expr(*cond);
                self.analyze_stmt(*body);
            }
            Stmt::If {
                cond,
                then,
                otherwise,
            } => {
                self.analyze_expr(*cond);
                self.analyze_stmt(*then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(*otherwise);
                }
            }
            Stmt::While { cond, body } => {
                self.analyze_expr(*cond);
                self.analyze_stmt(*body);
            }
            Stmt::DoWhile { body, cond } => {
                self.analyze_stmt(*body);
                self.analyze_expr(*cond);
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                if let Some(init) = init {
                    match init {
                        ForInit::Decl(decl) => self.analyze_decl(*decl),
                        ForInit::Expr(expr) => self.analyze_expr(*expr),
                    }
                }
                if let Some(cond) = cond {
                    self.analyze_expr(*cond);
                }
                if let Some(step) = step {
                    self.analyze_expr(*step);
                }
                self.analyze_stmt(*body);
            }
        }
    }

    fn analyze_expr(&mut self, expr: ExprRef) {
        match self.ast.expr(expr) {
            Expr::Call { .. } => todo!("handle function calls"),
            Expr::Var { .. } | Expr::Const(_) => {}
            Expr::Grouped(expr) => self.analyze_expr(*expr),
            Expr::Unary { op, expr } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                    self.assert_is_lvalue(*expr);
                    self.analyze_expr(*expr);
                }
                _ => self.analyze_expr(*expr),
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
                    self.analyze_expr(*lhs);
                    self.analyze_expr(*rhs);
                }
                _ => {
                    self.analyze_expr(*lhs);
                    self.analyze_expr(*rhs);
                }
            },
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.analyze_expr(*cond);
                self.analyze_expr(*then);
                self.analyze_expr(*otherwise);
            }
        }
    }

    fn assert_is_lvalue(&mut self, expr: ExprRef) -> bool {
        if !is_lvalue(self.ast, expr) {
            self.result.diagnostics.push(TyperDiagnostic {
                span: *self.ast.expr_span(expr),
                kind: TyperDiagnosticKind::InvalidLValue,
            });
            return false;
        }
        return true;
    }
}

// ---------------------------------------------------------------------------
// Support functions
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

// ---------------------------------------------------------------------------
// TyperDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum TyperDiagnosticKind {
    NotConstant,
    InvalidLValue,
    DuplicateSwitchCase,
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
        }
    }
}
