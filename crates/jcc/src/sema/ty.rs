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
    result: TyperResult,
    ctx: &'ctx mut SemaCtx,
    switch_cases: HashSet<Expr>,
}

impl<'ctx> TyperPass<'ctx> {
    pub fn new(ctx: &'ctx mut SemaCtx) -> Self {
        Self {
            ctx,
            switch_cases: HashSet::new(),
            result: TyperResult::default(),
        }
    }

    pub fn analyze(mut self, ast: &Ast) -> TyperResult {
        ast.items().iter().for_each(|item| {
            ast.block_items(item.body)
                .iter()
                .for_each(|block_item| match block_item {
                    BlockItem::Decl(decl) => self.analyze_decl(ast, *decl),
                    BlockItem::Stmt(stmt) => self.analyze_stmt(ast, *stmt),
                });
        });
        self.result
    }

    fn analyze_decl(&mut self, ast: &Ast, decl: DeclRef) {
        match ast.decl(decl) {
            Decl::Var { init, .. } => {
                if let Some(init) = init {
                    self.analyze_expr(ast, *init);
                }
            }
        }
    }

    fn analyze_stmt(&mut self, ast: &Ast, stmt: StmtRef) {
        match ast.stmt(stmt) {
            Stmt::Empty | Stmt::Goto(_) | Stmt::Break | Stmt::Continue => {}
            Stmt::Expr(expr) => self.analyze_expr(ast, *expr),
            Stmt::Return(expr) => self.analyze_expr(ast, *expr),
            Stmt::Default(stmt) => self.analyze_stmt(ast, *stmt),
            Stmt::Label { stmt, .. } => self.analyze_stmt(ast, *stmt),
            Stmt::Compound(items) => {
                ast.block_items(*items)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(decl) => self.analyze_decl(ast, *decl),
                        BlockItem::Stmt(stmt) => self.analyze_stmt(ast, *stmt),
                    });
            }
            Stmt::Case { expr, stmt } => {
                self.analyze_expr(ast, *expr);
                self.analyze_stmt(ast, *stmt);
            }
            Stmt::Switch { cond, body } => {
                if let Some(switch) = self.ctx.switches.get(&stmt) {
                    switch
                        .cases
                        .clone()
                        .iter()
                        .for_each(|stmt| match ast.stmt(*stmt) {
                            Stmt::Case { expr, .. } => {
                                if self.assert_is_constant(ast, *expr)
                                    && !self.switch_cases.insert(*ast.expr(*expr))
                                {
                                    self.result.diagnostics.push(TyperDiagnostic {
                                        span: *ast.expr_span(*expr),
                                        kind: TyperDiagnosticKind::DuplicateSwitchCase,
                                    });
                                }
                            }
                            _ => panic!("unexpected statement in switch case"),
                        });
                }
                self.switch_cases.clear();
                self.analyze_expr(ast, *cond);
                self.analyze_stmt(ast, *body);
            }
            Stmt::If {
                cond,
                then,
                otherwise,
            } => {
                self.analyze_expr(ast, *cond);
                self.analyze_stmt(ast, *then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(ast, *otherwise);
                }
            }
            Stmt::While { cond, body } => {
                self.analyze_expr(ast, *cond);
                self.analyze_stmt(ast, *body);
            }
            Stmt::DoWhile { body, cond } => {
                self.analyze_stmt(ast, *body);
                self.analyze_expr(ast, *cond);
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                if let Some(init) = init {
                    match init {
                        ForInit::Decl(decl) => self.analyze_decl(ast, *decl),
                        ForInit::Expr(expr) => self.analyze_expr(ast, *expr),
                    }
                }
                if let Some(cond) = cond {
                    self.analyze_expr(ast, *cond);
                }
                if let Some(step) = step {
                    self.analyze_expr(ast, *step);
                }
                self.analyze_stmt(ast, *body);
            }
        }
    }

    fn analyze_expr(&mut self, ast: &Ast, expr: ExprRef) {
        match ast.expr(expr) {
            Expr::Var { .. } | Expr::Const(_) => {}
            Expr::Grouped(expr) => self.analyze_expr(ast, *expr),
            Expr::Unary { op, expr } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                    self.assert_is_lvalue(ast, *expr);
                    self.analyze_expr(ast, *expr);
                }
                _ => self.analyze_expr(ast, *expr),
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
                | BinaryOp::BitLshAssign
                | BinaryOp::BitRshAssign => {
                    self.assert_is_lvalue(ast, *lhs);
                    self.analyze_expr(ast, *lhs);
                    self.analyze_expr(ast, *rhs);
                }
                _ => {
                    self.analyze_expr(ast, *lhs);
                    self.analyze_expr(ast, *rhs);
                }
            },
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.analyze_expr(ast, *cond);
                self.analyze_expr(ast, *then);
                self.analyze_expr(ast, *otherwise);
            }
        }
    }

    fn assert_is_lvalue(&mut self, ast: &Ast, expr: ExprRef) -> bool {
        if !is_lvalue(ast, expr) {
            self.result.diagnostics.push(TyperDiagnostic {
                span: *ast.expr_span(expr),
                kind: TyperDiagnosticKind::InvalidLValue,
            });
            return false;
        }
        return true;
    }

    fn assert_is_constant(&mut self, ast: &Ast, expr: ExprRef) -> bool {
        if !is_constant(ast, expr) {
            self.result.diagnostics.push(TyperDiagnostic {
                span: *ast.expr_span(expr),
                kind: TyperDiagnosticKind::NotConstant,
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
        Expr::Const(_) | Expr::Unary { .. } | Expr::Binary { .. } => false,
        Expr::Ternary { .. } => false,
    }
}

fn is_constant(ast: &Ast, expr: ExprRef) -> bool {
    match ast.expr(expr) {
        Expr::Const(_) => true,
        Expr::Grouped(expr) => is_constant(ast, *expr),
        Expr::Var { .. } | Expr::Unary { .. } | Expr::Binary { .. } | Expr::Ternary { .. } => false,
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
