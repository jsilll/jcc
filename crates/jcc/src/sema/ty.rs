use crate::parse::{
    Ast, BinaryOp, BlockItem, Decl, DeclRef, Expr, ExprRef, Stmt, StmtRef, UnaryOp,
};

use tacky::source_file::{diag::Diagnostic, SourceSpan};

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

pub struct TyperPass {
    result: TyperResult,
}

impl TyperPass {
    pub fn new() -> Self {
        Self {
            result: TyperResult::default(),
        }
    }

    pub fn analyze(mut self, ast: &Ast) -> TyperResult {
        ast.items().iter().for_each(|item| {
            ast.get_block_items(item.body)
                .iter()
                .for_each(|block_item| match block_item {
                    BlockItem::Decl(decl) => self.analyze_decl(ast, *decl),
                    BlockItem::Stmt(stmt) => self.analyze_stmt(ast, *stmt),
                });
        });
        self.result
    }

    fn analyze_decl(&mut self, ast: &Ast, decl: DeclRef) {
        match ast.get_decl(decl) {
            Decl::Var { init, .. } => {
                if let Some(init) = init {
                    self.analyze_expr(ast, *init);
                }
            }
        }
    }

    fn analyze_stmt(&mut self, ast: &Ast, stmt: StmtRef) {
        match ast.get_stmt(stmt) {
            Stmt::Empty | Stmt::Goto(_) | Stmt::Label { .. } => {}
            Stmt::Break(_) => todo!("handle break statements"),
            Stmt::Continue(_) => todo!("handle continue statements"),
            Stmt::Expr(expr) => self.analyze_expr(ast, *expr),
            Stmt::Return(expr) => self.analyze_expr(ast, *expr),
            Stmt::Compound(items) => {
                ast.get_block_items(*items)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(decl) => self.analyze_decl(ast, *decl),
                        BlockItem::Stmt(stmt) => self.analyze_stmt(ast, *stmt),
                    });
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
            Stmt::While { .. } => todo!("handle while statements"),
            Stmt::DoWhile { .. } => todo!("handle do-while statements"),
            Stmt::For { .. } => todo!("handle for statements"),
        }
    }

    fn analyze_expr(&mut self, ast: &Ast, expr: ExprRef) {
        match ast.get_expr(expr) {
            Expr::Var { .. } | Expr::Constant(_) => {}
            Expr::Grouped(expr) => self.analyze_expr(ast, *expr),
            Expr::Unary { op, expr } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                    self.must_be_lvalue(ast, *expr);
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
                    self.must_be_lvalue(ast, *lhs);
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

    fn must_be_lvalue(&mut self, ast: &Ast, expr: ExprRef) {
        if !is_lvalue(ast, expr) {
            self.result.diagnostics.push(TyperDiagnostic {
                span: *ast.get_expr_span(expr),
                kind: TyperDiagnosticKind::InvalidLValue,
            });
        }
    }
}

// ---------------------------------------------------------------------------
// Support functions
// ---------------------------------------------------------------------------

fn is_lvalue(ast: &Ast, expr: ExprRef) -> bool {
    match ast.get_expr(expr) {
        Expr::Var { .. } => true,
        Expr::Grouped(expr) => is_lvalue(ast, *expr),
        Expr::Constant(_) | Expr::Unary { .. } | Expr::Binary { .. } => false,
        Expr::Ternary { .. } => false,
    }
}

// ---------------------------------------------------------------------------
// TyperDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum TyperDiagnosticKind {
    InvalidLValue,
}

impl From<TyperDiagnostic> for Diagnostic {
    fn from(diagnostic: TyperDiagnostic) -> Self {
        match diagnostic.kind {
            TyperDiagnosticKind::InvalidLValue => Diagnostic::error(
                diagnostic.span,
                "invalid lvalue",
                "this expression is not a valid lvalue",
            ),
        }
    }
}
