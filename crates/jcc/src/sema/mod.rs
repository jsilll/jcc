mod symtab;

use symtab::SymbolTable;

use crate::parse::{
    Ast, BinaryOp, BlockItem, Decl, DeclRef, Expr, ExprRef, Stmt, StmtRef, UnaryOp,
};

use tacky::{
    source_file::{diag::Diagnostic, SourceSpan},
    string_interner::DefaultSymbol,
};

// ---------------------------------------------------------------------------
// AnalyzerError
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct AnalyzerDiagnostic {
    pub span: SourceSpan,
    pub kind: AnalyzerDiagnosticKind,
}

// ---------------------------------------------------------------------------
// AnalyzerResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct AnalyzerResult {
    pub diagnostics: Vec<AnalyzerDiagnostic>,
}

// let mut labels = HashSet::new();
// ast.stmts_labels.iter().for_each(|(_, label)| {
//     if !labels.insert(label.name) {
//         self.result.diagnostics.push(AnalyzerDiagnostic {
//             span: label.span,
//             kind: AnalyzerDiagnosticKind::RedeclaredLabel,
//         });
//     }
// });

// ---------------------------------------------------------------------------
// Analyzer
// ---------------------------------------------------------------------------

pub struct Analyzer {
    result: AnalyzerResult,
    symbols: SymbolTable<DefaultSymbol, DeclRef>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            result: AnalyzerResult::default(),
        }
    }

    pub fn analyze(mut self, ast: &mut Ast) -> AnalyzerResult {
        ast.items_iter_refs().for_each(|item| {
            self.symbols.push_scope();
            let body = ast.get_item(item).body.clone();
            body.iter().for_each(|block_item| match block_item {
                BlockItem::Decl(decl) => self.analyze_decl(ast, *decl),
                BlockItem::Stmt(stmt) => self.analyze_stmt(ast, *stmt),
            });
            self.symbols.pop_scope();
        });
        self.result
    }

    fn analyze_decl(&mut self, ast: &mut Ast, decl: DeclRef) {
        match ast.get_decl(decl) {
            Decl::Var { name, init: value } => {
                if let Some(_) = self.symbols.insert(*name, decl) {
                    self.result.diagnostics.push(AnalyzerDiagnostic {
                        span: *ast.get_decl_span(decl),
                        kind: AnalyzerDiagnosticKind::RedeclaredVariable,
                    });
                }
                if let Some(expr) = value {
                    self.analyze_expr(ast, *expr);
                }
            }
        }
    }

    fn analyze_stmt(&mut self, ast: &mut Ast, stmt: StmtRef) {
        match ast.get_stmt(stmt).clone() {
            Stmt::Empty => {}
            Stmt::Expr(expr) => self.analyze_expr(ast, expr),
            Stmt::Return(expr) => self.analyze_expr(ast, expr),
            Stmt::Goto(_) => todo!("handle goto statements"),
            Stmt::If {
                cond,
                then,
                otherwise,
            } => {
                self.analyze_expr(ast, cond);
                self.analyze_stmt(ast, then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(ast, otherwise);
                }
            }
        }
    }

    fn analyze_expr(&mut self, ast: &mut Ast, expr: ExprRef) {
        let span = *ast.get_expr_span(expr);
        if let Expr::Var { name, decl } = ast.get_expr_mut(expr) {
            match self.symbols.get(name) {
                Some(decl_ref) => {
                    *decl = Some(*decl_ref);
                }
                None => self.result.diagnostics.push(AnalyzerDiagnostic {
                    span,
                    kind: AnalyzerDiagnosticKind::UndefinedVariable,
                }),
            }
            return;
        }
        match ast.get_expr(expr).clone() {
            Expr::Constant(_) => {}
            Expr::Var { .. } => unreachable!(),
            Expr::Grouped(expr) => self.analyze_expr(ast, expr),
            Expr::Unary { op, expr } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                    self.must_be_lvalue(ast, expr);
                    self.analyze_expr(ast, expr);
                }
                _ => self.analyze_expr(ast, expr),
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
                    self.must_be_lvalue(ast, lhs);
                    self.analyze_expr(ast, lhs);
                    self.analyze_expr(ast, rhs);
                }
                _ => {
                    self.analyze_expr(ast, lhs);
                    self.analyze_expr(ast, rhs);
                }
            },
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.analyze_expr(ast, cond);
                self.analyze_expr(ast, then);
                self.analyze_expr(ast, otherwise);
            }
        }
    }

    fn must_be_lvalue(&mut self, ast: &Ast, expr: ExprRef) {
        if !is_lvalue(ast, expr) {
            self.result.diagnostics.push(AnalyzerDiagnostic {
                span: *ast.get_expr_span(expr),
                kind: AnalyzerDiagnosticKind::InvalidLValue,
            });
        }
    }
}

// ---------------------------------------------------------------------------
// AnalyzerDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum AnalyzerDiagnosticKind {
    InvalidLValue,
    UndefinedVariable,
    RedeclaredVariable,
    RedeclaredLabel,
}

impl From<AnalyzerDiagnostic> for Diagnostic {
    fn from(diagnostic: AnalyzerDiagnostic) -> Self {
        match diagnostic.kind {
            AnalyzerDiagnosticKind::InvalidLValue => Diagnostic::error(
                diagnostic.span,
                "invalid lvalue",
                "this expression is not a valid lvalue",
            ),
            AnalyzerDiagnosticKind::UndefinedVariable => Diagnostic::error(
                diagnostic.span,
                "undefined variable",
                "this variable is not declared",
            ),
            AnalyzerDiagnosticKind::RedeclaredVariable => Diagnostic::error(
                diagnostic.span,
                "redeclared variable",
                "this variable is already declared",
            ),
            AnalyzerDiagnosticKind::RedeclaredLabel => Diagnostic::error(
                diagnostic.span,
                "redeclared label",
                "this label is already declared",
            ),
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
