use crate::{
    ast::{Ast, BlockItem, Decl, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef},
    sema::SemaCtx,
};

use jcc_ssa::{
    interner::{SymbolTable}, sourcemap::{diag::Diagnostic, SourceSpan}
};

// ---------------------------------------------------------------------------
// ResolverResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct ResolverResult {
    pub diagnostics: Vec<ResolverDiagnostic>,
}

// ---------------------------------------------------------------------------
// ResolverDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct ResolverDiagnostic {
    pub span: SourceSpan,
    pub kind: ResolverDiagnosticKind,
}

// ---------------------------------------------------------------------------
// ResolverPass
// ---------------------------------------------------------------------------

pub struct ResolverPass<'a> {
    ast: &'a Ast,
    ctx: &'a mut SemaCtx,
    result: ResolverResult,
    symbols: SymbolTable<SymbolEntry>,
}

impl<'a> ResolverPass<'a> {
    pub fn new(ast: &'a Ast, ctx: &'a mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            result: ResolverResult::default(),
            symbols: SymbolTable::new(),
        }
    }

    pub fn check(mut self) -> ResolverResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| match self.ast.decl(*decl) {
                Decl::Var { .. } => todo!("handle variable declarations"),
                Decl::Func { name, body, params } => {
                    if let Some(entry) =
                        self.symbols.insert(*name, SymbolEntry::with_linkage(*decl))
                    {
                        if !entry.has_linkage {
                            self.result.diagnostics.push(ResolverDiagnostic {
                                span: *self.ast.decl_span(*decl),
                                kind: ResolverDiagnosticKind::RedeclaredFunction,
                            });
                        }
                    }
                    self.symbols.push_scope();
                    self.ast
                        .params(*params)
                        .iter()
                        .for_each(|param| self.visit_local_decl(*param));
                    if let Some(body) = body {
                        self.ast.block_items(*body).iter().for_each(
                            |block_item| match block_item {
                                BlockItem::Decl(decl) => self.visit_local_decl(*decl),
                                BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                            },
                        );
                    }
                    self.symbols.pop_scope();
                }
            });
        self.result
    }

    fn visit_local_decl(&mut self, decl: DeclRef) {
        match self.ast.decl(decl) {
            Decl::Func { body: Some(_), .. } => {
                self.result.diagnostics.push(ResolverDiagnostic {
                    span: *self.ast.decl_span(decl),
                    kind: ResolverDiagnosticKind::IllegalLocalFunctionDefinition,
                });
            }
            Decl::Var { name, init } => {
                if self
                    .symbols
                    .insert(*name, SymbolEntry::without_linkage(decl)).is_some()
                {
                    self.result.diagnostics.push(ResolverDiagnostic {
                        span: *self.ast.decl_span(decl),
                        kind: ResolverDiagnosticKind::RedeclaredVariable,
                    });
                }
                if let Some(expr) = init {
                    self.visit_expr(*expr);
                }
            }
            Decl::Func {
                name,
                params,
                body: None,
                ..
            } => {
                if let Some(entry) = self.symbols.insert(*name, SymbolEntry::with_linkage(decl)) {
                    if !entry.has_linkage {
                        self.result.diagnostics.push(ResolverDiagnostic {
                            span: *self.ast.decl_span(decl),
                            kind: ResolverDiagnosticKind::RedeclaredFunction,
                        });
                    }
                }
                self.symbols.push_scope();
                self.ast
                    .params(*params)
                    .iter()
                    .for_each(|param| self.visit_local_decl(*param));
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
            Stmt::Switch { cond, body } => {
                self.visit_expr(*cond);
                self.visit_stmt(*body);
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
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(decl) => self.visit_local_decl(*decl),
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
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
                        ForInit::VarDecl(decl) => self.visit_local_decl(*decl),
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
        }
    }

    fn visit_expr(&mut self, expr: ExprRef) {
        match self.ast.expr(expr) {
            Expr::Const(_) => {}
            Expr::Grouped(expr) => self.visit_expr(*expr),
            Expr::Unary { expr, .. } => self.visit_expr(*expr),
            Expr::Binary { lhs, rhs, .. } => {
                self.visit_expr(*lhs);
                self.visit_expr(*rhs);
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
            Expr::Var(name) => match self.symbols.get(name) {
                Some(entry) => {
                    self.ctx.names.insert(expr, entry.decl);
                }
                None => self.result.diagnostics.push(ResolverDiagnostic {
                    span: *self.ast.expr_span(expr),
                    kind: ResolverDiagnosticKind::UndefinedVariable,
                }),
            },
            Expr::Call { name, args } => {
                match self.symbols.get(name) {
                    Some(entry) => {
                        self.ctx.names.insert(expr, entry.decl);
                    }
                    None => {
                        self.result.diagnostics.push(ResolverDiagnostic {
                            span: *self.ast.expr_span(expr),
                            kind: ResolverDiagnosticKind::UndefinedFunction,
                        });
                    }
                }
                self.ast
                    .args(*args)
                    .iter()
                    .for_each(|arg| self.visit_expr(*arg));
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary structures
// ---------------------------------------------------------------------------

struct SymbolEntry {
    pub decl: DeclRef,
    pub has_linkage: bool,
}

impl SymbolEntry {
    fn with_linkage(decl: DeclRef) -> Self {
        Self {
            decl,
            has_linkage: true,
        }
    }

    fn without_linkage(decl: DeclRef) -> Self {
        Self {
            decl,
            has_linkage: false,
        }
    }
}

// ---------------------------------------------------------------------------
// ResolverDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum ResolverDiagnosticKind {
    UndefinedVariable,
    UndefinedFunction,
    RedeclaredVariable,
    RedeclaredFunction,
    IllegalLocalFunctionDefinition,
}

impl From<ResolverDiagnostic> for Diagnostic {
    fn from(diagnostic: ResolverDiagnostic) -> Self {
        match diagnostic.kind {
            ResolverDiagnosticKind::UndefinedVariable => Diagnostic::error(
                diagnostic.span,
                "undefined variable",
                "this variable is not declared",
            ),
            ResolverDiagnosticKind::UndefinedFunction => Diagnostic::error(
                diagnostic.span,
                "undefined function",
                "this function is not declared",
            ),
            ResolverDiagnosticKind::RedeclaredVariable => Diagnostic::error(
                diagnostic.span,
                "redeclared variable",
                "this variable is already declared",
            ),
            ResolverDiagnosticKind::RedeclaredFunction => Diagnostic::error(
                diagnostic.span,
                "redeclared function",
                "this function is already declared",
            ),
            ResolverDiagnosticKind::IllegalLocalFunctionDefinition => Diagnostic::error(
                diagnostic.span,
                "illegal local function definition",
                "local function definitions are not allowed",
            ),
        }
    }
}
