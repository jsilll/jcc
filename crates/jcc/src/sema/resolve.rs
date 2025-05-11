use crate::ast::parse::{Ast, BlockItem, Decl, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef};

use tacky::{
    source_file::{diag::Diagnostic, SourceSpan},
    Symbol,
};

use std::collections::HashMap;

use super::SemaCtx;

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
    symbols: SymbolTable<Symbol, DeclRef>,
}

impl<'a> ResolverPass<'a> {
    pub fn new(ast: &'a Ast, ctx: &'a mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            symbols: SymbolTable::new(),
            result: ResolverResult::default(),
        }
    }

    pub fn analyze(mut self) -> ResolverResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| match self.ast.decl(*decl) {
                Decl::Var { .. } => todo!("handle variable declarations"),
                Decl::Func { body, .. } => {
                    let body = body.expect("expected a function body");
                    self.symbols.push_scope();
                    self.ast
                        .block_items(body)
                        .iter()
                        .for_each(|block_item| match block_item {
                            BlockItem::Decl(decl) => self.visit_decl(*decl),
                            BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        });
                    self.symbols.pop_scope();
                }
            });
        self.result
    }

    fn visit_decl(&mut self, decl: DeclRef) {
        match self.ast.decl(decl) {
            Decl::Var { name, init } => {
                if let Some(_) = self.symbols.insert(*name, decl) {
                    self.result.diagnostics.push(ResolverDiagnostic {
                        span: *self.ast.decl_span(decl),
                        kind: ResolverDiagnosticKind::RedeclaredVariable,
                    });
                }
                if let Some(expr) = init {
                    self.visit_expr(*expr);
                }
            }
            Decl::Func { .. } => todo!("handle function declarations"),
        }
    }

    fn visit_stmt(&mut self, stmt: StmtRef) {
        match self.ast.stmt(stmt) {
            Stmt::Empty | Stmt::Break | Stmt::Continue | Stmt::Goto(_) => {}
            Stmt::Expr(expr) => self.visit_expr(*expr),
            Stmt::Return(expr) => self.visit_expr(*expr),
            Stmt::Default(stmt) => self.visit_stmt(*stmt),
            Stmt::Label { stmt, .. } => self.visit_stmt(*stmt),
            Stmt::Compound(items) => {
                self.symbols.push_scope();
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(decl) => self.visit_decl(*decl),
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                    });
                self.symbols.pop_scope();
            }
            Stmt::Case { expr, stmt } => {
                self.visit_expr(*expr);
                self.visit_stmt(*stmt);
            }
            Stmt::Switch { cond, body } => {
                self.visit_expr(*cond);
                self.visit_stmt(*body);
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
            Stmt::While { cond, body } => {
                self.visit_expr(*cond);
                self.visit_stmt(*body);
            }
            Stmt::DoWhile { body, cond } => {
                self.visit_stmt(*body);
                self.visit_expr(*cond);
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
                        ForInit::VarDecl(decl) => self.visit_decl(*decl),
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
            Expr::Call { .. } => todo!("handle function calls"),
            Expr::Grouped(expr) => self.visit_expr(*expr),
            Expr::Var(name) => match self.symbols.get(name) {
                Some(decl_ref) => {
                    self.ctx.vars.insert(expr, *decl_ref);
                }
                None => self.result.diagnostics.push(ResolverDiagnostic {
                    span: *self.ast.expr_span(expr),
                    kind: ResolverDiagnosticKind::UndefinedVariable,
                }),
            },
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
        }
    }
}

// ---------------------------------------------------------------------------
// ResolverDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum ResolverDiagnosticKind {
    UndefinedVariable,
    RedeclaredVariable,
}

impl From<ResolverDiagnostic> for Diagnostic {
    fn from(diagnostic: ResolverDiagnostic) -> Self {
        match diagnostic.kind {
            ResolverDiagnosticKind::UndefinedVariable => Diagnostic::error(
                diagnostic.span,
                "undefined variable",
                "this variable is not declared",
            ),
            ResolverDiagnosticKind::RedeclaredVariable => Diagnostic::error(
                diagnostic.span,
                "redeclared variable",
                "this variable is already declared",
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Symbol Table
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct SymbolTable<S, V> {
    global: HashMap<S, V>,
    scopes: Vec<HashMap<S, V>>,
}

impl<S, V> SymbolTable<S, V> {
    #[allow(dead_code)]
    pub fn new() -> SymbolTable<S, V> {
        SymbolTable {
            scopes: Vec::new(),
            global: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn clear(&mut self) {
        self.scopes.clear();
        self.global.clear();
    }

    #[allow(dead_code)]
    pub fn clear_scope(&mut self) {
        match self.scopes.last_mut() {
            None => self.global.clear(),
            Some(scope) => scope.clear(),
        }
    }

    #[allow(dead_code)]
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    #[allow(dead_code)]
    pub fn pop_scope(&mut self) -> Option<HashMap<S, V>> {
        self.scopes.pop()
    }
}

impl<S, V> SymbolTable<S, V>
where
    S: Eq + std::hash::Hash,
{
    #[allow(dead_code)]
    pub fn get(&self, key: &S) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            match scope.get(key) {
                None => continue,
                Some(value) => return Some(value),
            }
        }
        self.global.get(key)
    }

    #[allow(dead_code)]
    pub fn get_mut(&mut self, key: &S) -> Option<&mut V> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(key) {
                None => continue,
                Some(value) => return Some(value),
            }
        }
        self.global.get_mut(key)
    }

    #[allow(dead_code)]
    pub fn insert(&mut self, key: S, value: V) -> Option<V> {
        match self.scopes.last_mut() {
            None => self.global.insert(key, value),
            Some(scope) => scope.insert(key, value),
        }
    }

    #[allow(dead_code)]
    pub fn remove(&mut self, key: &S) -> Option<V> {
        match self.scopes.last_mut() {
            None => self.global.remove(key),
            Some(scope) => scope.remove(key),
        }
    }
}
