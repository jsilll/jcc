use crate::parse::{Ast, BlockItem, Decl, DeclRef, Expr, ExprRef, Stmt, StmtRef};

use tacky::{
    source_file::{diag::Diagnostic, SourceSpan},
    string_interner::DefaultSymbol,
};

use std::{collections::HashMap, hash::Hash};

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

pub struct ResolverPass {
    result: ResolverResult,
    symbols: SymbolTable<DefaultSymbol, DeclRef>,
}

impl ResolverPass {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            result: ResolverResult::default(),
        }
    }

    pub fn analyze(mut self, ast: &mut Ast) -> ResolverResult {
        ast.items_iter_refs().for_each(|item| {
            self.symbols.push_scope();
            ast.get_block_items(ast.get_item(item).body)
                .to_owned()
                .into_iter()
                .for_each(|block_item| match block_item {
                    BlockItem::Decl(decl) => self.analyze_decl(ast, decl),
                    BlockItem::Stmt(stmt) => self.analyze_stmt(ast, stmt),
                });
            self.symbols.pop_scope();
        });
        self.result
    }

    fn analyze_decl(&mut self, ast: &mut Ast, decl: DeclRef) {
        match ast.get_decl(decl) {
            Decl::Var { name, init } => {
                if let Some(_) = self.symbols.insert(*name, decl) {
                    self.result.diagnostics.push(ResolverDiagnostic {
                        span: *ast.get_decl_span(decl),
                        kind: ResolverDiagnosticKind::RedeclaredVariable,
                    });
                }
                if let Some(expr) = init {
                    self.analyze_expr(ast, *expr);
                }
            }
        }
    }

    fn analyze_stmt(&mut self, ast: &mut Ast, stmt: StmtRef) {
        match ast.get_stmt(stmt).clone() {
            Stmt::Empty | Stmt::Goto(_) => {}
            Stmt::Break(_) => todo!("handle break statements"),
            Stmt::Continue(_) => todo!("handle continue statements"),
            Stmt::Expr(expr) => self.analyze_expr(ast, expr),
            Stmt::Return(expr) => self.analyze_expr(ast, expr),
            Stmt::Label { stmt, .. } => self.analyze_stmt(ast, stmt),
            Stmt::Compound(items) => {
                self.symbols.push_scope();
                ast.get_block_items(items)
                    .to_owned()
                    .into_iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(decl) => self.analyze_decl(ast, decl),
                        BlockItem::Stmt(stmt) => self.analyze_stmt(ast, stmt),
                    });
                self.symbols.pop_scope();
            }
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
            Stmt::While { .. } => todo!("handle while statements"),
            Stmt::DoWhile { .. } => todo!("handle do-while statements"),
            Stmt::For { .. } => todo!("handle for statements"),
        }
    }

    fn analyze_expr(&mut self, ast: &mut Ast, expr: ExprRef) {
        if let Expr::Var { name, decl } = ast.get_expr_mut(expr) {
            match self.symbols.get(name) {
                Some(decl_ref) => {
                    *decl = Some(*decl_ref);
                }
                None => self.result.diagnostics.push(ResolverDiagnostic {
                    span: *ast.get_expr_span(expr),
                    kind: ResolverDiagnosticKind::UndefinedVariable,
                }),
            }
            return;
        }
        match ast.get_expr(expr).clone() {
            Expr::Constant(_) => {}
            Expr::Var { .. } => unreachable!(),
            Expr::Grouped(expr) => self.analyze_expr(ast, expr),
            Expr::Unary { expr, .. } => self.analyze_expr(ast, expr),
            Expr::Binary { lhs, rhs, .. } => {
                self.analyze_expr(ast, lhs);
                self.analyze_expr(ast, rhs);
            }
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
    S: Eq + Hash,
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
