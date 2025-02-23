use crate::parse::{Ast, BinaryOp, BlockItem, Decl, DeclRef, Expr, ExprRef, Stmt, StmtRef, UnaryOp};

use tacky::{
    source_file::{diag::Diagnostic, SourceSpan},
    string_interner::DefaultSymbol,
};

use std::{collections::HashMap, hash::Hash};

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
            Stmt::If { cond, then, otherwise } => {
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
            Expr::Ternary { cond, then, otherwise } => {
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
    pub fn new() -> SymbolTable<S, V> {
        SymbolTable {
            scopes: Vec::new(),
            global: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.global.clear();
    }

    pub fn clear_scope(&mut self) {
        match self.scopes.last_mut() {
            None => self.global.clear(),
            Some(scope) => scope.clear(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<HashMap<S, V>> {
        self.scopes.pop()
    }
}

impl<S, V> SymbolTable<S, V>
where
    S: Eq + Hash,
{
    pub fn get(&self, key: &S) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            match scope.get(key) {
                None => continue,
                Some(value) => return Some(value),
            }
        }
        self.global.get(key)
    }

    pub fn get_mut(&mut self, key: &S) -> Option<&mut V> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(key) {
                None => continue,
                Some(value) => return Some(value),
            }
        }
        self.global.get_mut(key)
    }

    pub fn insert(&mut self, key: S, value: V) -> Option<V> {
        match self.scopes.last_mut() {
            None => self.global.insert(key, value),
            Some(scope) => scope.insert(key, value),
        }
    }

    pub fn remove(&mut self, key: &S) -> Option<V> {
        match self.scopes.last_mut() {
            None => self.global.remove(key),
            Some(scope) => scope.remove(key),
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
