use crate::{
    ast::{
        Ast, AstSymbol, BlockItem, DeclKind, DeclRef, ExprKind, ExprRef, ForInit, StmtKind,
        StmtRef, StorageClass,
    },
    sema::SemaSymbol,
};

use jcc_ssa::{
    interner::{Symbol, SymbolTable},
    sourcemap::{diag::Diagnostic, SourceSpan},
};

use std::{collections::HashMap, num::NonZeroU32};

// ---------------------------------------------------------------------------
// ResolverPass
// ---------------------------------------------------------------------------

pub struct ResolverPass<'a, 'ctx> {
    /// The AST being analyzed
    ast: &'a Ast<'ctx>,
    /// The result of the name resolution
    result: ResolverResult,
    /// The current symbol count
    symbol_count: NonZeroU32,
    /// The current scope symbol table
    scope: SymbolTable<SymbolInfo>,
    /// The global symbols map
    globals: HashMap<Symbol, SemaSymbol>,
}

impl<'a, 'ctx> ResolverPass<'a, 'ctx> {
    pub fn new(ast: &'a Ast<'ctx>) -> Self {
        Self {
            ast,
            globals: HashMap::new(),
            scope: SymbolTable::new(),
            result: ResolverResult::default(),
            symbol_count: NonZeroU32::new(1).unwrap(),
        }
    }

    pub fn check(mut self) -> ResolverResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| self.visit_file_scope_decl(*decl));
        self.result.symbol_count = self.symbol_count.get() as usize;
        self.result
    }

    fn visit_file_scope_decl(&mut self, decl_ref: DeclRef) {
        let decl = &self.ast.decls[decl_ref];
        let symbol = self.get_or_create_global_symbol(&decl.name);
        self.scope
            .insert(decl.name.raw, SymbolInfo::with_linkage(symbol));
        match decl.kind {
            DeclKind::Var(init) => {
                if let Some(expr) = init {
                    self.visit_expr(expr);
                }
            }
            DeclKind::Func { body, params, .. } => {
                self.scope.push_scope();
                self.ast.sliced_decls[params]
                    .iter()
                    .for_each(|param| self.visit_block_scope_decl(*param));
                self.ast.sliced_items[body.unwrap_or_default()]
                    .iter()
                    .for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
                self.scope.pop_scope();
            }
        }
    }

    fn visit_block_scope_decl(&mut self, decl_ref: DeclRef) {
        let decl = &self.ast.decls[decl_ref];
        match &decl.kind {
            DeclKind::Var(init) => {
                let has_linkage = decl.storage == Some(StorageClass::Extern);
                self.get_or_create_scoped_symbol(decl_ref, &decl.name, has_linkage);
                if let Some(expr) = init {
                    self.visit_expr(*expr);
                }
            }
            DeclKind::Func { params, body, .. } => {
                if let Some(StorageClass::Static) = decl.storage {
                    self.result.diagnostics.push(ResolverDiagnostic {
                        span: decl.span,
                        kind: ResolverDiagnosticKind::IllegalLocalStaticFunction,
                    });
                }
                match body {
                    Some(_) => {
                        self.result.diagnostics.push(ResolverDiagnostic {
                            span: decl.span,
                            kind: ResolverDiagnosticKind::IllegalLocalFunctionDefinition,
                        });
                    }
                    None => {
                        let symbol = self.get_or_create_global_symbol(&decl.name);
                        if let Some(prev) = self
                            .scope
                            .insert(decl.name.raw, SymbolInfo::with_linkage(symbol))
                        {
                            if !prev.has_linkage {
                                self.result.diagnostics.push(ResolverDiagnostic {
                                    span: decl.span,
                                    kind: ResolverDiagnosticKind::ConflictingSymbol,
                                });
                            }
                        }
                    }
                }
                self.scope.push_scope();
                self.ast.sliced_decls[*params]
                    .iter()
                    .for_each(|param| self.visit_block_scope_decl(*param));
                self.scope.pop_scope();
            }
        }
    }

    fn visit_stmt(&mut self, stmt: StmtRef) {
        match &self.ast.stmts[stmt].kind {
            StmtKind::Empty
            | StmtKind::Break(_)
            | StmtKind::Continue(_)
            | StmtKind::Goto { .. } => {}
            StmtKind::Expr(expr) => self.visit_expr(*expr),
            StmtKind::Return(expr) => self.visit_expr(*expr),
            StmtKind::Default(stmt) => self.visit_stmt(*stmt),
            StmtKind::Label { stmt, .. } => self.visit_stmt(*stmt),
            StmtKind::Case { expr, stmt } => {
                self.visit_expr(*expr);
                self.visit_stmt(*stmt);
            }
            StmtKind::Switch { cond, body } => {
                self.visit_expr(*cond);
                self.visit_stmt(*body);
            }
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
                self.scope.push_scope();
                self.ast.sliced_items[*items]
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
                self.scope.pop_scope();
            }
            StmtKind::For {
                init,
                cond,
                step,
                body,
            } => {
                self.scope.push_scope();
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => self.visit_expr(*expr),
                        ForInit::VarDecl(decl) => self.visit_block_scope_decl(*decl),
                    }
                }
                if let Some(cond) = cond {
                    self.visit_expr(*cond);
                }
                if let Some(step) = step {
                    self.visit_expr(*step);
                }
                self.visit_stmt(*body);
                self.scope.pop_scope();
            }
        }
    }

    fn visit_expr(&mut self, expr_ref: ExprRef) {
        let expr = &self.ast.exprs[expr_ref];
        match &expr.kind {
            ExprKind::Const(_) => {}
            ExprKind::Grouped(expr) => self.visit_expr(*expr),
            ExprKind::Cast { expr, .. } => self.visit_expr(*expr),
            ExprKind::Unary { expr, .. } => self.visit_expr(*expr),
            ExprKind::Binary { lhs, rhs, .. } => {
                self.visit_expr(*lhs);
                self.visit_expr(*rhs);
            }
            ExprKind::Ternary { cond, then, other } => {
                self.visit_expr(*cond);
                self.visit_expr(*then);
                self.visit_expr(*other);
            }
            ExprKind::Var(name) => match self.scope.get(&name.raw) {
                Some(entry) => name.sema.set(entry.symbol),
                None => self.result.diagnostics.push(ResolverDiagnostic {
                    span: expr.span,
                    kind: ResolverDiagnosticKind::UndeclaredVariable,
                }),
            },
            ExprKind::Call { name, args } => {
                match self.scope.get(&name.raw) {
                    Some(entry) => name.sema.set(entry.symbol),
                    None => self.result.diagnostics.push(ResolverDiagnostic {
                        span: expr.span,
                        kind: ResolverDiagnosticKind::UndeclaredFunction,
                    }),
                }
                self.ast.sliced_exprs[*args]
                    .iter()
                    .for_each(|arg| self.visit_expr(*arg));
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Auxiliary structures
    // ---------------------------------------------------------------------------

    #[inline]
    fn get_or_create_global_symbol(&mut self, name: &AstSymbol) -> SemaSymbol {
        let symbol = self.globals.entry(name.raw).or_insert_with(|| {
            let symbol = SemaSymbol(self.symbol_count);
            self.symbol_count = self.symbol_count.saturating_add(1);
            symbol
        });
        name.sema.set(*symbol);
        *symbol
    }

    fn get_or_create_scoped_symbol(&mut self, decl: DeclRef, name: &AstSymbol, has_linkage: bool) {
        let entry = if has_linkage {
            SymbolInfo::with_linkage(self.get_or_create_global_symbol(name))
        } else {
            let symbol = SemaSymbol(self.symbol_count);
            self.symbol_count = self.symbol_count.saturating_add(1);
            name.sema.set(symbol);
            SymbolInfo::no_linkage(symbol)
        };
        if let Some(prev) = self.scope.insert(name.raw, entry) {
            if !(entry.has_linkage && prev.has_linkage) {
                self.result.diagnostics.push(ResolverDiagnostic {
                    span: self.ast.decls[decl].span,
                    kind: ResolverDiagnosticKind::ConflictingSymbol,
                });
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary structures
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct SymbolInfo {
    pub has_linkage: bool,
    pub symbol: SemaSymbol,
}

impl SymbolInfo {
    #[inline]
    fn no_linkage(symbol: SemaSymbol) -> Self {
        Self {
            symbol,
            has_linkage: false,
        }
    }

    #[inline]
    fn with_linkage(symbol: SemaSymbol) -> Self {
        Self {
            symbol,
            has_linkage: true,
        }
    }
}

// ---------------------------------------------------------------------------
// ResolverDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum ResolverDiagnosticKind {
    ConflictingSymbol,
    RedefinedFunction,
    RedeclaredVariable,
    UndeclaredVariable,
    UndeclaredFunction,
    IllegalLocalStaticFunction,
    IllegalLocalFunctionDefinition,
}

impl From<ResolverDiagnostic> for Diagnostic {
    fn from(diagnostic: ResolverDiagnostic) -> Self {
        match diagnostic.kind {
            ResolverDiagnosticKind::ConflictingSymbol => Diagnostic::error(
                diagnostic.span,
                "conflicting symbol",
                "this symbol conflicts with another declaration",
            ),
            ResolverDiagnosticKind::RedefinedFunction => Diagnostic::error(
                diagnostic.span,
                "redefined function",
                "this function is already defined",
            ),
            ResolverDiagnosticKind::RedeclaredVariable => Diagnostic::error(
                diagnostic.span,
                "redeclared variable",
                "this variable is already declared",
            ),
            ResolverDiagnosticKind::UndeclaredVariable => Diagnostic::error(
                diagnostic.span,
                "undeclared variable",
                "this variable is not declared in the current scope",
            ),
            ResolverDiagnosticKind::UndeclaredFunction => Diagnostic::error(
                diagnostic.span,
                "undeclared function",
                "this function is not declared in the current scope",
            ),
            ResolverDiagnosticKind::IllegalLocalStaticFunction => Diagnostic::error(
                diagnostic.span,
                "illegal local static function",
                "local static functions are not allowed",
            ),
            ResolverDiagnosticKind::IllegalLocalFunctionDefinition => Diagnostic::error(
                diagnostic.span,
                "illegal local function definition",
                "local function definitions are not allowed",
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// ResolverResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct ResolverResult {
    pub symbol_count: usize,
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
