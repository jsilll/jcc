use crate::{
    ast::{Ast, BlockItem, DeclKind, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef, StorageClass},
    sema::{SemaCtx, SemaSymbol},
};

use jcc_ssa::{
    interner::{Symbol, SymbolTable},
    sourcemap::{diag::Diagnostic, SourceSpan},
};

use std::num::NonZeroU32;

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
    resolved_counter: NonZeroU32,
    symbols: SymbolTable<SymbolEntry>,
}

impl<'a> ResolverPass<'a> {
    pub fn new(ast: &'a Ast, ctx: &'a mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            symbols: SymbolTable::new(),
            result: ResolverResult::default(),
            resolved_counter: NonZeroU32::new(1).unwrap(),
        }
    }

    pub fn check(mut self) -> ResolverResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| self.visit_file_scope_decl(*decl));
        self.result
    }

    #[inline]
    fn new_sema_symbol(&mut self) -> SemaSymbol {
        let symbol = SemaSymbol(self.resolved_counter);
        self.resolved_counter = self.resolved_counter.saturating_add(1);
        symbol
    }

    fn visit_file_scope_decl(&mut self, decl_ref: DeclRef) {
        let decl = self.ast.decl(decl_ref);
        let entry = SymbolEntry::with_linkage(self.new_sema_symbol());
        decl.name.sema.set(Some(entry.symbol));
        match decl.kind {
            DeclKind::Var(init) => {
                self.symbols.insert(decl.name.raw, entry);
                if let Some(expr) = init {
                    self.visit_expr(expr);
                }
            }
            DeclKind::Func { body, params, .. } => {
                self.symbols.insert(decl.name.raw, entry);
                self.symbols.push_scope();
                self.ast
                    .params(params)
                    .iter()
                    .for_each(|param| self.visit_block_scope_decl(*param));
                self.ast
                    .block_items(body.unwrap_or_default())
                    .iter()
                    .for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
                self.symbols.pop_scope();
            }
        }
    }

    fn visit_block_scope_decl(&mut self, decl_ref: DeclRef) {
        let decl = self.ast.decl(decl_ref);
        match decl.kind {
            DeclKind::Var(init) => {
                let entry = SymbolEntry::new(
                    self.new_sema_symbol(),
                    Some(StorageClass::Extern) == decl.storage,
                );
                decl.name.sema.set(Some(entry.symbol));
                if let Some(prev) = self.symbols.insert(decl.name.raw, entry) {
                    if !(entry.has_linkage && prev.has_linkage) {
                        self.result.diagnostics.push(ResolverDiagnostic {
                            span: *self.ast.decl_span(decl_ref),
                            kind: ResolverDiagnosticKind::ConflictingSymbol,
                        });
                    }
                }
                if let Some(expr) = init {
                    self.visit_expr(expr);
                }
            }
            DeclKind::Func { params, body, .. } => {
                if let Some(StorageClass::Static) = decl.storage {
                    self.result.diagnostics.push(ResolverDiagnostic {
                        span: *self.ast.decl_span(decl_ref),
                        kind: ResolverDiagnosticKind::IllegalLocalStaticFunction,
                    });
                }
                match body {
                    Some(_) => {
                        self.result.diagnostics.push(ResolverDiagnostic {
                            span: *self.ast.decl_span(decl_ref),
                            kind: ResolverDiagnosticKind::IllegalLocalFunctionDefinition,
                        });
                    }
                    None => {
                        let sema = self.new_sema_symbol();
                        self.ast.decl(decl_ref).name.sema.set(Some(sema));
                        if let Some(entry) = self
                            .symbols
                            .insert(decl.name.raw, SymbolEntry::with_linkage(sema))
                        {
                            if !entry.has_linkage {
                                self.result.diagnostics.push(ResolverDiagnostic {
                                    span: *self.ast.decl_span(decl_ref),
                                    kind: ResolverDiagnosticKind::ConflictingSymbol,
                                });
                            }
                        }
                    }
                }
                self.symbols.push_scope();
                self.ast
                    .params(params)
                    .iter()
                    .for_each(|param| self.visit_block_scope_decl(*param));
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
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
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
            Expr::Var(name) => match self.symbols.get(&name.raw) {
                Some(entry) => name.sema.set(Some(entry.symbol)),
                None => self.result.diagnostics.push(ResolverDiagnostic {
                    span: *self.ast.expr_span(expr),
                    kind: ResolverDiagnosticKind::UndeclaredVariable,
                }),
            },
            Expr::Call { name, args } => {
                match self.symbols.get(&name.raw) {
                    Some(entry) => name.sema.set(Some(entry.symbol)),
                    None => self.result.diagnostics.push(ResolverDiagnostic {
                        span: *self.ast.expr_span(expr),
                        kind: ResolverDiagnosticKind::UndeclaredFunction,
                    }),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct SymbolEntry {
    pub has_linkage: bool,
    pub symbol: SemaSymbol,
}

impl SymbolEntry {
    #[inline]
    fn new(symbol: SemaSymbol, has_linkage: bool) -> Self {
        Self {
            symbol,
            has_linkage,
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
