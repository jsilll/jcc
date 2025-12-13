use crate::{
    ast::{
        Ast, AstSymbol, BlockItem, Decl, DeclKind, Expr, ExprKind, ForInit, Stmt, StmtKind,
        StorageClass,
    },
    sema::SemaId,
};

use jcc_ssa::{
    codemap::{file::FileId, span::Span, Diagnostic, Label},
    interner::{Symbol, SymbolTable},
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
    globals: HashMap<Symbol, SemaId>,
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
            .root
            .iter()
            .for_each(|decl| self.visit_file_scope_decl(*decl));
        self.result.symbol_count = self.symbol_count.get() as usize;
        self.result
    }

    fn visit_file_scope_decl(&mut self, decl: Decl) {
        let data = &self.ast.decl[decl];
        let symbol = self.get_or_create_global_symbol(&data.name);
        self.scope
            .insert(data.name.name, SymbolInfo::with_linkage(symbol));
        match data.kind {
            DeclKind::Var(init) => {
                if let Some(expr) = init {
                    self.visit_expr(expr);
                }
            }
            DeclKind::Func { body, params, .. } => {
                self.scope.push_scope();
                self.ast.decls[params]
                    .iter()
                    .for_each(|param| self.visit_block_scope_decl(*param));
                self.ast.items[body.unwrap_or_default()]
                    .iter()
                    .for_each(|item| match item {
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        BlockItem::Decl(decl) => self.visit_block_scope_decl(*decl),
                    });
                self.scope.pop_scope();
            }
        }
    }

    fn visit_block_scope_decl(&mut self, decl: Decl) {
        let data = &self.ast.decl[decl];
        match &data.kind {
            DeclKind::Var(init) => {
                let has_linkage = matches!(data.storage, Some(StorageClass::Extern));
                self.get_or_create_scoped_symbol(decl, &data.name, has_linkage);
                if let Some(expr) = init {
                    self.visit_expr(*expr);
                }
            }
            DeclKind::Func { params, body, .. } => {
                if let Some(StorageClass::Static) = data.storage {
                    self.result.diagnostics.push(ResolverDiagnostic {
                        span: data.span,
                        file: self.ast.file,
                        kind: ResolverDiagnosticKind::IllegalLocalStaticFunction,
                    });
                }
                match body {
                    Some(_) => {
                        self.result.diagnostics.push(ResolverDiagnostic {
                            span: data.span,
                            file: self.ast.file,
                            kind: ResolverDiagnosticKind::IllegalLocalFunctionDefinition,
                        });
                    }
                    None => {
                        let symbol = self.get_or_create_global_symbol(&data.name);
                        if let Some(prev) = self
                            .scope
                            .insert(data.name.name, SymbolInfo::with_linkage(symbol))
                        {
                            if !prev.has_linkage {
                                self.result.diagnostics.push(ResolverDiagnostic {
                                    span: data.span,
                                    file: self.ast.file,
                                    kind: ResolverDiagnosticKind::ConflictingSymbol,
                                });
                            }
                        }
                    }
                }
                self.scope.push_scope();
                self.ast.decls[*params]
                    .iter()
                    .for_each(|param| self.visit_block_scope_decl(*param));
                self.scope.pop_scope();
            }
        }
    }

    fn visit_stmt(&mut self, stmt: Stmt) {
        match &self.ast.stmt[stmt].kind {
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
                self.ast.items[*items]
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

    fn visit_expr(&mut self, expr: Expr) {
        let data = &self.ast.expr[expr];
        match &data.kind {
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
            ExprKind::Var(name) => match self.scope.get(&name.name) {
                Some(entry) => name.id.set(Some(entry.symbol)),
                None => self.result.diagnostics.push(ResolverDiagnostic {
                    span: data.span,
                    file: self.ast.file,
                    kind: ResolverDiagnosticKind::UndeclaredVariable,
                }),
            },
            ExprKind::Call { name, args } => {
                match self.scope.get(&name.name) {
                    Some(entry) => name.id.set(Some(entry.symbol)),
                    None => self.result.diagnostics.push(ResolverDiagnostic {
                        span: data.span,
                        file: self.ast.file,
                        kind: ResolverDiagnosticKind::UndeclaredFunction,
                    }),
                }
                self.ast.exprs[*args]
                    .iter()
                    .for_each(|arg| self.visit_expr(*arg));
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Auxiliary structures
    // ---------------------------------------------------------------------------

    #[inline]
    fn get_or_create_global_symbol(&mut self, name: &AstSymbol) -> SemaId {
        let symbol = self.globals.entry(name.name).or_insert_with(|| {
            let symbol = SemaId(self.symbol_count);
            self.symbol_count = self.symbol_count.saturating_add(1);
            symbol
        });
        name.id.set(Some(*symbol));
        *symbol
    }

    fn get_or_create_scoped_symbol(&mut self, decl: Decl, name: &AstSymbol, has_linkage: bool) {
        let entry = if has_linkage {
            SymbolInfo::with_linkage(self.get_or_create_global_symbol(name))
        } else {
            let symbol = SemaId(self.symbol_count);
            self.symbol_count = self.symbol_count.saturating_add(1);
            name.id.set(Some(symbol));
            SymbolInfo::no_linkage(symbol)
        };
        if let Some(prev) = self.scope.insert(name.name, entry) {
            if !(entry.has_linkage && prev.has_linkage) {
                self.result.diagnostics.push(ResolverDiagnostic {
                    file: self.ast.file,
                    span: self.ast.decl[decl].span,
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
    pub symbol: SemaId,
}

impl SymbolInfo {
    #[inline]
    fn no_linkage(symbol: SemaId) -> Self {
        Self {
            symbol,
            has_linkage: false,
        }
    }

    #[inline]
    fn with_linkage(symbol: SemaId) -> Self {
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
            ResolverDiagnosticKind::ConflictingSymbol => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("symbol conflicts with previous declaration"),
                )
                .with_note("a symbol with this name already exists in the current scope"),
            ResolverDiagnosticKind::RedefinedFunction => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("function redefined"),
                )
                .with_note("this function already has a body; a function can only be defined once"),
            ResolverDiagnosticKind::RedeclaredVariable => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("variable redeclared in same scope"),
                )
                .with_note("a variable with this name already exists in the current scope"),
            ResolverDiagnosticKind::UndeclaredVariable => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("variable not declared"),
                )
                .with_note("no variable with this name exists in the current scope or any parent scope"),
            ResolverDiagnosticKind::UndeclaredFunction => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("function not declared"),
                )
                .with_note("no function with this name has been declared; functions must be declared before use"),
            ResolverDiagnosticKind::IllegalLocalStaticFunction => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("static storage class not allowed on local function"),
                )
                .with_note("function declarations inside other functions cannot use the 'static' storage class"),
            ResolverDiagnosticKind::IllegalLocalFunctionDefinition => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("cannot define function inside another function"),
                )
                .with_note("nested function definitions are not allowed in C; only function declarations are permitted inside functions"),
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
    pub span: Span,
    pub file: FileId,
    pub kind: ResolverDiagnosticKind,
}
