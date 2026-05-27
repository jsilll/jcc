use crate::{
    ast::{
        Ast, BlockItem, Decl, DeclKind, Expr, ExprKind, ForInit, Stmt, StmtKind, StorageClass,
        Symbol,
    },
    sema,
};

use jcc_backend::{
    codemap::{span::Span, Diagnostic, IntoDiagnostic, Issue, Label},
    interner::symtab::EntitySymbolTable,
    Ident,
};
use jcc_entity::{EntityCounter, EntityMap};

// ---------------------------------------------------------------------------
// ResolverPass
// ---------------------------------------------------------------------------

pub struct ResolverPass<'a, 'ctx> {
    /// The AST being analyzed
    ast: &'a Ast<'ctx>,
    /// The result of the name resolution
    result: ResolverResult,
    /// The global symbols map
    globals: EntityMap<Ident, sema::Symbol>,
    /// The current scope symbol table
    scope: EntitySymbolTable<Ident, SymbolInfo>,
}

impl<'a, 'ctx> ResolverPass<'a, 'ctx> {
    pub fn new(ast: &'a Ast<'ctx>) -> Self {
        Self {
            ast,
            globals: EntityMap::default(),
            result: ResolverResult::default(),
            scope: EntitySymbolTable::default(),
        }
    }

    pub fn check(mut self) -> ResolverResult {
        self.ast
            .root
            .iter()
            .for_each(|decl| self.visit_file_scope_decl(*decl));
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
                    self.result.issues.push(Issue::new(
                        ResolverIssue::IllegalLocalStaticFunction,
                        data.span,
                    ));
                }
                match body {
                    Some(_) => {
                        self.result.issues.push(Issue::new(
                            ResolverIssue::IllegalLocalFunctionDefinition,
                            data.span,
                        ));
                    }
                    None => {
                        let symbol = self.get_or_create_global_symbol(&data.name);
                        if let Some(prev) = self
                            .scope
                            .insert(data.name.name, SymbolInfo::with_linkage(symbol))
                        {
                            if !prev.has_linkage {
                                self.result
                                    .issues
                                    .push(Issue::new(ResolverIssue::ConflictingSymbol, data.span));
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
                Some(entry) => name.sema.set(Some(entry.symbol)),
                None => self
                    .result
                    .issues
                    .push(Issue::new(ResolverIssue::UndeclaredVariable, data.span)),
            },
            ExprKind::Call { name, args } => {
                match self.scope.get(&name.name) {
                    Some(entry) => name.sema.set(Some(entry.symbol)),
                    None => self
                        .result
                        .issues
                        .push(Issue::new(ResolverIssue::UndeclaredFunction, data.span)),
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
    fn get_or_create_global_symbol(&mut self, name: &Symbol) -> sema::Symbol {
        let symbol = self
            .globals
            .entry(name.name)
            .or_insert_with(|| self.result.counter.push());
        name.sema.set(Some(*symbol));
        *symbol
    }

    fn get_or_create_scoped_symbol(&mut self, decl: Decl, name: &Symbol, has_linkage: bool) {
        let entry = if has_linkage {
            SymbolInfo::with_linkage(self.get_or_create_global_symbol(name))
        } else {
            let symbol = self.result.counter.push();
            name.sema.set(Some(symbol));
            SymbolInfo::no_linkage(symbol)
        };
        if let Some(prev) = self.scope.insert(name.name, entry) {
            if !(entry.has_linkage && prev.has_linkage) {
                self.result.issues.push(Issue::new(
                    ResolverIssue::ConflictingSymbol,
                    self.ast.decl[decl].span,
                ));
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
    pub symbol: sema::Symbol,
}

impl SymbolInfo {
    #[inline]
    fn no_linkage(symbol: sema::Symbol) -> Self {
        Self {
            symbol,
            has_linkage: false,
        }
    }

    #[inline]
    fn with_linkage(symbol: sema::Symbol) -> Self {
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
pub enum ResolverIssue {
    ConflictingSymbol,
    RedefinedFunction,
    RedeclaredVariable,
    UndeclaredVariable,
    UndeclaredFunction,
    IllegalLocalStaticFunction,
    IllegalLocalFunctionDefinition,
}

impl IntoDiagnostic for ResolverIssue {
    fn into_diagnostic(self, span: Span) -> Diagnostic {
        let (msg, note) = match self {
            ResolverIssue::ConflictingSymbol => (
                "symbol conflicts with previous declaration",
                "a symbol with this name already exists in the current scope",
            ),
            ResolverIssue::RedefinedFunction => (
                "function redefined",
                "this function already has a body; a function can only be defined once",
            ),
            ResolverIssue::RedeclaredVariable => (
                "variable redeclared in same scope",
                "a variable with this name already exists in the current scope",
            ),
            ResolverIssue::UndeclaredVariable => (
                "variable not declared",
                "no variable with this name exists in the current scope or any parent scope",
            ),
            ResolverIssue::UndeclaredFunction => (
                "function not declared",
                "no function with this name has been declared; functions must be declared before use",
            ),
            ResolverIssue::IllegalLocalStaticFunction => (
                "static storage class not allowed on local function",
                "function declarations inside other functions cannot use the 'static' storage class",
            ),
            ResolverIssue::IllegalLocalFunctionDefinition => (
                "cannot define function inside another function",
                "nested function definitions are not allowed in C; only function declarations are permitted inside functions",
            ),
        };
        Diagnostic::error()
            .with_label(Label::primary(span).with_message(msg))
            .with_note(note)
    }
}

// ---------------------------------------------------------------------------
// ResolverResult
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct ResolverResult {
    pub issues: Vec<Issue<ResolverIssue>>,
    pub counter: EntityCounter<sema::Symbol>,
}
