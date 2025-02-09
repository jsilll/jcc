use crate::parse::{Ast, BlockItem, Decl, DeclRef, Expr, ExprRef, Item, Stmt, StmtRef};

use tacky::string_interner::DefaultSymbol;

use std::{collections::HashMap, hash::Hash};

// ---------------------------------------------------------------------------
// Analyzer
// ---------------------------------------------------------------------------

pub struct Analyzer<'a> {
    ast: &'a Ast,
    symbols: SymbolTable<DefaultSymbol, DeclRef>,
}

impl<'a> Analyzer<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            symbols: SymbolTable::new(),
        }
    }

    pub fn analyze(mut self) {
        self.ast.items().iter().for_each(|item| {
            self.analyze_item(item);
        });
    }

    fn analyze_item(&mut self, item: &Item) {
        self.symbols.push_scope();
        item.body.iter().for_each(|block_item| {
            self.analyze_block_item(block_item);
        });
        self.symbols.pop_scope();
    }

    fn analyze_block_item(&mut self, block_item: &BlockItem) {
        match block_item {
            BlockItem::Decl(decl) => self.analyze_decl(*decl),
            BlockItem::Stmt(stmt) => self.analyze_stmt(*stmt),
        }
    }

    fn analyze_decl(&mut self, decl: DeclRef) {
        match self.ast.get_decl(decl) {
            Decl::Var { name, .. } => {
                // TODO: Check for redeclaration and throw an error
                // if the variable is already declared in the current scope.
                self.symbols.insert(*name, decl);
            }
        }
    }

    fn analyze_stmt(&mut self, stmt: StmtRef) {
        match self.ast.get_stmt(stmt) {
            Stmt::Empty => {}
            Stmt::Expr(expr) => self.analyze_expr(*expr),
            Stmt::Return(expr) => self.analyze_expr(*expr),
        }
    }

    fn analyze_expr(&mut self, expr: ExprRef) {
        match self.ast.get_expr(expr) {
            Expr::Constant(_) => {}
            Expr::Var { .. } => todo!("resolve the var"),
            Expr::Grouped(expr) => self.analyze_expr(*expr),
            Expr::Unary { expr, .. } => self.analyze_expr(*expr),
            Expr::Binary { .. } => todo!("match against op"),
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
