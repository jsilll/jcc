use std::{collections::HashMap, hash::Hash};

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

