use std::collections::HashMap;

/// A `SymbolTable` is a data structure that manages symbols and their associated values.
/// It supports scoped symbol management, allowing for nested scopes and global symbols.
///
/// # Type Parameters
/// - `S`: The type of the symbol keys. Must implement `Eq` and `Hash`.
/// - `V`: The type of the values associated with the symbols.
#[derive(Debug, Default, Clone)]
pub struct SymbolTable<S, V> {
    /// The global symbol table, which stores symbols that are not scoped.
    global: HashMap<S, V>,
    /// A stack of scoped symbol tables, where each scope is represented by a `HashMap`.
    scopes: Vec<HashMap<S, V>>,
}

impl<S, V> SymbolTable<S, V> {
    /// Creates a new, empty `SymbolTable`.
    ///
    /// # Returns
    ///
    /// A new instance of `SymbolTable` with no global symbols or scopes.
    #[inline]
    pub fn new() -> Self {
        SymbolTable {
            scopes: Vec::new(),
            global: HashMap::new(),
        }
    }

    /// Clears all symbols from the global table and all scopes.
    ///
    /// After calling this method, the `SymbolTable` will be empty.
    #[inline]
    pub fn clear(&mut self) {
        self.scopes.clear();
        self.global.clear();
    }

    /// Clears all symbols from the current scope.
    ///
    /// If there are no active scopes, this will clear the global table instead.
    #[inline]
    pub fn clear_scope(&mut self) {
        match self.scopes.last_mut() {
            None => self.global.clear(),
            Some(scope) => scope.clear(),
        }
    }

    /// Pushes a new, empty scope onto the scope stack.
    ///
    /// This creates a new level of symbol visibility.
    #[inline]
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pops the most recent scope from the scope stack.
    ///
    /// # Returns
    ///
    /// The `HashMap` representing the popped scope, or `None` if there are no scopes.
    #[inline]
    pub fn pop_scope(&mut self) -> Option<HashMap<S, V>> {
        self.scopes.pop()
    }
}

impl<S, V> SymbolTable<S, V>
where
    S: Eq + std::hash::Hash,
{
    /// Retrieves a reference to the value associated with the given key.
    ///
    /// # Arguments
    ///
    /// * `key` - The key to look up in the symbol table.
    ///
    /// # Returns
    ///
    /// A reference to the value if the key exists, or `None` if it does not.
    ///
    /// This method searches from the most recent scope to the global table.
    pub fn get(&self, key: &S) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            match scope.get(key) {
                None => continue,
                Some(value) => return Some(value),
            }
        }
        self.global.get(key)
    }

    /// Retrieves a mutable reference to the value associated with the given key.
    ///
    /// # Arguments
    ///
    /// * `key` - The key to look up in the symbol table.
    ///
    /// # Returns
    ///
    /// A mutable reference to the value if the key exists, or `None` if it does not.
    ///
    /// This method searches from the most recent scope to the global table.
    pub fn get_mut(&mut self, key: &S) -> Option<&mut V> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(key) {
                None => continue,
                Some(value) => return Some(value),
            }
        }
        self.global.get_mut(key)
    }

    /// Inserts a key-value pair into the current scope or the global table.
    ///
    /// # Arguments
    ///
    /// * `key` - The key to insert.
    /// * `value` - The value to associate with the key.
    ///
    /// # Returns
    ///
    /// The previous value associated with the key, if it existed.
    ///
    /// If there are active scopes, the key-value pair is inserted into the most recent scope.
    /// Otherwise, it is inserted into the global table.
    #[inline]
    pub fn insert(&mut self, key: S, value: V) -> Option<V> {
        match self.scopes.last_mut() {
            None => self.global.insert(key, value),
            Some(scope) => scope.insert(key, value),
        }
    }

    /// Removes a key-value pair from the current scope or the global table.
    ///
    /// # Arguments
    ///
    /// * `key` - The key to remove.
    ///
    /// # Returns
    ///
    /// The value associated with the key, if it existed.
    ///
    /// If there are active scopes, the key is removed from the most recent scope.
    /// Otherwise, it is removed from the global table.
    #[inline]
    pub fn remove(&mut self, key: &S) -> Option<V> {
        match self.scopes.last_mut() {
            None => self.global.remove(key),
            Some(scope) => scope.remove(key),
        }
    }
}
