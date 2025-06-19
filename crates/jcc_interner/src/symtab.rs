use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
};

/// A `SymbolTable` is a data structure that manages symbols and their associated values.
/// It supports scoped symbol management, allowing for nested scopes and global symbols.
///
/// This implementation is optimized to avoid memory allocations when pushing and popping scopes.
/// It uses a pair of `HashMap` for fast lookups and an "undo log" to efficiently
/// revert changes when a scope ends.
///
/// # Type Parameters
/// - `S`: The type of the symbol keys. Must implement `Eq`, `Hash`, and `Clone`.
/// - `V`: The type of the values associated with the symbols. Must implement `Clone`.
#[derive(Debug, Clone)]
pub struct SymbolTable<S, V> {
    /// The global symbol table, which stores symbols that are not scoped.
    global: HashMap<S, V>,
    /// The single table containing the current state of all scoped symbols.
    scoped: HashMap<S, V>,
    /// A stack of indices into `history`, marking the start of each scope's changes.
    markers: Vec<u32>,
    /// A log of changes made in each scope, used for efficient popping.
    history: Vec<(S, Option<V>)>,
}

impl<S, V> Default for SymbolTable<S, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S, V> SymbolTable<S, V> {
    /// Creates a new, empty `SymbolTable`.
    #[inline]
    pub fn new() -> Self {
        SymbolTable {
            history: Vec::new(),
            global: HashMap::new(),
            scoped: HashMap::new(),
            markers: Vec::new(),
        }
    }

    /// Clears all symbols from the global table and all scopes.
    #[inline]
    pub fn clear(&mut self) {
        self.global.clear();
        self.scoped.clear();
        self.history.clear();
        self.markers.clear();
    }

    /// Returns `true` if there are any active scopes.
    #[inline]
    fn has_active_scope(&self) -> bool {
        !self.markers.is_empty()
    }

    /// Pushes a new, empty scope onto the scope stack.
    #[inline]
    pub fn push_scope(&mut self) {
        self.markers.push(self.history.len() as u32);
    }
}

impl<S, V> SymbolTable<S, V>
where
    S: Clone + Eq + Hash,
    V: Clone,
{
    /// Retrieves a reference to the value associated with the given key.
    #[inline]
    pub fn get(&self, key: &S) -> Option<&V> {
        self.scoped.get(key).or_else(|| self.global.get(key))
    }

    /// Retrieves a reference to the value associated with the given key in the global table.
    pub fn get_global(&self, key: &S) -> Option<&V> {
        self.global.get(key)
    }

    #[inline]
    /// Retrieves the entry for the given key in the global table.
    pub fn entry_global(&mut self, key: S) -> Entry<'_, S, V> {
        self.global.entry(key)
    }

    /// Retrieves the entry for the given key.
    #[inline]
    pub fn entry(&mut self, key: S) -> Entry<'_, S, V> {
        match self.has_active_scope() {
            false => self.global.entry(key),
            true => {
                let entry = self.scoped.entry(key);
                self.history.push((
                    entry.key().clone(),
                    match &entry {
                        Entry::Occupied(entry) => Some(entry.get().clone()),
                        Entry::Vacant(_) => None,
                    },
                ));
                entry
            }
        }
    }

    /// Clears all symbols from the current scope.
    #[inline]
    pub fn clear_scope(&mut self) {
        match self.has_active_scope() {
            true => {
                self.pop_scope();
                self.push_scope();
            }
            false => {
                self.global.clear();
            }
        }
    }

    /// Pops the most recent scope from the scope stack.
    pub fn pop_scope(&mut self) {
        if let Some(marker) = self.markers.pop() {
            for (key, prev) in self.history.drain(marker as usize..).rev() {
                match prev {
                    None => {
                        self.scoped.remove(&key);
                    }
                    Some(prev) => {
                        self.scoped.insert(key, prev);
                    }
                }
            }
        }
    }

    /// Inserts a key-value pair directly into the global table.
    #[inline]
    pub fn insert_global(&mut self, key: S, value: V) -> Option<V> {
        self.global.insert(key, value)
    }

    /// Inserts a key-value pair into the current scope or the global table.
    /// If there are active scopes, the pair is inserted into the most recent one.
    /// Otherwise, it is inserted into the global table.
    pub fn insert(&mut self, key: S, value: V) -> Option<V> {
        match self.has_active_scope() {
            true => {
                let old = self.scoped.insert(key.clone(), value);
                self.history.push((key, old.clone()));
                old
            }
            false => {
                let old = self.global.insert(key.clone(), value);
                old
            }
        }
    }

    /// Removes a key-value pair directly from the global table.
    #[inline]
    pub fn remove_global(&mut self, key: &S) -> Option<V> {
        self.global.remove(key)
    }

    /// Removes a key-value pair from the current scope or the global table.
    pub fn remove(&mut self, key: &S) -> Option<V> {
        if self.has_active_scope() {
            if let Some(old) = self.scoped.remove(key) {
                self.history.push((key.clone(), Some(old.clone())));
                return Some(old);
            }
        }

        self.global.remove(key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_and_default() {
        let table: SymbolTable<String, i32> = SymbolTable::new();
        assert!(table.global.is_empty());
        assert!(table.scoped.is_empty());
        assert!(table.history.is_empty());
        assert!(table.markers.is_empty());

        let table_default: SymbolTable<String, i32> = SymbolTable::default();
        assert!(table_default.global.is_empty());
    }

    #[test]
    fn test_global_insert_and_get() {
        let mut table = SymbolTable::new();
        table.insert_global("x".to_string(), 100);

        assert_eq!(table.get(&"x".to_string()), Some(&100));
        assert_eq!(table.get(&"y".to_string()), None);
    }

    #[test]
    fn test_simple_scoping() {
        let mut table = SymbolTable::new();

        // No scopes active, inserts into global
        table.insert("a".to_string(), 1);
        assert_eq!(table.get(&"a".to_string()), Some(&1));
        assert!(!table.has_active_scope());

        table.push_scope();
        assert!(table.has_active_scope());

        // Scope is active, inserts into scoped table
        table.insert("b".to_string(), 2);
        assert_eq!(table.get(&"b".to_string()), Some(&2));
        // Global variable still accessible
        assert_eq!(table.get(&"a".to_string()), Some(&1));

        table.pop_scope();
        assert!(!table.has_active_scope());

        // 'b' should be gone
        assert_eq!(table.get(&"b".to_string()), None);
        // 'a' should remain
        assert_eq!(table.get(&"a".to_string()), Some(&1));
    }

    #[test]
    fn test_shadowing() {
        let mut table = SymbolTable::new();
        table.insert("x".to_string(), 10); // Global 'x'

        table.push_scope();
        // Shadow 'x' in the new scope
        table.insert("x".to_string(), 20);
        assert_eq!(table.get(&"x".to_string()), Some(&20));

        table.push_scope();
        // Shadow 'x' again in an inner scope
        table.insert("x".to_string(), 30);
        assert_eq!(table.get(&"x".to_string()), Some(&30));

        table.pop_scope();
        // Back to the first scope's 'x'
        assert_eq!(table.get(&"x".to_string()), Some(&20));

        table.pop_scope();
        // Back to the global 'x'
        assert_eq!(table.get(&"x".to_string()), Some(&10));
    }

    #[test]
    fn test_remove_scoped() {
        let mut table = SymbolTable::new();
        table.insert_global("x".to_string(), 1);

        table.push_scope();
        table.insert("x".to_string(), 2); // Shadow 'x'
        table.insert("y".to_string(), 3);

        // Remove from the current scope
        let removed_y = table.remove(&"y".to_string());
        assert_eq!(removed_y, Some(3));
        assert_eq!(table.get(&"y".to_string()), None);

        // Remove the shadowed 'x'
        let removed_x = table.remove(&"x".to_string());
        assert_eq!(removed_x, Some(2));
        // Now `get` should find the global 'x'
        assert_eq!(table.get(&"x".to_string()), Some(&1));

        table.pop_scope();
        assert_eq!(table.get(&"x".to_string()), Some(&1));
    }

    #[test]
    fn test_remove_global() {
        let mut table = SymbolTable::new();
        table.insert_global("x".to_string(), 1);
        table.insert_global("y".to_string(), 2);

        // Remove a global when no scope is active
        let removed_y = table.remove(&"y".to_string());
        assert_eq!(removed_y, Some(2));
        assert_eq!(table.get(&"y".to_string()), None);

        // Use remove_global directly
        let removed_x = table.remove_global(&"x".to_string());
        assert_eq!(removed_x, Some(1));
        assert_eq!(table.get(&"x".to_string()), None);
    }

    #[test]
    fn test_clear() {
        let mut table = SymbolTable::new();
        table.insert_global("g".to_string(), 0);
        table.push_scope();
        table.insert("s".to_string(), 1);

        table.clear();

        assert!(table.global.is_empty());
        assert!(table.scoped.is_empty());
        assert!(table.history.is_empty());
        assert!(table.markers.is_empty());
        assert_eq!(table.get(&"g".to_string()), None);
        assert_eq!(table.get(&"s".to_string()), None);
    }

    #[test]
    fn test_clear_scope() {
        let mut table = SymbolTable::new();
        table.insert_global("g".to_string(), 0);
        table.push_scope();
        table.insert("s1".to_string(), 1);
        table.insert("s2".to_string(), 2);

        table.clear_scope();

        // Scoped symbols should be gone
        assert_eq!(table.get(&"s1".to_string()), None);
        assert_eq!(table.get(&"s2".to_string()), None);
        // Global should remain
        assert_eq!(table.get(&"g".to_string()), Some(&0));
        // We should still be in a (now empty) scope
        assert!(table.has_active_scope());

        table.pop_scope();
        // Clearing a non-existent scope should clear the global one
        table.clear_scope();
        assert_eq!(table.get(&"g".to_string()), None);
    }
}
