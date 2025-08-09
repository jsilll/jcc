use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
};

// Holds the value and the version at which it was last modified.
#[derive(Debug, Clone)]
struct ScopedValue<V> {
    value: V,
    version: u32,
}

/// A `SymbolTable` is a data structure that manages symbols and their associated values.
/// It supports scoped symbol management, allowing for nested scopes.
///
/// This implementation is optimized to avoid memory allocations when pushing and popping scopes.
/// It uses a pair of `HashMap` for fast lookups and an "undo log" to efficiently
/// revert changes when a scope ends. This version uses a versioning system to ensure
/// that a symbol's previous value is only logged once per scope.
///
/// # Type Parameters
/// - `S`: The type of the symbol keys. Must implement `Eq`, `Hash`, and `Clone`.
/// - `V`: The type of the values associated with the symbols. Must implement `Clone`.
#[derive(Debug, Clone)]
pub struct SymbolTable<S, V> {
    /// A stack of indices into `history`, marking the start of each scope's changes.
    markers: Vec<u32>,
    /// The single table containing the current state of all scoped symbols.
    /// The `u32` version corresponds to the scope depth.
    scoped: HashMap<S, ScopedValue<V>>,
    /// A log of changes made in each scope, used for efficient popping.
    history: Vec<(S, Option<ScopedValue<V>>)>,
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
            scoped: HashMap::new(),
            markers: Vec::with_capacity(16),
            history: Vec::with_capacity(64),
        }
    }

    /// Returns the current scope depth/version.
    #[inline]
    fn version(&self) -> u32 {
        self.markers.len() as u32
    }

    /// Clears all the symbols and all the scopes.
    #[inline]
    pub fn clear(&mut self) {
        self.scoped.clear();
        self.history.clear();
        self.markers.clear();
    }

    /// Pushes a new, empty scope onto the scope stack.
    #[inline]
    pub fn push_scope(&mut self) {
        // The new marker points to the end of the current history.
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
        self.scoped.get(key).map(|sv| &sv.value)
    }

    /// Clears all symbols from the current scope.
    #[inline]
    pub fn clear_scope(&mut self) {
        self.pop_scope();
        self.push_scope();
    }

    /// Pops the most recent scope from the scope stack, reverting any changes made within it.
    pub fn pop_scope(&mut self) {
        let marker = self.markers.pop().unwrap_or(0);
        for (key, value) in self.history.drain(marker as usize..).rev() {
            match value {
                None => self.scoped.remove(&key),
                Some(prev) => self.scoped.insert(key, prev),
            };
        }
    }

    /// Removes a key from the current scope or the global table.
    pub fn remove(&mut self, key: &S) -> Option<V> {
        if let Some(old) = self.scoped.remove(key) {
            if old.version < self.version() {
                self.history.push((key.clone(), Some(old.clone())));
                return None;
            }

            return Some(old.value);
        }
        None
    }

    /// Inserts a key-value pair into the current scope or the global table.
    /// If there are active scopes, the pair is inserted into the most recent one.
    /// Otherwise, it is inserted into the global table.
    ///
    /// Returns `None` if the key was newly inserted in the current scope, or the previous value if it was updated.
    pub fn insert(&mut self, key: S, value: V) -> Option<V> {
        let version = self.version();
        let value = ScopedValue { value, version };
        match self.scoped.entry(key) {
            Entry::Vacant(entry) => {
                self.history.push((entry.key().clone(), None));
                entry.insert(value);
                None
            }
            Entry::Occupied(mut entry) => match entry.get().version < version {
                false => Some(entry.insert(value).value),
                true => {
                    self.history
                        .push((entry.key().clone(), Some(entry.get().clone())));
                    entry.insert(value);
                    None
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_and_default() {
        let table: SymbolTable<String, i32> = SymbolTable::new();
        assert!(table.scoped.is_empty());
        assert!(table.history.is_empty());
        assert!(table.markers.is_empty());

        let table: SymbolTable<String, i32> = SymbolTable::default();
        assert!(table.scoped.is_empty());
        assert!(table.history.is_empty());
        assert!(table.markers.is_empty());
    }

    #[test]
    fn test_shadowing() {
        let mut table = SymbolTable::new();
        table.insert("x".to_string(), 10);

        table.push_scope();
        table.insert("x".to_string(), 20);
        assert_eq!(table.get(&"x".to_string()), Some(&20));

        table.push_scope();
        table.insert("x".to_string(), 30);
        assert_eq!(table.get(&"x".to_string()), Some(&30));

        table.pop_scope();
        assert_eq!(table.get(&"x".to_string()), Some(&20));

        table.pop_scope();
        assert_eq!(table.get(&"x".to_string()), Some(&10));
    }

    #[test]
    fn test_multiple_updates_single_history_entry() {
        let mut table = SymbolTable::new();
        table.insert("x".to_string(), 1);

        table.push_scope();
        assert_eq!(table.history.len(), 1);

        table.insert("x".to_string(), 10);
        assert_eq!(table.get(&"x".to_string()), Some(&10));
        assert_eq!(table.history.len(), 2);

        table.insert("x".to_string(), 20);
        assert_eq!(table.get(&"x".to_string()), Some(&20));
        assert_eq!(table.history.len(), 2);

        table.insert("x".to_string(), 30);
        assert_eq!(table.get(&"x".to_string()), Some(&30));
        assert_eq!(table.history.len(), 2);

        table.insert("y".to_string(), 99);
        assert_eq!(table.get(&"y".to_string()), Some(&99));
        assert_eq!(table.history.len(), 3);

        table.pop_scope();
        assert_eq!(table.get(&"x".to_string()), Some(&1));
        assert_eq!(table.get(&"y".to_string()), None);
    }

    #[test]
    fn test_clear_scope() {
        let mut table = SymbolTable::new();

        table.insert("g".to_string(), 0);
        table.push_scope();
        table.insert("s1".to_string(), 1);
        table.insert("s2".to_string(), 2);
        table.clear_scope();

        assert_eq!(table.get(&"s1".to_string()), None);
        assert_eq!(table.get(&"s2".to_string()), None);
        assert_eq!(table.get(&"g".to_string()), Some(&0));

        table.pop_scope();
        assert_eq!(table.get(&"g".to_string()), Some(&0));
    }
}
