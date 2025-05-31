mod symtab;

use std::{collections::HashMap, num::NonZeroU32};

pub type SymbolTable<V> = symtab::SymbolTable<Symbol, V>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Symbol(NonZeroU32);

impl Default for Symbol {
    fn default() -> Self {
        Symbol(unsafe { NonZeroU32::new_unchecked(1) })
    }
}

pub struct Interner {
    buf: String,
    full: Vec<String>,
    vec: Vec<&'static str>,
    map: HashMap<&'static str, Symbol>,
}

impl Default for Interner {
    fn default() -> Self {
        Self::with_capacity(1024)
    }
}

impl Interner {
    /// Creates a new `Interner` with a default capacity of 1024 bytes.
    pub fn new() -> Self {
        Self::with_capacity(1024)
    }

    /// Creates a new `Interner` with the specified initial capacity for the internal buffer.
    ///
    /// # Arguments
    ///
    /// * `capacity` - The initial capacity of the internal string buffer.
    pub fn with_capacity(capacity: usize) -> Self {
        Interner {
            full: Vec::new(),
            map: HashMap::new(),
            vec: vec![Default::default()],
            buf: String::with_capacity(capacity),
        }
    }

    /// Returns the number of unique strings currently interned.
    ///
    /// # Returns
    ///
    /// The number of unique strings stored in the interner.
    #[inline]
    pub fn len(&self) -> usize {
        self.vec[1..].len()
    }

    /// Checks if the interner is empty (i.e., no strings have been interned).
    ///
    /// # Returns
    ///
    /// `true` if the interner is empty, `false` otherwise.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.vec[1..].is_empty()
    }

    /// Retrieves the string associated with the given `Symbol`.
    ///
    /// # Arguments
    ///
    /// * `id` - The `Symbol` whose associated string is to be retrieved.
    ///
    /// # Returns
    ///
    /// A reference to the string associated with the given `Symbol`.
    ///
    /// # Panics
    ///
    /// Panics if the `Symbol` is invalid or out of bounds.
    #[inline]
    pub fn lookup(&self, id: Symbol) -> &str {
        self.vec[id.0.get() as usize]
    }

    /// Retrieves the string associated with the given `Symbol`, if it exists.
    ///
    /// # Arguments
    ///
    /// * `id` - The `Symbol` whose associated string is to be retrieved.
    ///
    /// # Returns
    ///
    /// `Some(&str)` if the `Symbol` is valid, or `None` if it is invalid or out of bounds.
    #[inline]
    pub fn get(&self, id: Symbol) -> Option<&str> {
        self.vec.get(id.0.get() as usize).copied()
    }

    /// Interns the given string and returns its associated `Symbol`.
    ///
    /// If the string has already been interned, the existing `Symbol` is returned.
    /// Otherwise, the string is added to the interner, and a new `Symbol` is created.
    ///
    /// # Arguments
    ///
    /// * `name` - The string to be interned.
    ///
    /// # Returns
    ///
    /// The `Symbol` associated with the interned string.
    #[inline]
    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let symbol = unsafe { Symbol(NonZeroU32::new_unchecked(self.vec.len() as u32)) };
        let name = unsafe { self.alloc(name) };
        self.map.insert(name, symbol);
        self.vec.push(name);
        symbol
    }

    /// Allocates a string in the interner's internal buffer and returns a static reference to it.
    ///
    /// This method is unsafe because it relies on the internal buffer's memory layout
    /// and assumes that the buffer will not be reallocated while the reference is in use.
    ///
    /// # Arguments
    ///
    /// * `name` - The string to be allocated.
    ///
    /// # Returns
    ///
    /// A static reference to the allocated string.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the returned reference is not used after the buffer is reallocated.
    #[inline]
    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let cap = (cap.max(name.len()) + 1).next_power_of_two();
            let full = std::mem::replace(&mut self.buf, String::with_capacity(cap));
            self.full.push(full);
        }
        let start = self.buf.len();
        self.buf.push_str(name);
        &*(&self.buf[start..] as *const str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_interner_is_empty() {
        let interner = Interner::new();
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
    }

    #[test]
    fn with_capacity_is_empty() {
        let interner = Interner::with_capacity(100);
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
    }

    #[test]
    fn intern_single_string() {
        let mut interner = Interner::new();
        let s1 = "hello";
        let sym1 = interner.intern(s1);

        assert_eq!(interner.len(), 1);
        assert!(!interner.is_empty());
        assert_eq!(interner.lookup(sym1), s1);
        assert_eq!(interner.get(sym1), Some(s1));
    }

    #[test]
    fn intern_same_string_multiple_times() {
        let mut interner = Interner::new();
        let s1 = "world";
        let sym1 = interner.intern(s1);
        let sym2 = interner.intern(s1);

        assert_eq!(sym1, sym2);
        assert_eq!(interner.len(), 1);
        assert_eq!(interner.lookup(sym1), s1);
    }

    #[test]
    fn intern_different_strings() {
        let mut interner = Interner::new();
        let s1 = "foo";
        let s2 = "bar";

        let sym1 = interner.intern(s1);
        let sym2 = interner.intern(s2);

        assert_ne!(sym1, sym2);
        assert_eq!(interner.len(), 2);
        assert_eq!(interner.lookup(sym1), s1);
        assert_eq!(interner.lookup(sym2), s2);
    }

    #[test]
    fn intern_empty_string() {
        let mut interner = Interner::new();
        let empty_str = "";
        let sym_empty = interner.intern(empty_str);

        assert_eq!(interner.len(), 1);
        assert_eq!(interner.lookup(sym_empty), empty_str);
        assert_eq!(interner.get(sym_empty), Some(empty_str));

        let sym_empty2 = interner.intern(empty_str);
        assert_eq!(sym_empty, sym_empty2);
        assert_eq!(interner.len(), 1);
    }
}
