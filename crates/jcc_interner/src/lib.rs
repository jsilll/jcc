//! A high-performance string interning library.
//!
//! This library provides efficient string interning functionality, allowing you to store
//! unique strings once and reference them via lightweight `Symbol` handles. String interning
//! is useful for:
//!
//! - Reducing memory usage when dealing with many duplicate strings
//! - Fast string comparison using symbol equality instead of string comparison
//! - Efficient storage for identifiers in compilers, interpreters, and parsers
//!
//! # Implementation Details
//!
//! The interner uses a bump allocator strategy to minimize allocations. Strings are stored
//! in an internal buffer that grows exponentially when needed. Old buffers are retained to
//! ensure all string references remain valid for the lifetime of the interner.

pub mod symtab;

use std::{collections::HashMap, marker::PhantomData, num::NonZeroU32};

/// A lightweight handle representing an interned string.
///
/// `Symbol` is a small, copyable type that acts as a unique identifier.
/// It is parameterized by `M` to ensure strict type safety between different interners.
pub struct Symbol<M>(NonZeroU32, PhantomData<M>);

impl<M> Copy for Symbol<M> {}
impl<M> Clone for Symbol<M> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<M> Eq for Symbol<M> {}
impl<M> PartialEq for Symbol<M> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<M> std::hash::Hash for Symbol<M> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<M> std::fmt::Debug for Symbol<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Symbol").field(&self.0).finish()
    }
}

// TODO: Remove this after introducing new IR
impl<M> Default for Symbol<M> {
    /// Returns a default `Symbol` with the maximum value.
    ///
    /// This default value is used as a sentinel and should not be used
    /// to look up strings in an interner.
    fn default() -> Self {
        Self(NonZeroU32::MAX, PhantomData)
    }
}

/// A string interner that stores unique strings and provides `Symbol` handles for efficient access.
///
/// The `Interner` maintains a mapping between strings and their associated symbols,
/// ensuring that each unique string is stored only once. It uses an internal buffer
/// allocation strategy to minimize memory allocations and fragmentation.
///
/// # Memory Management
///
/// The interner uses a bump allocator approach:
/// - Strings are appended to an internal buffer
/// - When the buffer is full, a new larger buffer is allocated
/// - Old buffers are retained to keep all string references valid
/// - The buffer grows exponentially to amortize allocation costs
pub struct Interner<M> {
    /// Internal string buffer for storing interned strings.
    buf: String,
    /// Previous full buffers to maintain string validity.
    full: Vec<String>,
    /// Mapping from `Symbol` indices to their corresponding strings.
    vec: Vec<&'static str>,
    /// Mapping for quick lookup of strings to their associated `Symbol`s.
    map: HashMap<&'static str, Symbol<M>>,
}

impl<M> Default for Interner<M> {
    fn default() -> Self {
        Self::with_capacity(1024)
    }
}

impl<M> Interner<M> {
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
            vec: Vec::new(),
            full: Vec::new(),
            map: HashMap::new(),
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
        self.vec.len()
    }

    /// Checks if the interner is empty (i.e., no strings have been interned).
    ///
    /// # Returns
    ///
    /// `true` if the interner is empty, `false` otherwise.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
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
    pub fn lookup(&self, id: Symbol<M>) -> &str {
        self.vec[id.0.get() as usize - 1]
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
    pub fn get(&self, id: Symbol<M>) -> Option<&str> {
        self.vec.get(id.0.get() as usize - 1).copied()
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
    pub fn intern(&mut self, name: &str) -> Symbol<M> {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        // Safety: We explicitly ensure that the NonZeroU32 is never zero by
        // starting the count from 1 and incrementing for each new string.
        let symbol = unsafe {
            Symbol(
                NonZeroU32::new_unchecked(u32::try_from(self.vec.len() + 1).unwrap_or(u32::MAX)),
                PhantomData,
            )
        };
        // Safety: The allocated string reference is valid as long as the interner is alive.
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
        &*std::ptr::from_ref::<str>(&self.buf[start..])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    struct TestMarker;
    type TestInterner = Interner<TestMarker>;

    #[test]
    fn new_interner_is_empty() {
        let interner = TestInterner::new();
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
    }

    #[test]
    fn with_capacity_is_empty() {
        let interner = TestInterner::with_capacity(100);
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
    }

    #[test]
    fn intern_single_string() {
        let mut interner = TestInterner::new();
        let s1 = "hello";
        let sym1 = interner.intern(s1);

        assert_eq!(interner.len(), 1);
        assert!(!interner.is_empty());
        assert_eq!(interner.lookup(sym1), s1);
        assert_eq!(interner.get(sym1), Some(s1));
    }

    #[test]
    fn intern_same_string_multiple_times() {
        let mut interner = TestInterner::new();
        let s1 = "world";
        let sym1 = interner.intern(s1);
        let sym2 = interner.intern(s1);

        assert_eq!(sym1, sym2);
        assert_eq!(interner.len(), 1);
        assert_eq!(interner.lookup(sym1), s1);
    }

    #[test]
    fn intern_different_strings() {
        let mut interner = TestInterner::new();
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
        let mut interner = TestInterner::new();
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
