//! An arena-based interning allocator.
//!
//! This module provides [`InternArena`], an arena allocator that *interns* values:
//! equal values are stored only once and are guaranteed to have a unique,
//! stable address for the lifetime of the arena.
//!
//! # Overview
//!
//! `InternArena<T>` combines an arena allocator with a hash set to deduplicate
//! values. When a value is interned:
//!
//! - If an equal value already exists in the arena, a reference to the existing
//!   value is returned.
//! - Otherwise, the value is allocated in the arena and becomes the canonical
//!   representative for that equivalence class.
//!
//! Interned values are accessed through [`Interned`], a lightweight handle that
//! behaves like `&T` but has *pointer-based* equality and hashing.
//!
//! # Guarantees
//!
//! For any two values `a` and `b` returned by the same arena:
//!
//! - If `a == b`, then `ptr::eq(&*a, &*b)` is guaranteed to be `true`.
//! - Equality, ordering, and hashing of [`Interned`] are **O(1)** and based on
//!   pointer identity.
//! - References returned from the arena are valid for the entire lifetime of
//!   the arena itself.
//!
//! This makes the interner particularly well-suited for use in compilers and
//! similar systems where structural equality is common but pointer identity
//! comparisons are desired (e.g. types, symbols, or AST nodes).
//!
//! # Recursive and Self-Referential Structures
//!
//! Interned values may recursively contain other [`Interned`] values, enabling
//! efficient representation of recursive data structures such as types or
//! expression trees.
//!
//! Deduplication is structural: for example, two independently constructed
//! `Vec<Interned<T>>` values with equal contents will be interned to the same
//! allocation, including empty vectors.
//!
//! # Zero-Sized Types
//!
//! Zero-sized types (ZSTs) are fully supported. Interning a ZST will result in
//! exactly one canonical instance stored in the arena.
//!
//! # Interior Mutability and Panics
//!
//! `InternArena` uses interior mutability to allow interning through a shared
//! reference. As a result, calling [`InternArena::intern`] may panic if it is
//! re-entered while already mutably borrowed (for example, if `T::hash` or
//! `T::eq` calls back into `intern()`).
//!
//! This restriction is typical for interning implementations and should not be
//! observable in well-behaved `Eq` / `Hash` implementations.
//!
//! # Thread Safety
//!
//! `InternArena` is **not thread-safe**. It is intended for single-threaded or
//! externally synchronized use. If you need a concurrent interner, additional
//! synchronization is required.

use std::{cell::RefCell, collections::HashSet};

use crate::Arena;

/// An arena allocator that deduplicates equal values.
#[derive(Default)]
pub struct InternArena<T> {
    arena: Arena<T>,
    set: RefCell<HashSet<OpaqueRef<T>>>,
}

impl<'arena, T> InternArena<T>
where
    T: Eq + std::hash::Hash,
{
    /// Creates a new empty interning arena.
    pub fn new() -> Self {
        InternArena {
            arena: Arena::new(),
            set: RefCell::new(HashSet::new()),
        }
    }

    /// Creates a new empty interning arena with the specified capacity.
    ///
    /// The arena will be able to hold at least `capacity` unique elements.
    pub fn with_capacity(capacity: usize) -> Self {
        InternArena {
            arena: Arena::with_capacity(capacity),
            set: RefCell::new(HashSet::with_capacity(capacity)),
        }
    }

    /// Returns the total number of items currently allocated in the arena.
    ///
    /// This counts items across all chunks.
    pub fn len(&self) -> usize {
        self.arena.len()
    }

    /// Returns `true` if the arena contains no elements across all chunks.
    pub fn is_empty(&self) -> bool {
        self.arena.is_empty()
    }

    /// Interns a value in the arena and returns a reference to it.
    ///
    /// If an equal value has already been allocated, returns a reference to
    /// the existing value. Otherwise, allocates the new value and returns a
    /// reference to it.
    ///
    /// The returned reference is valid for the lifetime of the arena itself,
    /// allowing multiple allocations to coexist without borrowing conflicts.
    ///
    /// # Panics
    ///
    /// Panics if called while already borrowed mutably (e.g., if `T::eq` or
    /// `T::hash` somehow calls back into `intern()`).
    #[inline]
    pub fn intern(&'arena self, v: T) -> Option<Interned<'arena, T>> {
        let mut set = self.set.borrow_mut();
        match set.get(&OpaqueRef(&raw const v)) {
            None => {
                let r = self.arena.alloc(v)?;
                set.insert(OpaqueRef(std::ptr::from_ref::<T>(r)));
                Some(Interned(r))
            }
            Some(&OpaqueRef(ptr)) => {
                // Safety: pointers in `set` are guaranteed to point to data
                // allocated in `self.arena`, so they live as long as 'arena.
                unsafe { Some(Interned(&*ptr)) }
            }
        }
    }
}

struct OpaqueRef<T>(*const T);

impl<T: Eq> Eq for OpaqueRef<T> {}
impl<T: PartialEq> PartialEq for OpaqueRef<T> {
    fn eq(&self, other: &Self) -> bool {
        // Safety: pointers are always valid (from arena)
        unsafe { *self.0 == *other.0 }
    }
}

impl<T: std::hash::Hash> std::hash::Hash for OpaqueRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Safety: pointer is always valid (from arena)
        unsafe { (*self.0).hash(state) }
    }
}

/// A handle to an interned value.
///
/// `Interned` behaves like a reference (`&T`) but guarantees that if
/// `a == b`, then `ptr::eq(&*a, &*b)`. This allows for O(1) equality checks
/// and hashing.
pub struct Interned<'arena, T>(&'arena T);

impl<T> Copy for Interned<'_, T> {}
impl<T> Clone for Interned<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Eq for Interned<'_, T> {}
impl<T> PartialEq for Interned<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> Ord for Interned<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::ptr::from_ref::<T>(self.0).cmp(&std::ptr::from_ref::<T>(other.0))
    }
}
impl<T> PartialOrd for Interned<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> AsRef<T> for Interned<'_, T> {
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<T> std::hash::Hash for Interned<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::from_ref::<T>(self.0).hash(state);
    }
}

impl<'a, T> std::ops::Deref for Interned<'a, T> {
    type Target = T;

    fn deref(&self) -> &'a T {
        self.0
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Interned<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0, f)
    }
}

impl<T> std::fmt::Debug for Interned<'_, T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Interned")
            .field(&self.0)
            .field(&std::ptr::from_ref::<T>(self.0))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Hash)]
    struct SimpleValue(i32);

    #[derive(Debug, PartialEq, Eq, Hash)]
    enum TyKind<'ctx> {
        Int,
        Arrow(Vec<Interned<'ctx, TyKind<'ctx>>>),
    }

    type Ty<'ctx> = Interned<'ctx, TyKind<'ctx>>;

    struct TyCtx<'ctx> {
        arena: InternArena<TyKind<'ctx>>,
    }

    impl<'ctx> TyCtx<'ctx> {
        fn new() -> Self {
            Self {
                arena: InternArena::new(),
            }
        }

        fn int(&'ctx self) -> Ty<'ctx> {
            self.arena.intern(TyKind::Int).unwrap()
        }

        fn arrow(&'ctx self, tys: Vec<Ty<'ctx>>) -> Ty<'ctx> {
            self.arena.intern(TyKind::Arrow(tys)).unwrap()
        }
    }

    #[test]
    fn test_identity_and_deduplication() {
        let arena = InternArena::new();
        let v1 = arena.intern(SimpleValue(42)).unwrap();
        let v2 = arena.intern(SimpleValue(42)).unwrap();
        let v3 = arena.intern(SimpleValue(100)).unwrap();
        assert_eq!(v1, v2);
        assert!(std::ptr::eq(&*v1, &*v2));
        assert_ne!(v1, v3);
        assert!(!std::ptr::eq(&*v1, &*v3));
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn test_empty_vectors_deduplication() {
        let ctx = TyCtx::new();
        let arr1 = ctx.arrow(vec![]);
        let arr2 = ctx.arrow(vec![]);
        assert_eq!(arr1, arr2);
        assert!(std::ptr::eq(&*arr1, &*arr2));
        assert_eq!(ctx.arena.len(), 1);
    }

    #[test]
    fn test_recursive_structures() {
        let ctx = TyCtx::new();
        let int = ctx.int();

        let func1 = ctx.arrow(vec![int, int]);
        let func2 = ctx.arrow(vec![int, int]);
        assert_eq!(func1, func2);
        assert!(std::ptr::eq(&*func1, &*func2));

        let nested = ctx.arrow(vec![func1, int]);
        match *nested {
            TyKind::Arrow(ref args) => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], func1);
                assert_eq!(args[1], int);
            }
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_stress_reallocation() {
        let arena = InternArena::new();

        let mut refs = Vec::new();
        for i in 0..2000 {
            refs.push(arena.intern(SimpleValue(i)).unwrap());
        }

        let v0 = arena.intern(SimpleValue(0)).unwrap();
        assert_eq!(refs[0], v0);
        assert!(std::ptr::eq(&*refs[0], &*v0));
    }
}
