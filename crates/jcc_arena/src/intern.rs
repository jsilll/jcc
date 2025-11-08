use std::{
    cell::RefCell,
    collections::HashSet,
    hash::{Hash, Hasher},
};

use crate::{frozen::FrozenVec, Interned};

/// An arena allocator that deduplicates equal values (also known as interning).
///
/// An `InternArena` allocates values on the heap but ensures that equal values
/// share the same memory location. When you allocate a value that's equal to
/// a previously allocated value, you get back a reference to the existing value
/// instead of allocating a new one.
///
/// References remain valid for the lifetime of the arena, and values are never
/// moved or deallocated until the entire arena is dropped.
///
/// # Requirements
///
/// The type `T` must implement `Eq` and `Hash` to enable deduplication.
///
/// # Implementation Notes
///
/// Internally uses a `HashSet` of pointers to track allocated values and a
/// `FrozenVec` to store the actual data. The HashSet uses `RefCell` for
/// interior mutability, allowing deduplication checks even through shared
/// references.
pub struct InternArena<T> {
    vec: FrozenVec<Box<T>>,
    set: RefCell<HashSet<InternedPtr<T>>>,
}

impl<T> Default for InternArena<T>
where
    T: Eq + Hash,
{
    /// Creates an empty interning arena.
    ///
    /// This is equivalent to calling `InternArena::new()`.
    fn default() -> Self {
        Self::new()
    }
}

impl<'arena, T> InternArena<T>
where
    T: Eq + Hash,
{
    /// Creates a new empty interning arena.
    pub fn new() -> Self {
        InternArena {
            vec: FrozenVec::new(),
            set: RefCell::new(HashSet::new()),
        }
    }

    /// Creates a new empty interning arena with the specified capacity.
    ///
    /// The arena will be able to hold at least `capacity` unique elements
    /// without reallocating. If `capacity` is 0, the arena will not allocate.
    pub fn with_capacity(capacity: usize) -> Self {
        InternArena {
            vec: FrozenVec::with_capacity(capacity),
            set: RefCell::new(HashSet::with_capacity(capacity)),
        }
    }

    /// Returns the number of unique elements currently allocated in the arena.
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Returns `true` if the arena contains no elements.
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// Returns the number of unique elements the arena can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    /// Reserves capacity for at least `additional` more unique elements.
    ///
    /// This may reserve more space to avoid frequent reallocations. After
    /// calling `reserve`, capacity will be greater than or equal to
    /// `self.len() + additional`.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    pub fn reserve(&self, additional: usize) {
        self.vec.reserve(additional);
        self.set.borrow_mut().reserve(additional);
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
    pub fn intern(&'arena self, v: T) -> Interned<'arena, T> {
        let mut set = self.set.borrow_mut();
        let ptr = InternedPtr(&v as *const T);
        if let Some(&InternedPtr(existing_ptr)) = set.get(&ptr) {
            // Safety: pointer came from FrozenVec, so it's valid for 'arena
            return Interned(unsafe { &*existing_ptr });
        }
        let reference = self.vec.push_get(Box::new(v));
        set.insert(InternedPtr(reference as *const T));
        Interned(reference)
    }
}

/// Wrapper type that implements Hash and Eq by dereferencing the pointer
struct InternedPtr<T>(*const T);

impl<T: Hash> Hash for InternedPtr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Safety: pointer is always valid (from FrozenVec)
        unsafe { (*self.0).hash(state) }
    }
}

impl<T: Eq> Eq for InternedPtr<T> {}
impl<T: PartialEq> PartialEq for InternedPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        // Safety: pointers are always valid (from FrozenVec)
        unsafe { *self.0 == *other.0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Simple test types
    #[derive(Debug, PartialEq, Eq, Hash)]
    struct SimpleValue(i32);

    pub type Ty<'ctx> = Interned<'ctx, TyKind<'ctx>>;

    #[derive(PartialEq, Eq, Hash)]
    pub enum TyKind<'ctx> {
        Void,
        Int,
        Long,
        Ptr(Ty<'ctx>),
        Func {
            ret: Ty<'ctx>,
            params: Vec<Ty<'ctx>>,
        },
    }

    // Helper context struct
    pub struct TyCtx<'ctx> {
        arena: InternArena<TyKind<'ctx>>,
    }

    impl<'ctx> TyCtx<'ctx> {
        pub fn new() -> Self {
            Self {
                arena: InternArena::new(),
            }
        }

        pub fn len(&self) -> usize {
            self.arena.len()
        }

        pub fn void(&'ctx self) -> Ty<'ctx> {
            self.arena.intern(TyKind::Void)
        }

        pub fn int(&'ctx self) -> Ty<'ctx> {
            self.arena.intern(TyKind::Int)
        }

        pub fn long(&'ctx self) -> Ty<'ctx> {
            self.arena.intern(TyKind::Long)
        }

        pub fn ptr(&'ctx self, pointee: Ty<'ctx>) -> Ty<'ctx> {
            self.arena.intern(TyKind::Ptr(pointee))
        }

        pub fn func(&'ctx self, ret: Ty<'ctx>, params: Vec<Ty<'ctx>>) -> Ty<'ctx> {
            self.arena.intern(TyKind::Func { ret, params })
        }
    }

    #[test]
    fn test_empty_arena() {
        let arena: InternArena<String> = InternArena::new();
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
    }

    #[test]
    fn test_default() {
        let arena: InternArena<SimpleValue> = InternArena::default();
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
    }

    #[test]
    fn test_basic_interning() {
        let arena = InternArena::new();
        let v1 = arena.intern(SimpleValue(42));
        let v2 = arena.intern(SimpleValue(42));
        let v3 = arena.intern(SimpleValue(43));

        // Same values should have same pointer
        assert!(v1 == v2);
        // Different values should have different pointers
        assert!(v1 != v3);

        // Should only have 2 unique values
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn test_string_interning() {
        let arena = InternArena::new();
        let s1 = arena.intern("hello".to_string());
        let s2 = arena.intern("hello".to_string());
        let s3 = arena.intern("world".to_string());

        assert!(s1 == s2);
        assert!(s1 != s3);
        assert_eq!(arena.len(), 2);
        assert_eq!(*s1, "hello");
        assert_eq!(*s3, "world");
    }

    #[test]
    fn test_arena_not_empty() {
        let arena = InternArena::new();
        arena.intern(SimpleValue(1));
        assert_eq!(arena.len(), 1);
        assert!(!arena.is_empty());
    }

    #[test]
    fn test_with_capacity() {
        let arena: InternArena<SimpleValue> = InternArena::with_capacity(10);
        assert_eq!(arena.len(), 0);
        assert!(arena.capacity() >= 10);
    }

    #[test]
    fn test_reserve() {
        let arena: InternArena<SimpleValue> = InternArena::new();
        let initial_capacity = arena.capacity();
        arena.reserve(100);
        assert!(arena.capacity() >= initial_capacity + 100);
    }

    #[test]
    fn test_multiple_interned_references() {
        let arena = InternArena::new();
        let v1 = arena.intern(SimpleValue(1));
        let v2 = arena.intern(SimpleValue(2));
        let v3 = arena.intern(SimpleValue(1)); // Same as v1

        // Can hold multiple references at once
        assert_eq!((*v1).0, 1);
        assert_eq!((*v2).0, 2);
        assert_eq!((*v3).0, 1);
        assert!(v1 == v3);
    }

    #[test]
    fn test_ty_basic_types() {
        let ctx = TyCtx::new();

        let void1 = ctx.void();
        let void2 = ctx.void();
        let int1 = ctx.int();
        let int2 = ctx.int();

        // Same types should be interned to same pointer
        assert!(void1 == void2);
        assert!(int1 == int2);
        assert!(void1 != int1);

        // Should only have 2 unique types
        assert_eq!(ctx.len(), 2);
    }

    #[test]
    fn test_ty_pointer_types() {
        let ctx = TyCtx::new();

        let int = ctx.int();
        let ptr_int1 = ctx.ptr(int);
        let ptr_int2 = ctx.ptr(int);

        // Same pointer types should be interned
        assert!(ptr_int1 == ptr_int2);

        let long = ctx.long();
        let ptr_long = ctx.ptr(long);

        // Different pointer types should not be interned
        assert!(ptr_int1 != ptr_long);

        // Should have: int, long, ptr(int), ptr(long)
        assert_eq!(ctx.len(), 4);
    }

    #[test]
    fn test_ty_nested_pointers() {
        let ctx = TyCtx::new();

        let int = ctx.int();
        let ptr_int = ctx.ptr(int);
        let ptr_ptr_int1 = ctx.ptr(ptr_int);
        let ptr_ptr_int2 = ctx.ptr(ptr_int);

        // Should deduplicate nested pointers
        assert!(ptr_ptr_int1 == ptr_ptr_int2);

        // Should have: int, ptr(int), ptr(ptr(int))
        assert_eq!(ctx.len(), 3);
    }

    #[test]
    fn test_ty_function_types() {
        let ctx = TyCtx::new();

        let void = ctx.void();
        let int = ctx.int();

        let func1 = ctx.func(void, vec![]);
        let func2 = ctx.func(void, vec![]);

        // Same function types should be interned
        assert!(func1 == func2);

        let func3 = ctx.func(int, vec![]);

        // Different return types
        assert!(func1 != func3);

        let func4 = ctx.func(void, vec![int]);

        // Different parameters
        assert!(func1 != func4);
    }

    #[test]
    fn test_ty_complex_function_types() {
        let ctx = TyCtx::new();

        let int = ctx.int();
        let long = ctx.long();
        let ptr_int = ctx.ptr(int);

        let func1 = ctx.func(int, vec![long, ptr_int]);
        let func2 = ctx.func(int, vec![long, ptr_int]);

        // Should deduplicate complex function types
        assert!(func1 == func2);

        // Different parameter order should create different type
        let func3 = ctx.func(int, vec![ptr_int, long]);
        assert!(func1 != func3);
    }

    #[test]
    fn test_ty_function_returning_function() {
        let ctx = TyCtx::new();

        let int = ctx.int();
        let void = ctx.void();

        // int -> void
        let inner_func = ctx.func(void, vec![int]);

        // () -> (int -> void)
        let outer_func1 = ctx.func(inner_func, vec![]);
        let outer_func2 = ctx.func(inner_func, vec![]);

        assert!(outer_func1 == outer_func2);
    }

    #[test]
    fn test_ty_structural_equality() {
        let ctx = TyCtx::new();

        // Create int -> int two different ways
        let int1 = ctx.int();
        let func1 = ctx.func(int1, vec![int1]);

        let int2 = ctx.int(); // This should be same as int1
        let func2 = ctx.func(int2, vec![int2]);

        // Should be same because int1 and int2 are interned to same pointer
        assert!(func1 == func2);
    }

    #[test]
    fn test_many_types() {
        let ctx = TyCtx::new();

        let int = ctx.int();

        // Create 1000 ptr(int) - should all intern to same value
        for _ in 0..1000 {
            let ptr_int = ctx.ptr(int);
            assert!(ptr_int == ctx.ptr(int));
        }

        // Should only have 2 types: int and ptr(int)
        assert_eq!(ctx.len(), 2);
    }

    #[test]
    fn test_vec_in_function() {
        let ctx = TyCtx::new();

        let int = ctx.int();
        let long = ctx.long();

        // Create functions with Vec parameters
        let params1 = vec![int, long, int];
        let func1 = ctx.func(int, params1.clone());
        let func2 = ctx.func(int, params1);

        assert!(func1 == func2);
    }

    #[test]
    fn test_interning_order_independence() {
        let arena1 = InternArena::new();
        let v1 = arena1.intern(SimpleValue(1));
        let _ = arena1.intern(SimpleValue(2));
        let v1_again = arena1.intern(SimpleValue(1));

        assert!(v1 == v1_again);
        assert_eq!(arena1.len(), 2);

        let arena2 = InternArena::new();
        let v2_first = arena2.intern(SimpleValue(2));
        let _ = arena2.intern(SimpleValue(1));
        let v2_again = arena2.intern(SimpleValue(2));

        assert!(v2_first == v2_again);
        assert_eq!(arena2.len(), 2);
    }

    #[test]
    fn test_hash_collision_handling() {
        // Types with same hash but different values
        #[derive(Debug, PartialEq, Eq)]
        struct BadHash(i32);

        impl std::hash::Hash for BadHash {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                // Always return same hash (terrible hash function)
                0.hash(state);
            }
        }

        let arena = InternArena::new();
        let v1 = arena.intern(BadHash(1));
        let v2 = arena.intern(BadHash(2));
        let v1_again = arena.intern(BadHash(1));

        // Should still work correctly despite hash collisions
        assert!(v1 == v1_again);
        assert!(v1 != v2);
        assert_eq!(arena.len(), 2);
    }
}
