use crate::{frozen::FrozenVec, ArenaRef};

/// A simple arena allocator that provides stable references to allocated values.
///
/// An `Arena` allocates values on the heap and returns references that remain
/// valid for the lifetime of the arena. Values are never moved or deallocated
/// until the entire arena is dropped, making it safe to hold multiple references
/// to different allocated values simultaneously.
///
/// # Implementation Notes
///
/// Internally, values are stored as `Box<T>` in a `FrozenVec`, which ensures
/// that once allocated, the memory location of each value never changes.
pub struct Arena<T> {
    vec: FrozenVec<Box<T>>,
}

impl<T> Default for Arena<T> {
    /// Creates an empty arena.
    ///
    /// This is equivalent to calling [`Arena::new()`].
    fn default() -> Self {
        Self::new()
    }
}

impl<'arena, T> Arena<T> {
    /// Creates a new empty arena.
    pub fn new() -> Self {
        Arena {
            vec: FrozenVec::new(),
        }
    }

    /// Creates a new empty arena with the specified capacity.
    ///
    /// The arena will be able to hold at least `capacity` elements without
    /// reallocating. If `capacity` is 0, the arena will not allocate.
    pub fn with_capacity(capacity: usize) -> Self {
        Arena {
            vec: FrozenVec::with_capacity(capacity),
        }
    }

    /// Returns the number of elements currently allocated in the arena.
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Returns `true` if the arena contains no elements.
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// Returns the number of elements the arena can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    /// Reserves capacity for at least `additional` more elements.
    ///
    /// This may reserve more space to avoid frequent reallocations. After
    /// calling `reserve`, capacity will be greater than or equal to
    /// `self.len() + additional`.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    pub fn reserve(&self, additional: usize) {
        self.vec.reserve(additional)
    }

    /// Allocates a value in the arena and returns a reference to it.
    ///
    /// The returned reference is valid for the lifetime of the arena itself,
    /// allowing multiple allocations to coexist without borrowing conflicts.
    #[inline]
    pub fn alloc(&'arena self, v: T) -> ArenaRef<'arena, T> {
        self.vec.push_get(Box::new(v))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let arena: Arena<i32> = Arena::default();
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
    }

    #[test]
    fn test_with_capacity() {
        let arena: Arena<i32> = Arena::with_capacity(10);
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
        assert!(arena.capacity() >= 10);
    }

    #[test]
    fn test_with_capacity_zero() {
        let arena: Arena<i32> = Arena::with_capacity(0);
        assert_eq!(arena.len(), 0);
        assert_eq!(arena.capacity(), 0);
    }

    #[test]
    fn test_new_arena_is_empty() {
        let arena: Arena<i32> = Arena::new();
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
    }

    #[test]
    fn test_alloc_single_value() {
        let arena = Arena::new();
        let x = arena.alloc(42);
        assert_eq!(*x, 42);
        assert_eq!(arena.len(), 1);
        assert!(!arena.is_empty());
    }

    #[test]
    fn test_alloc_multiple_values() {
        let arena = Arena::new();
        let x = arena.alloc(1);
        let y = arena.alloc(2);
        let z = arena.alloc(3);

        assert_eq!(*x, 1);
        assert_eq!(*y, 2);
        assert_eq!(*z, 3);
        assert_eq!(arena.len(), 3);
    }

    #[test]
    fn test_references_remain_valid() {
        let arena = Arena::new();
        let x = arena.alloc(100);
        let y = arena.alloc(200);
        let z = arena.alloc(300);

        // All references should remain valid simultaneously
        assert_eq!(*x + *y + *z, 600);
    }

    #[test]
    fn test_alloc_string() {
        let arena = Arena::new();
        let s1 = arena.alloc(String::from("hello"));
        let s2 = arena.alloc(String::from("world"));

        assert_eq!(s1.as_str(), "hello");
        assert_eq!(s2.as_str(), "world");
    }

    #[test]
    fn test_alloc_complex_type() {
        #[derive(Debug, PartialEq)]
        struct Point {
            x: i32,
            y: i32,
        }

        let arena = Arena::new();
        let p1 = arena.alloc(Point { x: 1, y: 2 });
        let p2 = arena.alloc(Point { x: 3, y: 4 });

        assert_eq!(p1.x, 1);
        assert_eq!(p1.y, 2);
        assert_eq!(p2.x, 3);
        assert_eq!(p2.y, 4);
    }

    #[test]
    fn test_reserve() {
        let arena: Arena<i32> = Arena::new();
        let initial_capacity = arena.capacity();

        arena.reserve(100);
        assert!(arena.capacity() >= initial_capacity + 100);
        assert_eq!(arena.len(), 0);
    }

    #[test]
    fn test_reserve_after_alloc() {
        let arena = Arena::new();
        arena.alloc(1);
        arena.alloc(2);

        let len_before = arena.len();
        arena.reserve(50);

        assert_eq!(arena.len(), len_before);
        assert!(arena.capacity() >= len_before + 50);
    }

    #[test]
    fn test_many_allocations() {
        let arena = Arena::new();
        let mut refs = Vec::new();

        for i in 0..1000 {
            refs.push(arena.alloc(i));
        }

        assert_eq!(arena.len(), 1000);

        // Verify all references are still valid and correct
        for (i, &val) in refs.iter().enumerate() {
            assert_eq!(*val, i);
        }
    }

    #[test]
    fn test_alloc_with_preallocated_capacity() {
        let arena = Arena::with_capacity(5);
        let initial_capacity = arena.capacity();

        for i in 0..5 {
            arena.alloc(i);
        }

        // Should not have reallocated
        assert_eq!(arena.capacity(), initial_capacity);
        assert_eq!(arena.len(), 5);
    }

    #[test]
    fn test_zero_sized_type() {
        struct Zst;

        let arena = Arena::new();
        let z1 = arena.alloc(Zst);
        let z2 = arena.alloc(Zst);

        assert_eq!(arena.len(), 2);

        // Can't do much with ZSTs, but we can verify they exist
        let _: &Zst = z1;
        let _: &Zst = z2;
    }

    #[test]
    fn test_references_can_coexist() {
        let arena = Arena::new();
        let a = arena.alloc(1);
        let b = arena.alloc(2);
        let c = arena.alloc(3);

        // This demonstrates that multiple immutable references
        // can exist simultaneously without borrow checker issues
        let sum = *a + *b + *c;
        assert_eq!(sum, 6);

        // References are still valid here
        assert_eq!(*a, 1);
        assert_eq!(*b, 2);
        assert_eq!(*c, 3);
    }

    #[test]
    fn test_alloc_returns_stable_references() {
        let arena = Arena::with_capacity(2);
        let x = arena.alloc(10);
        let x_ptr = x as *const i32;

        // Allocate more items
        arena.alloc(20);
        arena.alloc(30);

        // Original reference should still point to the same memory
        assert_eq!(x_ptr, x as *const i32);
        assert_eq!(*x, 10);
    }

    #[test]
    fn test_alloc_large_values() {
        let arena = Arena::new();
        let large_array = [0u8; 1024];
        let allocated = arena.alloc(large_array);

        assert_eq!(allocated.len(), 1024);
        assert_eq!(arena.len(), 1);
    }

    #[test]
    fn test_drop_count() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;

        let drop_count = Arc::new(AtomicUsize::new(0));

        struct DropCounter {
            count: Arc<AtomicUsize>,
        }

        impl Drop for DropCounter {
            fn drop(&mut self) {
                self.count.fetch_add(1, Ordering::SeqCst);
            }
        }

        {
            let arena = Arena::new();
            arena.alloc(DropCounter {
                count: drop_count.clone(),
            });
            arena.alloc(DropCounter {
                count: drop_count.clone(),
            });
            arena.alloc(DropCounter {
                count: drop_count.clone(),
            });

            assert_eq!(drop_count.load(Ordering::SeqCst), 0);
        }

        // All values should be dropped when arena is dropped
        assert_eq!(drop_count.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn test_alloc_with_different_types_in_separate_arenas() {
        let int_arena = Arena::new();
        let string_arena = Arena::new();

        let num = int_arena.alloc(42);
        let text = string_arena.alloc(String::from("test"));

        assert_eq!(*num, 42);
        assert_eq!(text.as_str(), "test");
    }

    #[test]
    fn test_capacity_growth() {
        let arena: Arena<i32> = Arena::new();
        let initial_capacity = arena.capacity();

        // Allocate enough to trigger reallocation
        for i in 0..100 {
            arena.alloc(i);
        }

        assert!(arena.capacity() > initial_capacity);
        assert_eq!(arena.len(), 100);
    }

    #[test]
    fn test_alloc_option() {
        let arena = Arena::new();
        let some_val = arena.alloc(Some(42));
        let none_val = arena.alloc(None::<i32>);

        assert_eq!(*some_val, Some(42));
        assert_eq!(*none_val, None);
    }

    #[test]
    fn test_alloc_result() {
        let arena = Arena::new();
        let ok_val = arena.alloc(Ok::<i32, String>(42));
        let err_val = arena.alloc(Err::<i32, String>(String::from("error")));

        assert_eq!(*ok_val, Ok(42));
        assert!(err_val.is_err());
    }

    #[test]
    fn test_alloc_nested_struct() {
        struct Inner {
            value: i32,
        }

        struct Outer {
            inner: Inner,
            name: String,
        }

        let arena = Arena::new();
        let outer = arena.alloc(Outer {
            inner: Inner { value: 42 },
            name: String::from("test"),
        });

        assert_eq!(outer.inner.value, 42);
        assert_eq!(outer.name, "test");
    }
}
