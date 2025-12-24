//! A simple bump-style arena allocator with stable addresses.
//!
//! `Arena<T>` allocates values of type `T` into contiguous chunks that are
//! never individually freed. Once a value is allocated, its memory address
//! remains stable for the lifetime of the arena.
//!
//! ## Design
//!
//! - Allocation is append-only, there is no deallocation.
//! - Memory is stored in chunks (`Vec<T>`), which grow geometrically.
//! - Previously allocated chunks are never reallocated or freed until the
//!   arena itself is dropped.
//! - Allocation APIs return `&mut T` or `&mut [T]` with lifetimes tied to the arena.
//!
//! ## Safety
//!
//! This arena uses interior mutability (`RefCell`) and `unsafe` code to return
//! mutable references that outlive the internal borrow guards. This is sound
//! because:
//!
//! - Chunks are never deallocated while the arena lives.
//! - Moving a `Vec<T>` does not invalidate pointers to its heap allocation.
//! - No allocation API ever returns the same mutable reference twice.
//! - Chunks are never reallocated after becoming active.
//!
//! It is **not** thread-safe and does not attempt to reclaim memory early.

pub mod intern;

use std::cell::RefCell;

/// An arena allocator that provides stable mutable references to allocated values.
///
/// Values allocated into the arena live until the arena is dropped. Allocation
/// is fast and amortized O(1), with occasional chunk growth.
pub struct Arena<T> {
    chunks: RefCell<Chunks<T>>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> {
    /// Creates a new arena with a heuristic default capacity.
    ///
    /// The initial capacity is chosen based on the size of `T`, targeting
    /// roughly 1024 bytes per chunk. For zero-sized types, this defaults
    /// to a capacity of 1024 elements.
    pub fn new() -> Self {
        // Calculate a reasonable default capacity based on T's size.
        // For ZSTs, size_of is 0, se we default to 1024 items.
        let capacity = 1024 / std::cmp::max(1, size_of::<T>());
        Arena::with_capacity(capacity)
    }

    /// Creates a new arena with the given initial capacity.
    ///
    /// The capacity specifies how many elements the first chunk can hold
    /// before the arena grows.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            chunks: RefCell::new(Chunks {
                rest: Vec::new(),
                curr: Vec::with_capacity(capacity),
            }),
        }
    }

    /// Returns the total number of items currently allocated in the arena.
    ///
    /// This counts items across all chunks.
    #[inline]
    pub fn len(&self) -> usize {
        let chunks = self.chunks.borrow();
        let len: usize = chunks.rest.iter().map(Vec::len).sum();
        len + chunks.curr.len()
    }

    /// Returns `true` if the arena contains no elements across all chunks.
    pub fn is_empty(&self) -> bool {
        let chunks = self.chunks.borrow();
        chunks.curr.is_empty() && chunks.rest.is_empty()
    }

    /// Allocates a single value in the arena and returns a mutable reference to it.
    ///
    /// Returns `None` if allocation fails due to capacity growth overflow.
    ///
    /// The returned reference is guaranteed to remain valid for the lifetime
    /// of the arena.
    #[inline]
    pub fn alloc(&self, value: T) -> Option<&mut T> {
        match self.alloc_fast(value) {
            Ok(r) => Some(r),
            Err(value) => self.alloc_extend(std::iter::once(value)).map(|s| &mut s[0]),
        }
    }

    /// Attempts to allocate a value in the current chunk without growing.
    ///
    /// This is a fast path that avoids iterator handling and chunk management.
    ///
    /// Returns `Err(value)` if the current chunk is full.
    #[inline]
    fn alloc_fast(&self, value: T) -> Result<&mut T, T> {
        let mut chunks = self.chunks.borrow_mut();
        let len = chunks.curr.len();
        if len >= chunks.curr.capacity() {
            return Err(value);
        }

        chunks.curr.push(value);

        // Safety:
        //
        // 1. The pointer comes from `chunks.curr`, which is on the heap
        // 2. We just pushed the value, so index `len` is valid and initialized
        // 3. The returned reference lifetime is bound to 'self, no the RefCell guard.
        //    This is safe because:
        //    a. `chunks.curr` is only moved to `chunks.rest`, never deallocated before the arena
        //    drops.
        //    b. Moving a Vec does not invalidate pointers to its heap data.
        //    c. We never allow the Vec to re-allocate it's storage buffer.
        //    d. We never give out mutable references to the same slot twice.
        unsafe { Ok(&mut *chunks.curr.as_mut_ptr().add(len)) }
    }

    /// Allocates multiple values from an iterator and returns them as a slice.
    ///
    /// If the iterator's lower size bound exceeds the available space in the
    /// current chunk, a new chunk is allocated immediately.
    ///
    /// Returns `None` if capacity growth overflows.
    ///
    /// The returned slice contains exactly the elements produced by the iterator
    /// and remains valid for the lifetime of the arena.
    pub fn alloc_extend(&self, iter: impl IntoIterator<Item = T>) -> Option<&mut [T]> {
        let mut iter = iter.into_iter();
        let iter_min_len = iter.size_hint().0;

        let mut chunks = self.chunks.borrow_mut();
        let curr_free_slots = chunks.curr.capacity() - chunks.curr.len();

        // Heuristic: If we know for sure the iterator needs more space than currently available,
        // force a new chunk immediately. This avoids checking capacity in a loop for every item.
        let start = if iter_min_len > curr_free_slots {
            chunks.reserve(iter_min_len);
            chunks.curr.extend(iter);
            0
        } else {
            let mut start = chunks.curr.len();

            // We loop and push. If the iterator "lied" and overflows, we handle it.
            let mut pushed_count = 0;
            while let Some(elem) = iter.next() {
                if chunks.curr.len() < chunks.curr.capacity() {
                    chunks.curr.push(elem);
                    pushed_count += 1;
                } else {
                    let chunks = &mut *chunks;
                    chunks.reserve(pushed_count + 1);
                    let prev = chunks.rest.last_mut()?;
                    chunks.curr.extend(prev.drain(prev.len() - pushed_count..));
                    chunks.curr.push(elem);
                    chunks.curr.extend(iter);
                    start = 0;
                    break;
                }
            }

            start
        };

        // Safety:
        //
        // - The slice spans elements that were just initialized via `push` or `extend`.
        // - The backing storage is owned by the arena and will not be reallocated.
        unsafe {
            Some(std::slice::from_raw_parts_mut(
                chunks.curr.as_mut_ptr().add(start),
                chunks.curr.len() - start,
            ))
        }
    }
}

struct Chunks<T> {
    /// The currently active chunk.
    curr: Vec<T>,
    /// Previously filled chunks.
    rest: Vec<Vec<T>>,
}

impl<T> Chunks<T> {
    /// Grows the arena by allocating a new chunk.
    ///
    /// This method is cold and intentionally not inlined, as it represents
    /// the slow path for allocation.
    ///
    /// Returns `None` if capacity growth overflows.
    #[cold]
    #[inline(never)]
    fn reserve(&mut self, capacity: usize) -> Option<()> {
        let double = self.curr.capacity().checked_mul(2)?;
        let required = capacity.checked_next_power_of_two()?;
        let capacity = std::cmp::max(double, required);

        let chunk = std::mem::replace(&mut self.curr, Vec::with_capacity(capacity));
        self.rest.push(chunk);
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zst() {
        struct Zst;
        let arena = Arena::new();
        let _a = arena.alloc(Zst).unwrap();
        let _b = arena.alloc(Zst).unwrap();
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn test_basic_alloc() {
        let arena = Arena::new();
        let a = arena.alloc(10).unwrap();
        let b = arena.alloc(20).unwrap();
        *a += 1;
        assert_eq!(*a, 11);
        assert_eq!(*b, 20);
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn test_alloc_slice_basic() {
        let arena = Arena::new();
        let slice = arena.alloc_extend(vec![1, 2, 3]).unwrap();
        assert_eq!(slice, &[1, 2, 3]);
    }

    #[test]
    fn test_persistence_across_chunks() {
        let arena = Arena::with_capacity(2);
        let a = arena.alloc(1).unwrap();
        let b = arena.alloc(2).unwrap();
        let c = arena.alloc(3).unwrap();
        assert_eq!(*a, 1);
        assert_eq!(*b, 2);
        assert_eq!(*c, 3);
    }

    #[test]
    fn test_alloc_extend_overflow_handling() {
        struct LyingIter {
            val: i32,
            count: i32,
        }

        impl Iterator for LyingIter {
            type Item = i32;

            fn size_hint(&self) -> (usize, Option<usize>) {
                (0, None)
            }

            fn next(&mut self) -> Option<Self::Item> {
                if self.count < 3 {
                    self.count += 1;
                    self.val += 1;
                    Some(self.val)
                } else {
                    None
                }
            }
        }

        let arena = Arena::with_capacity(2);
        let a = arena.alloc(100).unwrap();
        let slice = arena
            .alloc_extend(LyingIter { val: 199, count: 0 })
            .unwrap();
        assert_eq!(slice, &[200, 201, 202]);
        assert_eq!(*a, 100);
    }
}
