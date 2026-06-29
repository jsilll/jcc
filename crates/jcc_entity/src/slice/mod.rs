pub mod bucket;

use crate::EntityRef;

use std::marker::PhantomData;

/// A handle to a slice of entities stored in a `SlicePool`.
#[derive(Debug)]
pub struct EntitySlice<T>(u32, PhantomData<T>);

impl<T> Default for EntitySlice<T> {
    fn default() -> Self {
        Self(0, PhantomData)
    }
}

impl<T> Copy for EntitySlice<T> {}

impl<T> Clone for EntitySlice<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Eq for EntitySlice<T> {}

impl<T> PartialEq for EntitySlice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> std::hash::Hash for EntitySlice<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> EntitySlice<T> {
    /// Creates an empty entity slice.
    #[inline]
    pub fn empty() -> Self {
        Self::default()
    }
}

impl<T: EntityRef> jcc_identity::IdentityHashable for EntitySlice<T> {}

impl<T: EntityRef> EntityRef for EntitySlice<T> {
    #[inline]
    fn new(index: usize) -> Self {
        debug_assert!(index < (u32::MAX as usize));
        Self(u32::try_from(index).unwrap_or(0), PhantomData)
    }

    #[inline]
    fn index(self) -> usize {
        self.0 as usize
    }
}

/// A pool of packed, immutable slices.
///
/// Each slice is laid out in `data` as:
///
/// ```text
/// [ length_as_T | elem0 | elem1 | ... | elemN-1 ]
///   ^block_start  ^index (what EntitySlice stores)
/// ```
pub struct SlicePool<T> {
    data: Vec<T>,
}

impl<T> Default for SlicePool<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl<T: EntityRef> SlicePool<T> {
    /// Creates a new, empty slice pool.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new, empty slice pool with the specified capacity.
    ///
    /// The pool will be able to hold at least `capacity` elements
    /// total across all slices without reallocating.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Returns `true` if the pool contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the capacity of the pool.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    /// Returns the length of the slice, or 0 for the empty handle.
    #[inline]
    pub fn len_of(&self, slice: EntitySlice<T>) -> usize {
        self.try_len_of(slice).unwrap_or(0)
    }

    /// Read the length stored just before the slice elements, or `None` for the empty handle.
    #[inline]
    fn try_len_of(&self, slice: EntitySlice<T>) -> Option<usize> {
        self.data
            .get(slice.index().wrapping_sub(1))
            .map(|v| v.index())
    }

    /// Reserves capacity for at least `additional` more elements.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.data.reserve(additional);
    }

    /// Clears the pool, removing all slices.
    ///
    /// This invalidates all previously returned `EntitySlice<T>` handles.
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Returns a reference to the slice's elements.
    #[inline]
    pub fn get(&self, slice: EntitySlice<T>) -> &[T] {
        match self.try_len_of(slice) {
            None => &[],
            Some(len) => {
                let start = slice.index();
                &self.data[start..start + len]
            }
        }
    }

    /// Returns a mutable reference to the slice's elements.
    #[inline]
    pub fn get_mut(&mut self, slice: EntitySlice<T>) -> &mut [T] {
        match self.try_len_of(slice) {
            None => &mut [],
            Some(len) => {
                let start = slice.index();
                &mut self.data[start..start + len]
            }
        }
    }

    /// Allocate a new slice from an iterator, returning a handle to it.
    ///
    /// Layout written: `[T::from_u32(n), elem0, ..., elemN-1]`
    pub fn extend(&mut self, elements: impl IntoIterator<Item = T>) -> EntitySlice<T> {
        let block_start = self.data.len();
        self.data.push(T::new(0));
        let elems_start = self.data.len();
        self.data.extend(elements);
        match self.data.len() - elems_start {
            0 => {
                self.data.pop();
                EntitySlice::empty()
            }
            len => {
                self.data[block_start] = T::new(len);
                EntitySlice::new(elems_start)
            }
        }
    }
}

impl<T: EntityRef> std::ops::Index<EntitySlice<T>> for SlicePool<T> {
    type Output = [T];

    /// Returns a reference to the slice's elements.
    #[inline]
    fn index(&self, slice: EntitySlice<T>) -> &Self::Output {
        self.get(slice)
    }
}

impl<T: EntityRef> std::ops::IndexMut<EntitySlice<T>> for SlicePool<T> {
    /// Returns a mutable reference to the slice's elements.
    #[inline]
    fn index_mut(&mut self, slice: EntitySlice<T>) -> &mut Self::Output {
        self.get_mut(slice)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity_impl;

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    #[test]
    fn slice_pool_empty_iterator() {
        let mut pool = SlicePool::<TestEntity>::new();
        let empty = pool.extend([]);
        assert_eq!(pool.get(empty), &[]);
        assert_eq!(pool.len_of(empty), 0);
    }

    #[test]
    fn slice_pool_basic() {
        let mut pool = SlicePool::<TestEntity>::new();
        let e1 = TestEntity(1);
        let e2 = TestEntity(2);
        let e3 = TestEntity(3);

        let s1 = pool.extend([e1, e2]);
        let s2 = pool.extend([e3]);

        assert_eq!(pool.len_of(s1), 2);
        assert_eq!(pool.len_of(s2), 1);
        assert_eq!(pool.get(s1), &[e1, e2]);
        assert_eq!(pool.get(s2), &[e3]);
    }

    #[test]
    fn slice_pool_mutation() {
        let mut pool = SlicePool::<TestEntity>::new();
        let e1 = TestEntity(1);
        let e2 = TestEntity(2);
        let e3 = TestEntity(3);

        let s = pool.extend([e1, e2]);
        pool[s][0] = e3;

        assert_eq!(pool.get(s), &[e3, e2]);
    }

    #[test]
    fn slice_pool_multiple() {
        let mut pool = SlicePool::<TestEntity>::new();
        let entities: Vec<_> = (1..=5).map(TestEntity).collect();

        let s1 = pool.extend(entities[0..1].iter().copied());
        let s2 = pool.extend(entities[1..3].iter().copied());
        let s3 = pool.extend(entities[3..5].iter().copied());

        assert_eq!(pool.get(s1), &entities[0..1]);
        assert_eq!(pool.get(s2), &entities[1..3]);
        assert_eq!(pool.get(s3), &entities[3..5]);
    }
}
