use std::marker::PhantomData;

/// A handle to a variable-length slice of entities stored in a `SlicePool`.
///
/// This is a compact representation that references a slice of entities in a pool.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct EntitySlice<T> {
    offset: u32,
    length: u32,
    marker: PhantomData<T>,
}

impl<T> Default for EntitySlice<T> {
    fn default() -> Self {
        Self {
            offset: 0,
            length: 0,
            marker: PhantomData,
        }
    }
}

impl<T> EntitySlice<T> {
    /// Creates an empty entity slice.
    ///
    /// This is a zero-cost constant that doesn't allocate.
    #[inline]
    pub fn empty() -> Self {
        Self::default()
    }

    /// Returns the length of the slice.
    #[inline]
    pub fn len(self) -> usize {
        self.length as usize
    }

    /// Returns `true` if the slice is empty.
    #[inline]
    pub fn is_empty(self) -> bool {
        self.length == 0
    }

    /// Returns the offset into the pool's data array.
    ///
    /// This is for internal use only.
    #[inline]
    pub(crate) fn offset(self) -> usize {
        self.offset as usize
    }
}

/// A storage pool for variable-length slices of entities, referenced by `EntitySlice<T>`.
///
/// This is useful for storing slices of entities in a compact way
/// without needing to allocate a separate `Vec` for each slice.
///
/// The pool stores all slices contiguously in a single vector, and hands out
/// `EntitySlice<T>` handles that reference slices within the pool.
///
/// # Memory Efficiency
///
/// Instead of each AST node having its own `Vec<EntityRef<T>>`, they can store
/// a single `EntitySlice<T>` (8 bytes) that references data in a shared pool.
pub struct SlicePool<T> {
    data: Vec<T>,
}

impl<T> Default for SlicePool<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl<T: Copy> SlicePool<T> {
    /// Creates a new, empty slice pool.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new, empty slice pool with the specified capacity.
    ///
    /// The pool will be able to hold at least `capacity` elements total
    /// across all slices without reallocating.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Returns the total number of elements stored across all slices.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
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
    ///
    /// # Panics
    ///
    /// Panics if the slice handle is invalid (references out-of-bounds data).
    #[inline]
    pub fn get(&self, slice: EntitySlice<T>) -> &[T] {
        if slice.is_empty() {
            &[]
        } else {
            let start = slice.offset();
            let end = start + slice.len();
            &self.data[start..end]
        }
    }

    /// Returns a mutable reference to the slice's elements.
    ///
    /// This allows in-place modification of slice elements without reallocating.
    ///
    /// # Panics
    ///
    /// Panics if the slice handle is invalid (references out-of-bounds data).
    #[inline]
    pub fn get_mut(&mut self, slice: EntitySlice<T>) -> &mut [T] {
        if slice.is_empty() {
            &mut []
        } else {
            let start = slice.offset();
            let end = start + slice.len();
            &mut self.data[start..end]
        }
    }

    /// Allocates a new slice from an iterator of elements.
    ///
    /// This is useful when you're building a slice dynamically.
    pub fn extend(&mut self, elements: impl IntoIterator<Item = T>) -> EntitySlice<T> {
        let start = self.data.len();
        let offset = start.try_into().unwrap_or(u32::MAX);
        self.data.extend(elements);
        let length = (self.data.len() - start).try_into().unwrap_or_default();
        if length == 0 {
            EntitySlice::empty()
        } else {
            EntitySlice {
                offset,
                length,
                marker: PhantomData,
            }
        }
    }
}

impl<T: Copy> std::ops::Index<EntitySlice<T>> for SlicePool<T> {
    type Output = [T];

    /// Returns a reference to the slice's elements.
    ///
    /// # Panics
    ///
    /// Panics if the slice handle is invalid.
    #[inline]
    fn index(&self, slice: EntitySlice<T>) -> &Self::Output {
        self.get(slice)
    }
}

impl<T: Copy> std::ops::IndexMut<EntitySlice<T>> for SlicePool<T> {
    /// Returns a mutable reference to the slice's elements.
    ///
    /// # Panics
    ///
    /// Panics if the slice handle is invalid.
    #[inline]
    fn index_mut(&mut self, slice: EntitySlice<T>) -> &mut Self::Output {
        self.get_mut(slice)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{entity_impl, EntityRef};

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    #[test]
    fn entity_slice_size() {
        use std::mem::size_of;
        assert_eq!(size_of::<EntitySlice<TestEntity>>(), 8);
    }

    #[test]
    fn entity_slice_empty() {
        let slice = EntitySlice::<TestEntity>::empty();
        assert!(slice.is_empty());
        assert_eq!(slice.len(), 0);
    }

    #[test]
    fn slice_pool_empty_slice() {
        let mut pool = SlicePool::<TestEntity>::new();
        let empty = pool.extend([]);

        assert!(empty.is_empty());
        assert_eq!(pool.get(empty), &[]);
    }

    #[test]
    fn slice_pool_basic_operations() {
        let mut pool = SlicePool::<TestEntity>::new();

        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);
        let e3 = TestEntity::new(3);

        let slice1 = pool.extend([e1, e2]);
        let slice2 = pool.extend([e3]);

        assert_eq!(pool.get(slice1), &[e1, e2]);
        assert_eq!(pool.get(slice2), &[e3]);
        assert_eq!(slice1.len(), 2);
        assert_eq!(slice2.len(), 1);
    }

    #[test]
    fn slice_pool_indexing() {
        let mut pool = SlicePool::<TestEntity>::new();

        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);

        let slice = pool.extend([e1, e2]);

        assert_eq!(pool[slice][0], e1);
        assert_eq!(pool[slice][1], e2);

        pool[slice][0] = e2;
        assert_eq!(pool[slice][0], e2);
    }

    #[test]
    fn slice_pool_multiple_slices() {
        let mut pool = SlicePool::<TestEntity>::new();

        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);
        let e3 = TestEntity::new(3);

        let slice1 = pool.extend([e1]);
        let slice2 = pool.extend([e2, e3]);
        let slice3 = pool.extend([e1, e2, e3]);

        assert_eq!(pool.get(slice1), &[e1]);
        assert_eq!(pool.get(slice2), &[e2, e3]);
        assert_eq!(pool.get(slice3), &[e1, e2, e3]);
    }
}
