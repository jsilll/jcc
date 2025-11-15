use std::marker::PhantomData;

/// A handle to a variable-length list of entities stored in a `ListPool`.
///
/// This is a compact representation (just offset and length) that references
/// a slice of entities in a pool.
#[derive(Debug)]
pub struct EntityList<T> {
    offset: u32,
    length: u32,
    phantom: PhantomData<T>,
}

impl<T> Copy for EntityList<T> {}
impl<T> Clone for EntityList<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Eq for EntityList<T> {}
impl<T> PartialEq for EntityList<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset && self.length == other.length
    }
}

impl<T> std::hash::Hash for EntityList<T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.offset.hash(state);
        self.length.hash(state);
    }
}

impl<T> Default for EntityList<T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> EntityList<T> {
    /// Creates an empty entity list.
    ///
    /// This is a zero-cost constant that doesn't allocate.
    #[inline]
    pub const fn empty() -> Self {
        Self {
            offset: 0,
            length: 0,
            phantom: PhantomData,
        }
    }

    /// Returns the length of the list.
    #[inline]
    pub fn len(&self) -> usize {
        self.length as usize
    }

    /// Returns `true` if the list is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// Returns the offset into the pool's data array.
    ///
    /// This is for internal use only.
    #[inline]
    pub(crate) fn offset(&self) -> usize {
        self.offset as usize
    }
}

/// A storage pool for variable-length lists of entities.
///
/// This is useful for storing lists of child entities in AST nodes, such as:
/// - Statements in a block
/// - Arguments to a function call
/// - Fields in a struct
/// - Cases in a match expression
///
/// The pool stores all lists contiguously in a single vector, and hands out
/// `EntityList<T>` handles that reference slices within the pool.
///
/// # Memory Efficiency
///
/// Instead of each AST node having its own `Vec<EntityRef<T>>`, they can store
/// a single `EntityList<T>` (8 bytes) that references data in a shared pool.
#[derive(Debug)]
pub struct ListPool<T> {
    data: Vec<T>,
}

impl<T> Default for ListPool<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl<T: Copy> ListPool<T> {
    /// Creates a new, empty list pool.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new list pool with the specified capacity.
    ///
    /// The pool will be able to hold at least `capacity` elements total
    /// across all lists without reallocating.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Returns the total number of elements stored across all lists.
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

    /// Clears the pool, removing all lists.
    ///
    /// This invalidates all previously returned `EntityList<T>` handles.
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Allocates a new list from an iterator of elements.
    ///
    /// This is useful when you're building a list dynamically.
    pub fn extend(&mut self, elements: impl IntoIterator<Item = T>) -> EntityList<T> {
        let offset = self
            .data
            .len()
            .try_into()
            .expect("list pool size exceeded u32::MAX");

        let start_len = self.data.len();
        self.data.extend(elements);
        let length = (self.data.len() - start_len)
            .try_into()
            .expect("list length exceeded u32::MAX");

        if length == 0 {
            EntityList::empty()
        } else {
            EntityList {
                offset,
                length,
                phantom: PhantomData,
            }
        }
    }

    /// Returns a reference to the list's elements.
    ///
    /// # Panics
    ///
    /// Panics if the list handle is invalid (references out-of-bounds data).
    #[inline]
    pub fn get(&self, list: EntityList<T>) -> &[T] {
        if list.is_empty() {
            &[]
        } else {
            let start = list.offset();
            let end = start + list.len();
            &self.data[start..end]
        }
    }

    /// Returns a mutable reference to the list's elements.
    ///
    /// This allows in-place modification of list elements without reallocating.
    ///
    /// # Panics
    ///
    /// Panics if the list handle is invalid (references out-of-bounds data).
    #[inline]
    pub fn get_mut(&mut self, list: EntityList<T>) -> &mut [T] {
        if list.is_empty() {
            &mut []
        } else {
            let start = list.offset();
            let end = start + list.len();
            &mut self.data[start..end]
        }
    }
}

impl<T: Copy> std::ops::Index<EntityList<T>> for ListPool<T> {
    type Output = [T];

    /// Returns a reference to the list's elements.
    ///
    /// # Panics
    ///
    /// Panics if the list handle is invalid.
    #[inline]
    fn index(&self, list: EntityList<T>) -> &Self::Output {
        self.get(list)
    }
}

impl<T: Copy> std::ops::IndexMut<EntityList<T>> for ListPool<T> {
    /// Returns a mutable reference to the list's elements.
    ///
    /// # Panics
    ///
    /// Panics if the list handle is invalid.
    #[inline]
    fn index_mut(&mut self, list: EntityList<T>) -> &mut Self::Output {
        self.get_mut(list)
    }
}

#[cfg(test)]
mod list_tests {
    use crate::EntityRef;

    use super::*;

    #[derive(Debug)]
    struct TestEntity;

    #[test]
    fn entity_list_size() {
        use std::mem::size_of;
        assert_eq!(size_of::<EntityList<EntityRef<TestEntity>>>(), 8);
    }

    #[test]
    fn entity_list_empty() {
        let list = EntityList::<EntityRef<TestEntity>>::empty();
        assert!(list.is_empty());
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn list_pool_empty_list() {
        let mut pool = ListPool::<EntityRef<TestEntity>>::new();
        let empty = pool.extend([]);

        assert!(empty.is_empty());
        assert_eq!(pool.get(empty), &[]);
    }

    #[test]
    fn list_pool_basic_operations() {
        let mut pool = ListPool::<EntityRef<TestEntity>>::new();

        let e1 = EntityRef::new(1).unwrap();
        let e2 = EntityRef::new(2).unwrap();
        let e3 = EntityRef::new(3).unwrap();

        let list1 = pool.extend([e1, e2]);
        let list2 = pool.extend([e3]);

        assert_eq!(pool.get(list1), &[e1, e2]);
        assert_eq!(pool.get(list2), &[e3]);
        assert_eq!(list1.len(), 2);
        assert_eq!(list2.len(), 1);
    }

    #[test]
    fn list_pool_indexing() {
        let mut pool = ListPool::<EntityRef<TestEntity>>::new();

        let e1 = EntityRef::new(1).unwrap();
        let e2 = EntityRef::new(2).unwrap();

        let list = pool.extend([e1, e2]);

        assert_eq!(pool[list][0], e1);
        assert_eq!(pool[list][1], e2);

        pool[list][0] = e2;
        assert_eq!(pool[list][0], e2);
    }

    #[test]
    fn list_pool_multiple_lists() {
        let mut pool = ListPool::<EntityRef<TestEntity>>::new();

        let e1 = EntityRef::new(1).unwrap();
        let e2 = EntityRef::new(2).unwrap();
        let e3 = EntityRef::new(3).unwrap();

        let list1 = pool.extend([e1]);
        let list2 = pool.extend([e2, e3]);
        let list3 = pool.extend([e1, e2, e3]);

        assert_eq!(pool.get(list1), &[e1]);
        assert_eq!(pool.get(list2), &[e2, e3]);
        assert_eq!(pool.get(list3), &[e1, e2, e3]);
    }
}
