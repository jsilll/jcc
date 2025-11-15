//! Type-safe entity management for compiler data structures.
//!
//! This module provides a generic entity reference system that prevents mixing
//! different entity types at compile time. This is particularly useful for compiler
//! IR where you have multiple interrelated entity types (instructions, blocks,
//! functions, etc.) that should not be confused.

mod list;
pub use list::{EntityList, ListPool};

use std::{marker::PhantomData, num::NonZeroU32};

/// A type-safe entity reference that wraps a non-zero index.
///
/// This type uses a `PhantomData` marker to ensure that entity references
/// of different types cannot be mixed at compile time. For example, an
/// `EntityRef<Block>` cannot be used where an `EntityRef<Inst>` is expected.
///
/// The internal representation uses `NonZeroU32`, which enables niche
/// optimization for `Option<EntityRef<T>>` (making it the same size as
/// `EntityRef<T>`).
///
/// # Indexing
///
/// The internal index is 1-based (starts at 1) to allow the use of `NonZeroU32`.
/// When converted to `usize` for array indexing, it's automatically adjusted
/// to be 0-based via the `as_usize()` method.
#[derive(Debug)]
pub struct EntityRef<T> {
    index: NonZeroU32,
    phantom: PhantomData<T>,
}

impl<T> Copy for EntityRef<T> {}
impl<T> Clone for EntityRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Eq for EntityRef<T> {}
impl<T> PartialEq for EntityRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> std::hash::Hash for EntityRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}

impl<T> EntityRef<T> {
    /// Creates a new entity reference from a 1-based index.
    ///
    /// Returns `None` if the index is 0, since entity references must be non-zero.
    #[inline]
    pub fn new(index: u32) -> Option<Self> {
        NonZeroU32::new(index).map(|index| Self {
            index,
            phantom: PhantomData,
        })
    }

    /// Creates a new entity reference from a 1-based index.
    ///
    /// Returns `None` if the index is 0, since entity references must be non-zero.
    ///
    /// # Safety
    ///
    /// This passed `index` must not be zero.
    #[inline]
    pub unsafe fn new_unchecked(index: u32) -> Self {
        Self {
            phantom: PhantomData,
            index: NonZeroU32::new_unchecked(index),
        }
    }

    /// Returns the 1-based index of this entity reference.
    ///
    /// This is the raw index value stored internally. For array indexing,
    /// use `as_usize()` instead.
    #[inline]
    pub fn index(&self) -> u32 {
        self.index.get()
    }

    /// Converts this entity reference to a 0-based array index.
    ///
    /// This method subtracts 1 from the internal 1-based index to make it
    /// suitable for array indexing.
    #[inline]
    pub fn as_usize(&self) -> usize {
        (self.index.get() - 1) as usize
    }
}

/// Primary storage map for entities.
///
/// This is the canonical owner of entity data. Each entity is assigned a unique
/// `EntityRef<K>` when inserted, which can then be used to retrieve the data.
///
/// The type parameter `K` is a marker type that prevents mixing entity references
/// of different kinds, while `V` is the actual data type being stored.
#[derive(Debug)]
pub struct PrimaryMap<K, V> {
    data: Vec<V>,
    phantom: PhantomData<K>,
}

impl<K, V> Default for PrimaryMap<K, V> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            phantom: PhantomData,
        }
    }
}

impl<K, V> PrimaryMap<K, V> {
    /// Creates a new, empty `PrimaryMap`.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new `PrimaryMap` with the specified capacity.
    ///
    /// The map will be able to hold at least `capacity` elements without
    /// reallocating.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            phantom: PhantomData,
            data: Vec::with_capacity(capacity),
        }
    }

    /// Returns the number of entities in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the map contains no entities.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the capacity of the map.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    /// Reserves capacity for at least `additional` more elements.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.data.reserve(additional);
    }

    /// Clears the map, removing all entities.
    ///
    /// This invalidates all previously returned `EntityRef<K>` values.
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Adds a new entity to the map and returns its reference.
    ///
    /// # Panics
    ///
    /// Panics if the number of entities would exceed `u32::MAX - 1`, as entity
    /// references use `NonZeroU32` internally and thus can only represent
    /// indices from 1 to `u32::MAX`.
    #[inline]
    pub fn push(&mut self, value: V) -> EntityRef<K> {
        self.data.push(value);
        // Safe: len() is at least 1 after push, and we panic if it would overflow
        let index = self
            .data
            .len()
            .try_into()
            .expect("entity count exceeded u32::MAX");
        unsafe { EntityRef::new_unchecked(index) }
    }

    /// Returns a reference to the entity's data, if it exists.
    ///
    /// Returns `None` if the entity reference is out of bounds.
    #[inline]
    pub fn get(&self, entity: EntityRef<K>) -> Option<&V> {
        self.data.get(entity.as_usize())
    }

    /// Returns a mutable reference to the entity's data, if it exists.
    ///
    /// Returns `None` if the entity reference is out of bounds.
    #[inline]
    pub fn get_mut(&mut self, entity: EntityRef<K>) -> Option<&mut V> {
        self.data.get_mut(entity.as_usize())
    }

    /// Returns an iterator over just the values, without entity references.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.iter()
    }

    /// Returns a mutable iterator over just the values, without entity references.
    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.data.iter_mut()
    }

    /// Returns an iterator over all entities and their data.
    ///
    /// The iterator yields tuples of `(EntityRef<K>, &V)`.
    pub fn iter(&self) -> impl Iterator<Item = (EntityRef<K>, &V)> {
        self.data.iter().enumerate().map(|(i, v)| {
            // Safe: i + 1 is always in range [1, len],
            // and len fits in u32 or we would have panicked
            let entity = unsafe { EntityRef::new_unchecked((i + 1) as u32) };
            (entity, v)
        })
    }

    /// Returns a mutable iterator over all entities and their data.
    ///
    /// The iterator yields tuples of `(EntityRef<K>, &mut V)`.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (EntityRef<K>, &mut V)> {
        self.data.iter_mut().enumerate().map(|(i, v)| {
            // Safe: i + 1 is always in range [1, len],
            // and len fits in u32 or we would have panicked
            let entity = unsafe { EntityRef::new_unchecked((i + 1) as u32) };
            (entity, v)
        })
    }
}

impl<K, V> std::ops::Index<EntityRef<K>> for PrimaryMap<K, V> {
    type Output = V;

    /// Returns a reference to the entity's data.
    ///
    /// # Panics
    ///
    /// Panics if the entity reference is out of bounds.
    #[inline]
    fn index(&self, entity: EntityRef<K>) -> &Self::Output {
        &self.data[entity.as_usize()]
    }
}

impl<K, V> std::ops::IndexMut<EntityRef<K>> for PrimaryMap<K, V> {
    /// Returns a mutable reference to the entity's data.
    ///
    /// # Panics
    ///
    /// Panics if the entity reference is out of bounds.
    #[inline]
    fn index_mut(&mut self, entity: EntityRef<K>) -> &mut Self::Output {
        &mut self.data[entity.as_usize()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct TestEntity;

    #[test]
    fn option_size_optimization() {
        use std::mem::size_of;

        assert_eq!(
            size_of::<EntityRef<TestEntity>>(),
            size_of::<Option<EntityRef<TestEntity>>>()
        );
    }

    #[test]
    fn entity_ref_new() {
        let entity = EntityRef::<TestEntity>::new(1).unwrap();
        assert_eq!(entity.index(), 1);
        assert_eq!(entity.as_usize(), 0);

        assert!(EntityRef::<TestEntity>::new(0).is_none());
    }

    #[test]
    fn primary_map_basic_operations() {
        let mut map = PrimaryMap::<TestEntity, String>::new();
        assert!(map.is_empty());

        let e1 = map.push("first".to_string());
        let e2 = map.push("second".to_string());

        assert_eq!(map.len(), 2);
        assert_eq!(map.get(e1), Some(&"first".to_string()));
        assert_eq!(map.get(e2), Some(&"second".to_string()));
    }

    #[test]
    fn primary_map_indexing() {
        let mut map = PrimaryMap::<TestEntity, i32>::new();
        let e1 = map.push(42);
        let e2 = map.push(100);

        assert_eq!(map[e1], 42);
        assert_eq!(map[e2], 100);

        map[e1] = 84;
        assert_eq!(map[e1], 84);
    }

    #[test]
    fn primary_map_iteration() {
        let mut map = PrimaryMap::<TestEntity, i32>::new();
        let e1 = map.push(1);
        let e2 = map.push(2);
        let e3 = map.push(3);

        let collected: Vec<_> = map.iter().collect();
        assert_eq!(collected.len(), 3);
        assert_eq!(collected[0], (e1, &1));
        assert_eq!(collected[1], (e2, &2));
        assert_eq!(collected[2], (e3, &3));
    }
}
