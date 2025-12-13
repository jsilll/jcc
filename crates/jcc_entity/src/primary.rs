use std::marker::PhantomData;

use crate::EntityRef;

/// A primary mapping `K -> V` allocating dense entity references.
///
/// The `PrimaryMap` data structure uses the dense index space to implement a map with a vector.
///
/// A primary map contains the main definition of an entity, and it can be used to allocate new
/// entity references with the `push` method.
///
/// There should only be a single `PrimaryMap` instance for a given `EntityRef` type, otherwise
/// conflicting references will be created. Using unknown keys for indexing will cause a panic.
pub struct PrimaryMap<K: EntityRef, V> {
    data: Vec<V>,
    phantom: PhantomData<K>,
}

impl<K: EntityRef, V> Default for PrimaryMap<K, V> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            phantom: PhantomData,
        }
    }
}

impl<K: EntityRef, V> PrimaryMap<K, V> {
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
    /// This invalidates all previously returned `K` values.
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Adds a new entity to the map and returns its reference.
    #[inline]
    pub fn push(&mut self, value: V) -> K {
        let k = K::new(self.data.len());
        self.data.push(value);
        k
    }

    /// Returns a reference to the entity's data, if it exists.
    ///
    /// Returns `None` if the entity reference is out of bounds.
    #[inline]
    pub fn get(&self, entity: K) -> Option<&V> {
        self.data.get(entity.index())
    }

    /// Returns a mutable reference to the entity's data, if it exists.
    ///
    /// Returns `None` if the entity reference is out of bounds.
    #[inline]
    pub fn get_mut(&mut self, entity: K) -> Option<&mut V> {
        self.data.get_mut(entity.index())
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
    /// The iterator yields tuples of `(K, &V)`.
    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.data.iter().enumerate().map(|(i, v)| (K::new(i), v))
    }

    /// Returns a mutable iterator over all entities and their data.
    ///
    /// The iterator yields tuples of `(K, &mut V)`.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (K::new(i), v))
    }
}

impl<K: EntityRef, V> std::ops::Index<K> for PrimaryMap<K, V> {
    type Output = V;

    /// Returns a reference to the entity's data.
    ///
    /// # Panics
    ///
    /// Panics if the entity reference is out of bounds.
    #[inline]
    fn index(&self, entity: K) -> &Self::Output {
        &self.data[entity.index()]
    }
}

impl<K: EntityRef, V> std::ops::IndexMut<K> for PrimaryMap<K, V> {
    /// Returns a mutable reference to the entity's data.
    ///
    /// # Panics
    ///
    /// Panics if the entity reference is out of bounds.
    #[inline]
    fn index_mut(&mut self, entity: K) -> &mut Self::Output {
        &mut self.data[entity.index()]
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
