use crate::EntityRef;

use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
    slice,
};

/// A mapping K -> V for densely indexed entity references.
///
/// The `SecondaryMap` data structure uses the dense index space to implement a map with a vector.
/// Unlike `PrimaryMap`, a `SecondaryMap` can't be used to allocate entity references. It is used
/// to associate secondary information with entities.
///
/// The map does not track if an entry for a key has been inserted or not. Instead it behaves
/// as if all keys have a default entry from the beginning.
#[derive(Clone)]
pub struct SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    default: V,
    data: Vec<V>,
    phantom: PhantomData<K>,
}

impl<K, V> SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    /// Create a new, empty map with the specified capacity.
    ///
    /// The map will be able to hold exactly `capacity` elements without reallocating.
    pub fn with_capacity(capacity: usize) -> Self
    where
        V: Default,
    {
        Self {
            phantom: PhantomData,
            default: V::default(),
            data: Vec::with_capacity(capacity),
        }
    }

    /// Create a new empty map with a specified default value.
    ///
    /// This constructor does not require V to implement Default.
    pub fn with_default(default: V) -> Self {
        Self {
            default,
            data: Vec::new(),
            phantom: PhantomData,
        }
    }

    /// Get the number of elements currently stored in the map.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if the map is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the number of elements the map can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    /// Remove all elements from the map.
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Resize the map to have `n` entries by adding default values as needed.
    fn resize(&mut self, n: usize) {
        self.data.resize(n, self.default.clone());
    }

    /// Iterate over all the keys in this map.
    pub fn keys(&self) -> Keys<K> {
        Keys {
            index: 0,
            phantom: PhantomData,
            len: self.data.len(),
        }
    }

    /// Iterate over all the values in this map.
    pub fn values(&self) -> slice::Iter<V> {
        self.data.iter()
    }

    /// Iterate over all the values in this map, mutable edition.
    pub fn values_mut(&mut self) -> slice::IterMut<V> {
        self.data.iter_mut()
    }

    /// Iterate over all the keys and values in this map.
    pub fn iter(&self) -> Iter<K, V> {
        Iter {
            index: 0,
            elems: &self.data,
            phantom: PhantomData,
        }
    }

    /// Iterate over all the keys and values in this map, mutable edition.
    pub fn iter_mut(&mut self) -> IterMut<K, V> {
        IterMut {
            index: 0,
            phantom: PhantomData,
            elems: self.data.iter_mut(),
        }
    }
}

impl<K, V> Default for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone + Default,
{
    fn default() -> Self {
        Self {
            data: Vec::new(),
            default: V::default(),
            phantom: PhantomData,
        }
    }
}

/// Immutable indexing into a `SecondaryMap`.
///
/// All keys are permitted. Untouched entries have the default value.
impl<K, V> Index<K> for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    type Output = V;

    fn index(&self, key: K) -> &V {
        let index = key.index();
        self.data.get(index).unwrap_or(&self.default)
    }
}

/// Mutable indexing into a `SecondaryMap`.
///
/// The map grows as needed to accommodate new keys.
impl<K, V> IndexMut<K> for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    fn index_mut(&mut self, key: K) -> &mut V {
        let index = key.index();
        if index >= self.data.len() {
            self.resize(index + 1);
        }
        &mut self.data[index]
    }
}

impl<K, V> FromIterator<(K, V)> for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone + Default,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (K, V)>,
    {
        let mut map = Self::default();
        for (key, value) in iter {
            map[key] = value;
        }
        map
    }
}

pub struct Keys<K> {
    len: usize,
    index: usize,
    phantom: PhantomData<K>,
}

impl<K: EntityRef> ExactSizeIterator for Keys<K> {}

impl<K: EntityRef> Iterator for Keys<K> {
    type Item = K;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.len {
            let key = K::new(self.index);
            self.index += 1;
            Some(key)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.len - self.index;
        (remaining, Some(remaining))
    }
}

pub struct Iter<'a, K, V> {
    index: usize,
    elems: &'a [V],
    phantom: PhantomData<K>,
}

impl<'a, K, V> ExactSizeIterator for Iter<'a, K, V>
where
    K: EntityRef,
    V: Clone,
{
}

impl<'a, K, V> Iterator for Iter<'a, K, V>
where
    K: EntityRef,
    V: Clone,
{
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.elems.len() {
            let key = K::new(self.index);
            let value = &self.elems[self.index];
            self.index += 1;
            Some((key, value))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.elems.len() - self.index;
        (remaining, Some(remaining))
    }
}

pub struct IterMut<'a, K, V> {
    index: usize,
    elems: slice::IterMut<'a, V>,
    phantom: PhantomData<K>,
}

impl<'a, K, V> ExactSizeIterator for IterMut<'a, K, V>
where
    K: EntityRef,
    V: Clone,
{
}

impl<'a, K, V> Iterator for IterMut<'a, K, V>
where
    K: EntityRef,
{
    type Item = (K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        self.elems.next().map(|value| {
            let key = K::new(self.index);
            self.index += 1;
            (key, value)
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.elems.size_hint()
    }
}
impl<'a, K: EntityRef, V: Clone> IntoIterator for &'a SecondaryMap<K, V> {
    type Item = (K, &'a V);
    type IntoIter = Iter<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, K: EntityRef, V: Clone> IntoIterator for &'a mut SecondaryMap<K, V> {
    type Item = (K, &'a V);
    type IntoIter = Iter<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
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
    fn test_secondary_map_default() {
        let map: SecondaryMap<TestEntity, i32> = SecondaryMap::default();
        assert_eq!(map[TestEntity::new(0)], 0);
        assert_eq!(map[TestEntity::new(100)], 0);
    }

    #[test]
    fn test_secondary_map_with_default() {
        let map: SecondaryMap<TestEntity, i32> = SecondaryMap::with_default(42);
        assert_eq!(map[TestEntity::new(0)], 42);
        assert_eq!(map[TestEntity::new(100)], 42);
    }

    #[test]
    fn test_secondary_map_index_mut() {
        let mut map: SecondaryMap<TestEntity, i32> = SecondaryMap::default();
        map[TestEntity::new(0)] = 10;
        map[TestEntity::new(2)] = 20;

        assert_eq!(map[TestEntity::new(0)], 10);
        assert_eq!(map[TestEntity::new(1)], 0);
        assert_eq!(map[TestEntity::new(2)], 20);
    }

    #[test]
    fn test_secondary_map_iteration() {
        let mut map: SecondaryMap<TestEntity, i32> = SecondaryMap::default();
        map[TestEntity::new(0)] = 10;
        map[TestEntity::new(1)] = 20;
        map[TestEntity::new(2)] = 30;

        let items: Vec<_> = map.iter().collect();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], (TestEntity::new(0), &10));
        assert_eq!(items[1], (TestEntity::new(1), &20));
        assert_eq!(items[2], (TestEntity::new(2), &30));
    }

    #[test]
    fn test_secondary_map_from_iter() {
        let items = vec![
            (TestEntity::new(0), 10),
            (TestEntity::new(2), 20),
            (TestEntity::new(1), 15),
        ];
        let map: SecondaryMap<TestEntity, i32> = items.into_iter().collect();

        assert_eq!(map[TestEntity::new(0)], 10);
        assert_eq!(map[TestEntity::new(1)], 15);
        assert_eq!(map[TestEntity::new(2)], 20);
    }
}
