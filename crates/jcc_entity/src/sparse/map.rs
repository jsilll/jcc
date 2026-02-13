//! Sparse-set style map from entity keys to values.
//!
//! `SparseMap` stores entries in a dense `Vec<(K, V)>` for compact iteration,
//! and keeps a sparse index (`SecondaryMap<K, u32>`) that points each key to
//! its position in the dense storage.
//!
//! This design provides:
//! - O(1) expected `get`, `get_mut`, `insert`, and `remove`
//! - cache-friendly iteration over dense entries
//! - key-stable lookups where stale sparse indices are validated against the
//!   dense key before returning a value
//!
//! Removal is performed by swapping with the last dense element, so entry order
//! is not stable after removals.

use crate::{EntityRef, SecondaryMap};

/// A sparse-set map keyed by entity references.
///
/// Internally, values are stored densely as `(K, V)` pairs while `sparse`
/// tracks each key's current dense index. This makes iteration fast and keeps
/// lookup and update operations efficient.
///
/// Unlike an order-preserving map, removing a key may reorder entries because
/// removal is implemented with a swap-with-last strategy.
pub struct SparseMap<K, V>
where
    K: EntityRef,
{
    dense: Vec<(K, V)>,
    sparse: SecondaryMap<K, u32>,
}

impl<K, V> Default for SparseMap<K, V>
where
    K: EntityRef,
{
    fn default() -> Self {
        Self {
            dense: Vec::new(),
            sparse: SecondaryMap::new(),
        }
    }
}

impl<K, V> SparseMap<K, V>
where
    K: EntityRef,
{
    /// Create a new empty mapping.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.dense.len()
    }

    /// Returns `true` if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.dense.is_empty()
    }

    /// Return `true` if the map contains a value for the specified key.
    pub fn contains_key(&self, key: K) -> bool {
        self.get(key).is_some()
    }

    /// Get the values in the map as a slice of `(key, value)` pairs.
    ///
    /// The iteration order is determined by the preceding sequence of `insert` and
    /// `remove` operations. In particular, if no elements were removed, this is the insertion order.
    pub fn as_slice(&self) -> &[(K, V)] {
        &self.dense
    }

    /// Get an iterator over the keys in the map.
    ///
    /// The iteration order is determined by the preceding sequence of `insert` and
    /// `remove` operations. In particular, if no elements were removed, this is the insertion order.
    pub fn keys(&self) -> impl Iterator<Item = K> + '_ {
        self.dense.iter().map(|(k, _)| *k)
    }

    /// Get an iterator over the values in the map.
    ///
    /// The iteration order is determined by the preceding sequence of `insert` and
    /// `remove` operations. In particular, if no elements were removed, this is the insertion order.
    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.dense.iter().map(|(_, v)| v)
    }

    /// Get an iterator over the entries in the map, in the form of `(key, value)` pairs.
    ///
    /// The iteration order is determined by the preceding sequence of `insert` and
    /// `remove` operations. In particular, if no elements were removed, this is the insertion order.
    pub fn entries(&self) -> std::slice::Iter<'_, (K, V)> {
        self.dense.iter()
    }

    /// Returns a reference to the value corresponding to the key.
    pub fn get(&self, key: K) -> Option<&V> {
        if let Some(idx) = self.sparse.get(key).copied() {
            if let Some((k, v)) = self.dense.get(idx as usize) {
                if *k == key {
                    return Some(v);
                }
            }
        }
        None
    }

    /// Clears the map, removing all values.
    pub fn clear(&mut self) {
        self.dense.clear();
    }

    /// Remove the last value from the map and return it, if any.
    pub fn pop(&mut self) -> Option<(K, V)> {
        self.dense.pop()
    }

    /// Returns a mutable reference to the value corresponding to the key.
    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        if let Some(idx) = self.sparse.get(key).copied() {
            if let Some((k, v)) = self.dense.get_mut(idx as usize) {
                if *k == key {
                    return Some(v);
                }
            }
        }
        None
    }

    /// Insert a value into the map.
    ///
    /// If the map did not have this key present, `None` is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old value is returned.
    ///
    /// It is not necessary to provide a key since the value knows its own key already.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(entry) = self.get_mut(key) {
            return Some(std::mem::replace(entry, value));
        }
        let idx = self.dense.len();
        self.dense.push((key, value));
        self.sparse[key] = u32::try_from(idx).ok()?;
        None
    }

    /// Remove a value from the map and return it.
    ///
    /// If the map did not have this key present, `None` is returned.
    pub fn remove(&mut self, key: K) -> Option<V> {
        if let Some(idx) = self.sparse.get(key).copied() {
            let idx_usize = idx as usize;
            if let Some((k, _)) = self.dense.get(idx_usize) {
                if *k == key {
                    let (k, v) = self.dense.pop()?;
                    if idx_usize == self.dense.len() {
                        return Some(v);
                    }
                    let (_, v) = std::mem::replace(&mut self.dense[idx_usize], (k, v));
                    self.sparse[k] = idx;
                    return Some(v);
                }
            }
        }
        None
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
    fn sparse_map_basic_operations() {
        let mut map = SparseMap::new();
        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);

        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
        assert!(!map.contains_key(e1));
        assert_eq!(map.get(e1), None);
        assert_eq!(map.get_mut(e1), None);

        assert_eq!(map.insert(e1, 10), None);
        assert_eq!(map.insert(e2, 20), None);
        assert_eq!(map.len(), 2);
        assert!(!map.is_empty());
        assert!(map.contains_key(e1));
        assert!(map.contains_key(e2));
        assert_eq!(map.get(e1), Some(&10));
        assert_eq!(map.get(e2), Some(&20));
    }

    #[test]
    fn sparse_map_insert_replaces_existing_value() {
        let mut map = SparseMap::new();
        let e = TestEntity::new(7);

        assert_eq!(map.insert(e, 1), None);
        assert_eq!(map.insert(e, 2), Some(1));
        assert_eq!(map.len(), 1);
        assert_eq!(map.get(e), Some(&2));
    }

    #[test]
    fn sparse_map_get_mut_updates_value() {
        let mut map = SparseMap::new();
        let e = TestEntity::new(4);
        map.insert(e, 11);

        if let Some(value) = map.get_mut(e) {
            *value = 99;
        }

        assert_eq!(map.get(e), Some(&99));
    }

    #[test]
    fn sparse_map_remove_existing_and_missing_keys() {
        let mut map = SparseMap::new();
        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);
        let e3 = TestEntity::new(3);

        map.insert(e1, "one");
        map.insert(e2, "two");
        map.insert(e3, "three");

        assert_eq!(map.remove(e2), Some("two"));
        assert_eq!(map.len(), 2);
        assert_eq!(map.get(e2), None);
        assert!(!map.contains_key(e2));

        assert_eq!(map.remove(e2), None);
        assert_eq!(map.remove(TestEntity::new(99)), None);

        assert_eq!(map.get(e1), Some(&"one"));
        assert_eq!(map.get(e3), Some(&"three"));
    }

    #[test]
    fn sparse_map_pop_returns_last_dense_entry() {
        let mut map = SparseMap::new();
        let e1 = TestEntity::new(10);
        let e2 = TestEntity::new(20);

        map.insert(e1, 1);
        map.insert(e2, 2);

        assert_eq!(map.pop(), Some((e2, 2)));
        assert_eq!(map.len(), 1);
        assert_eq!(map.get(e2), None);
        assert_eq!(map.get(e1), Some(&1));

        assert_eq!(map.pop(), Some((e1, 1)));
        assert_eq!(map.pop(), None);
        assert!(map.is_empty());
    }

    #[test]
    fn sparse_map_clear_removes_all_entries() {
        let mut map = SparseMap::new();
        let e1 = TestEntity::new(0);
        let e2 = TestEntity::new(5);

        map.insert(e1, 3);
        map.insert(e2, 8);
        assert_eq!(map.len(), 2);

        map.clear();
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
        assert_eq!(map.get(e1), None);
        assert_eq!(map.get(e2), None);
        assert_eq!(map.entries().count(), 0);
    }

    #[test]
    fn sparse_map_iterators_and_slice_follow_dense_order() {
        let mut map = SparseMap::new();
        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);
        let e3 = TestEntity::new(3);

        map.insert(e1, 10);
        map.insert(e2, 20);
        map.insert(e3, 30);

        let keys: Vec<_> = map.keys().collect();
        assert_eq!(keys, vec![e1, e2, e3]);

        let values: Vec<_> = map.values().copied().collect();
        assert_eq!(values, vec![10, 20, 30]);

        let entries: Vec<_> = map.entries().copied().collect();
        assert_eq!(entries, vec![(e1, 10), (e2, 20), (e3, 30)]);
        assert_eq!(map.as_slice(), &[(e1, 10), (e2, 20), (e3, 30)]);
    }
}
