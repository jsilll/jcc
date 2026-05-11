//! Sparse-set style set of entity keys.
//!
//! `SparseSet` stores keys in a dense `Vec<K>` for compact iteration, and
//! keeps a sparse index (`SecondaryMap<K, u32>`) that points each key to its
//! position in the dense storage.
//!
//! This design provides:
//! - O(1) expected `contains`, `insert`, and `remove`
//! - cache-friendly iteration over dense keys
//! - key-stable lookups where stale sparse indices are validated against the
//!   dense key before reporting membership
//!
//! Removal is performed by swapping with the last dense element, so key order
//! is not stable after removals.

use crate::{EntityRef, SecondaryMap};

/// A sparse-set keyed by entity references.
///
/// Internally, keys are stored densely while `sparse` tracks each key's current
/// dense index. This makes iteration fast and keeps membership and update
/// operations efficient.
///
/// Unlike an order-preserving set, removing a key may reorder entries because
/// removal is implemented with a swap-with-last strategy.
pub struct SparseSet<K>
where
    K: EntityRef,
{
    dense: Vec<K>,
    sparse: SecondaryMap<K, u32>,
}

impl<K> Default for SparseSet<K>
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

impl<K> SparseSet<K>
where
    K: EntityRef,
{
    /// Create a new empty set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of keys in the set.
    pub fn len(&self) -> usize {
        self.dense.len()
    }

    /// Returns `true` if the set contains no keys.
    pub fn is_empty(&self) -> bool {
        self.dense.is_empty()
    }

    /// Get the keys in the set as a slice.
    ///
    /// The iteration order is determined by the preceding sequence of `insert` and
    /// `remove` operations. In particular, if no keys were removed, this is insertion order.
    pub fn as_slice(&self) -> &[K] {
        &self.dense
    }

    /// Get an iterator over the keys in the set.
    ///
    /// The iteration order is determined by the preceding sequence of `insert` and
    /// `remove` operations. In particular, if no keys were removed, this is insertion order.
    pub fn iter(&self) -> impl Iterator<Item = K> + '_ {
        self.dense.iter().copied()
    }

    /// Return `true` if the set contains the specified key.
    pub fn contains(&self, key: K) -> bool {
        if let Some(idx) = self.sparse.get(key).copied() {
            if let Some(k) = self.dense.get(idx as usize) {
                if *k == key {
                    return true;
                }
            }
        }
        false
    }

    /// Clears the set, removing all keys.
    pub fn clear(&mut self) {
        self.dense.clear();
    }

    /// Remove the last key from the set and return it, if any.
    pub fn pop(&mut self) -> Option<K> {
        self.dense.pop()
    }

    /// Insert a key into the set.
    ///
    /// If the inserion is successful, `true` is returned.
    ///
    /// If the set already contains the key, `false` is returned and the set is unchanged.
    pub fn insert(&mut self, key: K) -> bool {
        if self.contains(key) {
            return false;
        }
        let idx = self.dense.len();
        self.dense.push(key);
        if let Ok(idx) = u32::try_from(idx) {
            self.sparse[key] = idx;
            return true;
        }
        false
    }

    /// Remove a key from the set and return it.
    ///
    /// If the set did not have this key present, `None` is returned.
    pub fn remove(&mut self, key: K) -> bool {
        if let Some(idx) = self.sparse.get(key).copied() {
            let idx_usize = idx as usize;
            if let Some(k) = self.dense.get(idx_usize) {
                if *k == key {
                    if let Some(k) = self.dense.pop() {
                        if idx_usize == self.dense.len() {
                            return true;
                        }
                        self.dense[idx_usize] = k;
                        self.sparse[k] = idx;
                        return true;
                    }
                }
            }
        }
        false
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
    fn sparse_set_basic_operations() {
        let mut set = SparseSet::new();
        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);

        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
        assert!(!set.contains(e1));

        assert!(set.insert(e1));
        assert!(set.insert(e2));
        assert_eq!(set.len(), 2);
        assert!(!set.is_empty());
        assert!(set.contains(e1));
        assert!(set.contains(e2));
    }

    #[test]
    fn sparse_set_insert_existing_key_returns_false() {
        let mut set = SparseSet::new();
        let e = TestEntity::new(7);

        assert!(set.insert(e));
        assert!(!set.insert(e));
        assert_eq!(set.len(), 1);
        assert!(set.contains(e));
    }

    #[test]
    fn sparse_set_remove_existing_and_missing_keys() {
        let mut set = SparseSet::new();
        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);
        let e3 = TestEntity::new(3);

        set.insert(e1);
        set.insert(e2);
        set.insert(e3);

        assert!(set.remove(e2));
        assert_eq!(set.len(), 2);
        assert!(!set.contains(e2));

        assert!(!set.remove(e2));
        assert!(!set.remove(TestEntity::new(99)));

        assert!(set.contains(e1));
        assert!(set.contains(e3));
    }

    #[test]
    fn sparse_set_pop_returns_last_dense_key() {
        let mut set = SparseSet::new();
        let e1 = TestEntity::new(10);
        let e2 = TestEntity::new(20);

        set.insert(e1);
        set.insert(e2);

        assert_eq!(set.pop(), Some(e2));
        assert_eq!(set.len(), 1);
        assert!(!set.contains(e2));
        assert!(set.contains(e1));

        assert_eq!(set.pop(), Some(e1));
        assert_eq!(set.pop(), None);
        assert!(set.is_empty());
    }

    #[test]
    fn sparse_set_clear_removes_all_keys() {
        let mut set = SparseSet::new();
        let e1 = TestEntity::new(0);
        let e2 = TestEntity::new(5);

        set.insert(e1);
        set.insert(e2);
        assert_eq!(set.len(), 2);

        set.clear();
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
        assert!(!set.contains(e1));
        assert!(!set.contains(e2));
        assert_eq!(set.iter().count(), 0);
    }

    #[test]
    fn sparse_set_iterators_and_slice_follow_dense_order() {
        let mut set = SparseSet::new();
        let e1 = TestEntity::new(1);
        let e2 = TestEntity::new(2);
        let e3 = TestEntity::new(3);

        set.insert(e1);
        set.insert(e2);
        set.insert(e3);

        let keys: Vec<_> = set.iter().collect();
        assert_eq!(keys, vec![e1, e2, e3]);
        assert_eq!(set.as_slice(), &[e1, e2, e3]);
    }
}
