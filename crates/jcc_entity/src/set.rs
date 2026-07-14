//! A set implementation optimized for densely indexed entity references.
//!
//! This module provides the [`EntitySet`] type, which implements a mathematical set
//! using a bitset under the hood. It is specifically designed to work with types that
//! implement the [`crate::EntityRef`] trait, which represent indices into a dense array.
//!
//! By mapping entity references to bit indices, [`EntitySet`] provides incredibly fast,
//! allocation-free (after initial capacity is met) `O(1)` operations for insertion,
//! removal, and membership testing. It is significantly more memory-efficient than a
//! traditional `HashSet` as long as the underlying entity indices remain relatively dense.

use crate::EntityRef;

use jcc_bitset::set::BitSet;

use std::marker::PhantomData;

/// A set of `K` for densely indexed entity references.
#[derive(Clone, PartialEq, Eq)]
pub struct EntitySet<K: EntityRef> {
    bitset: BitSet,
    _phantom: PhantomData<K>,
}

impl<K: EntityRef> Default for EntitySet<K> {
    fn default() -> Self {
        Self {
            _phantom: PhantomData,
            bitset: BitSet::default(),
        }
    }
}

impl<K: EntityRef> Extend<K> for EntitySet<K> {
    fn extend<T: IntoIterator<Item = K>>(&mut self, iter: T) {
        for k in iter {
            self.insert(k);
        }
    }
}

impl<'a, K: EntityRef> IntoIterator for &'a EntitySet<K> {
    type Item = K;
    type IntoIter = Iter<'a, K>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<K: std::fmt::Debug + EntityRef> std::fmt::Debug for EntitySet<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<K: EntityRef> EntitySet<K> {
    /// Create a new empty set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new empty set with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            _phantom: PhantomData,
            bitset: BitSet::with_capacity(capacity),
        }
    }

    /// Returns `true` if the set contains no keys.
    pub fn is_empty(&self) -> bool {
        self.bitset.is_empty()
    }

    /// Returns `true` if the set contains the specified key.
    pub fn contains(&self, key: K) -> bool {
        self.bitset.contains(key.index())
    }

    /// Removes all entries from the set.
    pub fn clear(&mut self) {
        self.bitset.clear();
    }

    /// Removes and returns the highest-index entity from the set if it exists.
    pub fn pop(&mut self) -> Option<K> {
        let index = self.bitset.pop()?;
        Some(K::new(index))
    }

    /// Insert a key into the set.
    ///
    /// If the inserion is successful, `true` is returned.
    ///
    /// If the set already contains the key, `false` is returned and the set is unchanged.
    pub fn insert(&mut self, key: K) -> bool {
        self.bitset.insert(key.index())
    }

    /// Removes a key from the set and returns `true` if it was present.
    pub fn remove(&mut self, key: K) -> bool {
        self.bitset.remove(key.index())
    }

    /// Get an iterator over the keys in the set.
    ///
    /// Note: Items are always yielded in ascending order.
    pub fn iter(&self) -> Iter<'_, K> {
        Iter {
            _phantom: PhantomData,
            inner: self.bitset.iter(),
        }
    }
}

/// An iterator over the elements in an [`EntitySet`].
pub struct Iter<'a, K> {
    _phantom: PhantomData<K>,
    inner: jcc_bitset::set::Iter<'a>,
}

impl<K> Iterator for Iter<'_, K>
where
    K: EntityRef,
{
    type Item = K;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(K::new(self.inner.next()?))
    }
}

#[cfg(test)]
mod tests {
    use crate::{entity_impl, set::EntitySet};

    #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    const E0: TestEntity = TestEntity(0);
    const E1: TestEntity = TestEntity(1);
    const E2: TestEntity = TestEntity(2);
    const E3: TestEntity = TestEntity(3);
    const E4: TestEntity = TestEntity(4);
    const E5: TestEntity = TestEntity(5);
    const E6: TestEntity = TestEntity(6);
    const E7: TestEntity = TestEntity(7);
    const E8: TestEntity = TestEntity(8);
    const E9: TestEntity = TestEntity(9);
    const E10: TestEntity = TestEntity(10);
    const E11: TestEntity = TestEntity(11);
    const E12: TestEntity = TestEntity(12);
    const E14: TestEntity = TestEntity(14);
    const E15: TestEntity = TestEntity(15);
    const E16: TestEntity = TestEntity(16);
    const E19: TestEntity = TestEntity(19);
    const E20: TestEntity = TestEntity(20);
    const EMAX: TestEntity = TestEntity(u32::MAX);

    #[test]
    fn pop_ordered() {
        let mut m = EntitySet::new();
        m.insert(E0);
        m.insert(E1);
        m.insert(E2);
        assert_eq!(E2, m.pop().unwrap());
        assert_eq!(E1, m.pop().unwrap());
        assert_eq!(E0, m.pop().unwrap());
        assert!(m.pop().is_none());
        assert!(m.pop().is_none());
    }

    #[test]
    fn pop_unordered() {
        let mut m = EntitySet::new();
        let mut l = [E0, E1, E6, E7, E5, E9, E10, E2, E3, E11, E12];
        l.iter().for_each(|e| {
            m.insert(*e);
        });
        assert_eq!(m.bitset.max(), Some(12));
        l.sort();
        l.iter().rev().for_each(|e| {
            assert_eq!(*e, m.pop().unwrap());
        });
        assert!(m.is_empty());
    }

    #[test]
    fn big() {
        let mut m = EntitySet::new();
        assert!(m.is_empty());

        m.insert(E2);
        m.insert(E1);

        assert!(!m.is_empty());
        assert!(m.contains(E2));
        assert!(m.contains(E1));

        assert!(!m.contains(E0));
        assert!(!m.contains(E3));
        assert!(!m.contains(E4));
        assert!(!m.contains(E8));
        assert!(!m.contains(E15));
        assert!(!m.contains(E19));

        m.insert(E8);
        m.insert(E15);

        assert!(m.contains(E8));
        assert!(m.contains(E15));

        assert!(!m.contains(E3));
        assert!(!m.contains(E4));
        assert!(!m.contains(E9));
        assert!(!m.contains(E14));
        assert!(!m.contains(E16));
        assert!(!m.contains(E19));
        assert!(!m.contains(E20));
        assert!(!m.contains(EMAX));

        m.clear();
        assert!(m.is_empty());
    }

    #[test]
    fn debug() {
        let mut s = EntitySet::new();
        s.insert(TestEntity(2));
        s.insert(TestEntity(4));
        assert_eq!(format!("{s:?}"), "{test_entity2, test_entity4}");
    }
}
