use crate::{EntityMap, EntityRef};

use std::marker::PhantomData;

/// A compact, strongly-typed local index for an entity type `K`.
///
/// `Local<K>` is a dense, zero-based `u32` index into a [`LocalMap<K>`].
/// The phantom type parameter prevents mixing local indices for different
/// entity types at compile time.
///
/// Implements [`EntityRef`], so it can be passed anywhere that accepts an entity.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Local<K: EntityRef>(u32, PhantomData<K>);

impl<K: EntityRef> EntityRef for Local<K> {
    #[inline]
    fn new(index: usize) -> Self {
        debug_assert!(index < u32::MAX as usize);
        #[allow(clippy::cast_possible_truncation)]
        Self(index as u32, PhantomData)
    }

    #[inline]
    fn index(self) -> usize {
        self.0 as usize
    }
}

impl<K: EntityRef> std::fmt::Debug for Local<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Local<{}>({})", std::any::type_name::<K>(), self.0)
    }
}

/// A compaction map from sparse entity references `K` to dense a [`Local<K>`].
///
/// Assigns each newly seen `K` a unique `Local<K>` in insertion order, and
/// retains the reverse mapping such that the original `K` can be recovered.
pub struct LocalMap<K: EntityRef> {
    rev: Vec<K>,
    map: EntityMap<K, Local<K>>,
}

impl<K: EntityRef> Default for LocalMap<K> {
    fn default() -> Self {
        Self {
            rev: Vec::new(),
            map: EntityMap::default(),
        }
    }
}

impl<K: EntityRef> LocalMap<K> {
    /// Creates an empty local map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates an empty map with pre-allocated capacity for `n` entries.
    pub fn with_capacity(n: usize) -> Self {
        Self {
            rev: Vec::with_capacity(n),
            map: EntityMap::with_capacity_and_hasher(n, Default::default()),
        }
    }

    /// Returns `true` if no entities have been inserted.
    pub fn is_empty(&self) -> bool {
        self.rev.is_empty()
    }

    /// Returns the number of entities that have been inserted.
    pub fn len(&self) -> usize {
        self.rev.len()
    }

    /// Returns the original entity `K` for a given local index.
    pub fn key(&self, id: Local<K>) -> Option<K> {
        self.rev.get(id.index()).copied()
    }

    /// Returns the [`Local<K>`] index for `key`, or `None` if it has not been inserted.
    pub fn get(&self, key: K) -> Option<Local<K>> {
        self.map.get(&key).copied()
    }

    /// Removes all entries, resetting the map to empty.
    pub fn clear(&mut self) {
        self.map.clear();
        self.rev.clear();
    }

    /// Inserts `key` if not already present and returns its [`Local<K>`] index.
    ///
    /// If `key` was already inserted the existing local index is returned and the map is unchanged.
    pub fn push(&mut self, key: K) -> Local<K> {
        if let Some(&id) = self.map.get(&key) {
            return id;
        }
        let id = Local::new(self.rev.len());
        self.rev.push(key);
        self.map.insert(key, id);
        id
    }

    /// Iterates over inserted entities in insertion order.
    pub fn entities(&self) -> impl Iterator<Item = K> + '_ {
        self.rev.iter().copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity_impl;

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    const E1: TestEntity = TestEntity(10);
    const E2: TestEntity = TestEntity(42);
    const E3: TestEntity = TestEntity(100);

    #[test]
    fn empty() {
        let map = LocalMap::<TestEntity>::new();
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
    }

    #[test]
    fn push() {
        let mut map = LocalMap::<TestEntity>::new();
        assert_eq!(map.push(E1).index(), 0);
        assert_eq!(map.push(E2).index(), 1);
        assert_eq!(map.push(E3).index(), 2);
        assert!(!map.is_empty());
        assert_eq!(map.len(), 3);
    }

    #[test]
    fn push_duplicate() {
        let mut map = LocalMap::<TestEntity>::new();
        let l1 = map.push(E2);
        let l2 = map.push(E1);
        let l3 = map.push(E2);
        assert_eq!(l1, l3);
        assert_eq!(map.len(), 2);
        assert_eq!(l2.index(), 1);
    }

    #[test]
    fn get() {
        let mut map = LocalMap::<TestEntity>::new();
        let l1 = map.push(E1);
        let l2 = map.push(E2);
        assert_eq!(map.get(E3), None);
        assert_eq!(map.get(E1), Some(l1));
        assert_eq!(map.get(E2), Some(l2));
    }

    #[test]
    fn key() {
        let mut map = LocalMap::<TestEntity>::new();
        let l1 = map.push(E1);
        let l2 = map.push(E2);
        assert_eq!(map.key(l1), Some(E1));
        assert_eq!(map.key(l2), Some(E2));
        assert_eq!(map.key(Local::new(100)), None);
    }

    #[test]
    fn entities() {
        let mut map = LocalMap::<TestEntity>::new();
        for entity in [E1, E2, E3, E1] {
            map.push(entity);
        }
        assert_eq!(map.entities().collect::<Vec<_>>(), &[E1, E2, E3]);
    }
}
