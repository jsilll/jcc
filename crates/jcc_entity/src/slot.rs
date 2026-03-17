//! A compact slot-based storage with explicit liveness tracking and key reuse.
//!
//! `SlotMap` provides stable keys (`K`) to access stored values (`V`), while
//! allowing efficient removal and reuse of slots via an internal free list.
//!
//! Unlike traditional slot maps that embed generation counters in keys,
//! this implementation delegates liveness tracking to the value itself via
//! the [`SlotValue`] trait.
//!
//! ## Guarantees
//!
//! - Keys remain stable across insertions and deletions.
//! - Accessing a freed (dead) slot via safe APIs returns `None`.
//! - Indexing (`[]`) assumes the slot is alive (debug assertion enforced).
//!
//! ## Trade-offs
//!
//! - Requires value types to store their own liveness state.
//! - No generation counters so stale keys are not automatically invalidated.
//!
//! ## When to use
//!
//! This structure is useful when:
//!
//! - You want tight control over memory reuse.
//! - You need stable indices/handles into a collection.
//! - You can tolerate (or explicitly manage) stale keys.

use crate::{EntityRef, PrimaryMap};

/// Trait for values stored inside a [`SlotMap`].
///
/// This trait allows the container to track whether a slot is currently
/// occupied ("alive") or has been freed ("dead").
///
/// # Requirements
///
/// Implementors must ensure:
///
/// - `is_alive` accurately reflects the current state.
/// - `set_dead` marks the value as invalid and reusable.
pub trait SlotValue {
    /// Marks the value as dead.
    fn set_dead(&mut self);

    /// Returns whether the value is currently alive.
    fn is_alive(&self) -> bool;
}

/// A slot-based map with key reuse and explicit liveness tracking.
///
/// `SlotMap` stores values in a dense backing map and reuses freed slots
/// to avoid unnecessary allocations.
///
/// Keys are stable and can be reused after deletion.
///
/// # Type Parameters
///
/// - `K`: Key type implementing [`EntityRef`]
/// - `V`: Value type implementing [`SlotValue`]
pub struct SlotMap<K: EntityRef, V: SlotValue> {
    alive: usize,
    dead: Vec<K>,
    map: PrimaryMap<K, V>,
}

impl<K: EntityRef, V: SlotValue> Default for SlotMap<K, V> {
    fn default() -> Self {
        Self {
            alive: 0,
            dead: Vec::new(),
            map: PrimaryMap::new(),
        }
    }
}

impl<K: EntityRef, V: SlotValue> SlotMap<K, V> {
    /// Creates a new empty `SlotMap`.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new `SlotMap` with preallocated capacity.
    ///
    /// This reserves space in the underlying storage but does not populate it.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            alive: 0,
            dead: Vec::new(),
            map: PrimaryMap::with_capacity(capacity),
        }
    }

    /// Returns the number of alive elements in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.alive
    }

    /// Returns `true` if the map contains no alive elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.alive == 0
    }

    /// Returns `true` if the given key refers to an alive value.
    #[inline]
    pub fn is_alive(&self, key: K) -> bool {
        self.map.get(key).is_some_and(SlotValue::is_alive)
    }

    /// Clears the map, removing all values.
    #[inline]
    pub fn clear(&mut self) {
        self.alive = 0;
        self.map.clear();
        self.dead.clear();
    }

    /// Inserts a value into the map and returns its key.
    ///
    /// # Panics
    ///
    /// Panics if the value is not alive (i.e., `is_alive` returns `false`).
    pub fn push(&mut self, value: V) -> K {
        assert!(value.is_alive());
        self.alive += 1;
        match self.dead.pop() {
            Some(key) => {
                self.map[key] = value;
                key
            }
            None => self.map.push(value),
        }
    }

    /// Frees the slot associated with `key`.
    ///
    /// If the key is invalid or is already dead, this is a no-op.
    pub fn free(&mut self, key: K) {
        if let Some(value) = self.map.get_mut(key) {
            if value.is_alive() {
                self.alive -= 1;
                value.set_dead();
                self.dead.push(key);
            }
        }
    }

    /// Returns a reference to the value for `key` if it is alive.
    ///
    /// Returns `None` if the key is invalid or the slot is dead.
    #[inline]
    pub fn get(&self, key: K) -> Option<&V> {
        let value = self.map.get(key)?;
        if value.is_alive() {
            Some(value)
        } else {
            None
        }
    }

    /// Returns a mutable reference to the value for `key` if it is alive.
    ///
    /// Returns `None` if the key is invalid or the slot is dead.
    #[inline]
    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        let value = self.map.get_mut(key)?;
        if value.is_alive() {
            Some(value)
        } else {
            None
        }
    }

    /// Returns an iterator over references to alive values.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.map.values().filter(|v| v.is_alive())
    }

    /// Returns an iterator over mutable references to alive values.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.map.values_mut().filter(|v| v.is_alive())
    }

    /// Returns an iterator over all alive entries and their values.
    ///
    /// The iterator yields tuples of `(K, &V)`.
    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.map.iter().filter(|(_, v)| v.is_alive())
    }

    /// Returns an iterator over all alive entries and their values.
    ///
    /// The iterator yields tuples of `(K, &mut V)`.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> {
        self.map.iter_mut().filter(|(_, v)| v.is_alive())
    }
}

impl<K: EntityRef, V: SlotValue> std::ops::Index<K> for SlotMap<K, V> {
    type Output = V;

    /// Returns a reference to the value for `key`.
    ///
    /// # Panics
    ///
    /// Panics if the key is invalid or if the slot is dead in debug builds.
    #[inline]
    fn index(&self, key: K) -> &Self::Output {
        let v = &self.map[key];
        debug_assert!(v.is_alive());
        v
    }
}

impl<K: EntityRef, V: SlotValue> std::ops::IndexMut<K> for SlotMap<K, V> {
    /// Returns a mutable reference to the value for `key`.
    ///
    /// # Panics
    ///
    /// Panics if the key is invalid or if the slot is dead in debug builds.
    #[inline]
    fn index_mut(&mut self, key: K) -> &mut Self::Output {
        let v = &mut self.map[key];
        debug_assert!(v.is_alive());
        v
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity_impl;

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    struct TestValue {
        data: i32,
        alive: bool,
    }

    impl TestValue {
        fn new(data: i32) -> Self {
            Self { data, alive: true }
        }
    }

    impl SlotValue for TestValue {
        fn set_dead(&mut self) {
            self.alive = false;
        }

        fn is_alive(&self) -> bool {
            self.alive
        }
    }

    #[test]
    fn slot_map_basic_operations() {
        let mut map = SlotMap::<TestEntity, TestValue>::new();
        assert!(map.is_empty());

        let v1 = TestValue::new(1);
        let v2 = TestValue::new(2);

        let e1 = map.push(v1);
        let e2 = map.push(v2);

        assert_eq!(map.len(), 2);
        assert!(map.is_alive(e1));
        assert!(map.is_alive(e2));
        assert_eq!(map[e1].data, 1);
        assert_eq!(map[e2].data, 2);

        map.free(e1);

        assert_eq!(map.len(), 1);
        assert!(map.is_alive(e2));
        assert!(!map.is_alive(e1));
        assert_eq!(map.get(e1).is_none(), true);

        let v3 = TestValue::new(3);
        let e3 = map.push(v3);

        assert_eq!(map.len(), 2);
        assert_eq!(map[e3].data, 3);
    }

    #[test]
    fn slot_map_iteration() {
        let mut map = SlotMap::<TestEntity, TestValue>::new();
        map.push(TestValue::new(1));
        let e2 = map.push(TestValue::new(2));
        map.push(TestValue::new(3));

        map.free(e2);

        let values: Vec<i32> = map.values().map(|v| v.data).collect();
        assert_eq!(values, vec![1, 3]);

        let pairs = map.iter().map(|(k, v)| (k, v.data)).collect::<Vec<_>>();
        assert_eq!(pairs, vec![(TestEntity(0), 1), (TestEntity(2), 3)]);

        map.free(e2);

        let values: Vec<i32> = map.values().map(|v| v.data).collect();
        assert_eq!(values, vec![1, 3]);

        let pairs = map.iter().map(|(k, v)| (k, v.data)).collect::<Vec<_>>();
        assert_eq!(pairs, vec![(TestEntity(0), 1), (TestEntity(2), 3)]);
    }
}
