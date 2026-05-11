//! Identity-hashing collections for entity-keyed maps and sets.

use crate::EntityRef;

use std::{
    collections::{hash_map, hash_set, HashMap, HashSet},
    hash::{BuildHasherDefault, Hasher},
};

/// A pass-through hasher that uses integer values directly as hash codes.
///
/// This eliminates hashing overhead for types whose [`Hash`] implementation
/// reduces to a single integer write (e.g., `u32` newtypes like entity references).
/// Collision resistance is preserved as long as each key maps to a distinct integer.
#[derive(Default, Debug, Clone)]
pub struct EntityHasher(u64);

impl Hasher for EntityHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }

    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.0 = u64::from(i);
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.0 = i;
    }

    #[allow(clippy::panic)]
    fn write(&mut self, _: &[u8]) {
        panic!("EntityHasher only supports integer writes");
    }
}

/// A [`HashSet`] keyed by [`EntityRef`] types using identity hashing.
///
/// Hashing is a no-op: the key's integer index is used directly as the hash code.
pub struct EntitySet<K: EntityRef>(HashSet<K, BuildHasherDefault<EntityHasher>>);

impl<K: EntityRef> EntitySet<K> {
    /// Creates a new empty set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of elements.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the set contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Removes all elements.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns `true` if the set contains `key`.
    #[inline]
    pub fn contains(&self, key: &K) -> bool {
        self.0.contains(key)
    }

    /// Inserts `key`. Returns `true` if it was not already present.
    #[inline]
    pub fn insert(&mut self, key: K) -> bool {
        self.0.insert(key)
    }

    /// Removes `key`. Returns `true` if it was present.
    #[inline]
    pub fn remove(&mut self, key: &K) -> bool {
        self.0.remove(key)
    }

    /// Returns an iterator over the elements.
    #[inline]
    pub fn iter(&self) -> hash_set::Iter<'_, K> {
        self.0.iter()
    }
}

impl<K: EntityRef> Default for EntitySet<K> {
    fn default() -> Self {
        EntitySet(HashSet::default())
    }
}

impl<K: EntityRef + Clone> Clone for EntitySet<K> {
    fn clone(&self) -> Self {
        EntitySet(self.0.clone())
    }
}

impl<K: EntityRef + std::fmt::Debug> std::fmt::Debug for EntitySet<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<K: EntityRef> IntoIterator for EntitySet<K> {
    type Item = K;
    type IntoIter = hash_set::IntoIter<K>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K: EntityRef> IntoIterator for &'a EntitySet<K> {
    type Item = &'a K;
    type IntoIter = hash_set::Iter<'a, K>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

/// A [`HashMap`] keyed by [`EntityRef`] types using identity hashing.
///
/// Hashing is a no-op: the key's integer index is used directly as the hash code.
pub struct EntityMap<K: EntityRef, V>(HashMap<K, V, BuildHasherDefault<EntityHasher>>);

impl<K: EntityRef, V> EntityMap<K, V> {
    /// Creates a new empty map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of entries.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the map contains no entries.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Removes all entries.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns a reference to the value for `key`, or `None` if absent.
    #[inline]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.0.get(key)
    }

    /// Returns a mutable reference to the value for `key`, or `None` if absent.
    #[inline]
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.0.get_mut(key)
    }

    /// Returns `true` if the map contains an entry for `key`.
    #[inline]
    pub fn contains_key(&self, key: &K) -> bool {
        self.0.contains_key(key)
    }

    /// Inserts a key-value pair, returning the previous value if the key was present.
    #[inline]
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.0.insert(key, value)
    }

    /// Removes and returns the value for `key`, or `None` if absent.
    #[inline]
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.0.remove(key)
    }

    /// Gets the entry for `key` for in-place manipulation.
    #[inline]
    pub fn entry(&mut self, key: K) -> hash_map::Entry<'_, K, V> {
        self.0.entry(key)
    }

    /// Returns a mutable reference to the value for `key`, inserting `f()` if absent.
    #[inline]
    pub fn get_or_insert_with(&mut self, key: K, f: impl FnOnce() -> V) -> &mut V {
        self.0.entry(key).or_insert_with(f)
    }

    /// Returns an iterator over `(&key, &value)` pairs.
    #[inline]
    pub fn iter(&self) -> hash_map::Iter<'_, K, V> {
        self.0.iter()
    }

    /// Returns an iterator over `(&key, &mut value)` pairs.
    #[inline]
    pub fn iter_mut(&mut self) -> hash_map::IterMut<'_, K, V> {
        self.0.iter_mut()
    }

    /// Returns an iterator over keys.
    #[inline]
    pub fn keys(&self) -> hash_map::Keys<'_, K, V> {
        self.0.keys()
    }

    /// Returns an iterator over values.
    #[inline]
    pub fn values(&self) -> hash_map::Values<'_, K, V> {
        self.0.values()
    }

    /// Returns a mutable iterator over values.
    #[inline]
    pub fn values_mut(&mut self) -> hash_map::ValuesMut<'_, K, V> {
        self.0.values_mut()
    }
}

impl<K: EntityRef, V: Default> EntityMap<K, V> {
    /// Returns a mutable reference to the value for `key`, inserting a default value if absent.
    #[inline]
    pub fn get_or_insert_default(&mut self, key: K) -> &mut V {
        self.0.entry(key).or_default()
    }
}

impl<K: EntityRef, V> Default for EntityMap<K, V> {
    fn default() -> Self {
        EntityMap(HashMap::default())
    }
}

impl<K: EntityRef + Clone, V: Clone> Clone for EntityMap<K, V> {
    fn clone(&self) -> Self {
        EntityMap(self.0.clone())
    }
}

impl<K: EntityRef + std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for EntityMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<K: EntityRef, V> IntoIterator for EntityMap<K, V> {
    type Item = (K, V);
    type IntoIter = hash_map::IntoIter<K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K: EntityRef, V> IntoIterator for &'a EntityMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = hash_map::Iter<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, K: EntityRef, V> IntoIterator for &'a mut EntityMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = hash_map::IterMut<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}
