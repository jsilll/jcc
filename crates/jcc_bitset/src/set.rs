//! Dynamic bitsets.
//!
//! This module provides [`BitSet`], an arbitrarily large, dynamically-sized set
//! of non-negative integers backed by a contiguous sequence of block-storage words ([`BitWord`]).
//!
//! # Memory Layout & Performance
//!
//! Elements are stored compactly as single bits. For a storage type `T` where
//! `T::BITS = 64`, a value `N` occupies the `(N % 64)`-th bit of the `(N / 64)`-th word.
//!
//! - **Insert / Remove / Contains:** $O(1)$ constant time operations (ignoring occasional reallocation).
//! - **Space Efficiency:** Highly compact for dense sets, though sparse sets with exceptionally large values will allocate words for empty intervening space.

use crate::{BitStorage, word::BitWord};

/// A arbitrarily large bitset backed by dynamically-sized storage.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct BitSet<T = usize> {
    /// The largest value in the set.
    max: Option<usize>,
    /// The backing storage for the bitset.
    words: Box<[BitWord<T>]>,
}

impl<'a, T: BitStorage> IntoIterator for &'a BitSet<T> {
    type Item = usize;
    type IntoIter = Iter<'a, T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T: BitStorage> std::fmt::Debug for BitSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BitSet")
            .field("max", &self.max)
            .field("words", &self.words)
            .finish()
    }
}

impl<T: BitStorage> std::fmt::Display for BitSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut items = self.iter();
        if let Some(first) = items.next() {
            write!(f, "{first}")?;
            for item in items {
                write!(f, ", {item}")?;
            }
        }
        write!(f, "}}")
    }
}

impl BitSet {
    /// Construct a new, empty bitset.
    #[inline]
    pub fn new() -> Self {
        BitSet::default()
    }
}

impl<T: BitStorage> BitSet<T> {
    const WORD_BITS: usize = T::BITS as usize;

    /// Construct a new, empty bitset with enough
    /// space to store any `idx` such that `idx < capacity`.
    ///
    /// Note: The actual capacity reserved may be greater than that requested.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        let mut set = Self::default();
        set.ensure_capacity(capacity);
        set
    }

    /// Returns the largest item in the set.
    ///
    /// Returns `None` if this set is empty.
    #[inline]
    pub fn max(&self) -> Option<usize> {
        self.max
    }

    /// Removes and returns the largest item in the set.
    ///
    /// Returns `None` if this set is empty.
    #[inline]
    pub fn pop(&mut self) -> Option<usize> {
        let max = self.max()?;
        self.remove(max);
        Some(max)
    }

    /// Returns the number of items in the bitset.
    #[inline]
    pub fn len(&self) -> usize {
        self.words.iter().map(|w| usize::from(w.len())).sum()
    }

    /// Returns `true` if the bitset contains no items.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the number of items the bitset can hold without reallocating.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.words.len() * Self::WORD_BITS
    }

    /// Returns `true` if the bitset contains the item `idx`.
    #[inline]
    pub fn contains(&self, idx: usize) -> bool {
        let (word, bit) = Self::word_and_bit(idx);
        self.words.get(word).is_some_and(|w| w.contains(bit))
    }

    /// Inserts item `idx` into the bitset.
    ///
    /// Returns whether the item was newly inserted.
    #[inline]
    pub fn insert(&mut self, idx: usize) -> bool {
        self.ensure_capacity(idx + 1);
        let (word, bit) = Self::word_and_bit(idx);
        let is_new = self.words[word].insert(bit);
        self.max = self.max.map(|m| std::cmp::max(m, idx)).or(Some(idx));
        is_new
    }

    /// Removes item `idx` from the bitset.
    ///
    /// Returns whether the item was present in the set.
    #[inline]
    pub fn remove(&mut self, idx: usize) -> bool {
        let (word, bit) = Self::word_and_bit(idx);
        let was_present = self.words.get_mut(word).is_some_and(|w| w.remove(bit));
        if was_present && self.max.is_some_and(|m| m == idx) {
            self.max = self.compute_max(word);
        }
        was_present
    }

    /// Remove all items from the bitset.
    #[inline]
    pub fn clear(&mut self) {
        let Some(max) = self.max() else { return };
        let (word, _) = Self::word_and_bit(max);
        debug_assert!(self.words[word + 1..].iter().all(BitWord::is_empty));
        self.words[..=word].iter_mut().for_each(BitWord::clear);
        self.max = None;
    }

    /// Iterate over the items in the bitset.
    ///
    /// Note: Items are always yielded in sorted order.
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            base: 0,
            curr: None,
            words: self.words.iter(),
        }
    }

    /// Ensure there is space in the bitset for the values `0..capacity`.
    ///
    /// After calling `bitset.ensure_capacity(n)`, inserting any item `idx`
    /// where `idx < capacity` is guaranteed to succeed without growing the
    /// bitset's backing storage.
    #[inline]
    pub fn ensure_capacity(&mut self, capacity: usize) {
        let Some(capacity) = capacity.checked_sub(1) else {
            return;
        };
        let (word, _) = Self::word_and_bit(capacity);
        if word < self.words.len() {
            return;
        }
        let additional = std::cmp::max(4, word - self.words.len() + 1);
        let additional = std::cmp::max(additional, self.words.len());
        let mut vec = Vec::from(std::mem::take(&mut self.words));
        vec.reserve_exact(additional);
        vec.extend(std::iter::repeat_n(BitWord::new(), additional));
        self.words = vec.into_boxed_slice();
    }

    /// Update the `self.max` field, based on the contents of `self.words[0..=hint]`.
    ///
    /// - `hint` is the index of the last word that may contain a set bit.
    #[inline]
    fn compute_max(&mut self, hint: usize) -> Option<usize> {
        self.words[0..=hint]
            .iter()
            .enumerate()
            .rev()
            .find_map(|(word, w)| {
                let bit = w.max()?;
                Some(Self::elem(word, bit))
            })
    }

    /// Convert the pair of an index into
    /// `self.elems` and associated bit index into a set item.
    #[inline]
    fn elem(word: usize, bit: u8) -> usize {
        let bit = usize::from(bit);
        debug_assert!(bit < Self::WORD_BITS);
        word * Self::WORD_BITS + bit
    }

    /// Convert an item `idx` into the `word` that
    /// can be used to index into `self.elems` and
    /// the `bit` that can be tested in the `self.elems[word]`.
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn word_and_bit(i: usize) -> (usize, u8) {
        let bit = i % Self::WORD_BITS;
        let word = i / Self::WORD_BITS;
        let bit = u8::try_from(bit).unwrap();
        (word, bit)
    }
}

/// An iterator over the items in a [`BitSet`].
pub struct Iter<'a, T = usize> {
    base: usize,
    curr: Option<crate::word::Iter<T>>,
    words: std::slice::Iter<'a, BitWord<T>>,
}

impl<T: BitStorage> Iterator for Iter<'_, T> {
    type Item = usize;

    #[inline]
    fn next(&mut self) -> Option<usize> {
        // Hot
        if let Some(curr) = &mut self.curr {
            if let Some(bit) = curr.next() {
                return Some(self.base + bit as usize);
            }
            self.base += BitSet::<T>::WORD_BITS;
            self.curr = None;
        }
        // Cold
        for word in &mut self.words {
            if !word.is_empty() {
                let mut curr = word.iter();
                if let Some(bit) = curr.next() {
                    self.curr = Some(curr);
                    return Some(self.base + bit as usize);
                }
            }
            self.base += BitSet::<T>::WORD_BITS;
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let set = BitSet::new();
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
        assert_eq!(set.max(), None);
    }

    #[test]
    fn insert_and_contains() {
        let mut set = BitSet::new();
        assert!(set.insert(5));
        assert!(set.insert(120));

        assert!(!set.insert(5));

        assert!(set.contains(5));
        assert!(!set.contains(6));
        assert!(set.contains(120));

        assert_eq!(set.len(), 2);
        assert_eq!(set.max(), Some(120));
    }

    #[test]
    fn clear() {
        let mut set = BitSet::new();
        set.insert(10);
        set.insert(200);

        assert!(!set.is_empty());
        set.clear();

        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
        assert_eq!(set.max(), None);
    }

    #[test]
    fn pop() {
        let mut set = BitSet::new();
        set.insert(5);
        set.insert(42);
        set.insert(17);

        assert_eq!(set.pop(), Some(42));
        assert_eq!(set.pop(), Some(17));
        assert_eq!(set.pop(), Some(5));
        assert_eq!(set.pop(), None);
        assert!(set.is_empty());
    }

    #[test]
    fn remove_and_max() {
        let mut set = BitSet::new();
        set.insert(10);
        set.insert(50);
        set.insert(100);

        assert_eq!(set.max(), Some(100));

        assert!(set.remove(50));
        assert!(!set.contains(50));
        assert_eq!(set.max(), Some(100));

        assert!(!set.remove(50));

        assert!(set.remove(100));
        assert_eq!(set.max(), Some(10));

        assert!(set.remove(10));
        assert!(set.is_empty());
        assert_eq!(set.max(), None);
    }

    #[test]
    fn with_capacity_and_growth() {
        let set: BitSet<usize> = BitSet::with_capacity(150);
        assert!(set.capacity() >= 150);

        let mut set = BitSet::new();
        let initial = set.capacity();

        set.insert(500);
        assert!(set.contains(500));
        assert!(set.capacity() > initial);
    }

    #[test]
    fn iteration_order() {
        let mut set = BitSet::new();
        set.insert(100);
        set.insert(5);
        set.insert(50);

        let elems: Vec<usize> = set.iter().collect();
        assert_eq!(elems, vec![5, 50, 100]);
    }

    #[test]
    fn display() {
        let mut set = BitSet::new();
        set.insert(1);
        set.insert(3);
        set.insert(2);

        assert_eq!(format!("{set}"), "{1, 2, 3}");
    }

    #[test]
    fn debug() {
        let mut set = BitSet::new();
        set.insert(1);
        set.insert(3);
        set.insert(2);

        assert_eq!(
            format!("{set:?}"),
            "BitSet { max: Some(3), words: [BitWord { bits: [1, 2, 3] }, BitWord { bits: [] }, BitWord { bits: [] }, BitWord { bits: [] }] }"
        );
    }
}
