//! A compact bitset backed by a single machine word.
//!
//! This module provides [`BitWord`], a fixed-capacity set whose items are
//! represented as individual bits of an unsigned integer. It is intended as the
//! building block for larger bitset data structures, but is also useful on its
//! own whenever the universe of possible values fits within a single integer.
//!
//! A `BitWord<T>` stores the integers `0..T::BITS`, where each bit of the
//! underlying [`BitStorage`] value corresponds to one item. Membership tests,
//! insertion, removal, and queries for the smallest or largest item all
//! compile down to efficient bitwise operations.
//!
//! Iteration always yields items in ascending order, and the iterator also
//! implements [`DoubleEndedIterator`] to support reverse iteration.

use crate::BitStorage;

/// A bit word.
///
/// # Note
///
/// The underlying storage type must not exceed [`u8::MAX`] bits in size.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BitWord<T>(T);

impl<T> Default for BitWord<T>
where
    T: BitStorage,
{
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: BitStorage> From<T> for BitWord<T> {
    fn from(bits: T) -> Self {
        Self(bits)
    }
}

impl<T> IntoIterator for BitWord<T>
where
    T: BitStorage,
{
    type Item = u8;
    type IntoIter = Iter<T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> IntoIterator for &BitWord<T>
where
    T: BitStorage,
{
    type Item = u8;
    type IntoIter = Iter<T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> std::fmt::Display for BitWord<T>
where
    T: BitStorage,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut snd = 0;
        for idx in 0..T::BITS {
            if self.contains(idx) {
                write!(f, "{idx}")?;
                snd = idx + 1;
                break;
            }
        }
        for idx in snd..T::BITS {
            if self.contains(idx) {
                write!(f, ", {idx}")?;
            }
        }
        write!(f, "}}")
    }
}

impl<T> std::fmt::Debug for BitWord<T>
where
    T: BitStorage,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Bits<'a, T>(&'a BitWord<T>);
        impl<T> std::fmt::Debug for Bits<'_, T>
        where
            T: BitStorage,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut l = f.debug_list();
                for i in 0..T::BITS {
                    if self.0.contains(i) {
                        l.entry(&i);
                    }
                }
                l.finish()
            }
        }
        let mut s = f.debug_struct("BitWord");
        s.field("bits", &Bits(self));
        s.finish()
    }
}

impl<T: BitStorage> BitWord<T> {
    /// Create a new, empty word.
    #[inline]
    pub fn new() -> Self {
        Self(T::from(0))
    }

    /// Returns the number of items in the word.
    #[inline]
    pub fn len(&self) -> u8 {
        self.0.count_ones()
    }

    /// Returns `true` if the word contains no items.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0 == T::from(0)
    }

    /// Return the largest item in the word or `None` if empty.
    #[inline]
    pub fn max(&self) -> Option<u8> {
        if self.0 == T::from(0) {
            None
        } else {
            let lz = self.0.leading_zeros();
            Some(T::BITS - lz - 1)
        }
    }

    /// Return the smallest item in this word or `None` if empty.
    #[inline]
    pub fn min(&self) -> Option<u8> {
        if self.0 == T::from(0) {
            None
        } else {
            Some(self.0.trailing_zeros())
        }
    }

    /// Returns `true` if the word contains item `idx`.
    ///
    /// # Panics
    ///
    /// Panics if `idx` is out of bounds.
    #[inline]
    pub fn contains(&self, idx: u8) -> bool {
        assert!(idx < T::BITS, "index out of bounds");
        (self.0 & (T::from(1) << idx)) != T::from(0)
    }

    /// Removes all items from the word.
    #[inline]
    pub fn clear(&mut self) {
        self.0 = T::from(0);
    }

    /// Remove and returns the largest item in the word.
    #[inline]
    pub fn pop_max(&mut self) -> Option<u8> {
        let max = self.max()?;
        self.remove(max);
        Some(max)
    }

    /// Removes and returns the smallest item in the word.
    #[inline]
    pub fn pop_min(&mut self) -> Option<u8> {
        let min = self.min()?;
        self.remove(min);
        Some(min)
    }

    /// Removes item `idx` from the word.
    ///
    /// Returns whether the item was in the word.
    #[inline]
    pub fn remove(&mut self, idx: u8) -> bool {
        let was_present = self.contains(idx);
        self.0 = self.0 & !(T::from(1) << idx);
        was_present
    }

    /// Inserts item `idx` into the word.
    ///
    /// Returns whether the item was newly inserted.
    #[inline]
    pub fn insert(&mut self, idx: u8) -> bool {
        let is_new = !self.contains(idx);
        self.0 = self.0 | (T::from(1) << idx);
        is_new
    }

    /// Iterate over the items in the word.
    ///
    /// Note: Items are always yielded in sorted order.
    #[inline]
    pub fn iter(self) -> Iter<T> {
        Iter { word: self }
    }
}

/// An iterator over the items in a [`BitWord`].
pub struct Iter<T> {
    word: BitWord<T>,
}

impl<T> Iterator for Iter<T>
where
    T: BitStorage,
{
    type Item = u8;
    #[inline]
    fn next(&mut self) -> Option<u8> {
        self.word.pop_min()
    }
}

impl<T> ExactSizeIterator for Iter<T>
where
    T: BitStorage,
{
    #[inline]
    fn len(&self) -> usize {
        usize::from(self.word.len())
    }
}

impl<T> DoubleEndedIterator for Iter<T>
where
    T: BitStorage,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.word.pop_max()
    }
}

#[cfg(test)]
mod tests {
    use crate::{BitStorage, word::BitWord};

    #[test]
    #[should_panic]
    fn out_of_bounds() {
        let word = BitWord::<u8>::new();
        word.contains(8);
    }

    #[test]
    fn from() {
        // 10u8: 0000_1010 = {1, 3}
        let word = BitWord::<u8>::from(10u8);
        assert_eq!(word.len(), 2);
        assert!(word.contains(1));
        assert!(word.contains(3));
    }

    #[test]
    fn new() {
        let word1 = BitWord::<u32>::new();
        let word2 = BitWord::<u32>::default();
        assert_eq!(word1, word2);
        assert!(word1.is_empty());
        assert!(word2.is_empty());
        assert_eq!(word1.len(), 0);
        assert_eq!(word2.len(), 0);
    }

    #[test]
    fn insert_and_contains() {
        let mut word = BitWord::<u32>::new();
        assert!(word.insert(5));
        assert!(!word.insert(5));
        assert!(word.contains(5));
        assert_eq!(word.len(), 1);
        assert!(!word.is_empty());
    }

    #[test]
    fn clear() {
        let mut word = BitWord::<u8>::new();
        word.insert(1);
        word.insert(7);
        word.clear();
        assert!(word.is_empty());
        assert_eq!(word.len(), 0);
    }

    #[test]
    fn remove() {
        let mut word = BitWord::<u16>::new();
        word.insert(2);
        word.insert(8);
        assert!(word.remove(2));
        assert!(!word.remove(2));
        assert!(!word.contains(2));
        assert!(word.contains(8));
        assert_eq!(word.len(), 1);
    }

    #[test]
    fn min_max() {
        let mut word = BitWord::<u64>::new();
        assert_eq!(word.min(), None);
        assert_eq!(word.max(), None);

        word.insert(10);
        assert_eq!(word.min(), Some(10));
        assert_eq!(word.max(), Some(10));

        word.insert(5);
        word.insert(42);
        assert_eq!(word.min(), Some(5));
        assert_eq!(word.max(), Some(42));
    }

    #[test]
    fn pop_min_max() {
        let mut word = BitWord::<u32>::new();
        word.insert(3);
        word.insert(15);
        word.insert(7);

        assert_eq!(word.pop_min(), Some(3));
        assert_eq!(word.len(), 2);
        assert!(!word.contains(3));

        assert_eq!(word.pop_max(), Some(15));
        assert_eq!(word.len(), 1);
        assert!(!word.contains(15));

        assert_eq!(word.pop_max(), Some(7));
        assert!(word.is_empty());

        assert_eq!(word.pop_min(), None);
        assert_eq!(word.pop_max(), None);
    }

    #[test]
    fn boundaries() {
        let mut word = BitWord::<u8>::new();
        let max = <u8 as BitStorage>::BITS - 1;
        word.insert(0);
        word.insert(max);
        assert!(word.contains(0));
        assert!(word.contains(max));
        assert_eq!(word.min(), Some(0));
        assert_eq!(word.max(), Some(max));
    }

    #[test]
    fn iterator_forward() {
        let mut word = BitWord::<u16>::new();
        word.insert(1);
        word.insert(5);
        word.insert(9);

        let mut iter = word.into_iter();
        assert_eq!(iter.len(), 3);
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), Some(9));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.len(), 0);
    }

    #[test]
    fn iterator_double_ended() {
        let mut word = BitWord::<u16>::new();
        word.insert(2);
        word.insert(4);
        word.insert(6);
        word.insert(8);
        let mut iter = word.into_iter();

        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next_back(), Some(8));

        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next_back(), Some(6));

        assert_eq!(iter.next(), None);
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn debug() {
        let mut word = BitWord::<u8>::new();
        word.insert(1);
        word.insert(4);
        assert_eq!(format!("{:?}", word), "BitWord { bits: [1, 4] }");
    }

    #[test]
    fn display() {
        let mut word = BitWord::<u8>::new();
        assert_eq!(format!("{}", word), "{}");

        word.insert(0);
        assert_eq!(format!("{}", word), "{0}");

        word.insert(3);
        word.insert(7);
        assert_eq!(format!("{}", word), "{0, 3, 7}");
    }
}
