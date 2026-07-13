//! Bitset implementations.
//!
//! This crate provides two distinct bitset implementations optimized for different
//! use cases, memory layouts, and performance characteristics:
//!
//! 1. [`BitSet`]: A highly efficient, stack-allocated bitset built on top of a
//!    fixed-size, non-heap-allocating storage type (parameterized via the
//!    [`BitSetStorage`] trait). It is typically backed by standard primitive integers.
//!
//! 2. [`CompoundBitSet`]: A dynamically sized bitset capable of storing more bits
//!    than can fit into a single primitive type, utilizing internal heap allocations
//!    to grow as needed.
//!
//! Instead of binding the small bitset strictly to raw integers, `BitSet<T>` is generic
//! over any type implementing the [`BitSetStorage`] trait. By default, this trait is
//! implemented for all standard unsigned primitives (`u8`, `u16`, `u32`, `u64`, `u128`)
//! as well as `usize`.

/// A trait implemented by all types that can
/// be used as backing storage of a [`BitSet`].
///
/// The trait is already implemented for `u{8, 16, 32, 64, 128}`.
pub trait BitSetStorage:
    Copy
    + Default
    + PartialEq
    + From<u8>
    + std::ops::Add<Output = Self>
    + std::ops::Sub<Output = Self>
    + std::ops::Not<Output = Self>
    + std::ops::BitOr<Output = Self>
    + std::ops::BitAnd<Output = Self>
    + std::ops::Shl<u8, Output = Self>
    + std::ops::Shr<u8, Output = Self>
{
    /// Count the number of set bits.
    fn count_ones(self) -> u8;

    /// Count the number of leading zeros.
    fn leading_zeros(self) -> u8;

    /// Count the number of trailing zeros.
    fn trailing_zeros(self) -> u8;
}

macro_rules! impl_bitset {
    ( $int:ty ) => {
        impl BitSetStorage for $int {
            #[inline]
            fn count_ones(self) -> u8 {
                u8::try_from(self.count_ones()).unwrap()
            }

            #[inline]
            fn leading_zeros(self) -> u8 {
                u8::try_from(self.leading_zeros()).unwrap()
            }

            #[inline]
            fn trailing_zeros(self) -> u8 {
                u8::try_from(self.trailing_zeros()).unwrap()
            }
        }
    };
}

impl_bitset!(u8);
impl_bitset!(u16);
impl_bitset!(u32);
impl_bitset!(u64);
impl_bitset!(u128);
impl_bitset!(usize);

/// A small bitset.
///
/// # Note
///
/// The underlying storage type must not exceed [`u8::MAX`] bits in size.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BitSet<T>(T);

impl<T> Default for BitSet<T>
where
    T: BitSetStorage,
{
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: BitSetStorage> From<T> for BitSet<T> {
    fn from(bits: T) -> Self {
        Self(bits)
    }
}

impl<T> IntoIterator for BitSet<T>
where
    T: BitSetStorage,
{
    type Item = u8;
    type IntoIter = Iter<T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> IntoIterator for &BitSet<T>
where
    T: BitSetStorage,
{
    type Item = u8;
    type IntoIter = Iter<T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> std::fmt::Display for BitSet<T>
where
    T: BitSetStorage,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut snd = 0;
        for idx in 0..Self::capacity() {
            if self.contains(idx) {
                write!(f, "{idx}")?;
                snd = idx + 1;
                break;
            }
        }
        for idx in snd..Self::capacity() {
            if self.contains(idx) {
                write!(f, ", {idx}")?;
            }
        }
        write!(f, "}}")
    }
}

impl<T> std::fmt::Debug for BitSet<T>
where
    T: BitSetStorage,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Bits<'a, T>(&'a BitSet<T>);
        impl<T> std::fmt::Debug for Bits<'_, T>
        where
            T: BitSetStorage,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut l = f.debug_list();
                for i in 0..BitSet::<T>::capacity() {
                    if self.0.contains(i) {
                        l.entry(&i);
                    }
                }
                l.finish()
            }
        }
        let mut s = f.debug_struct("BitSet");
        s.field("bits", &Bits(self));
        s.finish()
    }
}

impl<T> BitSet<T>
where
    T: BitSetStorage,
{
    /// Create a new, empty set.
    #[inline]
    pub fn new() -> Self {
        Self(T::from(0))
    }

    /// Returns maximum number of items that can be stored.
    ///
    /// # Panics
    ///
    /// Panics if the size of `T` is greater than [`u8::MAX`].
    #[inline]
    #[allow(clippy::expect_used)]
    pub fn capacity() -> u8 {
        u8::try_from(size_of::<T>())
            .expect("BitSet storage type exceeds u8::MAX bytes")
            .checked_mul(8)
            .expect("BitSet capacity exceeds u8::MAX")
    }

    /// Returns the number of items in the set.
    #[inline]
    pub fn len(&self) -> u8 {
        self.0.count_ones()
    }

    /// Returns `true` if the set contains no items.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0 == T::from(0)
    }

    /// Return the largest item in the set or `None` if empty.
    #[inline]
    pub fn max(&self) -> Option<u8> {
        if self.0 == T::from(0) {
            None
        } else {
            let lz = self.0.leading_zeros();
            Some(Self::capacity() - lz - 1)
        }
    }

    /// Return the smallest item in this set or `None` if empty.
    #[inline]
    pub fn min(&self) -> Option<u8> {
        if self.0 == T::from(0) {
            None
        } else {
            Some(self.0.trailing_zeros())
        }
    }

    /// Returns `true` if the set contains item `idx`.
    ///
    /// # Panics
    ///
    /// Panics if `idx` is out of bounds.
    #[inline]
    pub fn contains(&self, idx: u8) -> bool {
        assert!(idx < Self::capacity());
        (self.0 & (T::from(1) << idx)) != T::from(0)
    }

    /// Removes all entries from the set.
    #[inline]
    pub fn clear(&mut self) {
        self.0 = T::from(0);
    }

    /// Remove and returns the largest item in the set.
    #[inline]
    pub fn pop_max(&mut self) -> Option<u8> {
        let max = self.max()?;
        self.remove(max);
        Some(max)
    }

    /// Removes and returns the smallest item in the set.
    #[inline]
    pub fn pop_min(&mut self) -> Option<u8> {
        let min = self.min()?;
        self.remove(min);
        Some(min)
    }

    /// Removes item `idx` from the set.
    ///
    /// Returns whether the item was in the set.
    #[inline]
    pub fn remove(&mut self, idx: u8) -> bool {
        let was_present = self.contains(idx);
        self.0 = self.0 & !(T::from(1) << idx);
        was_present
    }

    /// Inserts item `idx` into the set.
    ///
    /// Returns whether the item was newly inserted.
    pub fn insert(&mut self, idx: u8) -> bool {
        let is_new = !self.contains(idx);
        self.0 = self.0 | (T::from(1) << idx);
        is_new
    }

    /// Iterate over the items in the set.
    ///
    /// Note: Items are always yielded in sorted order.
    #[inline]
    pub fn iter(self) -> Iter<T> {
        Iter { bitset: self }
    }
}

/// An iterator over the items in a [`BitSet`].
pub struct Iter<T> {
    bitset: BitSet<T>,
}

impl<T> Iterator for Iter<T>
where
    T: BitSetStorage,
{
    type Item = u8;
    #[inline]
    fn next(&mut self) -> Option<u8> {
        self.bitset.pop_min()
    }
}

impl<T> ExactSizeIterator for Iter<T>
where
    T: BitSetStorage,
{
    #[inline]
    fn len(&self) -> usize {
        usize::from(self.bitset.len())
    }
}

impl<T> DoubleEndedIterator for Iter<T>
where
    T: BitSetStorage,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.bitset.pop_max()
    }
}

#[cfg(test)]
mod tests {
    use crate::BitSet;

    #[test]
    #[should_panic]
    fn out_of_bounds() {
        let set = BitSet::<u8>::new();
        set.contains(8);
    }

    #[test]
    fn from() {
        // 10u8: 0000_1010 = {1, 3}
        let set = BitSet::<u8>::from(10u8);
        assert_eq!(set.len(), 2);
        assert!(set.contains(1));
        assert!(set.contains(3));
    }

    #[test]
    fn capacity() {
        assert_eq!(BitSet::<u8>::capacity(), 8);
        assert_eq!(BitSet::<u16>::capacity(), 16);
        assert_eq!(BitSet::<u32>::capacity(), 32);
        assert_eq!(BitSet::<u64>::capacity(), 64);
        assert_eq!(BitSet::<u128>::capacity(), 128);
    }

    #[test]
    fn new() {
        let set1 = BitSet::<u32>::new();
        let set2 = BitSet::<u32>::default();
        assert_eq!(set1, set2);
        assert!(set1.is_empty());
        assert!(set2.is_empty());
        assert_eq!(set1.len(), 0);
        assert_eq!(set2.len(), 0);
    }

    #[test]
    fn insert_and_contains() {
        let mut set = BitSet::<u32>::new();
        assert!(set.insert(5));
        assert!(!set.insert(5));
        assert!(set.contains(5));
        assert_eq!(set.len(), 1);
        assert!(!set.is_empty());
    }

    #[test]
    fn clear() {
        let mut set = BitSet::<u8>::new();
        set.insert(1);
        set.insert(7);
        set.clear();
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
    }

    #[test]
    fn remove() {
        let mut set = BitSet::<u16>::new();
        set.insert(2);
        set.insert(8);
        assert!(set.remove(2));
        assert!(!set.remove(2));
        assert!(!set.contains(2));
        assert!(set.contains(8));
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn min_max() {
        let mut set = BitSet::<u64>::new();
        assert_eq!(set.min(), None);
        assert_eq!(set.max(), None);

        set.insert(10);
        assert_eq!(set.min(), Some(10));
        assert_eq!(set.max(), Some(10));

        set.insert(5);
        set.insert(42);
        assert_eq!(set.min(), Some(5));
        assert_eq!(set.max(), Some(42));
    }

    #[test]
    fn pop_min_max() {
        let mut set = BitSet::<u32>::new();
        set.insert(3);
        set.insert(15);
        set.insert(7);

        assert_eq!(set.pop_min(), Some(3));
        assert_eq!(set.len(), 2);
        assert!(!set.contains(3));

        assert_eq!(set.pop_max(), Some(15));
        assert_eq!(set.len(), 1);
        assert!(!set.contains(15));

        assert_eq!(set.pop_max(), Some(7));
        assert!(set.is_empty());

        assert_eq!(set.pop_min(), None);
        assert_eq!(set.pop_max(), None);
    }

    #[test]
    fn boundaries() {
        let mut set = BitSet::<u8>::new();
        let max = BitSet::<u8>::capacity() - 1;
        set.insert(0);
        set.insert(max);
        assert!(set.contains(0));
        assert!(set.contains(max));
        assert_eq!(set.min(), Some(0));
        assert_eq!(set.max(), Some(max));
    }

    #[test]
    fn iterator_forward() {
        let mut set = BitSet::<u16>::new();
        set.insert(1);
        set.insert(5);
        set.insert(9);

        let mut iter = set.into_iter();
        assert_eq!(iter.len(), 3);
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), Some(9));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.len(), 0);
    }

    #[test]
    fn iterator_double_ended() {
        let mut set = BitSet::<u16>::new();
        set.insert(2);
        set.insert(4);
        set.insert(6);
        set.insert(8);
        let mut iter = set.into_iter();

        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next_back(), Some(8));

        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next_back(), Some(6));

        assert_eq!(iter.next(), None);
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn debug() {
        let mut set = BitSet::<u8>::new();
        set.insert(1);
        set.insert(4);
        assert_eq!(format!("{:?}", set), "BitSet { bits: [1, 4] }");
    }

    #[test]
    fn display() {
        let mut set = BitSet::<u8>::new();
        assert_eq!(format!("{}", set), "{}");

        set.insert(0);
        assert_eq!(format!("{}", set), "{0}");

        set.insert(3);
        set.insert(7);
        assert_eq!(format!("{}", set), "{0, 3, 7}");
    }
}
