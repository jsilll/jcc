//! Dense matrices using fixed-width bitsets.

use crate::word::BitWord;

/// A trait for types that can provide a slice of words for a row of a [`BitMatrix`].
pub trait BitRowRef {
    /// Returns the underlying slice of words for this row.
    fn words(&self) -> &[BitWord<usize>];
}

/// A shared view over one row of a [`BitMatrix`].
///
/// Obtained via [`BitMatrix::row`]. Because it only holds a slice reference,
/// `BitRow` is `Copy` and its methods consume it by value so that iterators
/// can carry the full row lifetime `'a` rather than a shorter borrow lifetime.
#[derive(Copy, Clone)]
pub struct BitRow<'a>(&'a [BitWord<usize>]);

impl BitRowRef for BitRow<'_> {
    fn words(&self) -> &[BitWord<usize>] {
        self.0
    }
}

impl<'a> BitRow<'a> {
    /// Returns `true` if `bit` is set in this row.
    pub fn contains(self, bit: usize) -> bool {
        let word = bit / usize::BITS as usize;
        let offset = (bit % usize::BITS as usize) as u8;
        self.0.get(word).map_or(false, |w| w.contains(offset))
    }

    /// Iterates over the set bits in this row in ascending order.
    pub fn iter(self) -> impl Iterator<Item = usize> + 'a {
        self.0
            .iter()
            .enumerate()
            .flat_map(|(idx, word)| {
                let base = idx * usize::BITS as usize;
                word.iter().map(move |offset| base + offset as usize)
            })
    }
}

/// An exclusive view over one row of a [`BitMatrix`].
///
/// Obtained via [`BitMatrix::row_mut`] or [`BitMatrix::two_rows_mut`].
/// Derefs to [`BitRow`], so all read-only operations (`contains`, `iter`)
/// are available on `BitRowMut` without an explicit conversion.
pub struct BitRowMut<'a>(&'a mut [BitWord<usize>]);

impl BitRowRef for BitRowMut<'_> {
    fn words(&self) -> &[BitWord<usize>] {
        self.0
    }
}

impl<'a> BitRowMut<'a> {
    /// Returns `true` if `bit` is set in this row.
    pub fn contains(self, bit: usize) -> bool {
        let word = bit / usize::BITS as usize;
        let offset = (bit % usize::BITS as usize) as u8;
        self.0.get(word).map_or(false, |w| w.contains(offset))
    }

    /// Iterates over the set bits in this row in ascending order.
    pub fn iter(self) -> impl Iterator<Item = usize> + 'a {
        self.0
            .iter()
            .enumerate()
            .flat_map(|(idx, word)| {
                let base = idx * usize::BITS as usize;
                word.iter().map(move |offset| base + offset as usize)
            })
    }

    /// Inserts `bit` into this row, returning `true` if the row changed.
    pub fn insert(&mut self, bit: usize) -> bool {
        let word = bit / usize::BITS as usize;
        let offset = (bit % usize::BITS as usize) as u8;
        self.0.get_mut(word).map_or(false, |w| w.insert(offset))
    }

    /// `self |= other` — returns `true` if any bit changed.
    pub fn union_with(&mut self, other: impl BitRowRef) -> bool {
        let mut changed = false;
        for (dst, src) in self.0.iter_mut().zip(other.words().iter()) {
            let old = *dst;
            *dst |= *src;
            changed |= *dst != old;
        }
        changed
    }

    /// `self |= (source & !exclude)` — returns `true` if any bit changed.
    pub fn union_with_difference(
        &mut self,
        source: impl BitRowRef,
        exclude: impl BitRowRef,
    ) -> bool {
        self.0
            .iter_mut()
            .zip(source.words().iter().zip(exclude.words().iter()))
            .fold(false, |changed, (dst, (src, excl))| {
                let old = *dst;
                *dst |= *src & !*excl;
                (*dst != old) || changed
            })
    }
}

/// A row-major matrix of fixed-width bitsets.
///
/// Every row has the same number of bits and shares one contiguous backing
/// allocation. It is intended for dataflow facts indexed by dense local IDs.
pub struct BitMatrix {
    rows: usize,
    words_per_row: usize,
    data: Vec<BitWord<usize>>,
}

impl BitMatrix {
    /// Creates an all-zero matrix with `rows` rows and `bits_per_row` bits per row.
    pub fn new(rows: usize, bits_per_row: usize) -> Self {
        let words_per_row = bits_per_row.div_ceil(usize::BITS as usize);
        let len = rows
            .checked_mul(words_per_row)
            .expect("matrix dimensions overflow");
        Self {
            rows,
            words_per_row,
            data: vec![BitWord::default(); len],
        }
    }

    /// Returns whether `row` contains `bit`.
    ///
    /// Returns `false` if either index is out of bounds.
    pub fn contains(&self, row: usize, bit: usize) -> bool {
        self.row(row).map_or(false, |r| r.contains(bit))
    }

    /// Inserts `bit` into `row`, returning whether the row changed.
    ///
    /// Returns `false` if the row is unchanged or the index is out of bounds.
    pub fn insert(&mut self, row: usize, bit: usize) -> bool {
        self.row_mut(row).map_or(false, |mut r| r.insert(bit))
    }

    /// Returns a shared view over `row`, or `None` if `row` is out of bounds.
    pub fn row(&self, row: usize) -> Option<BitRow<'_>> {
        if row >= self.rows {
            return None;
        }
        let start = row * self.words_per_row;
        let end = start + self.words_per_row;
        Some(BitRow(&self.data[start..end]))
    }

    /// Returns an exclusive view over `row`, or `None` if `row` is out of bounds.
    pub fn row_mut(&mut self, row: usize) -> Option<BitRowMut<'_>> {
        if row >= self.rows {
            return None;
        }
        let start = row * self.words_per_row;
        let end = start + self.words_per_row;
        Some(BitRowMut(&mut self.data[start..end]))
    }

    /// Returns exclusive views over two distinct rows simultaneously.
    ///
    /// The tuple elements correspond to `lhs` and `rhs` in argument order.
    ///
    /// # Panics
    ///
    /// Panics if either index is out of bounds or if `lhs == rhs`.
    pub fn two_rows_mut(&mut self, lhs: usize, rhs: usize) -> (BitRowMut<'_>, BitRowMut<'_>) {
        assert!(lhs < self.rows && rhs < self.rows, "row index out of bounds");
        assert_ne!(lhs, rhs, "rows must differ");
        let width = self.words_per_row;
        let (wa, wb) = if lhs < rhs {
            let (left, right) = self.data.split_at_mut(rhs * width);
            (&mut left[lhs * width..][..width], &mut right[..width])
        } else {
            let (left, right) = self.data.split_at_mut(lhs * width);
            (&mut right[..width], &mut left[rhs * width..][..width])
        };
        (BitRowMut(wa), BitRowMut(wb))
    }
}
