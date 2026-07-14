//! Bitset implementations.
//!
//! This crate provides two distinct bitset implementations optimized for different
//! use cases, memory layouts, and performance characteristics:
//!
//! 1. [`word::BitWord`]: A highly efficient, stack-allocated bitset built on top of a
//!    fixed-size storage type (parameterized via the [`BitStorage`] trait).
//!
//! 2. [`set::BitSet`]: A arbitrarily large sized bitset capable of storing more bits
//!    than can fit into a single primitive type, utilizing internal heap allocations
//!    to grow as needed.

pub mod set;
pub mod word;

/// A trait implemented by all types that can
/// be used as backing storage of a [`word::BitWord`].
///
/// The trait is already implemented for `u{8, 16, 32, 64, 128}`.
pub trait BitStorage:
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
    /// The capacity of the set.
    const BITS: u8;

    /// Count the number of set bits.
    fn count_ones(self) -> u8;

    /// Count the number of leading zeros.
    fn leading_zeros(self) -> u8;

    /// Count the number of trailing zeros.
    fn trailing_zeros(self) -> u8;
}

macro_rules! impl_bitset {
    ( $int:ty ) => {
        impl BitStorage for $int {
            #[allow(clippy::cast_possible_truncation)]
            const BITS: u8 = {
                let cap = std::mem::size_of::<$int>() * 8;
                assert!(
                    cap <= u8::MAX as usize,
                    "The bit count exceeds a u8 container"
                );
                cap as u8
            };

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

#[cfg(test)]
mod tests {
    use crate::BitStorage;

    #[test]
    fn capacity() {
        assert_eq!(<u8 as BitStorage>::BITS, 8);
        assert_eq!(<u16 as BitStorage>::BITS, 16);
        assert_eq!(<u32 as BitStorage>::BITS, 32);
        assert_eq!(<u64 as BitStorage>::BITS, 64);
        assert_eq!(<u128 as BitStorage>::BITS, 128);
    }
}
