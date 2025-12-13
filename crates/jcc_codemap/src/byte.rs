//! Byte position types for source code locations.
//!
//! This module provides the [`BytePos`] type, which represents a zero-indexed byte offset
//! into a source file. Byte positions are the foundation of the span system and are used
//! throughout the library to track locations in source code.
//!
//! # Why Byte Positions?
//!
//! Using byte offsets rather than character positions or line/column pairs offers several advantages:
//! - **Efficient**: Byte offsets are simple integers that can be compared and manipulated quickly
//! - **Stable**: They don't change when you calculate line/column information
//! - **Compact**: A single `u32` can represent any position in files up to 4GB
//! - **Direct slicing**: Byte positions map directly to string slice indices in Rust
//!
//! # Conversions
//!
//! `BytePos` can be created from `u32` or `usize` values.
//!
//! Note that converting from `usize` will saturate at `u32::MAX` if the value is too large.

use std::ops::{Add, Sub};

/// A zero-indexed byte offset into a source file.
///
/// This is the fundamental unit for tracking positions in source code.
/// All span operations work with byte positions, which are then converted
/// to line/column information for displaying diagnostics to the user.
#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub(crate) u32);

impl BytePos {
    /// The zero byte position.
    pub const ZERO: BytePos = BytePos(0);

    /// Converts the byte position to a `u32`.
    #[inline]
    pub fn to_u32(self) -> u32 {
        self.0
    }

    /// Converts the byte position to a `usize`.
    #[inline]
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl Add<u32> for BytePos {
    type Output = BytePos;

    #[inline]
    fn add(self, rhs: u32) -> BytePos {
        BytePos(self.0 + rhs)
    }
}

impl Sub for BytePos {
    type Output = u32;

    #[inline]
    fn sub(self, rhs: BytePos) -> u32 {
        self.0 - rhs.0
    }
}

impl From<u32> for BytePos {
    fn from(val: u32) -> Self {
        BytePos(val)
    }
}

impl From<usize> for BytePos {
    fn from(val: usize) -> Self {
        BytePos(u32::try_from(val).unwrap_or(u32::MAX))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_defaults_and_constants() {
        assert_eq!(BytePos::default(), BytePos(0));
        assert_eq!(BytePos::ZERO, BytePos(0));
        assert_eq!(BytePos::ZERO.to_u32(), 0);
    }

    #[test]
    fn test_arithmetic_add_u32() {
        let start = BytePos(10);
        let result = start + 5;
        assert_eq!(result, BytePos(15));
        assert_eq!(start, BytePos(10));
    }

    #[test]
    fn test_arithmetic_sub_bytepos() {
        let end = BytePos(20);
        let start = BytePos(5);
        let diff = end - start;
        assert_eq!(diff, 15u32);
    }

    #[test]
    #[should_panic]
    fn test_arithmetic_sub_underflow() {
        let p1 = BytePos(10);
        let p2 = BytePos(20);
        let _ = p1 - p2;
    }

    #[test]
    fn test_conversions_u32() {
        let pos = BytePos::from(42u32);
        assert_eq!(pos.0, 42);
        assert_eq!(pos.to_u32(), 42);
        assert_eq!(pos.to_usize(), 42);
    }

    #[test]
    fn test_conversions_usize_safe() {
        let val: usize = 1024;
        let pos = BytePos::from(val);
        assert_eq!(pos.to_u32(), 1024);
    }

    #[test]
    fn test_conversions_usize_saturating() {
        let large_val = (u32::MAX as usize) + 100;
        let pos = BytePos::from(large_val);
        assert_eq!(pos, BytePos(u32::MAX));
        assert_eq!(pos.to_u32(), u32::MAX);
    }

    #[test]
    fn test_ordering() {
        let p1 = BytePos(10);
        let p2 = BytePos(20);
        let p3 = BytePos(10);

        assert!(p1 < p2);
        assert!(p2 > p1);
        assert!(p1 <= p3);
        assert_eq!(p1, p3);
    }

    #[test]
    fn test_debug_formatting() {
        let p = BytePos(123);
        assert_eq!(format!("{:?}", p), "BytePos(123)");
    }
}
