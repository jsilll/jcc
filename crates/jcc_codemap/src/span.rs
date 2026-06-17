//! Source code spans representing regions of text.
//!
//! This module provides the [`Span`] type, which represents a contiguous region of source
//! code as a half-open range of byte positions: `[start, end)`. Spans are the primary way
//! to track the location of language constructs in your compiler or interpreter.
//!
//! # Overview
//!
//! A [`Span`] is a lightweight, copyable type that stores two [`BytePos`] values:
//! - A start position (inclusive)
//! - An end position (exclusive)
//!
//! # Design Rationale
//!
//! Spans use byte positions rather than line/column pairs because:
//! - They're more compact (8 bytes vs 16+ bytes with file ID)
//! - They're faster to compare and manipulate
//! - They can be converted to line/column information on demand
//! - They work naturally with Rust's string slicing
//!
//! The half-open interval `[start, end)` convention ensures that:
//! - Empty spans are well-defined (`start == end`)
//! - Adjacent spans can share a boundary without overlap
//! - The length is always `end - start`

use std::ops::Range;

use crate::byte::BytePos;

/// A region of source code.
///
/// Spans are half-open ranges [start, end) of byte positions.
/// They are cheap to copy and should be used liberally throughout
/// your AST and IR structures.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Span {
    start: BytePos,
    end: BytePos,
}

impl Span {
    /// A default span representing no source code location.
    pub const DEFAULT: Self = Self {
        start: BytePos(0),
        end: BytePos(0),
    };

    /// Creates a new span from a start and end position.
    ///
    /// Returns None if start > end.
    #[inline]
    pub fn new(start: impl Into<BytePos>, end: impl Into<BytePos>) -> Option<Self> {
        let start = start.into();
        let end = end.into();
        if start.0 > end.0 {
            None
        } else {
            Some(Self { start, end })
        }
    }

    /// Creates an empty span at the given position.
    #[inline]
    pub fn empty(pos: impl Into<BytePos>) -> Self {
        let pos = pos.into();
        Self {
            start: pos,
            end: pos,
        }
    }

    /// Creates a span covering a single byte position.
    #[inline]
    pub fn single(pos: impl Into<BytePos>) -> Self {
        let pos = pos.into();
        Self {
            start: pos,
            end: pos + 1,
        }
    }

    /// Returns the start byte position.
    #[inline]
    pub const fn start(&self) -> BytePos {
        self.start
    }

    /// Returns the end byte position.
    #[inline]
    pub const fn end(&self) -> BytePos {
        self.end
    }

    /// Returns the first byte position in this span, or None if empty.
    #[inline]
    pub fn first(&self) -> Option<BytePos> {
        if self.is_empty() {
            None
        } else {
            Some(self.start)
        }
    }

    /// Returns the last byte position in this span (end - 1), or None if empty.
    #[inline]
    pub fn last(&self) -> Option<BytePos> {
        if self.is_empty() {
            None
        } else {
            Some(BytePos(self.end.0 - 1))
        }
    }

    /// Returns the length of the span in bytes.
    #[inline]
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    /// Returns true if the span is empty (start == end).
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.start.0 == self.end.0
    }

    /// Checks if this span overlaps with another span.
    #[inline]
    pub fn overlaps(&self, other: Span) -> bool {
        self.start < other.end && other.start < self.end
    }

    /// Checks if this span contains the entire other span.
    #[inline]
    pub fn contains(&self, other: Span) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// Checks if this span contains the given byte position.
    #[inline]
    pub fn contains_byte(&self, pos: BytePos) -> bool {
        self.start <= pos && pos < self.end
    }

    /// Returns a new span with the given offset added to both start and end.
    #[inline]
    pub fn offset(self, offset: u32) -> Self {
        Self {
            start: self.start + offset,
            end: self.end + offset,
        }
    }

    /// Extends this span to include another position.
    #[inline]
    pub fn extend_to(self, pos: BytePos) -> Self {
        Self {
            start: BytePos(self.start.0.min(pos.0)),
            end: BytePos(self.end.0.max(pos.0)),
        }
    }

    /// Merges two spans into a span that covers both.
    ///
    /// The resulting span starts at the minimum start position
    /// and ends at the maximum end position.
    #[inline]
    pub fn merge(self, other: Self) -> Self {
        Self {
            start: BytePos(self.start.0.min(other.start.0)),
            end: BytePos(self.end.0.max(other.end.0)),
        }
    }

    /// Shrinks the span from both ends by the given amount.
    ///
    /// Returns None if the resulting span would be invalid.
    pub fn shrink(self, amount: u32) -> Option<Self> {
        let start = self.start + amount;
        let end = BytePos(self.end.0.saturating_sub(amount));
        Self::new(start, end)
    }

    /// Shrinks the span from the left by the given amount.
    ///
    /// Returns None if the resulting span would be invalid.
    pub fn shrink_left(self, amount: u32) -> Option<Self> {
        let start = self.start + amount;
        Self::new(start, self.end)
    }

    /// Shrinks the span from the right by the given amount.
    ///
    /// Returns None if the resulting span would be invalid.
    pub fn shrink_right(self, amount: u32) -> Option<Self> {
        let end = BytePos(self.end.0.saturating_sub(amount));
        Self::new(self.start, end)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        Range {
            start: span.start.to_usize(),
            end: span.end.to_usize(),
        }
    }
}

impl From<Span> for Range<BytePos> {
    fn from(span: Span) -> Self {
        Range {
            start: span.start,
            end: span.end,
        }
    }
}

impl TryFrom<Range<u32>> for Span {
    type Error = ();

    fn try_from(range: Range<u32>) -> Result<Self, Self::Error> {
        Self::new(range.start, range.end).ok_or(())
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn new_valid_span() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert_eq!(span.start(), BytePos(10));
        assert_eq!(span.end(), BytePos(20));
        assert_eq!(span.len(), 10);
    }

    #[test]
    fn new_equal_positions() {
        let span = Span::new(10u32, 10u32).unwrap();
        assert!(span.is_empty());
        assert_eq!(span.len(), 0);
    }

    #[test]
    fn new_invalid_span() {
        // start > end should return None
        assert!(Span::new(20u32, 10u32).is_none());
    }

    #[test]
    fn empty_span() {
        let span = Span::empty(10u32);
        assert!(span.is_empty());
        assert_eq!(span.len(), 0);
        assert_eq!(span.start(), BytePos(10));
        assert_eq!(span.end(), BytePos(10));
    }

    #[test]
    fn single_span() {
        let span = Span::single(10u32);
        assert!(!span.is_empty());
        assert_eq!(span.len(), 1);
        assert_eq!(span.start(), BytePos(10));
        assert_eq!(span.end(), BytePos(11));
    }

    #[test]
    fn default_span() {
        let span = Span::DEFAULT;
        assert!(span.is_empty());
        assert_eq!(span.start(), BytePos(0));
        assert_eq!(span.end(), BytePos(0));
    }

    #[test]
    fn first_nonempty() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert_eq!(span.first(), Some(BytePos(10)));
    }

    #[test]
    fn first_empty() {
        let span = Span::empty(10u32);
        assert_eq!(span.first(), None);
    }

    #[test]
    fn last_nonempty() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert_eq!(span.last(), Some(BytePos(19)));
    }

    #[test]
    fn last_single() {
        let span = Span::single(10u32);
        assert_eq!(span.last(), Some(BytePos(10)));
    }

    #[test]
    fn last_empty() {
        let span = Span::empty(10u32);
        assert_eq!(span.last(), None);
    }

    #[test]
    fn contains_byte_in_middle() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert!(span.contains_byte(BytePos(15)));
    }

    #[test]
    fn contains_byte_at_start() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert!(span.contains_byte(BytePos(10)));
    }

    #[test]
    fn contains_byte_at_end() {
        let span = Span::new(10u32, 20u32).unwrap();
        // Half-open interval: end is exclusive
        assert!(!span.contains_byte(BytePos(20)));
    }

    #[test]
    fn contains_byte_before() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert!(!span.contains_byte(BytePos(9)));
    }

    #[test]
    fn contains_byte_after() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert!(!span.contains_byte(BytePos(21)));
    }

    #[test]
    fn contains_byte_empty_span() {
        let span = Span::empty(10u32);
        assert!(!span.contains_byte(BytePos(10)));
    }

    #[test]
    fn contains_span_identical() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(10u32, 20u32).unwrap();
        assert!(span1.contains(span2));
    }

    #[test]
    fn contains_span_inside() {
        let outer = Span::new(10u32, 30u32).unwrap();
        let inner = Span::new(15u32, 25u32).unwrap();
        assert!(outer.contains(inner));
        assert!(!inner.contains(outer));
    }

    #[test]
    fn contains_span_at_boundaries() {
        let outer = Span::new(10u32, 30u32).unwrap();
        let inner = Span::new(10u32, 30u32).unwrap();
        assert!(outer.contains(inner));
    }

    #[test]
    fn contains_span_partial_overlap() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(15u32, 25u32).unwrap();
        assert!(!span1.contains(span2));
        assert!(!span2.contains(span1));
    }

    #[test]
    fn contains_empty_span() {
        let span = Span::new(10u32, 20u32).unwrap();
        let empty = Span::empty(15u32);
        assert!(span.contains(empty));
    }

    #[test]
    fn overlaps_partial() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(15u32, 25u32).unwrap();
        assert!(span1.overlaps(span2));
        assert!(span2.overlaps(span1));
    }

    #[test]
    fn overlaps_contained() {
        let outer = Span::new(10u32, 30u32).unwrap();
        let inner = Span::new(15u32, 25u32).unwrap();
        assert!(outer.overlaps(inner));
        assert!(inner.overlaps(outer));
    }

    #[test]
    fn overlaps_adjacent_no_overlap() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(20u32, 30u32).unwrap();
        // Adjacent spans don't overlap (half-open intervals)
        assert!(!span1.overlaps(span2));
        assert!(!span2.overlaps(span1));
    }

    #[test]
    fn overlaps_separated() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(25u32, 35u32).unwrap();
        assert!(!span1.overlaps(span2));
        assert!(!span2.overlaps(span1));
    }

    #[test]
    fn overlaps_empty_spans() {
        let empty1 = Span::empty(15u32);
        let empty2 = Span::empty(15u32);
        assert!(!empty1.overlaps(empty2));
    }

    #[test]
    fn offset_positive() {
        let span = Span::new(10u32, 20u32).unwrap();
        let offset = span.offset(5);
        assert_eq!(offset.start(), BytePos(15));
        assert_eq!(offset.end(), BytePos(25));
        assert_eq!(offset.len(), span.len());
    }

    #[test]
    fn offset_zero() {
        let span = Span::new(10u32, 20u32).unwrap();
        let offset = span.offset(0);
        assert_eq!(offset, span);
    }

    #[test]
    fn extend_to_after() {
        let span = Span::new(10u32, 20u32).unwrap();
        let extended = span.extend_to(BytePos(25));
        assert_eq!(extended.start(), BytePos(10));
        assert_eq!(extended.end(), BytePos(25));
    }

    #[test]
    fn extend_to_before() {
        let span = Span::new(10u32, 20u32).unwrap();
        let extended = span.extend_to(BytePos(5));
        assert_eq!(extended.start(), BytePos(5));
        assert_eq!(extended.end(), BytePos(20));
    }

    #[test]
    fn extend_to_inside() {
        let span = Span::new(10u32, 20u32).unwrap();
        let extended = span.extend_to(BytePos(15));
        assert_eq!(extended, span);
    }

    #[test]
    fn merge_overlapping() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(15u32, 25u32).unwrap();
        let merged = span1.merge(span2);
        assert_eq!(merged.start(), BytePos(10));
        assert_eq!(merged.end(), BytePos(25));
    }

    #[test]
    fn merge_separated() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(30u32, 40u32).unwrap();
        let merged = span1.merge(span2);
        assert_eq!(merged.start(), BytePos(10));
        assert_eq!(merged.end(), BytePos(40));
    }

    #[test]
    fn merge_contained() {
        let outer = Span::new(10u32, 30u32).unwrap();
        let inner = Span::new(15u32, 25u32).unwrap();
        let merged = outer.merge(inner);
        assert_eq!(merged, outer);
    }

    #[test]
    fn merge_identical() {
        let span = Span::new(10u32, 20u32).unwrap();
        let merged = span.merge(span);
        assert_eq!(merged, span);
    }

    #[test]
    fn span_shrink() {
        let span = Span::new(10u32, 20u32).unwrap();
        let shrunk = span.shrink(2).unwrap();
        assert_eq!(shrunk.start(), BytePos(12));
        assert_eq!(shrunk.end(), BytePos(18));
    }

    #[test]
    fn shrink_to_empty() {
        let span = Span::new(10u32, 20u32).unwrap();
        let shrunk = span.shrink(5).unwrap();
        assert!(shrunk.is_empty());
        assert_eq!(shrunk.start(), BytePos(15));
        assert_eq!(shrunk.end(), BytePos(15));
    }

    #[test]
    fn shrink_too_much() {
        let span = Span::new(10u32, 20u32).unwrap();
        // Shrinking too much returns None
        assert!(span.shrink(10).is_none());
        assert!(span.shrink(11).is_none());
    }

    #[test]
    fn shrink_zero() {
        let span = Span::new(10u32, 20u32).unwrap();
        let shrunk = span.shrink(0).unwrap();
        assert_eq!(shrunk, span);
    }

    #[test]
    fn shrink_single_span() {
        let span = Span::single(10u32);
        // Can't shrink a single-byte span
        assert!(span.shrink(1).is_none());
    }

    #[test]
    fn shrink_left() {
        let span = Span::new(10u32, 20u32).unwrap();
        let shrunk = span.shrink_left(2).unwrap();
        assert_eq!(shrunk.start(), BytePos(12));
        assert_eq!(shrunk.end(), BytePos(20));
    }

    #[test]
    fn shrink_left_too_much() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert!(span.shrink_left(11).is_none());
    }

    #[test]
    fn shrink_left_to_empty() {
        let span = Span::new(10u32, 20u32).unwrap();
        let shrunk = span.shrink_left(10).unwrap();
        assert!(shrunk.is_empty());
        assert_eq!(shrunk.start(), BytePos(20));
    }

    #[test]
    fn shrink_right() {
        let span = Span::new(10u32, 20u32).unwrap();
        let shrunk = span.shrink_right(2).unwrap();
        assert_eq!(shrunk.start(), BytePos(10));
        assert_eq!(shrunk.end(), BytePos(18));
    }

    #[test]
    fn shrink_right_too_much() {
        let span = Span::new(10u32, 20u32).unwrap();
        assert!(span.shrink_right(11).is_none());
    }

    #[test]
    fn shrink_right_to_empty() {
        let span = Span::new(10u32, 20u32).unwrap();
        let shrunk = span.shrink_right(10).unwrap();
        assert!(shrunk.is_empty());
        assert_eq!(shrunk.end(), BytePos(10));
    }

    #[test]
    fn to_range_usize() {
        let span = Span::new(10u32, 20u32).unwrap();
        let range: Range<usize> = span.into();
        assert_eq!(range, 10..20);
    }

    #[test]
    fn to_range_bytepos() {
        let span = Span::new(10u32, 20u32).unwrap();
        let range: Range<BytePos> = span.into();
        assert_eq!(range.start, BytePos(10));
        assert_eq!(range.end, BytePos(20));
    }

    #[test]
    fn try_from_range_valid() {
        let range = 10u32..20u32;
        let span: Span = range.try_into().unwrap();
        assert_eq!(span.start(), BytePos(10));
        assert_eq!(span.end(), BytePos(20));
    }

    #[test]
    #[allow(clippy::reversed_empty_ranges)]
    fn try_from_range_invalid() {
        let range = 20u32..10u32;
        let result: Result<Span, ()> = range.try_into();
        assert!(result.is_err());
    }

    #[test]
    fn zero_position_span() {
        let span = Span::new(0u32, 10u32).unwrap();
        assert_eq!(span.start(), BytePos(0));
        assert_eq!(span.len(), 10);
    }

    #[test]
    fn large_span() {
        let span = Span::new(0u32, u32::MAX).unwrap();
        assert_eq!(span.len(), u32::MAX as usize);
    }

    #[test]
    fn adjacent_spans_share_boundary() {
        let span1 = Span::new(10u32, 20u32).unwrap();
        let span2 = Span::new(20u32, 30u32).unwrap();

        // They share the boundary at position 20
        assert_eq!(span1.end(), span2.start());
        assert!(!span1.overlaps(span2));
        assert!(!span1.contains_byte(BytePos(20)));
        assert!(span2.contains_byte(BytePos(20)));
    }
}
