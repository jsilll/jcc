//! Type-safe entity management for compiler data structures.
//!
//! This module provides a generic entity reference system that prevents mixing
//! different entity types at compile time. This is particularly useful for a compiler
//! IR where you have multiple interrelated entity types (instructions, blocks,
//! functions, etc.) that should not be confused.

mod list;
mod primary;
mod secondary;

pub use list::{EntityList, ListPool};
pub use primary::PrimaryMap;
pub use secondary::SecondaryMap;

/// Trait for types that can be used as entity references.
pub trait EntityRef: Copy + Eq {
    /// Create a new entity reference
    fn new(index: usize) -> Self;
    /// Get the index of the entity to index the array
    fn index(self) -> usize;
}

/// Implements the `EntityRef` trait and utility methods for a new entity type.
///
/// Optionally, also implements `Display` and `Debug` with a custom prefix.
///
/// Generated methods include `new`, `index`, `from_u32`, `as_u32`, `as_bits`, and `from_bits`.
#[macro_export]
macro_rules! entity_impl {
    ($entity:ident, $display_prefix:expr) => {
        $crate::entity_impl!($entity);

        impl ::core::fmt::Debug for $entity {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                (self as &dyn ::core::fmt::Display).fmt(f)
            }
        }

        impl ::core::fmt::Display for $entity {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                write!(f, concat!($display_prefix, "{}"), self.0)
            }
        }
    };

    ($entity:ident) => {
        impl $crate::EntityRef for $entity {
            #[inline]
            fn new(index: usize) -> Self {
                debug_assert!(index < (::core::u32::MAX as usize));
                $entity(index as u32)
            }

            #[inline]
            fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl $entity {
            /// Create a new instance from a `u32`.
            #[inline]
            #[allow(dead_code, unreachable_pub, reason = "macro-generated code")]
            pub fn from_u32(x: u32) -> Self {
                debug_assert!(x < ::core::u32::MAX);
                $entity(x)
            }

            /// Return the underlying index value as a `u32`.
            #[inline]
            #[allow(dead_code, unreachable_pub, reason = "macro-generated code")]
            pub fn as_u32(self) -> u32 {
                self.0
            }

            /// Return the raw bit encoding for this instance.
            ///
            /// # Warning
            ///
            /// The raw bit encoding is opaque and has no
            /// guaranteed correspondence to the entity's index. It encodes the
            /// entire state of this index value: either a valid index or an
            /// invalid-index sentinel. The value returned by this method should
            /// only be passed to `from_bits`.
            #[inline]
            #[allow(dead_code, unreachable_pub, reason = "macro-generated code")]
            pub fn as_bits(self) -> u32 {
                self.0
            }

            /// Create a new instance from the raw bit encoding.
            ///
            /// # Warning
            ///
            /// The raw bit encoding is opaque and has no
            /// guaranteed correspondence to the entity's index. It encodes the
            /// entire state of this index value: either a valid index or an
            /// invalid-index sentinel. The value returned by this method should
            /// only be given bits from `as_bits`.
            #[inline]
            #[allow(dead_code, unreachable_pub, reason = "macro-generated code")]
            pub fn from_bits(x: u32) -> Self {
                $entity(x)
            }
        }
    };
}
