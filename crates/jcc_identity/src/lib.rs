//! A pass-through identity hasher for integer-like keys.
//!
//! Types opt in by implementing [`IdentityHashable`], guaranteeing their
//! [`Hash`](std::hash::Hash) implementation calls exactly one integer write method
//! per hashing operation. [`IdentityHasher<K>`] then uses that integer directly as
//! the hash code, eliminating all hashing overhead.

use std::{
    hash::{BuildHasherDefault, Hasher},
    marker::PhantomData,
};

/// Marker trait for types safe to use with [`IdentityHasher`].
///
/// Implementors must guarantee that their [`Hash`](std::hash::Hash) impl calls
/// exactly one integer write method (`write_u8`, `write_u16`, `write_u32`,
/// `write_u64`, `write_usize`, or their signed equivalents) per hashing operation.
pub trait IdentityHashable: Copy + Eq + std::hash::Hash {}

impl IdentityHashable for u8 {}
impl IdentityHashable for u16 {}
impl IdentityHashable for u32 {}
impl IdentityHashable for u64 {}
impl IdentityHashable for usize {}
impl IdentityHashable for i8 {}
impl IdentityHashable for i16 {}
impl IdentityHashable for i32 {}
impl IdentityHashable for i64 {}
impl IdentityHashable for isize {}

/// Convenience alias for [`BuildHasherDefault<IdentityHasher<K>>`].
pub type BuildIdentityHasher<K> = BuildHasherDefault<IdentityHasher<K>>;

/// A [`HashMap`](std::collections::HashMap) using identity hashing for [`IdentityHashable`] keys.
pub type IntMap<K, V> = std::collections::HashMap<K, V, BuildIdentityHasher<K>>;

/// A [`HashSet`](std::collections::HashSet) using identity hashing for [`IdentityHashable`] keys.
pub type IntSet<K> = std::collections::HashSet<K, BuildIdentityHasher<K>>;

/// A pass-through hasher that uses integer values directly as hash codes.
///
/// Eliminates hashing overhead for types whose [`Hash`](std::hash::Hash)
/// implementation reduces to a single integer write. Collision resistance is
/// preserved as long as each key maps to a distinct integer.
///
/// In debug builds, an assertion fires if more than one write is made per hashing
/// operation, catching accidental use with multi-field or byte-level key types.
#[cfg(debug_assertions)]
pub struct IdentityHasher<K: IdentityHashable>(u64, bool, PhantomData<K>);

/// A pass-through hasher that uses integer values directly as hash codes.
///
/// Eliminates hashing overhead for types whose [`Hash`](std::hash::Hash)
/// implementation reduces to a single integer write. Collision resistance is
/// preserved as long as each key maps to a distinct integer.
///
/// In debug builds, an assertion fires if more than one write is made per hashing
/// operation, catching accidental use with multi-field or byte-level key types.
#[cfg(not(debug_assertions))]
pub struct IdentityHasher<K: IdentityHashable>(u64, PhantomData<K>);

#[cfg(debug_assertions)]
impl<K: IdentityHashable> Default for IdentityHasher<K> {
    fn default() -> Self {
        Self(0, false, PhantomData)
    }
}

#[cfg(not(debug_assertions))]
impl<K: IdentityHashable> Default for IdentityHasher<K> {
    fn default() -> Self {
        Self(0, PhantomData)
    }
}

#[cfg(debug_assertions)]
impl<K: IdentityHashable> Clone for IdentityHasher<K> {
    fn clone(&self) -> Self {
        Self(self.0, self.1, PhantomData)
    }
}

#[cfg(not(debug_assertions))]
impl<K: IdentityHashable> Clone for IdentityHasher<K> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<K: IdentityHashable> std::fmt::Debug for IdentityHasher<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("IdentityHasher").field(&self.0).finish()
    }
}

#[cfg(debug_assertions)]
impl<K: IdentityHashable> Hasher for IdentityHasher<K> {
    fn finish(&self) -> u64 {
        assert!(self.1, "IdentityHasher: no integer write was called");
        self.0
    }

    fn write_u32(&mut self, i: u32) {
        assert!(!self.1, "IdentityHasher: write called more than once");
        self.1 = true;
        self.0 = u64::from(i);
    }

    fn write_u64(&mut self, i: u64) {
        assert!(!self.1, "IdentityHasher: write called more than once");
        self.1 = true;
        self.0 = i;
    }

    #[allow(clippy::panic)]
    fn write(&mut self, _: &[u8]) {
        panic!("IdentityHasher only supports integer writes");
    }
}

#[cfg(not(debug_assertions))]
impl<K: IdentityHashable> Hasher for IdentityHasher<K> {
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
        panic!("IdentityHasher only supports integer writes");
    }
}
