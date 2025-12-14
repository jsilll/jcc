//! Marker traits for types that dereference to stable memory addresses.
//!
//! This module provides two unsafe marker traits:
//!
//! - [`StableDeref`]: For types whose dereferenced address remains valid even
//!   when the container itself is moved in memory.
//! - [`CloneStableDeref`]: For types whose clones dereference to the same
//!   address as the original.
//!
//! # Examples of Stable Types
//!
//! Standard library types that implement `StableDeref` include:
//! - [`Box<T>`](alloc::boxed::Box)
//! - [`Vec<T>`](alloc::vec::Vec)
//! - [`Rc<T>`](alloc::rc::Rc) (also implements `CloneStableDeref`)
//! - [`Arc<T>`](alloc::sync::Arc) (also implements `CloneStableDeref`)
//! - [`String`](alloc::string::String)
//!
//! # `no_std` Support
//!
//! This crate supports `no_std` environments. Disable the `std` feature to use
//! it without the standard library:
//!
//! Note that in `no_std` mode, the trait implementations for standard library
//! types are not available, but you can still implement the traits for your
//! own types.

extern crate alloc;
extern crate core;

use core::{
    cell::{Ref, RefMut},
    ops::Deref,
};

/// A marker trait for types that dereference to stable memory addresses.
///
/// # Safety
///
/// Implementors must ensure that the address returned by `Deref` remains valid
/// even if the container is moved in memory.
pub unsafe trait StableDeref: Deref {}

// A marker trait for types whose clones dereference to the same address as the original.
// pub unsafe trait CloneStableDeref: StableDeref + Clone {}

unsafe impl<T: ?Sized> StableDeref for &T {}
unsafe impl<T: ?Sized> StableDeref for &mut T {}
// unsafe impl<'a, T: ?Sized> CloneStableDeref for &'a T {}
unsafe impl<'a, T: ?Sized> StableDeref for Ref<'a, T> {}
unsafe impl<'a, T: ?Sized> StableDeref for RefMut<'a, T> {}

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;

use std::ffi::{CStr, CString, OsStr, OsString};
use std::path::{Path, PathBuf};
use std::sync::{MutexGuard, RwLockReadGuard, RwLockWriteGuard};

unsafe impl StableDeref for String {}
unsafe impl<T> StableDeref for Vec<T> {}
unsafe impl<T: ?Sized> StableDeref for Rc<T> {}
unsafe impl<'a> StableDeref for Cow<'a, str> {}
unsafe impl<T: ?Sized> StableDeref for Box<T> {}
unsafe impl<T: ?Sized> StableDeref for Arc<T> {}
unsafe impl<'a, T: Clone> StableDeref for Cow<'a, [T]> {}

// #[cfg(feature = "alloc")]
// unsafe impl<T: ?Sized> CloneStableDeref for Rc<T> {}
// #[cfg(all(feature = "alloc", target_has_atomic = "ptr"))]
// unsafe impl<T: ?Sized> CloneStableDeref for Arc<T> {}

unsafe impl StableDeref for PathBuf {}
unsafe impl StableDeref for CString {}
unsafe impl StableDeref for OsString {}

unsafe impl<'a> StableDeref for Cow<'a, Path> {}
unsafe impl<'a> StableDeref for Cow<'a, CStr> {}
unsafe impl<'a> StableDeref for Cow<'a, OsStr> {}

unsafe impl<'a, T: ?Sized> StableDeref for MutexGuard<'a, T> {}
unsafe impl<'a, T: ?Sized> StableDeref for RwLockReadGuard<'a, T> {}
unsafe impl<'a, T: ?Sized> StableDeref for RwLockWriteGuard<'a, T> {}
