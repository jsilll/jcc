//! A thread-unsafe, append-only vector that allows shared references to its elements.
//!
//! # Overview
//!
//! `FrozenVec<T>` is a specialized data structure that provides append-only semantics
//! with stable element references. Once elements are added to the vector, references
//! to them remain valid even as the vector grows (provided `T: StableDeref`).
//!
//! # Key Properties
//!
//! - **Append-only**: Elements can be added but never removed or modified
//! - **Stable references**: References to elements remain valid across push operations
//! - **Interior mutability**: Allows mutation through shared references (`&self`)
//! - **Not thread-safe**: This type is NOT `Sync` and should not be shared across threads
//!
//! # Use Cases
//!
//! `FrozenVec` is ideal for scenarios where you need:
//! - To build up a collection while holding references to earlier elements
//! - Arena-like allocation patterns without explicit lifetime management
//! - Append-only data structures with stable addressing
//! - Cache or intern tables where elements are never modified
//!
//! # Safety
//!
//! This type uses `UnsafeCell` to provide interior mutability. It is safe to use
//! in single-threaded contexts, but **must not** be shared across threads. The
//! stability of references relies on the `StableDeref` trait, which ensures that
//! the referent's address doesn't change even if the container is moved or grows.

use std::{cell::UnsafeCell, cmp::Ordering, ops::Index};

use crate::traits::StableDeref;

/// An append-only vector with stable element references.
///
/// `FrozenVec<T>` provides a vector-like data structure that allows appending elements
/// while holding references to existing elements. This is achieved through interior
/// mutability and the `StableDeref` trait.
///
/// # Type Parameters
///
/// - `T`: The element type. For most operations that return references, `T` must
///   implement `StableDeref`.
///
/// # Thread Safety
///
/// **Important**: `FrozenVec` is **not** thread-safe. It does not implement `Sync`
/// and should never be shared across threads. Use appropriate synchronization
/// primitives if thread-safe access is required.
///
/// # Memory Layout
///
/// Internally, `FrozenVec` wraps a `Vec<T>` in an `UnsafeCell`, allowing mutation
/// through shared references. The vector may reallocate as it grows, but elements
/// of type `T: StableDeref` provide stable references to their target data.
pub struct FrozenVec<T> {
    vec: UnsafeCell<Vec<T>>,
}

impl<T> FrozenVec<T> {
    /// Creates a new, empty `FrozenVec`.
    pub const fn new() -> Self {
        Self {
            vec: UnsafeCell::new(Vec::new()),
        }
    }

    /// Creates a new, empty `FrozenVec` with the specified capacity.
    ///
    /// The vector will be able to hold at least `capacity` elements without
    /// reallocating. If `capacity` is 0, the vector will not allocate.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            vec: UnsafeCell::new(Vec::with_capacity(capacity)),
        }
    }

    /// Returns the number of elements in the vector.
    pub fn len(&self) -> usize {
        unsafe {
            let vec = self.vec.get();
            (*vec).len()
        }
    }

    /// Returns `true` if the vector contains no elements.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the number of elements the vector can hold without reallocating.
    pub fn capacity(&self) -> usize {
        unsafe {
            let vec = self.vec.get();
            (*vec).capacity()
        }
    }

    /// Appends an element to the back of the vector.
    ///
    /// This operation may cause reallocation if the capacity is exceeded.
    /// However, references to elements obtained through `StableDeref` remain valid.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    pub fn push(&self, val: T) {
        unsafe {
            let vec = self.vec.get();
            (*vec).push(val)
        }
    }

    /// Reserves capacity for at least `additional` more elements.
    ///
    /// After calling this method, capacity will be greater than or equal to
    /// `self.len() + additional`. Does nothing if the capacity is already sufficient.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    pub fn reserve(&self, additional: usize) {
        unsafe {
            let vec = self.vec.get();
            (*vec).reserve(additional)
        }
    }
}

impl<T> Default for FrozenVec<T> {
    /// Creates an empty `FrozenVec<T>`.
    fn default() -> Self {
        FrozenVec::new()
    }
}

impl<T> From<Vec<T>> for FrozenVec<T> {
    /// Converts a `Vec<T>` into a `FrozenVec<T>`.
    ///
    /// This is a zero-cost conversion that takes ownership of the vector.
    fn from(vec: Vec<T>) -> Self {
        Self {
            vec: UnsafeCell::new(vec),
        }
    }
}

impl<A> FromIterator<A> for FrozenVec<A> {
    /// Creates a `FrozenVec<A>` from an iterator.
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = A>,
    {
        let vec: Vec<_> = iter.into_iter().collect();
        vec.into()
    }
}

impl<T: Clone> Clone for FrozenVec<T> {
    /// Creates a deep copy of the `FrozenVec`.
    ///
    /// This clones all elements in the vector.
    fn clone(&self) -> Self {
        let vec = unsafe { self.vec.get().as_ref().unwrap() };
        Self {
            vec: vec.clone().into(),
        }
    }
}

impl<T: StableDeref> Index<usize> for FrozenVec<T> {
    type Output = T::Target;

    /// Returns a reference to an element at the given index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    fn index(&self, idx: usize) -> &T::Target {
        self.get(idx).unwrap_or_else(|| {
            panic!(
                "index out of bounds: the len is {} but the index is {}",
                self.len(),
                idx
            )
        })
    }
}

impl<T> std::convert::AsMut<Vec<T>> for FrozenVec<T> {
    /// Gets mutable access to the underlying vector.
    ///
    /// This is safe because it requires a `&mut self`, ensuring no shared
    /// references exist to the 'frozen' contents.
    fn as_mut(&mut self) -> &mut Vec<T> {
        unsafe { &mut *self.vec.get() }
    }
}

impl<T: StableDeref + PartialEq> PartialEq for FrozenVec<T>
where
    T::Target: PartialEq,
{
    /// Compares two `FrozenVec` instances for equality.
    ///
    /// Two vectors are equal if they have the same length and all elements
    /// are pairwise equal.
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.vec.get().as_ref() == other.vec.get().as_ref() }
    }
}

impl<'a, T: StableDeref> IntoIterator for &'a FrozenVec<T> {
    type Item = &'a T::Target;
    type IntoIter = Iter<'a, T>;

    /// Creates an iterator over references to the vector's elements.
    fn into_iter(self) -> Iter<'a, T> {
        Iter { vec: self, idx: 0 }
    }
}

impl<T: Copy> FrozenVec<T> {
    /// Returns a copy of an element at the given index.
    ///
    /// Returns `None` if the index is out of bounds.
    ///
    /// This method is available only when `T` implements `Copy`.
    pub fn get_copy(&self, index: usize) -> Option<T> {
        unsafe {
            let vec = self.vec.get();
            (*vec).get(index).copied()
        }
    }

    /// Returns an iterator that yields copies of the elements.
    ///
    /// It is safe to push to the vector during iteration.
    pub fn copy_iter(&self) -> CopyIter<'_, T> {
        CopyIter { vec: self, idx: 0 }
    }
}

impl<T: StableDeref> FrozenVec<T> {
    /// Returns an iterator over references to the vector's elements.
    ///
    /// It is safe to push to the vector during iteration.
    pub fn iter(&self) -> Iter<'_, T> {
        self.into_iter()
    }

    /// Consumes the `FrozenVec` and returns the underlying `Vec<T>`.
    pub fn into_vec(self) -> Vec<T> {
        self.vec.into_inner()
    }

    /// Returns a reference to the first element, or `None` if empty.
    pub fn first(&self) -> Option<&T::Target> {
        unsafe {
            let vec = self.vec.get();
            (*vec).first().map(|x| &**x)
        }
    }

    /// Returns a reference to the last element, or `None` if empty.
    pub fn last(&self) -> Option<&T::Target> {
        unsafe {
            let vec = self.vec.get();
            (*vec).last().map(|x| &**x)
        }
    }

    /// Returns a reference to an element at the given index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn get(&self, index: usize) -> Option<&T::Target> {
        unsafe {
            let vec = self.vec.get();
            (*vec).get(index).map(|x| &**x)
        }
    }

    /// Returns a reference to an element without bounds checking.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `index < self.len()`, otherwise this
    /// function exhibits undefined behavior.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T::Target {
        let vec = self.vec.get();
        (*vec).get_unchecked(index)
    }

    /// Appends an element and returns a reference to it.
    ///
    /// This is a convenience method that combines `push` and `get` in a single
    /// operation, useful when you need an immediate reference to the pushed element.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    pub fn push_get(&self, val: T) -> &T::Target {
        unsafe {
            let vec = self.vec.get();
            (*vec).push(val);
            &*(&**(*vec).get_unchecked((*vec).len() - 1) as *const T::Target)
        }
    }

    /// Binary searches this sorted vector for a given element.
    ///
    /// If the value is found, returns `Ok` containing the index. If not found,
    /// returns `Err` containing the index where the element should be inserted
    /// to maintain sort order.
    ///
    /// This function assumes the vector is sorted according to the `Ord` trait.
    ///
    /// # Safety Notes
    ///
    /// This implementation is safe even if the user's comparison function pushes
    /// to the vector during the search.
    pub fn binary_search(&self, x: &T::Target) -> Result<usize, usize>
    where
        T::Target: Ord,
    {
        self.binary_search_by(|p| p.cmp(x))
    }

    /// Binary searches this sorted vector with a key extraction function.
    ///
    /// Searches for an element by comparing the result of calling `f` on each
    /// element with the provided key `b`.
    ///
    /// # Safety Notes
    ///
    /// This implementation is safe even if the key extraction function pushes
    /// to the vector during the search.
    pub fn binary_search_by_key<'a, B, F>(&'a self, b: &B, mut f: F) -> Result<usize, usize>
    where
        F: FnMut(&'a T::Target) -> B,
        B: Ord,
    {
        self.binary_search_by(|k| f(k).cmp(b))
    }

    /// Binary searches this sorted vector with a comparator function.
    ///
    /// The comparator function should return an ordering between its argument
    /// and the desired target. If a matching element is found, returns `Ok`
    /// with its index. Otherwise, returns `Err` with the index where it should
    /// be inserted.
    ///
    /// # Safety Notes
    ///
    /// This implementation handles the case where the user's comparator function
    /// might push elements to the vector during the search.
    pub fn binary_search_by<'a, F>(&'a self, mut f: F) -> Result<usize, usize>
    where
        F: FnMut(&'a T::Target) -> Ordering,
    {
        let mut size = self.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;
            let cmp = f(unsafe { self.get_unchecked(mid) });
            if cmp == Ordering::Less {
                left = mid + 1;
            } else if cmp == Ordering::Greater {
                right = mid;
            } else {
                return Ok(mid);
            }
            size = right - left;
        }
        Err(left)
    }

    /// Returns the index of the partition point according to the given predicate.
    ///
    /// This is the index of the first element of the second partition, where the
    /// first partition contains elements for which `pred` returns `true`, and the
    /// second contains elements for which it returns `false`.
    ///
    /// # Safety Notes
    ///
    /// This implementation is safe even if the predicate function pushes to the
    /// vector during the search.
    pub fn partition_point<P>(&self, mut pred: P) -> usize
    where
        P: FnMut(&T::Target) -> bool,
    {
        let mut left = 0;
        let mut right = self.len();
        while left != right {
            let mid = left + (right - left) / 2;
            let value = unsafe { self.get_unchecked(mid) };
            if pred(value) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }
        left
    }
}

/// Iterator over `FrozenVec`, obtained via [`.iter()`](FrozenVec::iter) or
/// by using `&frozen_vec` in a for loop.
///
/// # Safety
///
/// It is safe to push to the vector during iteration. The iterator will
/// observe elements that are present at the time of each `next()` call.
pub struct Iter<'a, T> {
    idx: usize,
    vec: &'a FrozenVec<T>,
}

impl<'a, T: StableDeref> Iterator for Iter<'a, T> {
    type Item = &'a T::Target;

    fn next(&mut self) -> Option<&'a T::Target> {
        if let Some(ret) = self.vec.get(self.idx) {
            self.idx += 1;
            Some(ret)
        } else {
            None
        }
    }
}

/// Iterator over `FrozenVec` that yields copies of elements, obtained via
/// [`.copy_iter()`](FrozenVec::copy_iter).
///
/// This iterator is available only when `T` implements `Copy`.
///
/// # Safety
///
/// It is safe to push to the vector during iteration.
pub struct CopyIter<'a, T> {
    idx: usize,
    vec: &'a FrozenVec<T>,
}

impl<'a, T: Copy> Iterator for CopyIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ret) = self.vec.get_copy(self.idx) {
            self.idx += 1;
            Some(ret)
        } else {
            None
        }
    }
}

#[test]
fn test_iteration() {
    let vec = vec!["a", "b", "c", "d"];
    let frozen: FrozenVec<_> = vec.clone().into();

    assert_eq!(vec, frozen.iter().collect::<Vec<_>>());
    for (e1, e2) in vec.iter().zip(frozen.iter()) {
        assert_eq!(*e1, e2);
    }

    assert_eq!(vec.len(), frozen.iter().count())
}

#[test]
fn test_accessors() {
    let vec: FrozenVec<String> = FrozenVec::new();

    assert!(vec.is_empty());
    assert_eq!(vec.len(), 0);
    assert_eq!(vec.first(), None);
    assert_eq!(vec.last(), None);
    assert_eq!(vec.get(1), None);

    vec.push("a".to_string());

    let item = vec.first().unwrap();

    vec.push("b".to_string());
    vec.push("c".to_string());

    assert!(!vec.is_empty());
    assert_eq!(vec.len(), 3);
    assert_eq!(vec.first(), Some("a"));
    assert_eq!(vec.last(), Some("c"));
    assert_eq!(vec.get(1), Some("b"));

    assert_eq!(item, "a");
}

#[test]
fn test_non_stable_deref() {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    struct Moo(i32);
    let vec: FrozenVec<Moo> = FrozenVec::new();

    assert!(vec.is_empty());
    assert_eq!(vec.len(), 0);
    assert_eq!(vec.get_copy(1), None);

    vec.push(Moo(1));
    vec.push(Moo(2));
    vec.push(Moo(3));

    assert!(!vec.is_empty());
    assert_eq!(vec.len(), 3);
    assert_eq!(vec.get_copy(1), Some(Moo(2)));
}

#[test]
fn test_binary_search() {
    let vec: FrozenVec<_> = vec!["ab", "cde", "fghij"].into();

    assert_eq!(vec.binary_search("cde"), Ok(1));
    assert_eq!(vec.binary_search("cdf"), Err(2));
    assert_eq!(vec.binary_search("a"), Err(0));
    assert_eq!(vec.binary_search("g"), Err(3));

    assert_eq!(vec.binary_search_by_key(&1, |x| x.len()), Err(0));
    assert_eq!(vec.binary_search_by_key(&3, |x| x.len()), Ok(1));
    assert_eq!(vec.binary_search_by_key(&4, |x| x.len()), Err(2));

    assert_eq!(vec.partition_point(|x| x.len() < 4), 2);
    assert_eq!(vec.partition_point(|_| false), 0);
    assert_eq!(vec.partition_point(|_| true), 3);
}
