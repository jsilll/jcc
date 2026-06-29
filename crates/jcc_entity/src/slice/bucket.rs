//! Two-phase builder for constructing per-key buckets in a [`SlicePool`].
//!
//! `BucketBuilder` operates in two phases:
//!
//! 1. **Counting**: record how many values each key will receive.
//! 2. **Filling**: allocate the backing storage and populate each bucket.
//!
//! Splitting construction into two phases allows all storage to be allocated
//! up front while still supporting efficient insertion.

use crate::{
    slice::{EntitySlice, SlicePool},
    EntityRef, SecondaryMap,
};

use std::marker::PhantomData;

type Counts<'a, K> = &'a mut SecondaryMap<K, u32>;
type Slices<'a, K, V> = &'a mut SecondaryMap<K, EntitySlice<V>>;

/// Marker type for the counting phase.
///
/// In this phase, the builder records how many values each key will contain.
pub struct Counting;

/// Marker type for the filling phase.
///
/// In this phase, storage has been allocated and values are written into the
/// reserved buckets.
pub struct Filling;

/// Builder for efficiently constructing per-key buckets in two passes.
///
/// The builder starts in the [`Counting`] phase, where calls to [`Self::bump`]
/// accumulate the number of values associated with each key. Calling
/// [`Self::reserve`] allocates storage for every bucket and transitions the
/// builder into the [`Filling`] phase, where values can be inserted with
/// `push`.
pub struct BucketBuilder<'a, K: EntityRef, V, Phase> {
    _phase: PhantomData<Phase>,
    pool: &'a mut SlicePool<V>,
    counts: Counts<'a, K>,
    slices: Slices<'a, K, V>,
}

impl<'a, K: EntityRef, V: EntityRef> BucketBuilder<'a, K, V, Filling> {
    /// Consume the builder.
    pub fn finish(self) {
        debug_assert!(self.counts.values().all(|c| *c == 0));
    }

    /// Push one value into `key`'s slice, filling back-to-front.
    ///
    /// Panics in debug if you push more values than were counted.
    pub fn push(&mut self, key: K, value: V) {
        let idx = self.counts[key] - 1;
        self.counts[key] = idx;
        let slice = self.slices[key];
        self.pool[slice][idx as usize] = value;
    }
}

impl<'a, K: EntityRef, V: EntityRef> BucketBuilder<'a, K, V, Counting> {
    /// Create a new builder in the counting phase.
    ///
    /// The provided `counts` map must be zero-initialized. After recording all
    /// counts with [`Self::bump`], call [`Self::reserve`] to allocate storage
    /// and transition into the filling phase.
    pub fn new(
        pool: &'a mut SlicePool<V>,
        counts: Counts<'a, K>,
        slices: Slices<'a, K, V>,
    ) -> Self {
        debug_assert!(counts.values().all(|c| *c == 0));
        Self {
            pool,
            counts,
            slices,
            _phase: PhantomData,
        }
    }

    /// Increment the count for `key` by one edge.
    pub fn bump(&mut self, key: K) {
        self.counts[key] += 1;
    }

    /// Reserve all slices based on accumulated counts, using `sentinel`
    /// as the fill value (e.g. `K::new(0)` or any cheap default).
    ///
    /// Transitions into the filling phase.
    pub fn reserve(self, keys: impl Iterator<Item = K>) -> BucketBuilder<'a, K, V, Filling> {
        for key in keys {
            let count = self.counts[key] as usize;
            self.slices[key] = self.pool.extend(std::iter::repeat_n(V::new(0), count));
        }
        BucketBuilder {
            pool: self.pool,
            counts: self.counts,
            slices: self.slices,
            _phase: PhantomData,
        }
    }
}
