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

type Pool<'a, V> = &'a mut SlicePool<V>;
type Counts<'a, K> = &'a mut SecondaryMap<K, u32>;
type Slices<'a, K, V> = &'a mut SecondaryMap<K, EntitySlice<V>>;

/// Marker type for the counting phase.
///
/// In this phase, the builder records how many values each key will contain.
pub struct Counting;

/// Marker type for the filling phase.
///
/// In this phase, storage has been allocated and values are written into the reserved buckets.
pub struct Filling;

/// Reusable buffer for constructing bucketed collections.
///
/// `BucketScratch` owns the temporary count map required by
/// [`BucketBuilder`]. Reusing a single workspace across multiple bucket
/// constructions avoids reallocating the intermediate counting storage.
pub struct BucketScratch<K: EntityRef> {
    counts: SecondaryMap<K, u32>,
}

impl<K: EntityRef> Default for BucketScratch<K> {
    fn default() -> Self {
        Self {
            counts: SecondaryMap::default(),
        }
    }
}

impl<K: EntityRef> BucketScratch<K> {
    /// Creates a two-phase builder that constructs buckets in `pool`.
    ///
    /// The builder uses this workspace to record per-key counts before
    /// allocating storage in `pool` and writing slices into `slices`.
    ///
    /// The workspace is reset as the builder is used and may be reused for
    /// subsequent bucket constructions once the builder is finished.
    pub fn builder<'a, V: EntityRef>(
        &'a mut self,
        pool: Pool<'a, V>,
        slices: Slices<'a, K, V>,
    ) -> BucketBuilder<'a, K, V, Counting> {
        debug_assert!(self.counts.values().all(|c| *c == 0));
        BucketBuilder {
            pool,
            slices,
            _phase: PhantomData,
            counts: &mut self.counts,
        }
    }
}

/// Builder for efficiently constructing per-key buckets in two passes.
///
/// The builder starts in the [`Counting`] phase, where calls to [`Self::bump`]
/// accumulate the number of values associated with each key. Calling
/// [`Self::allocate`] allocates storage for every bucket and transitions the
/// builder into the [`Filling`] phase, where values can be inserted with `push`.
pub struct BucketBuilder<'a, K: EntityRef, V, Phase> {
    pool: Pool<'a, V>,
    counts: Counts<'a, K>,
    slices: Slices<'a, K, V>,
    _phase: PhantomData<Phase>,
}

impl<K: EntityRef, V: EntityRef> BucketBuilder<'_, K, V, Filling> {
    /// Push one value into `key`'s slice, filling back-to-front.
    ///
    /// ## Panics
    ///
    /// Panics in debug if you push more values than were counted.
    pub fn push(&mut self, key: K, value: V) {
        debug_assert!(self.counts[key] > 0);
        let idx = self.counts[key] - 1;
        self.counts[key] = idx;
        let slice = self.slices[key];
        self.pool[slice][idx as usize] = value;
    }
}

impl<'a, K: EntityRef, V: EntityRef> BucketBuilder<'a, K, V, Counting> {
    /// Increment the count for `key` by one.
    pub fn bump(&mut self, key: K) {
        self.counts[key] += 1;
    }

    /// Allocates storage for every non-empty bucket
    /// and transitions the builder into the filling phase.
    pub fn allocate(self) -> BucketBuilder<'a, K, V, Filling> {
        let needed: u32 = self.counts.values().sum();
        self.pool.reserve(needed as usize);
        for (key, &count) in self.counts.iter() {
            if count == 0 {
                continue;
            }
            self.slices[key] = self
                .pool
                .extend(std::iter::repeat_n(V::new(0), count as usize));
        }
        BucketBuilder {
            pool: self.pool,
            counts: self.counts,
            slices: self.slices,
            _phase: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SecondaryMap, entity_impl, slice::SlicePool};

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestKey(u32);
    entity_impl!(TestKey, "test_key");

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestValue(u32);
    entity_impl!(TestValue, "test_value");

    #[test]
    fn empty_bucket_is_left_empty() {
        let mut pool = SlicePool::<TestValue>::default();
        let mut slices = SecondaryMap::<TestKey, _>::default();
        let mut scratch = BucketScratch::<TestKey>::default();

        let key = TestKey::new(5);

        scratch.builder(&mut pool, &mut slices).allocate();

        assert_eq!(pool[slices[key]].len(), 0);
    }

    #[test]
    fn allocate_creates_correct_bucket_sizes() {
        let mut pool = SlicePool::<TestValue>::default();
        let mut slices = SecondaryMap::<TestKey, _>::default();
        let mut scratch = BucketScratch::<TestKey>::default();

        let k0 = TestKey::new(0);
        let k1 = TestKey::new(1);
        let k2 = TestKey::new(2);

        let mut builder = scratch.builder(&mut pool, &mut slices);

        builder.bump(k0);
        builder.bump(k0);
        builder.bump(k2);

        builder.allocate();

        assert_eq!(pool[slices[k0]].len(), 2);
        assert_eq!(pool[slices[k2]].len(), 1);
        assert_eq!(pool[slices[k1]].len(), 0);
    }

    #[test]
    fn push_preserves_insertion_order() {
        let mut pool = SlicePool::<TestValue>::default();
        let mut slices = SecondaryMap::<TestKey, _>::default();
        let mut scratch = BucketScratch::<TestKey>::default();

        let key = TestKey::new(0);

        let mut builder = scratch.builder(&mut pool, &mut slices);

        builder.bump(key);
        builder.bump(key);
        builder.bump(key);

        let mut builder = builder.allocate();

        builder.push(key, TestValue::new(10));
        builder.push(key, TestValue::new(20));
        builder.push(key, TestValue::new(30));

        let slice = &pool[slices[key]];

        assert_eq!(slice, &[TestValue::new(30), TestValue::new(20), TestValue::new(10),]);
    }

    #[test]
    fn multiple_buckets_are_independent() {
        let mut pool = SlicePool::<TestValue>::default();
        let mut slices = SecondaryMap::<TestKey, _>::default();
        let mut scratch = BucketScratch::<TestKey>::default();

        let a = TestKey::new(0);
        let b = TestKey::new(1);

        let mut builder = scratch.builder(&mut pool, &mut slices);

        builder.bump(a);
        builder.bump(b);
        builder.bump(a);

        let mut builder = builder.allocate();

        builder.push(a, TestValue::new(1));
        builder.push(b, TestValue::new(2));
        builder.push(a, TestValue::new(3));

        assert_eq!(&pool[slices[a]], &[TestValue::new(3), TestValue::new(1)]);
        assert_eq!(&pool[slices[b]], &[TestValue::new(2)]);
    }

    #[test]
    fn scratch_can_be_reused() {
        let mut pool = SlicePool::<TestValue>::default();
        let mut slices = SecondaryMap::<TestKey, _>::default();
        let mut scratch = BucketScratch::<TestKey>::default();

        let key = TestKey::new(0);

        {
            let mut builder = scratch.builder(&mut pool, &mut slices);

            builder.bump(key);
            let mut builder = builder.allocate();
            builder.push(key, TestValue::new(1));
        }

        {
            let mut builder = scratch.builder(&mut pool, &mut slices);

            builder.bump(key);
            builder.bump(key);

            let mut builder = builder.allocate();
            builder.push(key, TestValue::new(2));
            builder.push(key, TestValue::new(3));
        }

        assert_eq!(&pool[slices[key]], &[TestValue::new(3), TestValue::new(2)]);
    }
}
