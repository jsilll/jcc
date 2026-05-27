use crate::EntityRef;

use std::marker::PhantomData;

/// A counter that allocates entity references without storing associated data.
///
/// This is the allocation-only counterpart to [`PrimaryMap`](crate::PrimaryMap): it hands out
/// dense entity keys in order, but does not hold any values. Use it when entity data lives
/// elsewhere — for example, in a [`SecondaryMap`](crate::SecondaryMap) that is populated by a
/// later pass.
pub struct EntityCounter<K: EntityRef> {
    next: usize,
    phantom: PhantomData<K>,
}

impl<K: EntityRef> Default for EntityCounter<K> {
    fn default() -> Self {
        Self {
            next: 0,
            phantom: PhantomData,
        }
    }
}

impl<K: EntityRef> EntityCounter<K> {
    /// Creates a new counter starting at index 0.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns `true` if no entities have been allocated yet.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.next == 0
    }

    /// Returns the number of entities allocated so far.
    ///
    /// This is useful as a capacity hint when creating a
    /// [`SecondaryMap`](crate::SecondaryMap) that will be
    /// indexed by the entities from this counter.
    #[inline]
    pub fn len(&self) -> usize {
        self.next
    }

    /// Allocates a new entity reference and advances the counter.
    #[inline]
    pub fn push(&mut self) -> K {
        let k = K::new(self.next);
        self.next += 1;
        k
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity_impl;

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestEntity(u32);
    entity_impl!(TestEntity, "e");

    #[test]
    fn starts_at_zero() {
        let mut counter: EntityCounter<TestEntity> = EntityCounter::new();
        assert!(counter.is_empty());
        assert_eq!(counter.len(), 0);
        let first = counter.push();
        assert_eq!(first.index(), 0);
    }

    #[test]
    fn sequential_allocation() {
        let mut counter: EntityCounter<TestEntity> = EntityCounter::new();
        let e0 = counter.push();
        let e1 = counter.push();
        let e2 = counter.push();
        assert_eq!(e0.index(), 0);
        assert_eq!(e1.index(), 1);
        assert_eq!(e2.index(), 2);
        assert_eq!(counter.len(), 3);
    }

    #[test]
    fn default_is_empty() {
        let counter: EntityCounter<TestEntity> = EntityCounter::default();
        assert!(counter.is_empty());
    }
}
