//! An arena-backed, push-only linked list collection.
//!
//! This module provides a `LinkedList` abstraction.  Instead of allocating
//! individual nodes on the heap via the system allocator, it stores all nodes
//! contiguously in a `PrimaryMap`.
//!
//! Lists are keyed by an `EntityRef` and accessed via a `SecondaryMap`,
//! ensuring that looking up the head of a list is an $O(1)$ operation
//! with good cache locality.
//!
//! # Semantics
//!
//! The `LinkedList` structure implements **push-only** semantics. Nodes are
//! appended to the underlying arena and each logical list grows by prepending
//! nodes. This is efficient for per-pass data structures that are built, consumed,
//! and then dropped in their entirety.

use crate::{entity_impl, EntityRef, PackedOption, PrimaryMap, SecondaryMap};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Node(u32);
entity_impl!(Node, "node");

struct NodeData<V> {
    value: V,
    next: PackedOption<Node>,
}

/// A collection of singly-linked lists indexed by strongly-typed entities.
///
/// The struct maintains a single pool of memory for all nodes across all lists.
/// The heads of the lists are tracked in a flat array (`SecondaryMap`), which
/// means memory usage scales with the maximum `K` entity created, not the number
/// of lists actually instantiated.
///
/// # Type Parameters
/// * `K`: The `EntityRef` acting as the key/owner of a specific linked list.
/// * `V`: The value stored inside the list nodes.
pub struct LinkedList<K, V>
where
    K: EntityRef,
{
    nodes: PrimaryMap<Node, NodeData<V>>,
    heads: SecondaryMap<K, PackedOption<Node>>,
}

impl<K, V> Default for LinkedList<K, V>
where
    K: EntityRef,
{
    fn default() -> Self {
        Self {
            nodes: PrimaryMap::default(),
            heads: SecondaryMap::default(),
        }
    }
}

impl<K: EntityRef, V> LinkedList<K, V> {
    /// Creates a new empty linked list.
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            heads: SecondaryMap::new(),
        }
    }

    /// Returns `true` if the list for the given key `k` is empty.
    pub fn is_empty(&self, key: K) -> bool {
        self.heads[key].is_none()
    }

    /// Returns a reference to the first value in the list for
    /// the given key `k`, or `None` if the list is empty.
    pub fn head(&self, key: K) -> Option<&V> {
        let head = self.heads[key].expand()?;
        Some(&self.nodes[head].value)
    }

    /// Returns an iterator over the values for a given key `k`.
    pub fn iter(&self, key: K) -> Iter<'_, V> {
        Iter {
            nodes: &self.nodes,
            curr: self.heads[key],
        }
    }

    /// Removes all elements from all lists.
    ///
    /// Note: Keeps the underlying memory allocated for future reuse.
    pub fn clear(&mut self) {
        self.nodes.clear();
        self.heads.clear();
    }

    /// Pushes a new value to the front of the list for key `k`.
    pub fn push_front(&mut self, key: K, value: V) {
        let next = self.heads[key];
        self.heads[key] = self.nodes.push(NodeData { value, next }).into();
    }
}

/// An iterator that traverses the linked list.
pub struct Iter<'a, V> {
    curr: PackedOption<Node>,
    nodes: &'a PrimaryMap<Node, NodeData<V>>,
}

impl<V> std::iter::FusedIterator for Iter<'_, V> {}

impl<'a, V> Iterator for Iter<'a, V> {
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.curr.expand()?;
        let data = &self.nodes[node];
        self.curr = data.next;
        Some(&data.value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    const E0: TestEntity = TestEntity(0);
    const E1: TestEntity = TestEntity(1);
    const E2: TestEntity = TestEntity(2);

    #[test]
    fn empty() {
        let list: LinkedList<TestEntity, i32> = LinkedList::new();
        assert!(list.is_empty(E0));
        assert!(list.iter(E0).next().is_none());
    }

    #[test]
    fn default() {
        let mut list = LinkedList::default();
        list.push_front(E0, "hello");
        assert_eq!(list.iter(E0).next(), Some(&"hello"));
    }

    #[test]
    fn head() {
        let mut list = LinkedList::new();
        assert_eq!(list.head(E0), None);
        list.push_front(E0, 10);
        assert_eq!(list.head(E0), Some(&10));
        list.push_front(E0, 20);
        assert_eq!(list.head(E0), Some(&20));
    }

    #[test]
    fn push_front_and_iterate() {
        let mut list = LinkedList::new();
        list.push_front(E0, 10);
        list.push_front(E0, 20);
        list.push_front(E0, 30);
        assert_eq!(
            list.iter(E0).copied().collect::<Vec<i32>>(),
            vec![30, 20, 10]
        );
    }

    #[test]
    fn multiple_keys_are_independent() {
        let mut list = LinkedList::new();
        list.push_front(E0, 1);
        list.push_front(E1, 100);
        list.push_front(E0, 2);
        list.push_front(E1, 200);
        assert!(list.iter(E2).collect::<Vec<_>>().is_empty());
        assert_eq!(list.iter(E0).copied().collect::<Vec<i32>>(), vec![2, 1]);
        assert_eq!(list.iter(E1).copied().collect::<Vec<i32>>(), vec![200, 100]);
    }

    #[test]
    fn clear() {
        let mut list = LinkedList::new();

        list.push_front(E0, 10);
        list.push_front(E0, 20);
        list.push_front(E1, 100);
        assert_eq!(list.iter(E0).copied().collect::<Vec<_>>(), vec![20, 10]);
        assert_eq!(list.iter(E1).copied().collect::<Vec<_>>(), vec![100]);

        list.clear();
        assert!(list.head(E0).is_none());
        assert!(list.head(E1).is_none());
        assert!(list.iter(E0).next().is_none());
        assert!(list.iter(E1).next().is_none());

        list.push_front(E1, 30);
        list.push_front(E1, 40);
        assert!(list.head(E0).is_none());
        assert_eq!(list.iter(E1).copied().collect::<Vec<_>>(), vec![40, 30]);
    }
}
