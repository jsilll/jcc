#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AbstractHeap {
    pre: u32,
    post: u32,
}
impl AbstractHeap {
    pub fn new(pre: u32, post: u32) -> Self {
        assert!(pre <= post);
        Self { pre, post }
    }

    pub fn interferes_with(&self, other: &AbstractHeap) -> bool {
        self.pre <= other.post && other.pre <= self.post
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HalfFastEffects {
    heaps: Vec<AbstractHeap>,
}

impl HalfFastEffects {
    pub fn new() -> Self {
        HalfFastEffects { heaps: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.heaps.len()
    }

    pub fn add(&mut self, range: AbstractHeap) {
        self.heaps.push(range);
    }

    pub fn interferes_with(&self, heap: &AbstractHeap) -> bool {
        self.heaps.iter().any(|h| h.interferes_with(heap))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FastEffects {
    pub reads: HalfFastEffects,
    pub writes: HalfFastEffects,
}

impl FastEffects {
    pub fn new() -> Self {
        Self {
            reads: HalfFastEffects::new(),
            writes: HalfFastEffects::new(),
        }
    }

    pub fn interferes_with(&self, other: &FastEffects) -> bool {
        // TODO: Improve this

        // Read-write interference
        for read in &self.reads.heaps {
            for write in &other.writes.heaps {
                if read.interferes_with(write) {
                    return true;
                }
            }
        }

        // Write-read interference
        for write in &self.writes.heaps {
            for read in &other.reads.heaps {
                if write.interferes_with(read) {
                    return true;
                }
            }
        }

        // Write-write interference
        for write1 in &self.writes.heaps {
            for write2 in &other.writes.heaps {
                if write1.interferes_with(write2) {
                    return true;
                }
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_interference() {
        let a = AbstractHeap::new(1, 5);
        let b = AbstractHeap::new(6, 10);
        assert!(!a.interferes_with(&b));
        assert!(!b.interferes_with(&a));
    }

    #[test]
    fn test_full_interference() {
        let a = AbstractHeap::new(1, 10);
        let b = AbstractHeap::new(5, 8);
        assert!(a.interferes_with(&b));
        assert!(b.interferes_with(&a));
    }

    #[test]
    fn test_edge_interference() {
        let a = AbstractHeap::new(1, 5);
        let b = AbstractHeap::new(5, 10);
        assert!(a.interferes_with(&b));
        assert!(b.interferes_with(&a));
    }

    #[test]
    fn test_full_containment() {
        let a = AbstractHeap::new(5, 10);
        let b = AbstractHeap::new(6, 9);
        assert!(a.interferes_with(&b));
        assert!(b.interferes_with(&a));
    }

    #[test]
    fn test_no_interference_with_disjoint_ranges() {
        let a = AbstractHeap::new(1, 5);
        let b = AbstractHeap::new(6, 7);
        assert!(!a.interferes_with(&b));
        assert!(!b.interferes_with(&a));
    }

    #[test]
    fn test_reverse_interference() {
        let a = AbstractHeap::new(6, 10);
        let b = AbstractHeap::new(1, 5);
        assert!(!a.interferes_with(&b));
        assert!(!b.interferes_with(&a));
    }

    #[test]
    fn test_same_range() {
        let a = AbstractHeap::new(3, 7);
        let b = AbstractHeap::new(3, 7);
        assert!(a.interferes_with(&b));
        assert!(b.interferes_with(&a));
    }

    #[test]
    fn test_reverse_edge_interference() {
        let a = AbstractHeap::new(5, 10);
        let b = AbstractHeap::new(10, 15);
        assert!(a.interferes_with(&b));
        assert!(b.interferes_with(&a));
    }

    #[test]
    fn test_edge_case_same_start() {
        let a = AbstractHeap::new(5, 10);
        let b = AbstractHeap::new(5, 12);
        assert!(a.interferes_with(&b));
        assert!(b.interferes_with(&a));
    }

    #[test]
    fn test_edge_case_same_end() {
        let a = AbstractHeap::new(3, 7);
        let b = AbstractHeap::new(1, 7);
        assert!(a.interferes_with(&b));
        assert!(b.interferes_with(&a));
    }

    #[test]
    fn test_new_fast_effects() {
        let effects = HalfFastEffects::new();
        assert_eq!(effects.len(), 0);
    }

    #[test]
    fn test_add_range() {
        let mut effects = HalfFastEffects::new();
        effects.add(AbstractHeap::new(1, 5));
        assert_eq!(effects.len(), 1);
        effects.add(AbstractHeap::new(10, 15));
        assert_eq!(effects.len(), 2);
    }

    #[test]
    fn test_fast_effects_interferes_with_empty() {
        let effects = HalfFastEffects::new();
        let range = AbstractHeap::new(1, 5);
        assert!(!effects.interferes_with(&range));
    }

    #[test]
    fn test_fast_effects_no_interference() {
        let mut effects = HalfFastEffects::new();
        effects.add(AbstractHeap::new(1, 5));
        assert!(!effects.interferes_with(&AbstractHeap::new(6, 10)));
        assert!(!effects.interferes_with(&AbstractHeap::new(0, 0)));
    }

    #[test]
    fn test_fast_effects_interference_overlap() {
        let mut effects = HalfFastEffects::new();
        effects.add(AbstractHeap::new(1, 5));

        // Overlapping ranges
        assert!(effects.interferes_with(&AbstractHeap::new(3, 7)));
        assert!(effects.interferes_with(&AbstractHeap::new(0, 2)));
        assert!(effects.interferes_with(&AbstractHeap::new(4, 10)));
    }

    #[test]
    fn test_fast_effects_interference_boundary() {
        let mut effects = HalfFastEffects::new();
        effects.add(AbstractHeap::new(1, 5));

        // Ranges that meet at boundary
        assert!(effects.interferes_with(&AbstractHeap::new(5, 10)));
        assert!(effects.interferes_with(&AbstractHeap::new(0, 1)));
    }

    #[test]
    fn test_fast_effects_interference_contained() {
        let mut effects = HalfFastEffects::new();
        effects.add(AbstractHeap::new(1, 10));

        // Range completely contained within existing range
        assert!(effects.interferes_with(&AbstractHeap::new(3, 7)));

        // Existing range completely contained within new range
        assert!(effects.interferes_with(&AbstractHeap::new(0, 15)));
    }

    #[test]
    fn test_fast_effects_multiple_ranges() {
        let mut effects = HalfFastEffects::new();
        effects.add(AbstractHeap::new(1, 5));
        effects.add(AbstractHeap::new(10, 15));
        effects.add(AbstractHeap::new(20, 25));

        // Test interference with each range
        assert!(effects.interferes_with(&AbstractHeap::new(3, 4)));
        assert!(effects.interferes_with(&AbstractHeap::new(12, 13)));
        assert!(effects.interferes_with(&AbstractHeap::new(23, 24)));

        // Test no interference with gaps
        assert!(!effects.interferes_with(&AbstractHeap::new(6, 9)));
        assert!(!effects.interferes_with(&AbstractHeap::new(16, 19)));
        assert!(!effects.interferes_with(&AbstractHeap::new(26, 30)));
    }

    #[test]
    fn test_fast_effects_edge_cases() {
        let mut effects = HalfFastEffects::new();

        // Add a zero-length range
        effects.add(AbstractHeap::new(5, 5));

        // Should interfere with itself
        assert!(effects.interferes_with(&AbstractHeap::new(5, 5)));

        // Should interfere with ranges that include it
        assert!(effects.interferes_with(&AbstractHeap::new(3, 7)));

        // Should not interfere with adjacent ranges
        assert!(!effects.interferes_with(&AbstractHeap::new(0, 4)));
        assert!(!effects.interferes_with(&AbstractHeap::new(6, 10)));
    }

    #[test]
    fn test_fast_effects_abstract_heap_interferes_with() {
        // Direct tests for AbstractHeap.interferes_with

        // Same range
        let heap1 = AbstractHeap::new(1, 5);
        let heap2 = AbstractHeap::new(1, 5);
        assert!(heap1.interferes_with(&heap2));

        // Completely disjoint
        let heap3 = AbstractHeap::new(10, 15);
        assert!(!heap1.interferes_with(&heap3));
        assert!(!heap3.interferes_with(&heap1));

        // Overlapping
        let heap4 = AbstractHeap::new(3, 8);
        assert!(heap1.interferes_with(&heap4));
        assert!(heap4.interferes_with(&heap1));

        // Adjacent at boundary
        let heap5 = AbstractHeap::new(5, 10);
        assert!(heap1.interferes_with(&heap5));
        assert!(heap5.interferes_with(&heap1));

        // One contains the other
        let heap6 = AbstractHeap::new(2, 4);
        assert!(heap1.interferes_with(&heap6));
        assert!(heap6.interferes_with(&heap1));
    }
}
