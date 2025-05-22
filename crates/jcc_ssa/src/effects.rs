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

impl Default for HalfFastEffects {
    fn default() -> Self {
        Self::new()
    }
}

impl HalfFastEffects {
    pub fn new() -> Self {
        HalfFastEffects { heaps: Vec::new() }
    }

    pub fn clear(&mut self) {
        self.heaps.clear();
    }

    pub fn len(&self) -> usize {
        self.heaps.len()
    }

    pub fn is_empty(&self) -> bool {
        self.heaps.is_empty()
    }

    pub fn push(&mut self, range: AbstractHeap) {
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

impl Default for FastEffects {
    fn default() -> Self {
        Self::new()
    }
}

impl FastEffects {
    pub fn new() -> Self {
        Self {
            reads: HalfFastEffects::new(),
            writes: HalfFastEffects::new(),
        }
    }

    pub fn clear(&mut self) {
        self.reads.clear();
        self.writes.clear();
    }

    pub fn is_empty(&self) -> bool {
       self.reads.is_empty() && self.writes.is_empty() 
    }

    pub fn interferes_with(&self, other: &Self) -> bool {
        // TODO: Improve performance?

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
