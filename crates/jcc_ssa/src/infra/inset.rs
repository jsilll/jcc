use super::IR;

/// A data structure for efficiently batching instruction insertions into IR blocks.
///
/// `InsertionSet` allows you to queue multiple instruction insertions at various
/// positions within a block, then apply them all at once in a single pass. This
/// is more efficient than inserting instructions one by one, which would require
/// multiple array modifications.
///
/// # Usage Pattern
///
/// 1. Create a new `InsertionSet`
/// 2. Queue insertions using `insert_before`
/// 3. Apply all insertions with `execute`
/// 4. Reuse the same `InsertionSet` for multiple blocks
///
/// # Example
///
/// ```rust
/// let mut insertion_set = InsertionSet::new();
/// insertion_set.insert_before(0, some_instruction);
/// insertion_set.insert_before(2, another_instruction);
/// insertion_set.execute(&mut ir, block_ref);
/// ```
#[derive(Debug, Default)]
pub struct InsertionSet<P: IR> {
    /// Log of pending insertions: (index, instruction)
    /// Sorted by insertion order, not necessarily by index
    log: Vec<(u32, P::InstRef)>,
    /// Buffer for building the new instruction sequence
    buf: Option<Vec<P::InstRef>>,
}

impl<P: IR> InsertionSet<P> {
    /// Creates a new empty insertion set.
    ///
    /// This is equivalent to `InsertionSet::default()` but more explicit.
    #[inline]
    pub fn new() -> Self {
        Self {
            buf: None,
            log: Vec::new(),
        }
    }

    /// Returns the number of queued insertions.
    pub fn len(&self) -> usize {
        self.log.len()
    }

    /// Returns true if there are no queued insertions.
    pub fn is_empty(&self) -> bool {
        self.log.is_empty()
    }

    /// Clears all queued insertions without executing them.
    pub fn clear(&mut self) {
        self.log.clear();
    }

    /// Queues an instruction to be inserted before the specified index.
    ///
    /// Multiple instructions can be inserted at the same index - they will
    /// appear in the order they were added to the insertion set.
    ///
    /// # Arguments
    ///
    /// * `idx` - The index before which to insert the instruction
    /// * `inst` - The instruction reference to insert
    ///
    /// # Note
    ///
    /// The insertion is not applied immediately. Call `execute` to apply
    /// all queued insertions.
    #[inline]
    pub fn insert_before(&mut self, idx: impl Into<u32>, inst: P::InstRef) {
        self.log.push((idx.into(), inst));
    }

    /// Applies all queued insertions to the specified block.
    ///
    /// This method processes all insertions in a single pass, building a new
    /// instruction sequence that includes both the original instructions
    /// (excluding no-ops) and the inserted instructions at their specified positions.
    ///
    /// After execution, the insertion set is ready to be reused for another block.
    ///
    /// # Arguments
    ///
    /// * `p` - Mutable reference to the IR implementation
    /// * `block` - The block to apply insertions to
    ///
    /// # Behavior
    ///
    /// - Instructions are inserted at their specified indices
    /// - No-op instructions from the original block are filtered out
    /// - Multiple insertions at the same index appear in insertion order
    /// - Insertions beyond the block length are appended to the end
    pub fn execute(&mut self, p: &mut P, block: P::BlockRef) {
        if self.log.is_empty() {
            return;
        }

        let buf = self.buf.get_or_insert_with(Vec::new);
        buf.clear();

        let mut log_idx = 0;
        let n_log_insts = self.log.len();
        let n_insts = p.block_insts(block).len();
        for idx in 0..n_insts {
            while log_idx < n_log_insts && self.log[log_idx].0 == idx as u32 {
                buf.push(self.log[log_idx].1);
                log_idx += 1;
            }
            let inst = p.block_insts(block)[idx];
            if !p.is_nop(inst) {
                buf.push(inst);
            }
        }
        while log_idx < n_log_insts {
            buf.push(self.log[log_idx].1);
            log_idx += 1;
        }

        self.log.clear();
        self.buf = Some(p.swap_block_insts(
            block,
            self.buf.take().expect("buffer should be initialized"),
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    // Mock IR implementation for testing
    #[derive(Debug)]
    struct MockIR {
        blocks: HashMap<u32, Vec<u32>>,
        nop_instructions: std::collections::HashSet<u32>,
    }

    impl MockIR {
        fn new() -> Self {
            Self {
                blocks: HashMap::new(),
                nop_instructions: std::collections::HashSet::new(),
            }
        }

        fn add_block(&mut self, block_id: u32, instructions: Vec<u32>) {
            self.blocks.insert(block_id, instructions);
        }

        fn mark_as_nop(&mut self, inst: u32) {
            self.nop_instructions.insert(inst);
        }

        fn get_block_insts(&self, block_id: u32) -> Vec<u32> {
            self.blocks.get(&block_id).cloned().unwrap_or_default()
        }
    }

    impl IR for MockIR {
        type InstRef = u32;
        type BlockRef = u32;

        fn is_nop(&self, inst: Self::InstRef) -> bool {
            self.nop_instructions.contains(&inst)
        }

        fn block_insts(&self, block: Self::BlockRef) -> &[Self::InstRef] {
            self.blocks.get(&block).map(|v| v.as_slice()).unwrap_or(&[])
        }

        fn swap_block_insts(
            &mut self,
            block: Self::BlockRef,
            insts: Vec<Self::InstRef>,
        ) -> Vec<Self::InstRef> {
            self.blocks.insert(block, insts).unwrap_or_default()
        }
    }

    #[test]
    fn test_empty_insertion_set() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2, 3]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.execute(&mut ir, 0);

        // Should remain unchanged
        assert_eq!(ir.get_block_insts(0), vec![1, 2, 3]);
    }

    #[test]
    fn test_single_insertion_at_beginning() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2, 3]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(0 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        assert_eq!(ir.get_block_insts(0), vec![99, 1, 2, 3]);
    }

    #[test]
    fn test_single_insertion_in_middle() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2, 3]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(1 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        assert_eq!(ir.get_block_insts(0), vec![1, 99, 2, 3]);
    }

    #[test]
    fn test_single_insertion_at_end() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2, 3]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(3 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        assert_eq!(ir.get_block_insts(0), vec![1, 2, 3, 99]);
    }

    #[test]
    fn test_multiple_insertions_same_position() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2, 3]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(1 as u32, 98);
        insertion_set.insert_before(1 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        assert_eq!(ir.get_block_insts(0), vec![1, 98, 99, 2, 3]);
    }

    #[test]
    fn test_multiple_insertions_different_positions() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2, 3]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(0 as u32, 97);
        insertion_set.insert_before(2 as u32, 98);
        insertion_set.insert_before(3 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        assert_eq!(ir.get_block_insts(0), vec![97, 1, 2, 98, 3, 99]);
    }

    #[test]
    fn test_nop_filtering() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2, 3, 4]);
        ir.mark_as_nop(2);
        ir.mark_as_nop(4);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(1 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        // No-ops (2, 4) should be filtered out
        assert_eq!(ir.get_block_insts(0), vec![1, 99, 3]);
    }

    #[test]
    fn test_insertion_beyond_block_length() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(5 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        assert_eq!(ir.get_block_insts(0), vec![1, 2, 99]);
    }

    #[test]
    fn test_empty_block() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![]);

        let mut insertion_set = InsertionSet::new();
        insertion_set.insert_before(0 as u32, 99);
        insertion_set.execute(&mut ir, 0);

        assert_eq!(ir.get_block_insts(0), vec![99]);
    }

    #[test]
    fn test_reuse_insertion_set() {
        let mut ir = MockIR::new();
        ir.add_block(0, vec![1, 2]);
        ir.add_block(1, vec![3, 4]);

        let mut insertion_set = InsertionSet::new();

        // First block
        insertion_set.insert_before(0 as u32, 99);
        insertion_set.execute(&mut ir, 0);
        assert_eq!(ir.get_block_insts(0), vec![99, 1, 2]);

        // Second block - insertion set should be cleared
        insertion_set.insert_before(1 as u32, 88);
        insertion_set.execute(&mut ir, 1);
        assert_eq!(ir.get_block_insts(1), vec![3, 88, 4]);
    }

    #[test]
    fn test_insertion_set_methods() {
        let mut insertion_set = InsertionSet::<MockIR>::new();

        assert!(insertion_set.is_empty());
        assert_eq!(insertion_set.len(), 0);

        insertion_set.insert_before(0 as u32, 1);
        insertion_set.insert_before(1 as u32, 2);

        assert!(!insertion_set.is_empty());
        assert_eq!(insertion_set.len(), 2);

        insertion_set.clear();

        assert!(insertion_set.is_empty());
        assert_eq!(insertion_set.len(), 0);
    }
}
