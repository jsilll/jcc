use super::{Indexed, IR};

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
#[derive(Debug)]
pub struct InsertionSet<P: IR> {
    /// Log of pending insertions: (index, instruction)
    /// Sorted by insertion order, not necessarily by index
    log: Vec<(u32, P::Inst)>,
    /// Buffer for building the new instruction sequence
    buf: Option<Vec<P::InstRef>>,
}

impl<P: IR> Default for InsertionSet<P> {
    fn default() -> Self {
        Self {
            buf: None,
            log: Vec::new(),
        }
    }
}

impl<P: IR> InsertionSet<P> {
    /// Creates a new empty insertion set.
    ///
    /// This is equivalent to `InsertionSet::default()` but more explicit.
    #[inline]
    pub fn new() -> Self {
        Self::default()
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
    /// * `inst` - The instruction to insert
    ///
    /// # Note
    ///
    /// The insertion is not applied immediately. Call `execute` to apply
    /// all queued insertions.
    #[inline]
    pub fn before(&mut self, idx: impl Into<u32>, inst: P::Inst) {
        self.log.push((idx.into(), inst));
    }

    /// Queues an instruction to be inserted after the specified index.
    ///
    /// Multiple instructions can be inserted at the same index - they will
    /// appear in the order they were added to the insertion set.
    ///
    /// # Arguments
    ///
    /// * `idx` - The index after which to insert the instruction
    /// * `inst` - The instruction to insert
    ///
    /// # Note
    ///
    /// The insertion is not applied immediately. Call `execute` to apply
    /// all queued insertions.
    #[inline]
    pub fn after(&mut self, idx: impl Into<u32>, inst: P::Inst) {
        self.log.push((idx.into() + 1, inst));
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
    pub fn apply(&mut self, p: &mut P, block: P::BlockRef) {
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
                let mut inst = self.log[log_idx].1.clone();
                inst.set_idx(buf.len() as u32);
                buf.push(p.new_inst(inst));
                log_idx += 1;
            }
            let inst = p.block_insts(block)[idx];
            if !p.is_nop(inst) {
                buf.push(inst);
            }
        }

        self.log[log_idx..].iter().for_each(|(_, inst)| {
            let mut inst = inst.clone();
            inst.set_idx(buf.len() as u32);
            buf.push(p.new_inst(inst));
        });

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

    #[derive(Debug, Clone, PartialEq)]
    struct TestInst {
        id: u32,
        idx: u32,
        is_nop: bool,
    }

    impl TestInst {
        fn new(id: u32) -> Self {
            Self {
                id,
                idx: 0,
                is_nop: false,
            }
        }

        fn nop() -> Self {
            Self {
                id: 0,
                idx: 0,
                is_nop: true,
            }
        }
    }

    impl Indexed for TestInst {
        fn set_idx(&mut self, idx: u32) {
            self.idx = idx;
        }
    }

    #[derive(Debug)]
    struct TestIR {
        instructions: Vec<TestInst>,
        blocks: HashMap<u32, Vec<u32>>,
    }

    impl TestIR {
        fn new() -> Self {
            Self {
                blocks: HashMap::new(),
                instructions: Vec::new(),
            }
        }

        fn create_block(&mut self, block_id: u32, insts: Vec<TestInst>) -> u32 {
            let mut indices = Vec::new();
            for inst in insts {
                indices.push(self.instructions.len() as u32);
                self.instructions.push(inst);
            }
            self.blocks.insert(block_id, indices);
            block_id
        }

        fn get_block_instructions(&self, block_id: u32) -> Vec<&TestInst> {
            self.blocks
                .get(&block_id)
                .map(|indices| {
                    indices
                        .iter()
                        .map(|&i| &self.instructions[i as usize])
                        .collect()
                })
                .unwrap_or_else(Vec::new)
        }
    }

    impl IR for TestIR {
        type Inst = TestInst;
        type InstRef = u32;
        type BlockRef = u32;

        fn is_nop(&self, inst: Self::InstRef) -> bool {
            self.instructions[inst as usize].is_nop
        }

        fn new_inst(&mut self, inst: Self::Inst) -> Self::InstRef {
            let idx = self.instructions.len() as u32;
            self.instructions.push(inst);
            idx
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
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block = ir.create_block(1, vec![TestInst::new(1), TestInst::new(2)]);
        assert!(insertion_set.is_empty());
        assert_eq!(insertion_set.len(), 0);

        insertion_set.apply(&mut ir, block);
        let instructions = ir.get_block_instructions(1);

        assert_eq!(instructions.len(), 2);
        assert_eq!(instructions[0].id, 1);
        assert_eq!(instructions[1].id, 2);
    }

    #[test]
    fn test_insert_before() {
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block = ir.create_block(1, vec![TestInst::new(1), TestInst::new(2)]);
        insertion_set.before(0_u32, TestInst::new(10));
        insertion_set.before(1_u32, TestInst::new(11));

        assert_eq!(insertion_set.len(), 2);

        insertion_set.apply(&mut ir, block);
        let instructions = ir.get_block_instructions(1);

        assert_eq!(instructions.len(), 4);
        assert_eq!(instructions[0].id, 10);
        assert_eq!(instructions[1].id, 1);
        assert_eq!(instructions[2].id, 11);
        assert_eq!(instructions[3].id, 2);
    }

    #[test]
    fn test_insert_after() {
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block = ir.create_block(1, vec![TestInst::new(1), TestInst::new(2)]);
        insertion_set.after(0_u32, TestInst::new(10));
        insertion_set.after(1_u32, TestInst::new(11));

        insertion_set.apply(&mut ir, block);
        let instructions = ir.get_block_instructions(1);

        assert_eq!(instructions.len(), 4);
        assert_eq!(instructions[0].id, 1);
        assert_eq!(instructions[1].id, 10);
        assert_eq!(instructions[2].id, 2);
        assert_eq!(instructions[3].id, 11);
    }

    #[test]
    fn test_multiple_insertions_at_same_index() {
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block = ir.create_block(1, vec![TestInst::new(1), TestInst::new(2)]);
        insertion_set.before(1_u32, TestInst::new(10));
        insertion_set.before(1_u32, TestInst::new(11));
        insertion_set.before(1_u32, TestInst::new(12));

        insertion_set.apply(&mut ir, block);
        let instructions = ir.get_block_instructions(1);

        assert_eq!(instructions.len(), 5);
        assert_eq!(instructions[0].id, 1);
        assert_eq!(instructions[1].id, 10);
        assert_eq!(instructions[2].id, 11);
        assert_eq!(instructions[3].id, 12);
        assert_eq!(instructions[4].id, 2);
    }

    #[test]
    fn test_insertions_beyond_block_length() {
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block = ir.create_block(1, vec![TestInst::new(1), TestInst::new(2)]);
        insertion_set.before(5_u32, TestInst::new(10));
        insertion_set.before(10_u32, TestInst::new(11));

        insertion_set.apply(&mut ir, block);
        let instructions = ir.get_block_instructions(1);

        assert_eq!(instructions.len(), 4);
        assert_eq!(instructions[0].id, 1);
        assert_eq!(instructions[1].id, 2);
        assert_eq!(instructions[2].id, 10);
        assert_eq!(instructions[3].id, 11);
    }

    #[test]
    fn test_nop_filtering() {
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block = ir.create_block(
            1,
            vec![
                TestInst::new(1),
                TestInst::nop(),
                TestInst::new(2),
                TestInst::nop(),
            ],
        );
        insertion_set.before(1_u32, TestInst::new(10));
        insertion_set.before(3_u32, TestInst::new(11));

        insertion_set.apply(&mut ir, block);
        let instructions = ir.get_block_instructions(1);

        assert_eq!(instructions.len(), 4);
        assert_eq!(instructions[0].id, 1);
        assert_eq!(instructions[1].id, 10);
        assert_eq!(instructions[2].id, 2);
        assert_eq!(instructions[3].id, 11);
    }

    #[test]
    fn test_index_updating() {
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block = ir.create_block(1, vec![TestInst::new(1), TestInst::new(2)]);
        insertion_set.before(0_u32, TestInst::new(10));
        insertion_set.before(1_u32, TestInst::new(11));

        insertion_set.apply(&mut ir, block);

        let instructions = ir.get_block_instructions(1);
        assert_eq!(instructions[0].idx, 0);
        assert_eq!(instructions[2].idx, 2);
    }

    #[test]
    fn test_clear() {
        let mut insertion_set: InsertionSet<TestIR> = InsertionSet::new();
        insertion_set.before(0_u32, TestInst::new(10));
        insertion_set.before(1_u32, TestInst::new(11));

        assert_eq!(insertion_set.len(), 2);
        assert!(!insertion_set.is_empty());

        insertion_set.clear();

        assert_eq!(insertion_set.len(), 0);
        assert!(insertion_set.is_empty());
    }

    #[test]
    fn test_reuse_after_apply() {
        let mut ir = TestIR::new();
        let mut insertion_set = InsertionSet::new();

        let block1 = ir.create_block(1, vec![TestInst::new(1), TestInst::new(2)]);
        let block2 = ir.create_block(2, vec![TestInst::new(3), TestInst::new(4)]);

        insertion_set.before(0_u32, TestInst::new(10));
        insertion_set.apply(&mut ir, block1);

        assert!(insertion_set.is_empty());

        insertion_set.before(1_u32, TestInst::new(20));
        insertion_set.apply(&mut ir, block2);

        let instructions1 = ir.get_block_instructions(1);
        assert_eq!(instructions1.len(), 3);
        assert_eq!(instructions1[0].id, 10);
        assert_eq!(instructions1[1].id, 1);
        assert_eq!(instructions1[2].id, 2);

        let instructions2 = ir.get_block_instructions(2);
        assert_eq!(instructions2.len(), 3);
        assert_eq!(instructions2[0].id, 3);
        assert_eq!(instructions2[1].id, 20);
        assert_eq!(instructions2[2].id, 4);
    }
}
