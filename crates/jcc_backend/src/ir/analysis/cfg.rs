use crate::ir::{
    analysis::{order::Order, BlockList},
    Block, Program,
};

use jcc_entity::{ListPool, SecondaryMap};

#[derive(Default)]
pub struct ControlFlowGraph {
    preds_lists: ListPool<Block>,
    degree: SecondaryMap<Block, u32>,
    preds: SecondaryMap<Block, BlockList>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn preds(&self, block: Block) -> &[Block] {
        self.preds_lists.get(self.preds[block])
    }

    pub fn compute(&mut self, prog: &Program, order: &Order) {
        self.preds_lists.clear();
        for data in prog.functions.values() {
            if let Some(entry) = data.entry {
                for &block in order.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        self.degree[succ] += 1;
                    }
                }
            }
        }
        for (block, &degree) in self.degree.iter() {
            let n = degree as usize;
            self.preds[block] = self.preds_lists.extend(std::iter::repeat_n(block, n));
        }
        for data in prog.functions.values() {
            if let Some(entry) = data.entry {
                for &block in order.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        let idx = (self.degree[succ] - 1) as usize;
                        self.preds_lists[self.preds[succ]][idx] = block;
                        self.degree[succ] = idx as u32;
                    }
                }
            }
        }
    }
}
