use crate::ir::{
    analysis::{order::Order, BlockList},
    Block, Program,
};

use jcc_entity::{ListPool, SecondaryMap};

#[derive(Default)]
pub struct ControlFlowGraph {
    preds_pool: ListPool<Block>,
    degree: SecondaryMap<Block, u32>,
    preds: SecondaryMap<Block, BlockList>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn preds(&self, block: Block) -> impl IntoIterator<Item = Block> + '_ {
        self.preds_pool[self.preds[block]].iter().copied()
    }

    pub fn compute(&mut self, prog: &Program, order: &Order) {
        self.preds_pool.clear();
        for data in prog.functions.values() {
            if let Some(entry) = data.entry {
                // Compute degree of each block.
                for block in order.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        self.degree[succ] += 1;
                    }
                }

                // Allocate space for predecessors of each block.
                for block in order.rpo(entry) {
                    let degree = self.degree[block] as usize;
                    self.preds[block] = self.preds_pool.extend(std::iter::repeat_n(block, degree));
                }

                // Fill predecessors of each block.
                for block in order.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        let idx = (self.degree[succ] - 1) as usize;
                        self.preds_pool[self.preds[succ]][idx] = block;
                        self.degree[succ] = idx as u32;
                    }
                }
            }
        }
    }
}
