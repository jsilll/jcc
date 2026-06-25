use crate::ir::{analysis::order::Order, Block, Program};

use jcc_entity::{EntitySlice, SecondaryMap, SlicePool};

#[derive(Default)]
pub struct ControlFlowGraph {
    pool: SlicePool<Block>,
    in_degree: SecondaryMap<Block, u32>,
    preds: SecondaryMap<Block, EntitySlice<Block>>,
}

impl ControlFlowGraph {
    pub fn preds(&self, block: Block) -> impl IntoIterator<Item = Block> + '_ {
        self.pool[self.preds[block]].iter().copied()
    }

    pub fn compute(&mut self, prog: &Program, order: &Order) {
        self.pool.clear();

        for data in prog.functions.values() {
            if let Some(entry) = data.entry {
                // Compute degree of each block.
                for block in order.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        self.in_degree[succ] += 1;
                    }
                }

                // Allocate space for predecessors of each block.
                for block in order.rpo(entry) {
                    let degree = self.in_degree[block] as usize;
                    self.preds[block] = self.pool.extend(std::iter::repeat_n(block, degree));
                }

                // Fill predecessors of each block.
                for block in order.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        let idx = self.in_degree[succ] - 1;

                        self.in_degree[succ] = idx;
                        let slice = self.preds[succ];
                        self.pool[slice][idx as usize] = block;
                    }
                }
            }
        }
    }
}
