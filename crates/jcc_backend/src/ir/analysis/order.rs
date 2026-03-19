use jcc_entity::{ListPool, SecondaryMap};

use crate::ir::{analysis::BlockList, Block, Program};

#[derive(Default)]
pub struct Order {
    scratch: Vec<Block>,
    rpo_lists: ListPool<Block>,
    rpo: SecondaryMap<Block, BlockList>,
}

impl Order {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn rpo(&self, block: Block) -> &[Block] {
        &self.rpo_lists[self.rpo[block]]
    }

    pub fn compute(&mut self, prog: &Program) {
        self.rpo_lists.clear();
        for data in prog.functions.values() {
            self.scratch.extend(data.blocks_post(prog));
            let rpo = self.rpo_lists.extend(self.scratch.iter().rev().copied());
            for block in self.scratch.drain(..) {
                self.rpo[block] = rpo;
            }
        }
    }
}
