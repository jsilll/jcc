use jcc_entity::{ListPool, SecondaryMap};

use crate::ir::{analysis::BlockList, Block, Program};

#[derive(Default)]
pub struct Order {
    scratch: Vec<Block>,
    rpo_lists: ListPool<Block>,
    rpo_idx: SecondaryMap<Block, u32>,
    rpo: SecondaryMap<Block, BlockList>,
}

impl Order {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn rpo_idx(&self, block: Block) -> u32 {
        self.rpo_idx[block]
    }

    pub fn rpo(&self, block: Block) -> impl IntoIterator<Item = Block> + '_ {
        self.rpo_lists[self.rpo[block]].iter().copied()
    }

    pub fn compute(&mut self, prog: &Program) {
        self.rpo_lists.clear();
        for data in prog.functions.values() {
            self.scratch.extend(data.blocks_post(prog));
            let rpo = self.rpo_lists.extend(self.scratch.iter().rev().copied());
            for (idx, block) in self.scratch.drain(..).rev().enumerate() {
                self.rpo_idx[block] = idx as u32;
                self.rpo[block] = rpo;
            }
        }
    }
}
