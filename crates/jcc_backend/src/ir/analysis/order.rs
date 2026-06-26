use jcc_entity::{EntitySlice, SecondaryMap, SlicePool};

use crate::ir::{Block, Program};

#[derive(Default)]
pub struct Order {
    scratch: Vec<Block>,
    pool: SlicePool<Block>,
    rpo_idx: SecondaryMap<Block, u32>,
    rpo: SecondaryMap<Block, EntitySlice<Block>>,
}

impl Order {
    pub fn rpo_idx(&self, block: Block) -> u32 {
        self.rpo_idx[block]
    }

    pub fn rpo(&self, block: Block) -> impl Iterator<Item = Block> + '_ {
        self.pool[self.rpo[block]].iter().copied()
    }

    pub fn compute(&mut self, prog: &Program) {
        self.pool.clear();
        for data in prog.functions.values() {
            self.scratch.extend(data.blocks_post(prog));
            let rpo = self.pool.extend(self.scratch.iter().rev().copied());
            for (idx, block) in self.scratch.drain(..).rev().enumerate() {
                self.rpo_idx[block] = idx as u32;
                self.rpo[block] = rpo;
            }
        }
    }
}
