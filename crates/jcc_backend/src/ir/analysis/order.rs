use jcc_entity::{EntityList, ListPool, SecondaryMap};

use crate::ir::{Block, Program};

type RpoList = EntityList<Block>;

#[derive(Default)]
pub struct Order {
    scratch: Vec<Block>,
    rpo_lists: ListPool<Block>,
    rpo: SecondaryMap<Block, RpoList>,
}

impl Order {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn rpo(&self, block: Block) -> &[Block] {
        &self.rpo_lists[self.rpo[block]]
    }

    pub fn clear(&mut self) {
        self.rpo.clear();
        self.rpo_lists.clear();
    }

    pub fn compute(&mut self, prog: &Program) {
        self.clear();
        for data in prog.functions.values() {
            self.scratch.extend(data.blocks_post(prog));
            let rpo = self.rpo_lists.extend(self.scratch.iter().rev().copied());
            for block in self.scratch.drain(..) {
                self.rpo[block] = rpo;
            }
        }
    }
}
