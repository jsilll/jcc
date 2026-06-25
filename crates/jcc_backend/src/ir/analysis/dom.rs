use crate::ir::{
    analysis::{cfg::ControlFlowGraph, order::Order},
    Block, Program,
};

use jcc_entity::{EntitySlice, PackedOption, SecondaryMap, SlicePool};

#[derive(Default)]
pub struct Dominance {
    pool: SlicePool<Block>,
    degree: SecondaryMap<Block, u32>,
    idom: SecondaryMap<Block, PackedOption<Block>>,
    children: SecondaryMap<Block, EntitySlice<Block>>,
}

impl Dominance {
    pub fn idom(&self, block: Block) -> Option<Block> {
        self.idom[block].expand()
    }

    pub fn children(&self, block: Block) -> impl IntoIterator<Item = Block> + '_ {
        let slice = self.children[block];
        self.pool[slice].iter().copied()
    }

    pub fn compute(&mut self, prog: &Program, order: &Order, cfg: &ControlFlowGraph) {
        self.pool.clear();

        for (_, data) in prog.functions.iter() {
            if let Some(entry) = data.entry {
                let mut changed = true;
                self.idom[entry] = entry.into();
                while changed {
                    changed = false;
                    for block in order.rpo(entry) {
                        if let Some(mut dom) = cfg
                            .preds(block)
                            .into_iter()
                            .find(|pred| self.idom[*pred].is_some())
                        {
                            for mut pred in cfg.preds(block) {
                                if pred == dom || self.idom[pred].is_none() {
                                    continue;
                                }
                                while pred != dom {
                                    while order.rpo_idx(pred) > order.rpo_idx(dom) {
                                        pred = self.idom[pred].expect("pred has no idom");
                                    }
                                    while order.rpo_idx(dom) > order.rpo_idx(pred) {
                                        dom = self.idom[dom].expect("block has no idom");
                                    }
                                }
                            }
                            if self.idom[block] != dom.into() {
                                self.idom[block] = dom.into();
                                changed = true;
                            }
                        }
                    }
                }

                // Compute the degree of each block.
                for block in order.rpo(entry) {
                    if let Some(idom) = self.idom(block) {
                        if block != idom {
                            self.degree[idom] += 1;
                        }
                    }
                }

                // Allocate space for children of each block.
                for block in order.rpo(entry) {
                    let degree = self.degree[block] as usize;
                    self.children[block] = self.pool.extend(std::iter::repeat_n(block, degree));
                }

                // Fill children of each block.
                for block in order.rpo(entry) {
                    if let Some(idom) = self.idom(block) {
                        if block != idom {
                            let idx = self.degree[idom] - 1;

                            self.degree[idom] = idx;
                            let slice = self.children[idom];
                            self.pool[slice][idx as usize] = block;
                        }
                    }
                }
            }
        }
    }
}
