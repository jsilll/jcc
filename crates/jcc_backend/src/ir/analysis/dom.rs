use crate::ir::{
    analysis::{cfg::ControlFlowGraph, order::Order},
    Block, Program,
};

use jcc_entity::{EntitySlice, SecondaryMap, SlicePool};

#[derive(Default)]
pub struct Dominance {
    pool: SlicePool<Block>,
    degree: SecondaryMap<Block, u32>,
    idom: SecondaryMap<Block, Option<Block>>,
    children: SecondaryMap<Block, EntitySlice<Block>>,
}

impl Dominance {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn idom(&self, block: Block) -> Option<Block> {
        self.idom[block]
    }

    pub fn children(&self, block: Block) -> impl IntoIterator<Item = Block> + '_ {
        self.pool[self.children[block]].iter().copied()
    }

    pub fn compute(&mut self, prog: &Program, order: &Order, cfg: &ControlFlowGraph) {
        for (_, data) in prog.functions.iter() {
            if let Some(entry) = data.entry {
                self.idom[entry] = Some(entry);
                let mut changed = true;
                while changed {
                    changed = false;

                    for block in order.rpo(entry) {
                        if let Some(mut dom) = cfg
                            .preds(block)
                            .into_iter()
                            .find(|pred| self.idom[*pred].is_some())
                        {
                            for pred in cfg.preds(block) {
                                if pred == dom || self.idom[pred].is_none() {
                                    continue;
                                }
                                let mut pred = pred;
                                while pred != dom {
                                    while order.rpo_idx(pred) > order.rpo_idx(dom) {
                                        pred = self.idom[pred].expect("block has no idom");
                                    }
                                    while order.rpo_idx(dom) > order.rpo_idx(pred) {
                                        dom = self.idom[dom].expect("block has no idom");
                                    }
                                }
                            }
                            if self.idom[block] != Some(dom) {
                                changed = true;
                                self.idom[block] = Some(dom);
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
                            let idx = (self.degree[idom] - 1) as usize;
                            self.pool[self.children[idom]][idx] = block;
                            self.degree[idom] = idx as u32;
                        }
                    }
                }
            }
        }
    }
}
