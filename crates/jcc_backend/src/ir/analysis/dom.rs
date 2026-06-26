use crate::ir::{
    analysis::{cfg::ControlFlowGraph, order::Order},
    Block, Program,
};

use jcc_entity::{EntitySlice, PackedOption, SecondaryMap, SlicePool};

#[derive(Default)]
pub struct Dominance {
    /// A temporary map for storing node degrees.
    degree: SecondaryMap<Block, u32>,
    /// The immediate dominator (idom) of each block.
    idom: SecondaryMap<Block, PackedOption<Block>>,

    /// A pool of blocks storing dominator tree children.
    children_pool: SlicePool<Block>,
    /// Maps each block to a slice of its children in the dominator tree.
    children: SecondaryMap<Block, EntitySlice<Block>>,

    /// A pool of blocks storing dominance frontiers.
    frontier_pool: SlicePool<Block>,
    /// Maps each block to a slice of blocks in its dominance frontier.
    frontier: SecondaryMap<Block, EntitySlice<Block>>,
}

impl Dominance {
    /// Returns the immediate dominator of the given block, if it has one.
    pub fn idom(&self, block: Block) -> Option<Block> {
        self.idom[block].expand()
    }

    /// Returns an iterator over the children of the given block in the dominator tree.
    pub fn children(&self, block: Block) -> impl Iterator<Item = Block> + '_ {
        let slice = self.children[block];
        self.children_pool[slice].iter().copied()
    }

    /// Returns an iterator over the blocks in the dominance frontier of the given block.
    pub fn frontier(&self, block: Block) -> impl Iterator<Item = Block> + '_ {
        let slice = self.frontier[block];
        self.frontier_pool[slice].iter().copied()
    }

    /// Computes the dominance relation, the dominator tree, and
    /// the dominance frontiers for all functions in the program.
    pub fn compute(&mut self, prog: &Program, ord: &Order, cfg: &ControlFlowGraph) {
        self.children_pool.clear();
        self.frontier_pool.clear();

        for (_, data) in prog.functions.iter() {
            let Some(entry) = data.entry else {
                continue;
            };

            let mut changed = true;
            self.idom[entry] = entry.into();

            while changed {
                changed = false;

                for block in ord.rpo(entry) {
                    let Some(mut dom) = cfg
                        .preds(block)
                        .into_iter()
                        .find(|pred| self.idom(*pred).is_some())
                    else {
                        continue;
                    };

                    for pred in cfg.preds(block) {
                        if pred == dom || self.idom[pred].is_none() {
                            continue;
                        }
                        dom = self.intersect(dom, pred, ord);
                    }

                    if self.idom[block] != dom.into() {
                        self.idom[block] = dom.into();
                        changed = true;
                    }
                }
            }

            self.compute_children(entry, ord);
            self.compute_frontier(entry, ord, cfg);
        }
    }

    fn compute_children(&mut self, entry: Block, ord: &Order) {
        debug_assert!(ord.rpo(entry).all(|b| self.degree[b] == 0));

        // Count children.
        for block in ord.rpo(entry) {
            if let Some(idom) = self.idom(block) {
                if block != idom {
                    self.degree[idom] += 1;
                }
            }
        }

        // Allocate slices.
        for block in ord.rpo(entry) {
            let degree = self.degree[block] as usize;
            self.children[block] = self
                .children_pool
                .extend(std::iter::repeat_n(block, degree));
        }

        // Fill slices.
        //
        // Reuse `degree` as a reverse insertion cursor.
        for block in ord.rpo(entry) {
            if let Some(idom) = self.idom(block) {
                if block != idom {
                    let idx = self.degree[idom] - 1;

                    self.degree[idom] = idx;
                    let slice = self.children[idom];
                    self.children_pool[slice][idx as usize] = block;
                }
            }
        }
    }

    fn compute_frontier(&mut self, entry: Block, ord: &Order, cfg: &ControlFlowGraph) {
        debug_assert!(ord.rpo(entry).all(|b| self.degree[b] == 0));

        // Count frontier size.
        for block in ord.rpo(entry) {
            if cfg.preds(block).nth(1).is_none() {
                continue;
            };
            self.walk_frontier(block, cfg, |this, runner| {
                this.degree[runner] += 1;
            });
        }

        // Allocate slices.
        for block in ord.rpo(entry) {
            let degree = self.degree[block] as usize;
            self.frontier[block] = self
                .frontier_pool
                .extend(std::iter::repeat_n(block, degree));
        }

        // Fill slices.
        //
        // Reuse `degree` as a reverse insertion cursor.
        for block in ord.rpo(entry) {
            if cfg.preds(block).nth(1).is_none() {
                continue;
            };
            self.walk_frontier(block, cfg, |this, runner| {
                let idx = this.degree[runner] - 1;

                this.degree[runner] = idx;
                let slice = this.frontier[runner];
                this.frontier_pool[slice][idx as usize] = block;
            });
        }
    }

    fn walk_frontier(
        &mut self,
        block: Block,
        cfg: &ControlFlowGraph,
        mut visit: impl FnMut(&mut Self, Block),
    ) {
        let Some(idom) = self.idom(block) else {
            return;
        };
        for mut runner in cfg.preds(block) {
            while idom != runner {
                let Some(next) = self.idom(runner) else {
                    break;
                };
                visit(self, runner);
                runner = next;
            }
        }
    }

    fn intersect(&self, mut lhs: Block, mut rhs: Block, ord: &Order) -> Block {
        while lhs != rhs {
            while ord.rpo_idx(lhs) > ord.rpo_idx(rhs) {
                lhs = self.idom(lhs).expect("block has no idom");
            }
            while ord.rpo_idx(rhs) > ord.rpo_idx(lhs) {
                rhs = self.idom(rhs).expect("block has no idom");
            }
        }
        lhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        ir::testutil::{check_parse, parse_ir},
        IdentInterner,
    };

    use jcc_codemap::simple::SimpleFiles;

    fn setup(input: &str) -> (Vec<Block>, Dominance) {
        let mut db = SimpleFiles::new();
        let mut interner = IdentInterner::new();
        let ir = parse_ir(&mut db, &mut interner, input);
        check_parse(&mut db, &ir).unwrap_or_else(|report| panic!("{report}"));

        let prog = &ir.program;
        let mut ord = Order::default();
        let mut dom = Dominance::default();
        let mut cfg = ControlFlowGraph::default();

        ord.compute(prog);
        cfg.compute(prog, &ord);
        dom.compute(prog, &ord, &cfg);

        let entry = prog.functions.values().next().unwrap().entry.unwrap();
        (ord.rpo(entry).into_iter().collect(), dom)
    }

    #[test]
    fn simple() {
        let (rpo, dom) = setup(
            r#"
            define @classify {
            bb0:
              %0 = param i1 #0
              br i1 %0, bb1, bb2

            bb1:
              br bb3

            bb2:
              br bb3

            bb3:
              ret void
            }
        "#,
        );

        assert_eq!(rpo.len(), 4);

        assert!(dom.frontier(rpo[0]).eq([]));
        assert!(dom.frontier(rpo[1]).eq([rpo[3]]));
        assert!(dom.frontier(rpo[2]).eq([rpo[3]]));
        assert!(dom.frontier(rpo[3]).eq([]));
    }

    #[test]
    fn looped() {
        let (rpo, dom) = setup(
            r#"
            define @loop {
            bb0:
              br bb1

            bb1:
              %0 = param i1 #0
              br i1 %0, bb2, bb3

            bb2:
              br bb4

            bb3:
              br bb4

            bb4:
              %1 = param i1 #1
              br i1 %1, bb1, bb5

            bb5:
              ret void
            }
        "#,
        );

        assert_eq!(rpo.len(), 6);

        assert!(dom.frontier(rpo[0]).eq([]));
        assert!(dom.frontier(rpo[1]).eq([rpo[1]]));
        assert!(dom.frontier(rpo[2]).eq([rpo[4]]));
        assert!(dom.frontier(rpo[3]).eq([rpo[4]]));
        assert!(dom.frontier(rpo[4]).eq([rpo[1]]));
        assert!(dom.frontier(rpo[5]).eq([]));
    }
}
