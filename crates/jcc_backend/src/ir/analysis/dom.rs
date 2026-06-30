use crate::ir::{
    analysis::{cfg::ControlFlowGraph, order::Order},
    Block, Program,
};

use jcc_entity::{
    slice::{bucket::BucketScratch, EntitySlice, SlicePool},
    PackedOption, SecondaryMap,
};

type IDomMap = SecondaryMap<Block, PackedOption<Block>>;

#[derive(Default)]
pub struct Dominance {
    /// The immediate dominator of each block.
    idom: IDomMap,
    /// A pool of blocks storing dominator tree children.
    pool: SlicePool<Block>,
    /// A scratch map for the bucket builders.
    scratch: BucketScratch<Block>,
    /// Maps each block to a slice of its children in the dominator tree.
    children: SecondaryMap<Block, EntitySlice<Block>>,
    /// Maps each block to a slice of blocks in its dominance frontier.
    frontier: SecondaryMap<Block, EntitySlice<Block>>,
}

impl Dominance {
    /// Returns the immediate dominator of the given block, if it has one.
    pub fn idom(&self, block: Block) -> Option<Block> {
        self.idom[block].expand()
    }

    /// Returns the dominated children of `block`.
    pub fn children(&self, block: Block) -> impl Iterator<Item = Block> + '_ {
        let slice = self.children[block];
        self.pool[slice].iter().copied()
    }

    /// Returns the dominance frontier of `block`.
    ///
    /// ## Notes
    ///
    /// - The iterator may contain duplicate blocks.
    /// - Consumers requiring set semantics are expected to deduplicate.
    pub fn frontier(&self, block: Block) -> impl Iterator<Item = Block> + '_ {
        let slice = self.frontier[block];
        self.pool[slice].iter().copied()
    }

    /// Computes the dominance relation, the dominator tree, and
    /// the dominance frontiers for all functions in the program.
    pub fn compute(&mut self, prog: &Program, ord: &Order, cfg: &ControlFlowGraph) {
        self.idom.clear();
        self.pool.clear();
        self.children.clear();
        self.frontier.clear();

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
        let mut b = self.scratch.builder(&mut self.pool, &mut self.children);
        for block in ord.rpo(entry) {
            if let Some(idom) = self.idom[block].expand() {
                if block != idom {
                    b.bump(idom);
                }
            }
        }
        let mut b = b.allocate();
        for block in ord.rpo(entry) {
            if let Some(idom) = self.idom[block].expand() {
                if block != idom {
                    b.push(idom, block);
                }
            }
        }
    }

    fn compute_frontier(&mut self, entry: Block, ord: &Order, cfg: &ControlFlowGraph) {
        let mut b = self.scratch.builder(&mut self.pool, &mut self.frontier);
        for block in ord.rpo(entry) {
            if cfg.preds(block).nth(1).is_none() {
                continue;
            };
            Self::walk_frontier(block, &self.idom, cfg, |runner| {
                b.bump(runner);
            });
        }
        let mut b = b.allocate();
        for block in ord.rpo(entry) {
            if cfg.preds(block).nth(1).is_none() {
                continue;
            };
            Self::walk_frontier(block, &self.idom, cfg, |runner| {
                b.push(runner, block);
            });
        }
    }

    fn walk_frontier(
        block: Block,
        idom: &IDomMap,
        cfg: &ControlFlowGraph,
        mut visit: impl FnMut(Block),
    ) {
        let Some(dom) = idom[block].expand() else {
            return;
        };
        for mut runner in cfg.preds(block) {
            while dom != runner {
                let Some(next) = idom[runner].expand() else {
                    break;
                };
                visit(runner);
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

    fn setup(input: &str) -> Dominance {
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
        dom
    }

    #[test]
    fn simple() {
        let dom = setup(
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

        assert!(dom.frontier(Block::from_u32(0)).eq([]));
        assert!(dom.frontier(Block::from_u32(1)).eq([Block::from_u32(3)]));
        assert!(dom.frontier(Block::from_u32(2)).eq([Block::from_u32(3)]));
        assert!(dom.frontier(Block::from_u32(3)).eq([]));
    }

    #[test]
    fn looped() {
        let dom = setup(
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

        assert!(dom.frontier(Block::from_u32(0)).eq([]));
        assert!(dom.frontier(Block::from_u32(1)).eq([Block::from_u32(1)]));
        assert!(dom.frontier(Block::from_u32(2)).eq([Block::from_u32(4)]));
        assert!(dom.frontier(Block::from_u32(3)).eq([Block::from_u32(4)]));
        assert!(dom.frontier(Block::from_u32(4)).eq([Block::from_u32(1)]));
        assert!(dom.frontier(Block::from_u32(5)).eq([]));
    }

    #[test]
    fn non_dominating_diamond() {
        let dom = setup(
            r#"
        define @diamond {
        bb0:
          %0 = param i1 #0
          br i1 %0, bb1, bb2

        bb1:
          %1 = param i1 #1
          br i1 %1, bb3, bb4

        bb2:
          br bb5

        bb3:
          br bb5

        bb4:
          br bb5

        bb5:
          ret void
        }
    "#,
        );

        // Note: For now the frontier() API does not provide set semantics
        let special = [Block::from_u32(5), Block::from_u32(5)];

        assert!(dom.frontier(Block::from_u32(0)).eq([]));
        assert!(dom.frontier(Block::from_u32(1)).eq(special));
        assert!(dom.frontier(Block::from_u32(2)).eq([Block::from_u32(5)]));
        assert!(dom.frontier(Block::from_u32(3)).eq([Block::from_u32(5)]));
        assert!(dom.frontier(Block::from_u32(4)).eq([Block::from_u32(5)]));
        assert!(dom.frontier(Block::from_u32(5)).eq([]));
    }
}
