use crate::ir::{analysis::order::Order, Block, Program};

use jcc_entity::{EntitySlice, SecondaryMap, SlicePool};

#[derive(Default)]
pub struct ControlFlowGraph {
    pool: SlicePool<Block>,
    in_degree: SecondaryMap<Block, u32>,
    preds: SecondaryMap<Block, EntitySlice<Block>>,
}

impl ControlFlowGraph {
    /// Returns one item for every incoming CFG edge.
    ///
    /// ## Notes
    ///
    /// If multiple edges originate from the same predecessor
    /// (e.g. a `switch` with repeated destinations), the predecessor appears multiple times.
    pub fn preds(&self, block: Block) -> impl Iterator<Item = Block> + '_ {
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

#[cfg(test)]
mod tests {
    use super::*;

    use jcc_codemap::simple::SimpleFiles;

    use crate::{
        ir::testutil::{check_parse, parse_ir},
        IdentInterner,
    };

    fn setup(input: &str) -> ControlFlowGraph {
        let mut db = SimpleFiles::new();
        let mut interner = IdentInterner::new();
        let ir = parse_ir(&mut db, &mut interner, input);
        check_parse(&mut db, &ir).unwrap_or_else(|report| panic!("{report}"));
        let prog = &ir.program;
        let mut ord = Order::default();
        let mut cfg = ControlFlowGraph::default();
        ord.compute(prog);
        cfg.compute(prog, &ord);
        cfg
    }

    #[test]
    fn linear() {
        let cfg = setup(
            r#"
            define @chain {
            bb0:
              br bb1
            bb1:
              br bb2
            bb2:
              ret void
            }
        "#,
        );

        assert!(cfg.preds(Block::from_u32(0)).eq([]));
        assert!(cfg.preds(Block::from_u32(1)).eq([Block::from_u32(0)]));
        assert!(cfg.preds(Block::from_u32(2)).eq([Block::from_u32(1)]));
    }

    #[test]
    fn diamond() {
        let cfg = setup(
            r#"
            define @diamond {
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

        assert!(cfg.preds(Block::from_u32(0)).eq([]));
        assert!(cfg.preds(Block::from_u32(1)).eq([Block::from_u32(0)]));
        assert!(cfg.preds(Block::from_u32(2)).eq([Block::from_u32(0)]));
        assert!(cfg
            .preds(Block::from_u32(3))
            .eq([Block::from_u32(1), Block::from_u32(2)]));
    }

    #[test]
    fn back_edge() {
        let cfg = setup(
            r#"
            define @loop {
            bb0:
              br bb1
            bb1:
              %0 = param i1 #0
              br i1 %0, bb2, bb3
            bb2:
              br bb1
            bb3:
              ret void
            }
        "#,
        );

        assert!(cfg.preds(Block::from_u32(0)).eq([]));
        assert!(cfg
            .preds(Block::from_u32(1))
            .eq([Block::from_u32(2), Block::from_u32(0)]));
        assert!(cfg.preds(Block::from_u32(2)).eq([Block::from_u32(1)]));
        assert!(cfg.preds(Block::from_u32(3)).eq([Block::from_u32(1)]));
    }

    #[test]
    fn switch() {
        let cfg = setup(
            r#"
            define @sw {
            bb0:
              %0 = param i32 #0
              switch %0 [ default: bb3, 0: bb1, 1: bb2 ]
            bb1:
              ret void
            bb2:
              ret void
            bb3:
              ret void
            }
        "#,
        );

        assert!(cfg.preds(Block::from_u32(0)).eq([]));
        assert!(cfg.preds(Block::from_u32(1)).eq([Block::from_u32(0)]));
        assert!(cfg.preds(Block::from_u32(2)).eq([Block::from_u32(0)]));
        assert!(cfg.preds(Block::from_u32(3)).eq([Block::from_u32(0)]));
    }

    #[test]
    fn switch_repeated_target() {
        let cfg = setup(
            r#"
            define @sw_shared {
            bb0:
              %0 = param i32 #0
              switch %0 [ default: bb1, 0: bb1, 1: bb2 ]
            bb1:
              ret void
            bb2:
              ret void
            }
        "#,
        );

        assert!(cfg.preds(Block::from_u32(0)).eq([]));
        assert!(cfg
            .preds(Block::from_u32(1))
            .eq([Block::from_u32(0), Block::from_u32(0)]));
        assert!(cfg.preds(Block::from_u32(2)).eq([Block::from_u32(0)]));
    }
}
