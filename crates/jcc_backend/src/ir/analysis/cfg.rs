use crate::ir::{analysis::order::Order, Block, Program};

use jcc_entity::{
    slice::{bucket::BucketScratch, EntitySlice, SlicePool},
    SecondaryMap,
};

#[derive(Default)]
pub struct Control {
    pool: SlicePool<Block>,
    entries: EntitySlice<Block>,
    scratch: BucketScratch<Block>,
    slices: SecondaryMap<Block, EntitySlice<Block>>,
}

impl Control {
    /// Returns the entry block for every function in the program.
    pub fn entries(&self) -> &[Block] {
        &self.pool[self.entries]
    }

    /// Returns one block for every incoming CFG edge.
    ///
    /// ## Notes
    ///
    /// If multiple edges originate from the same predecessor
    /// (e.g. a `switch` with repeated destinations), the predecessor appears multiple times.
    pub fn preds(&self, block: Block) -> &[Block] {
        &self.pool[self.slices[block]]
    }

    pub fn compute(&mut self, prog: &Program, ord: &Order) {
        self.pool.clear();
        self.entries = self
            .pool
            .extend(prog.functions.values().filter_map(|f| f.entry));
        for data in prog.functions.values() {
            if let Some(entry) = data.entry {
                let mut b = self.scratch.builder(&mut self.pool, &mut self.slices);
                for &block in ord.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        b.bump(succ);
                    }
                }
                let mut b = b.allocate();
                for &block in ord.rpo(entry) {
                    for succ in prog.blocks[block].term.successors() {
                        b.push(succ, block);
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

    const BB0: Block = Block::from_u32(0);
    const BB1: Block = Block::from_u32(1);
    const BB2: Block = Block::from_u32(2);
    const BB3: Block = Block::from_u32(3);

    fn setup(input: &str) -> Control {
        let mut db = SimpleFiles::new();
        let mut interner = IdentInterner::new();
        let ir = parse_ir(&mut db, &mut interner, input);
        check_parse(&mut db, &ir).unwrap_or_else(|report| panic!("{report}"));
        let prog = &ir.program;
        let mut ord = Order::default();
        let mut cfg = Control::default();
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

        assert_eq!(cfg.preds(BB0), []);
        assert_eq!(cfg.preds(BB1), [BB0]);
        assert_eq!(cfg.preds(BB2), [BB1]);
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

        assert_eq!(cfg.preds(BB0), []);
        assert_eq!(cfg.preds(BB1), [BB0]);
        assert_eq!(cfg.preds(BB2), [BB0]);
        assert_eq!(cfg.preds(BB3), [BB1, BB2]);
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

        assert_eq!(cfg.preds(BB0), []);
        assert_eq!(cfg.preds(BB1), [BB2, BB0]);
        assert_eq!(cfg.preds(BB2), [BB1]);
        assert_eq!(cfg.preds(BB3), [BB1]);
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

        assert_eq!(cfg.preds(BB0), []);
        assert_eq!(cfg.preds(BB1), [BB0]);
        assert_eq!(cfg.preds(BB2), [BB0]);
        assert_eq!(cfg.preds(BB3), [BB0]);
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

        assert_eq!(cfg.preds(BB0), []);
        assert_eq!(cfg.preds(BB1), [BB0, BB0]);
        assert_eq!(cfg.preds(BB2), [BB0]);
    }
}
