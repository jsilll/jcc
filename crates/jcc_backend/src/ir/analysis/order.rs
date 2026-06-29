use jcc_entity::{SecondaryMap, slice::{EntitySlice, SlicePool}};

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

#[cfg(test)]
mod tests {
    use super::*;

    use jcc_codemap::simple::SimpleFiles;

    use crate::{
        ir::testutil::{check_parse, parse_ir},
        IdentInterner,
    };

    fn setup(input: &str) -> Order {
        let mut db = SimpleFiles::new();
        let mut interner = IdentInterner::new();
        let ir = parse_ir(&mut db, &mut interner, input);
        check_parse(&mut db, &ir).unwrap_or_else(|report| panic!("{report}"));
        let mut ord = Order::default();
        ord.compute(&ir.program);
        ord
    }

    #[test]
    fn linear() {
        let ord = setup(
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

        assert!(ord.rpo(Block::from_u32(0)).eq([
            Block::from_u32(0),
            Block::from_u32(1),
            Block::from_u32(2)
        ]));

        assert_eq!(ord.rpo_idx(Block::from_u32(0)), 0);
        assert_eq!(ord.rpo_idx(Block::from_u32(1)), 1);
        assert_eq!(ord.rpo_idx(Block::from_u32(2)), 2);
    }

    #[test]
    fn diamond() {
        let ord = setup(
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

        assert!(ord.rpo(Block::from_u32(0)).eq([
            Block::from_u32(0),
            Block::from_u32(2),
            Block::from_u32(1),
            Block::from_u32(3),
        ]));

        assert_eq!(ord.rpo_idx(Block::from_u32(0)), 0);
        assert_eq!(ord.rpo_idx(Block::from_u32(2)), 1);
        assert_eq!(ord.rpo_idx(Block::from_u32(1)), 2);
        assert_eq!(ord.rpo_idx(Block::from_u32(3)), 3);
    }

    #[test]
    fn back_edge() {
        let ord = setup(
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

        assert!(ord.rpo(Block::from_u32(0)).eq([
            Block::from_u32(0),
            Block::from_u32(1),
            Block::from_u32(3),
            Block::from_u32(2),
        ]));

        assert_eq!(ord.rpo_idx(Block::from_u32(0)), 0);
        assert_eq!(ord.rpo_idx(Block::from_u32(1)), 1);
        assert_eq!(ord.rpo_idx(Block::from_u32(3)), 2);
        assert_eq!(ord.rpo_idx(Block::from_u32(2)), 3);
    }

    #[test]
    fn two_functions() {
        let ord = setup(
            r#"
            define @f {
            bb0:
              br bb1
            bb1:
              ret void
            }

            define @g {
            bb2:
              br bb3
            bb3:
              ret void
            }
        "#,
        );

        assert!(ord
            .rpo(Block::from_u32(0))
            .eq([Block::from_u32(0), Block::from_u32(1),]));

        assert!(ord
            .rpo(Block::from_u32(2))
            .eq([Block::from_u32(2), Block::from_u32(3),]));

        assert_eq!(ord.rpo_idx(Block::from_u32(0)), 0);
        assert_eq!(ord.rpo_idx(Block::from_u32(1)), 1);
        assert_eq!(ord.rpo_idx(Block::from_u32(2)), 0);
        assert_eq!(ord.rpo_idx(Block::from_u32(3)), 1);
    }
}
