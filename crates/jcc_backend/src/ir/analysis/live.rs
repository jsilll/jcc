use jcc_bitset::matrix::{BitMatrix, BitRow};
use jcc_entity::{EntityRef, Local, LocalMap};

use crate::ir::{
    analysis::{cfg::Control, order::Order},
    ty::Ty,
    Block, Program, Value,
};

/// Liveness analysis results for a single function.
///
/// Internally uses compact per-function local IDs and a flat [`BitMatrix`] for
/// both `live_in` and `live_out`, avoiding per-block heap allocations.
/// Results are exposed through query methods that translate back to [`Value`].
pub struct Liveness {
    /// Compaction map for values: global `Value` ↔ dense local index.
    values: LocalMap<Value>,
    /// Compaction map for blocks: global `Block` ↔ dense local index.
    blocks: LocalMap<Block>,
    /// Row = local block index, column = local value index.
    in_mat: BitMatrix,
    /// Row = local block index, column = local value index.
    out_mat: BitMatrix,
}

impl Liveness {
    /// Returns `true` if `value` is live on entry to `block`.
    pub fn is_live_in(&self, block: Block, value: Value) -> bool {
        let Some(block_id) = self.blocks.get(block) else {
            return false;
        };
        let Some(value_id) = self.values.get(value) else {
            return false;
        };
        self.in_mat.contains(block_id.index(), value_id.index())
    }

    /// Returns `true` if `value` is live on exit from `block`.
    pub fn is_live_out(&self, block: Block, value: Value) -> bool {
        let Some(block_id) = self.blocks.get(block) else {
            return false;
        };
        let Some(value_id) = self.values.get(value) else {
            return false;
        };
        self.out_mat.contains(block_id.index(), value_id.index())
    }

    /// Iterates over the values live on entry to `block`, in definition order.
    ///
    /// Returns an empty iterator for unreachable blocks.
    pub fn live_in(&self, block: Block) -> impl Iterator<Item = Value> + '_ {
        self.blocks.get(block).into_iter().flat_map(|id| {
            self.in_mat
                .row(id.index())
                .into_iter()
                .flat_map(BitRow::iter)
                .map(|idx| self.values.key(Local::new(idx)).unwrap())
        })
    }

    /// Iterates over the values live on exit from `block`, in definition order.
    ///
    /// Returns an empty iterator for unreachable blocks.
    pub fn live_out(&self, block: Block) -> impl Iterator<Item = Value> + '_ {
        self.blocks.get(block).into_iter().flat_map(|id| {
            self.out_mat
                .row(id.index())
                .into_iter()
                .flat_map(BitRow::iter)
                .map(|idx| self.values.key(Local::new(idx)).unwrap())
        })
    }

    pub fn compute(entry: Block, prog: &Program, cfg: &Control, order: &Order) -> Self {
        let rpo = order.rpo(entry);
        let mut blocks = LocalMap::with_capacity(rpo.len());
        for &block in rpo {
            blocks.push(block);
        }

        let mut values = LocalMap::new();
        for &block in rpo {
            for &value in &prog.blocks[block].insts {
                if prog.values[value].inst.ty() != Ty::Void {
                    values.push(value);
                }
            }
        }

        let rows = blocks.len();
        let bits = values.len();
        let mut live_in = BitMatrix::new(rows, bits);
        let mut live_out = BitMatrix::new(rows, bits);
        let mut kill = BitMatrix::new(rows, bits);

        // Seed live-in with each block's gen set and record its kill set.
        // Upsilon's phi field identifies its destination; only its value field
        // is a liveness use on the predecessor edge.
        for &block in rpo {
            let block_id = blocks.get(block).unwrap().index();
            for &val in &prog.blocks[block].insts {
                let inst = &prog.values[val].inst;
                let mut add_use = |op| {
                    let op_data = &prog.values[op];
                    if op_data.inst.ty() != Ty::Void
                        && (op_data.block != block || op_data.idx >= prog.values[val].idx)
                    {
                        live_in.insert(block_id, values.get(op).unwrap().index());
                    }
                };
                match inst {
                    crate::ir::inst::Inst::Upsilon { value, .. } => add_use(*value),
                    _ => {
                        for op in inst.operands() {
                            add_use(op);
                        }
                    }
                }
                if inst.ty() != Ty::Void {
                    kill.insert(block_id, values.get(val).unwrap().index());
                }
            }
            for op in prog.blocks[block].term.operands() {
                let op_data = &prog.values[op];
                if op_data.inst.ty() != Ty::Void && op_data.block != block {
                    live_in.insert(block_id, values.get(op).unwrap().index());
                }
            }
        }

        // A block's live-in set only grows. When it does, only its predecessors
        // can be affected, so propagate changes through a reverse-CFG worklist
        // instead of repeatedly scanning every block.
        let mut pending = vec![false; rows];
        let mut worklist = Vec::with_capacity(rpo.len());
        for &block in rpo {
            let block_id = blocks.get(block).unwrap().index();
            pending[block_id] = true;
            worklist.push(block_id);
        }
        while let Some(block_id) = worklist.pop() {
            pending[block_id] = false;
            let block = rpo[block_id];
            for &pred in cfg.preds(block) {
                let Some(pred_id) = blocks.get(pred) else {
                    continue;
                };
                let pred_id = pred_id.index();
                live_out
                    .row_mut(pred_id)
                    .unwrap()
                    .union_with(live_in.row(block_id).unwrap());

                if pred_id == block_id {
                    continue;
                }
                let (src, mut target) = live_in.two_rows_mut(block_id, pred_id);
                let changed =
                    target.union_with_difference(src, kill.row(pred_id).unwrap());
                if changed && !pending[pred_id] {
                    pending[pred_id] = true;
                    worklist.push(pred_id);
                }
            }
        }

        Self {
            values,
            blocks,
            in_mat: live_in,
            out_mat: live_out,
        }
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

    const BB0: Block = Block::from_u32(0);
    const BB1: Block = Block::from_u32(1);
    const BB2: Block = Block::from_u32(2);

    const V0: Value = Value::from_u32(0);
    const V1: Value = Value::from_u32(1);
    const V2: Value = Value::from_u32(2);
    const V3: Value = Value::from_u32(3);

    fn setup(input: &str) -> Liveness {
        let mut db = SimpleFiles::new();
        let mut interner = IdentInterner::new();
        let ir = parse_ir(&mut db, &mut interner, input);
        check_parse(&mut db, &ir).unwrap_or_else(|report| panic!("{report}"));

        let prog = &ir.program;
        let mut ord = Order::default();
        let mut cfg = Control::default();
        ord.compute(prog);
        cfg.compute(prog, &ord);
        Liveness::compute(BB0, prog, &cfg, &ord)
    }

    fn assert_live_eq(live: impl Iterator<Item = Value>, expected: &[Value]) {
        assert_eq!(live.collect::<Vec<_>>(), expected);
    }

    #[test]
    fn straight_line() {
        let live = setup(
            r#"
            define @chain {
            bb0:
              %0 = param i32 #0
              %1 = const i32 1
              %2 = add i32 %0, %1
              br bb1

            bb1:
              %3 = const i32 2
              %4 = add i32 %2, %3
              ret %4
            }
        "#,
        );

        assert_live_eq(live.live_in(BB0), &[]);
        assert_live_eq(live.live_out(BB0), &[V2]);
        assert_live_eq(live.live_in(BB1), &[V2]);
        assert_live_eq(live.live_out(BB1), &[]);
    }

    #[test]
    fn looped() {
        let live = setup(
            r#"
            define @loop {
            bb0:
              %0 = param i1 #0
              %1 = param i32 #1
              br bb1

            bb1:
              %2 = const i32 1
              %3 = add i32 %1, %2
              br i1 %0, bb1, bb2

            bb2:
              ret %3
            }
        "#,
        );

        assert_live_eq(live.live_in(BB0), &[]);
        assert_live_eq(live.live_out(BB0), &[V0, V1]);
        assert_live_eq(live.live_in(BB1), &[V0, V1]);
        assert_live_eq(live.live_out(BB1), &[V0, V1, V3]);
        assert_live_eq(live.live_in(BB2), &[V3]);
        assert_live_eq(live.live_out(BB2), &[]);
    }

    #[test]
    fn upsilon_does_not_make_its_phi_live_in() {
        let live = setup(
            r#"
            define @phi_edge {
            bb0:
              %0 = param i32 #0
              br bb1

            bb1:
              upsilon %0 -> %1
              br bb2

            bb2:
              %1 = phi i32
              ret %1
            }
        "#,
        );

        assert_live_eq(live.live_in(BB0), &[]);
        assert_live_eq(live.live_out(BB0), &[V0]);
        assert_live_eq(live.live_in(BB1), &[V0]);
        assert_live_eq(live.live_out(BB1), &[]);
        assert_live_eq(live.live_in(BB2), &[]);
        assert_live_eq(live.live_out(BB2), &[]);
    }
}
