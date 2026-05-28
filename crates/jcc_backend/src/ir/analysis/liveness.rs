use jcc_entity::{EntitySet, SecondaryMap};

use crate::ir::{
    analysis::{cfg::ControlFlowGraph, order::Order},
    inst::Inst,
    term::Terminator,
    ty::Ty,
    Block, Program, Value,
};

pub struct Liveness {
    pub live_in: SecondaryMap<Block, EntitySet<Value>>,
    pub live_out: SecondaryMap<Block, EntitySet<Value>>,
}

impl Liveness {
    pub fn compute(entry: Block, prog: &Program, _cfg: &ControlFlowGraph, order: &Order) -> Self {
        let mut live_in: SecondaryMap<Block, EntitySet<Value>> = SecondaryMap::new();
        let mut live_out: SecondaryMap<Block, EntitySet<Value>> = SecondaryMap::new();

        let rpo: Vec<Block> = order.rpo(entry).into_iter().collect();

        let mut changed = true;
        while changed {
            changed = false;

            // Reverse RPO converges faster for backward problems.
            for &block in rpo.iter().rev() {
                let mut new_out: EntitySet<Value> = EntitySet::default();
                for succ in prog.blocks[block].term.successors() {
                    for &v in live_in[succ].iter() {
                        new_out.insert(v);
                    }
                }

                // Compute per-block gen/kill in forward order.
                let mut gen: EntitySet<Value> = EntitySet::default();
                let mut kill: EntitySet<Value> = EntitySet::default();

                for &val in &prog.blocks[block].insts {
                    let inst = &prog.values[val].inst;
                    for u in inst_uses(inst) {
                        if !kill.contains(&u) {
                            gen.insert(u);
                        }
                    }
                    if inst.ty() != Ty::Void {
                        kill.insert(val);
                    }
                }
                for u in term_uses(&prog.blocks[block].term) {
                    if !kill.contains(&u) {
                        gen.insert(u);
                    }
                }

                // live_in[B] = gen[B] ∪ (live_out[B] - kill[B])
                let mut new_in: EntitySet<Value> = gen;
                for &v in new_out.iter() {
                    if !kill.contains(&v) {
                        new_in.insert(v);
                    }
                }

                let in_changed = new_in.len() != live_in[block].len()
                    || new_in.iter().any(|v| !live_in[block].contains(v));
                let out_changed = new_out.len() != live_out[block].len()
                    || new_out.iter().any(|v| !live_out[block].contains(v));

                if in_changed || out_changed {
                    changed = true;
                    live_in[block] = new_in;
                    live_out[block] = new_out;
                }
            }
        }

        Self { live_in, live_out }
    }
}

/// Returns the values *read* by an instruction (not the one it defines).
/// For Upsilon, only the `value` operand is a use; `phi` is a write target.
pub fn inst_uses(inst: &Inst) -> Vec<Value> {
    match inst {
        Inst::Noop
        | Inst::Phi(_)
        | Inst::ConstNull(_)
        | Inst::GlobalAddr(_)
        | Inst::Const { .. }
        | Inst::Param { .. }
        | Inst::Alloca { .. } => vec![],

        Inst::Upsilon { value, .. } => vec![*value],

        Inst::Load { ptr, .. } => vec![*ptr],
        Inst::Store { ptr, value, .. } => vec![*ptr, *value],
        Inst::GetElementPtr { ptr, indices, .. } => {
            let mut v = vec![*ptr];
            v.extend_from_slice(indices);
            v
        }
        Inst::Unary { operand, .. } => vec![*operand],
        Inst::Binary { lhs, rhs, .. }
        | Inst::ICmp { lhs, rhs, .. }
        | Inst::FCmp { lhs, rhs, .. } => vec![*lhs, *rhs],

        Inst::Trunc { value, .. }
        | Inst::FTrunc { value, .. }
        | Inst::ZExt { value, .. }
        | Inst::SExt { value, .. }
        | Inst::FExt { value, .. }
        | Inst::Bitcast { value, .. }
        | Inst::IntToPtr { value, .. }
        | Inst::PtrToInt { value, .. }
        | Inst::FpToSi { value, .. }
        | Inst::FpToUi { value, .. }
        | Inst::SiToFp { value, .. }
        | Inst::UiToFp { value, .. } => vec![*value],

        Inst::Call { args, .. } => args.clone(),
        Inst::IndirectCall { ptr, args, .. } => {
            let mut v = vec![*ptr];
            v.extend_from_slice(args);
            v
        }
        Inst::Select {
            cond,
            then_val,
            else_val,
            ..
        } => vec![*cond, *then_val, *else_val],
    }
}

pub fn term_uses(term: &Terminator) -> Vec<Value> {
    match term {
        Terminator::Ret(Some(v)) => vec![*v],
        Terminator::CondBr { cond, .. } => vec![*cond],
        Terminator::Switch { value, .. } => vec![*value],
        _ => vec![],
    }
}
