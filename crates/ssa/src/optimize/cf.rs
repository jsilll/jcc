use crate::{BinaryOp, InstKind, Program};

pub fn optimize_constant_add(p: &mut Program) {
    for b in p.block_iter() {
        for idx in 0..p.block(b).insts.len() {
            let i = p.block(b).insts[idx];
            let inst = p.inst(i).clone();
            if let InstKind::Binary {
                op: BinaryOp::Add,
                lhs,
                rhs,
            } = &inst.kind
            {
                match (&p.inst(*lhs).kind, &p.inst(*rhs).kind) {
                    (InstKind::Const(0), _) => {
                        p.inst_mut(i).into_identity(*rhs);
                    }
                    (_, InstKind::Const(0)) => {
                        p.inst_mut(i).into_identity(*lhs);
                    }
                    (InstKind::Const(c1), InstKind::Const(c2)) => {
                        p.inst_mut(i).kind = InstKind::Const(c1 + c2);
                    }
                    _ => {}
                }
            }
        }
    }
}
