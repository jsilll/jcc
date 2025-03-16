use crate::{insertion::InsertionSet, BinaryOp, Inst, InstKind, InstRef, Program};

pub fn optimize_constant_add(p: &mut Program) {
    let mut iset = InsertionSet::new();
    let mut iteration_list: Vec<InstRef> = Vec::with_capacity(32);

    for b in p.get_blocks_iter() {

        iteration_list.clear();
        iteration_list.extend(&p.get_block(b).insts);

        for i in &iteration_list {
            let inst = p.get_inst(*i).clone();
            if let InstKind::Binary {
                op: BinaryOp::Add,
                lhs,
                rhs,
            } = &inst.kind
            {
                let l = p.get_inst(*lhs).kind.clone();
                let r = p.get_inst(*rhs).kind.clone();
                match (l, r) {
                    (InstKind::Const(0), _) => {
                        p.get_inst_mut(*i).into_identity(*rhs);
                    }
                    (_, InstKind::Const(0)) => {
                        p.get_inst_mut(*i).into_identity(*lhs);
                    }
                    (InstKind::Const(c1), InstKind::Const(c2)) => {
                        *p.get_inst_mut(*i) = Inst {
                            ty: inst.ty,
                            kind: InstKind::Const(c1 + c2),
                        }
                    }
                    _ => {}
                }
            }
        }

        iset.execute(p, b);
    }
}