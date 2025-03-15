use ssa::{insertion::InsertionSet, Inst, InstKind, OpCode, Program, Type};

pub fn optimize_constant_add(p: &mut Program) {
    let mut iset = InsertionSet::new();
    for b in p.get_blocks_iter() {
        for i in &p.get_block(b).insts.clone() {
            let inst = p.get_inst(*i);
            if let InstKind::Generic {
                opcode: OpCode::Add,
                args,
            } = &inst.kind
            {
                match args[..] {
                    [a1, a2] => {
                        let arg1 = p.get_inst(a1).clone();
                        let arg2 = p.get_inst(a2).clone();
                        match (&arg1.kind, &arg2.kind) {
                            (InstKind::Const(0), _) => {
                                p.get_inst_mut(*i).into_identity(a2);
                            }
                            (_, InstKind::Const(0)) => {
                                p.get_inst_mut(*i).into_identity(a1);
                            }
                            (InstKind::Const(c1), InstKind::Const(c2)) => {
                                *p.get_inst_mut(*i) = Inst {
                                    ty: arg1.ty,
                                    kind: InstKind::Const(c1 + c2),
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
        iset.execute(p, b);
    }
}

pub fn main() {
    let mut p = Program::new();

    let f = p.add_func("example");

    // Create the blocks
    let root = p.add_block("root");
    let then_block = p.add_block("then");
    let else_block = p.add_block("else");
    let return_block = p.add_block("return");

    // Add blocks to the function
    let f_ = p.get_func_mut(f);
    f_.blocks.push(root);
    f_.blocks.push(then_block);
    f_.blocks.push(else_block);
    f_.blocks.push(return_block);

    // Set up the successors
    p.get_block_mut(root).succ.extend([then_block, else_block]);
    p.get_block_mut(then_block).succ.push(return_block);
    p.get_block_mut(else_block).succ.push(return_block);

    // Create the root block instructions
    let arg0 = p.add_inst(Inst {
        ty: Type::Int32,
        kind: InstKind::GetArg,
    });
    let arg1 = p.add_inst(Inst {
        ty: Type::Int32,
        kind: InstKind::GetArg,
    });
    let b = p.get_block_mut(root);
    b.insts.push(arg0);
    b.insts.push(arg1);

    // Create phi
    let phi = p.add_inst(Inst::phi(Type::Int32));

    // Create then block instructions
    let one = p.add_inst(Inst::const_i32(1));
    let x1 = p.add_inst(Inst::add_i32(arg1, one));
    let u1 = p.add_inst(Inst::upsilon(phi, x1));
    let b = p.get_block_mut(then_block);
    b.insts.extend([one, x1, u1]);

    // Create else block instructions
    let two = p.add_inst(Inst::const_i32(2));
    let x2 = p.add_inst(Inst::add_i32(arg1, two));
    let u2 = p.add_inst(Inst::upsilon(phi, x2));
    let b = p.get_block_mut(else_block);
    b.insts.extend([one, x1, u2]);

    // Create return block instructions
    let c1 = p.add_inst(Inst::const_i32(3));
    let c2 = p.add_inst(Inst::const_i32(42));
    let c3 = p.add_inst(Inst::add_i32(c1, c2));
    let res = p.add_inst(Inst::add_i32(phi, c3));
    let ret = p.add_inst(Inst::ret(res));
    let b = p.get_block_mut(return_block);
    b.insts.extend([phi, c1, c2, c3, res, ret]);

    println!("{}", p);

    optimize_constant_add(&mut p);
    // TODO: dead code removal

    println!("{}", p);
}
