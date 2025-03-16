use ssa::{Inst, InstKind, Program, Type};

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
    b.insts.extend([two, x2, u2]);

    // Create return block instructions
    let c1 = p.add_inst(Inst::const_i32(3));
    let c2 = p.add_inst(Inst::const_i32(42));
    let c3 = p.add_inst(Inst::add_i32(c1, c2));
    let c4 = p.add_inst(Inst::const_i32(0));
    let c5 = p.add_inst(Inst::add_i32(phi, c3));
    let res = p.add_inst(Inst::add_i32(c5, c4));
    let ret = p.add_inst(Inst::ret(res));
    let b = p.get_block_mut(return_block);
    b.insts.extend([phi, c1, c2, c3, c4, c5, res, ret]);

    println!("{}", p);

    ssa::opt::cf::optimize_constant_add(&mut p);

    println!("{}", p);

    ssa::opt::dce::eliminate_dead_code(&mut p);

    println!("{}", p);

    ssa::opt::dce::remove_redundant_identity(&mut p);

    println!("{}", p);
}
