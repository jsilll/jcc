use ssa::{Inst, Program, Type};

use string_interner::DefaultStringInterner;

pub fn main() {
    let mut interner = DefaultStringInterner::new();
    let mut program = Program::new(&mut interner);

    let f = program.push_func("example");

    // Create the blocks
    let root = program.push_block("root");
    let then_block = program.push_block("then");
    let else_block = program.push_block("else");
    let return_block = program.push_block("return");

    // Add blocks to the function
    program
        .func_mut(f)
        .blocks
        .extend([root, then_block, else_block, return_block]);

    // Create the root block instructions
    let arg0 = program.push_inst(Inst::get_arg(Type::Int32));
    let arg1 = program.push_inst(Inst::get_arg(Type::Int32));
    program.block_mut(root).insts.extend([arg0, arg1]);

    // Create phi
    let phi = program.push_inst(Inst::phi(Type::Int32));

    // Create then block instructions
    let one = program.push_inst(Inst::const_i32(1));
    let x1 = program.push_inst(Inst::add_i32(arg1, one));
    let u1 = program.push_inst(Inst::upsilon(phi, x1));
    program
        .block_mut(then_block)
        .insts
        .extend([one, x1, u1]);

    // Create else block instructions
    let two = program.push_inst(Inst::const_i32(2));
    let x2 = program.push_inst(Inst::add_i32(arg1, two));
    let u2 = program.push_inst(Inst::upsilon(phi, x2));
    program
        .block_mut(else_block)
        .insts
        .extend([two, x2, u2]);

    // Create return block instructions
    let c1 = program.push_inst(Inst::const_i32(3));
    let c2 = program.push_inst(Inst::const_i32(42));
    let c3 = program.push_inst(Inst::add_i32(c1, c2));
    let c4 = program.push_inst(Inst::const_i32(0));
    let c5 = program.push_inst(Inst::add_i32(phi, c3));
    let res = program.push_inst(Inst::add_i32(c5, c4));
    let ret = program.push_inst(Inst::ret(res));
    program
        .block_mut(return_block)
        .insts
        .extend([phi, c1, c2, c3, c4, c5, res, ret]);

    // Set up the successors TODO: Perform this automatically
    program
        .block_mut(root)
        .succs
        .extend([then_block, else_block]);
    program.block_mut(then_block).succs.push(return_block);
    program.block_mut(else_block).succs.push(return_block);

    println!("{}", program);

    ssa::optimize::cf::optimize_constant_add(&mut program);

    println!("{}", program);

    ssa::optimize::dce::eliminate_dead_code(&mut program);

    println!("{}", program);

    ssa::optimize::dce::remove_redundant_identity(&mut program);

    println!("{}", program);
}
