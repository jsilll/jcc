use std::{hint::black_box, time::Instant};

use jcc_backend::{
    codemap::span::Span,
    ir::{
        analysis::{cfg::Control, live::Liveness, order::Order},
        inst::{BinaryOp, Inst},
        term::Terminator,
        ty::Ty,
        Block, BlockData, FunctionData, Program, Value, ValueData,
    },
    IdentInterner,
};

const BLOCKS: usize = 256;
const LANES: usize = 16;
const OPS_PER_BLOCK: usize = 8;
const SAMPLES: usize = 9;
const ITERS_PER_SAMPLE: usize = 100;

fn push_block(prog: &mut Program, name: jcc_backend::Ident) -> Block {
    prog.blocks.push(BlockData::new(name, Span::DEFAULT))
}

fn push_inst(prog: &mut Program, block: Block, inst: Inst) -> Value {
    let idx = prog.blocks[block].insts.len() as u32;
    let value = prog.values.push(ValueData {
        idx,
        span: Span::DEFAULT,
        inst,
        block,
    });
    prog.blocks[block].insts.push(value);
    value
}

fn program() -> (Program, Block, Order, Control) {
    let mut interner = IdentInterner::new();
    let name = interner.intern("liveness_bench");
    let mut prog = Program::default();
    let function = prog
        .functions
        .push(FunctionData::new(name, true, Span::DEFAULT));

    let entry = push_block(&mut prog, name);
    let body: Vec<_> = (0..BLOCKS).map(|_| push_block(&mut prog, name)).collect();
    let exit = push_block(&mut prog, name);
    prog.functions[function].entry = Some(entry);

    let cond = push_inst(&mut prog, entry, Inst::param(Ty::I1, 0));
    let lanes: Vec<_> = (0..LANES)
        .map(|idx| push_inst(&mut prog, entry, Inst::param(Ty::I32, (idx + 1) as u32)))
        .collect();
    prog.blocks[entry].term = Terminator::br(body[0]);

    for (idx, &block) in body.iter().enumerate() {
        let mut value = lanes[idx % LANES];
        for op in 0..OPS_PER_BLOCK {
            value = push_inst(
                &mut prog,
                block,
                Inst::binary(BinaryOp::Add, Ty::I32, value, lanes[(idx + op + 1) % LANES]),
            );
        }
        prog.blocks[block].term = if let Some(&next) = body.get(idx + 1) {
            Terminator::br(next)
        } else {
            Terminator::cond_br(cond, body[0], exit)
        };
    }

    let mut value = lanes[0];
    for &lane in &lanes[1..] {
        value = push_inst(
            &mut prog,
            exit,
            Inst::binary(BinaryOp::Add, Ty::I32, value, lane),
        );
    }
    prog.blocks[exit].term = Terminator::ret(Some(value));

    let mut order = Order::default();
    let mut control = Control::default();
    order.compute(&prog);
    control.compute(&prog, &order);
    (prog, entry, order, control)
}

fn main() {
    let (prog, entry, order, control) = program();

    let warmup = Liveness::compute(entry, &prog, &control, &order);
    black_box(warmup);

    let mut samples = Vec::with_capacity(SAMPLES);
    for _ in 0..SAMPLES {
        let start = Instant::now();
        for _ in 0..ITERS_PER_SAMPLE {
            let live = Liveness::compute(entry, &prog, &control, &order);
            black_box(live);
        }
        samples.push(start.elapsed() / ITERS_PER_SAMPLE as u32);
    }
    samples.sort_unstable();

    println!(
        "liveness/{BLOCKS}-block-loop/{LANES}-live-lanes/{OPS_PER_BLOCK}-ops: median {:?}",
        samples[SAMPLES / 2]
    );
}
