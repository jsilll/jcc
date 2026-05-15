use std::collections::HashMap;

use jcc_codemap::span::Span;
use jcc_entity::{EntityRef, EntitySet};

use crate::ir::{
    analysis::{cfg::ControlFlowGraph, dom::Dominance, order::Order},
    inst::Inst,
    term::Terminator,
    ty::Ty,
    Block, Program, Value, ValueData,
};

// Algorithm (5 phases, per function):
//
//   1. Promotable alloca detection — scans all instructions; an Alloca is promotable if its only uses are as the ptr in
//   Load/Store. Any other use (passed to a call, stored as a value, used in a terminator) marks it non-promotable.
//   2. Dominance frontier computation — standard runner algorithm: for each join point (≥2 preds), walk from each predecessor up
//   to its idom, inserting the join point into each runner's DF set.
//   3. Phi placement via iterated DF — for each promotable alloca, collect its def-blocks (blocks containing stores), then expand
//    to the iterated dominance frontier (IDF) using a worklist. Insert a Phi(ty) at the front of each IDF block, adjusting
//   existing instruction indices.
//   4. SSA rename — iterative DFS over the dominator tree with explicit Enter/Leave stack entries for snapshot/restore of
//   per-alloca value stacks. On Store → alloca: push new value, noop the store. On Load ← alloca: rename load to current value
//   (inserting Const 0 for uninitialized reads, which are UB in C). Before leaving each block: insert Upsilon { phi, value } for
//   every successor phi.
//   5. Substitution — single pass over all instructions and terminators, replacing any Value operand that appears in the rename
//   map with its SSA replacement.

#[derive(Default)]
pub struct Mem2Reg;

impl Mem2Reg {
    pub fn new() -> Self {
        Self
    }

    pub fn run(
        &mut self,
        prog: &mut Program,
        order: &Order,
        cfg: &ControlFlowGraph,
        dom: &Dominance,
    ) {
        let entries: Vec<_> = prog.functions.values().filter_map(|f| f.entry).collect();
        for entry in entries {
            self.run_function(entry, prog, order, cfg, dom);
        }
    }

    fn run_function(
        &mut self,
        entry: Block,
        prog: &mut Program,
        order: &Order,
        cfg: &ControlFlowGraph,
        dom: &Dominance,
    ) {
        let allocas = collect_promotable_allocas(entry, prog, order);
        if allocas.is_empty() {
            return;
        }

        // df[block.index()] = list of blocks in block's dominance frontier
        let df = compute_df(entry, order, cfg, dom);

        // phi_map[(alloca.index(), block.index())] = phi Value inserted at block for alloca
        let mut phi_map: HashMap<(usize, usize), Value> = HashMap::new();
        for &(alloca_val, ty) in &allocas {
            place_phis_for_alloca(alloca_val, ty, entry, prog, order, &df, &mut phi_map);
        }

        // rename_map[load_value.index()] = replacement_value.index()
        let mut rename_map: HashMap<usize, usize> = HashMap::new();
        rename_pass(entry, prog, dom, &allocas, &phi_map, &mut rename_map);

        substitute_all(entry, prog, order, &rename_map);
    }
}

// -----------------------------------------------------------------------
// Step 1: Identify promotable allocas
// -----------------------------------------------------------------------

fn collect_promotable_allocas(entry: Block, prog: &Program, order: &Order) -> Vec<(Value, Ty)> {
    let mut candidates: Vec<(Value, Ty)> = Vec::new();
    let mut non_promotable: EntitySet<Value> = EntitySet::new();

    for block in order.rpo(entry) {
        for &val in &prog.blocks[block].insts {
            match prog.values[val].inst.clone() {
                Inst::Alloca { ty, .. } => {
                    candidates.push((val, ty));
                }
                Inst::Store { ptr, value, .. } => {
                    // ptr used as store destination: promotable if it's an alloca (don't mark)
                    // value is the data being stored: if it's an alloca its address escapes
                    if matches!(prog.values[value].inst, Inst::Alloca { .. }) {
                        non_promotable.insert(value);
                    }
                    let _ = ptr;
                }
                Inst::Load { .. } => {
                    // ptr used as load source: promotable if it's an alloca (don't mark)
                }
                inst => {
                    for used in inst_operands(&inst) {
                        if matches!(prog.values[used].inst, Inst::Alloca { .. }) {
                            non_promotable.insert(used);
                        }
                    }
                }
            }
        }
        // Any alloca used in a terminator (e.g. ret alloca_ptr) escapes
        for used in term_operands(&prog.blocks[block].term) {
            if matches!(prog.values[used].inst, Inst::Alloca { .. }) {
                non_promotable.insert(used);
            }
        }
    }

    candidates
        .into_iter()
        .filter(|(val, _)| !non_promotable.contains(val))
        .collect()
}

// -----------------------------------------------------------------------
// Step 2: Dominance frontiers
// -----------------------------------------------------------------------

fn compute_df(
    entry: Block,
    order: &Order,
    cfg: &ControlFlowGraph,
    dom: &Dominance,
) -> Vec<Vec<Block>> {
    let mut df_sets: HashMap<usize, EntitySet<Block>> = HashMap::new();

    for block in order.rpo(entry) {
        let preds: Vec<_> = cfg.preds(block).into_iter().collect();
        if preds.len() < 2 {
            continue;
        }
        let idom_of_block = dom.idom(block).unwrap_or(block);
        for pred in preds {
            let mut runner = pred;
            while runner != idom_of_block {
                df_sets.entry(runner.index()).or_default().insert(block);
                runner = dom
                    .idom(runner)
                    .expect("block missing idom in df computation");
            }
        }
    }

    let max_idx = df_sets.keys().copied().max().map_or(0, |m| m + 1);
    let mut df: Vec<Vec<Block>> = vec![Vec::new(); max_idx];
    for (idx, set) in df_sets {
        df[idx] = set.into_iter().collect();
    }
    df
}

// -----------------------------------------------------------------------
// Step 3: Phi placement via iterated dominance frontier
// -----------------------------------------------------------------------

fn place_phis_for_alloca(
    alloca_val: Value,
    ty: Ty,
    entry: Block,
    prog: &mut Program,
    order: &Order,
    df: &[Vec<Block>],
    phi_map: &mut HashMap<(usize, usize), Value>,
) {
    // Collect blocks that contain stores to this alloca
    let mut def_blocks: EntitySet<Block> = EntitySet::new();
    for block in order.rpo(entry) {
        for &val in &prog.blocks[block].insts {
            if let Inst::Store { ptr, .. } = prog.values[val].inst {
                if ptr == alloca_val {
                    def_blocks.insert(block);
                    break;
                }
            }
        }
    }

    // Iterate to fixed point: phi placements create new def blocks
    let mut worklist: Vec<Block> = def_blocks.iter().copied().collect();
    let mut has_phi: EntitySet<Block> = EntitySet::new();

    while let Some(block) = worklist.pop() {
        let frontiers = df.get(block.index()).map(|v| v.as_slice()).unwrap_or(&[]);
        for &frontier in frontiers {
            if has_phi.insert(frontier) {
                let phi_val = insert_phi_at_front(frontier, ty, prog);
                phi_map.insert((alloca_val.index(), frontier.index()), phi_val);
                // The frontier is now also a def block via the phi
                if def_blocks.insert(frontier) {
                    worklist.push(frontier);
                }
            }
        }
    }
}

fn insert_phi_at_front(block: Block, ty: Ty, prog: &mut Program) -> Value {
    // Shift existing instructions' positional indices
    for &existing in &prog.blocks[block].insts {
        prog.values[existing].idx += 1;
    }
    let phi_val = prog.values.push(ValueData {
        inst: Inst::Phi(ty),
        span: Span::default(),
        block,
        idx: 0,
    });
    prog.blocks[block].insts.insert(0, phi_val);
    phi_val
}

// -----------------------------------------------------------------------
// Step 4: SSA rename — replace loads/stores with SSA values
// -----------------------------------------------------------------------

fn rename_pass(
    entry: Block,
    prog: &mut Program,
    dom: &Dominance,
    allocas: &[(Value, Ty)],
    phi_map: &HashMap<(usize, usize), Value>,
    rename_map: &mut HashMap<usize, usize>,
) {
    let n = allocas.len();
    // Per-alloca stack of current SSA values (None = no value yet / undef)
    let mut stacks: Vec<Vec<Option<Value>>> = vec![Vec::new(); n];

    enum Visit {
        Enter(Block),
        Leave(Vec<usize>), // snapshot of stack lengths before entering the block
    }

    let mut worklist: Vec<Visit> = vec![Visit::Enter(entry)];

    while let Some(visit) = worklist.pop() {
        match visit {
            Visit::Leave(snapshot) => {
                for (i, &len) in snapshot.iter().enumerate() {
                    stacks[i].truncate(len);
                }
            }

            Visit::Enter(block) => {
                let snapshot: Vec<usize> = stacks.iter().map(|s| s.len()).collect();
                worklist.push(Visit::Leave(snapshot));

                // Push dominator-tree children so they're visited after this block
                let children: Vec<_> = dom.children(block).into_iter().collect();
                for child in children.into_iter().rev() {
                    worklist.push(Visit::Enter(child));
                }

                // Process instructions in this block
                let insts: Vec<Value> = prog.blocks[block].insts.clone();
                for val in insts {
                    match prog.values[val].inst.clone() {
                        Inst::Phi(_) => {
                            // A phi we inserted for some alloca defines its SSA value here
                            for (ai, &(alloca_val, _)) in allocas.iter().enumerate() {
                                if phi_map.get(&(alloca_val.index(), block.index())) == Some(&val) {
                                    stacks[ai].push(Some(val));
                                }
                            }
                        }
                        Inst::Store { ptr, value, .. } => {
                            for (ai, &(alloca_val, _)) in allocas.iter().enumerate() {
                                if ptr == alloca_val {
                                    stacks[ai].push(Some(value));
                                    prog.values[val].inst = Inst::Noop;
                                    break;
                                }
                            }
                        }
                        Inst::Load { ptr, ty, .. } => {
                            for (ai, &(alloca_val, _)) in allocas.iter().enumerate() {
                                if ptr == alloca_val {
                                    let replacement =
                                        current_or_zero(&mut stacks[ai], ty, block, prog);
                                    rename_map.insert(val.index(), replacement.index());
                                    prog.values[val].inst = Inst::Noop;
                                    break;
                                }
                            }
                        }
                        _ => {}
                    }
                }

                // Insert upsilons for phi nodes in each successor
                let mut seen_succs: EntitySet<Block> = EntitySet::new();
                let succs: Vec<_> = prog.blocks[block].term.successors().collect();
                for succ in succs {
                    if !seen_succs.insert(succ) {
                        continue;
                    }
                    for (ai, &(alloca_val, ty)) in allocas.iter().enumerate() {
                        if let Some(&phi_val) = phi_map.get(&(alloca_val.index(), succ.index())) {
                            let current = current_or_zero(&mut stacks[ai], ty, block, prog);
                            let upsilon_idx = prog.blocks[block].insts.len() as u32;
                            let upsilon = prog.values.push(ValueData {
                                inst: Inst::Upsilon {
                                    phi: phi_val,
                                    value: current,
                                },
                                span: Span::default(),
                                block,
                                idx: upsilon_idx,
                            });
                            prog.blocks[block].insts.push(upsilon);
                        }
                    }
                }

                // Noop-ify allocas that have been fully promoted
                for val in prog.blocks[block].insts.clone() {
                    if let Inst::Alloca { .. } = prog.values[val].inst {
                        for &(alloca_val, _) in allocas {
                            if val == alloca_val {
                                prog.values[val].inst = Inst::Noop;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}

fn current_or_zero(stack: &mut [Option<Value>], ty: Ty, block: Block, prog: &mut Program) -> Value {
    if let Some(Some(val)) = stack.last().copied() {
        return val;
    }
    // Uninitialized read (UB in C) — produce a zero constant
    let idx = prog.blocks[block].insts.len() as u32;
    let zero = prog.values.push(ValueData {
        inst: Inst::Const { ty, value: 0 },
        span: Span::default(),
        block,
        idx,
    });
    prog.blocks[block].insts.push(zero);
    zero
}

// -----------------------------------------------------------------------
// Step 5: Substitute renamed values throughout all instructions
// -----------------------------------------------------------------------

fn substitute_all(entry: Block, prog: &mut Program, order: &Order, rename: &HashMap<usize, usize>) {
    if rename.is_empty() {
        return;
    }
    for block in order.rpo(entry) {
        let insts: Vec<Value> = prog.blocks[block].insts.clone();
        for val in insts {
            let new_inst = substitute_inst(&prog.values[val].inst.clone(), rename);
            prog.values[val].inst = new_inst;
        }
        let new_term = substitute_term(&prog.blocks[block].term.clone(), rename);
        prog.blocks[block].term = new_term;
    }
}

fn r(val: Value, rename: &HashMap<usize, usize>) -> Value {
    rename
        .get(&val.index())
        .copied()
        .map(Value::new)
        .unwrap_or(val)
}

fn substitute_inst(inst: &Inst, rename: &HashMap<usize, usize>) -> Inst {
    match inst {
        Inst::Noop
        | Inst::Phi(_)
        | Inst::ConstNull(_)
        | Inst::GlobalAddr(_)
        | Inst::Const { .. }
        | Inst::Param { .. }
        | Inst::Alloca { .. } => inst.clone(),

        Inst::Upsilon { phi, value } => Inst::Upsilon {
            phi: r(*phi, rename),
            value: r(*value, rename),
        },
        Inst::Load { ty, ptr, align } => Inst::Load {
            ty: *ty,
            ptr: r(*ptr, rename),
            align: *align,
        },
        Inst::Store { ptr, align, value } => Inst::Store {
            ptr: r(*ptr, rename),
            align: *align,
            value: r(*value, rename),
        },
        Inst::GetElementPtr { ty, ptr, indices } => Inst::GetElementPtr {
            ty: *ty,
            ptr: r(*ptr, rename),
            indices: indices.iter().map(|&v| r(v, rename)).collect(),
        },
        Inst::Unary { ty, op, operand } => Inst::Unary {
            ty: *ty,
            op: *op,
            operand: r(*operand, rename),
        },
        Inst::Binary { ty, lhs, rhs, op } => Inst::Binary {
            ty: *ty,
            lhs: r(*lhs, rename),
            rhs: r(*rhs, rename),
            op: *op,
        },
        Inst::ICmp { lhs, rhs, pred } => Inst::ICmp {
            lhs: r(*lhs, rename),
            rhs: r(*rhs, rename),
            pred: *pred,
        },
        Inst::FCmp { lhs, rhs, pred } => Inst::FCmp {
            lhs: r(*lhs, rename),
            rhs: r(*rhs, rename),
            pred: *pred,
        },
        Inst::Trunc { to, value } => Inst::Trunc {
            to: *to,
            value: r(*value, rename),
        },
        Inst::FTrunc { to, value } => Inst::FTrunc {
            to: *to,
            value: r(*value, rename),
        },
        Inst::ZExt { to, value } => Inst::ZExt {
            to: *to,
            value: r(*value, rename),
        },
        Inst::SExt { to, value } => Inst::SExt {
            to: *to,
            value: r(*value, rename),
        },
        Inst::FExt { to, value } => Inst::FExt {
            to: *to,
            value: r(*value, rename),
        },
        Inst::Bitcast { to, value } => Inst::Bitcast {
            to: *to,
            value: r(*value, rename),
        },
        Inst::IntToPtr { to, value } => Inst::IntToPtr {
            to: *to,
            value: r(*value, rename),
        },
        Inst::PtrToInt { to, value } => Inst::PtrToInt {
            to: *to,
            value: r(*value, rename),
        },
        Inst::FpToSi { to, value } => Inst::FpToSi {
            to: *to,
            value: r(*value, rename),
        },
        Inst::FpToUi { to, value } => Inst::FpToUi {
            to: *to,
            value: r(*value, rename),
        },
        Inst::SiToFp { to, value } => Inst::SiToFp {
            to: *to,
            value: r(*value, rename),
        },
        Inst::UiToFp { to, value } => Inst::UiToFp {
            to: *to,
            value: r(*value, rename),
        },
        Inst::Call { ty, func, args } => Inst::Call {
            ty: *ty,
            func: *func,
            args: args.iter().map(|&v| r(v, rename)).collect(),
        },
        Inst::IndirectCall { ty, ptr, args } => Inst::IndirectCall {
            ty: *ty,
            ptr: r(*ptr, rename),
            args: args.iter().map(|&v| r(v, rename)).collect(),
        },
        Inst::Select {
            ty,
            cond,
            then_val,
            else_val,
        } => Inst::Select {
            ty: *ty,
            cond: r(*cond, rename),
            then_val: r(*then_val, rename),
            else_val: r(*else_val, rename),
        },
    }
}

fn substitute_term(term: &Terminator, rename: &HashMap<usize, usize>) -> Terminator {
    match term {
        Terminator::Ret(Some(v)) => Terminator::Ret(Some(r(*v, rename))),
        Terminator::CondBr {
            cond,
            then_block,
            else_block,
        } => Terminator::CondBr {
            cond: r(*cond, rename),
            then_block: *then_block,
            else_block: *else_block,
        },
        Terminator::Switch {
            value,
            default,
            cases,
        } => Terminator::Switch {
            value: r(*value, rename),
            default: *default,
            cases: cases.clone(),
        },
        other => other.clone(),
    }
}

// -----------------------------------------------------------------------
// Helpers: instruction and terminator operand enumeration
// -----------------------------------------------------------------------

fn inst_operands(inst: &Inst) -> Vec<Value> {
    match inst {
        Inst::Noop
        | Inst::Phi(_)
        | Inst::ConstNull(_)
        | Inst::GlobalAddr(_)
        | Inst::Const { .. }
        | Inst::Param { .. }
        | Inst::Alloca { .. } => vec![],

        Inst::Upsilon { phi, value } => vec![*phi, *value],
        Inst::Load { ptr, .. } => vec![*ptr],
        Inst::Store { ptr, value, .. } => vec![*ptr, *value],
        Inst::GetElementPtr { ptr, indices, .. } => {
            let mut v = vec![*ptr];
            v.extend_from_slice(indices);
            v
        }
        Inst::Unary { operand, .. } => vec![*operand],
        Inst::Binary { lhs, rhs, .. } => vec![*lhs, *rhs],
        Inst::ICmp { lhs, rhs, .. } => vec![*lhs, *rhs],
        Inst::FCmp { lhs, rhs, .. } => vec![*lhs, *rhs],
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

fn term_operands(term: &Terminator) -> Vec<Value> {
    match term {
        Terminator::Ret(Some(v)) => vec![*v],
        Terminator::CondBr { cond, .. } => vec![*cond],
        Terminator::Switch { value, .. } => vec![*value],
        _ => vec![],
    }
}
