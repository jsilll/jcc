use crate::ir::{
    analysis::{dom::Dominance, order::Order},
    inst::Inst,
    ty::Ty,
    Block, Program, Value, ValueData,
};

use jcc_codemap::span::Span;
use jcc_entity::EntitySet;
use std::collections::HashMap;

/// Traversal state for the single-pass dominator tree DFS.
enum Visit {
    Enter(Block),
    /// Records the length of the centralized undo log when this block was entered.
    Leave(usize),
}

#[derive(Default)]
pub struct Mem2Reg {
    // --- Core Algorithm State ---
    /// Promotable allocas and their types, in RPO order.
    candidates: Vec<(Value, Ty)>,

    /// alloca_index[alloca_val] = index into `candidates`
    alloca_index: HashMap<Value, usize>,

    /// phi_map[(alloca_val, block)] = phi Value inserted at block for alloca_val.
    phi_map: HashMap<(Value, Block), Value>,

    /// phi_to_alloca[phi_val] = index into `candidates`
    phi_to_alloca: HashMap<Value, usize>,

    // --- Reusable Arena Buffers (Zero Allocator Churn) ---
    non_promotable: EntitySet<Value>,
    rename_map: HashMap<Value, Value>,

    /// Per-alloca stack tracking current SSA values. Sub-vectors retain capacity.
    stacks: Vec<Vec<Value>>,

    /// Centralized undo log tracking which stacks to pop when leaving a block.
    undo_log: Vec<usize>,

    /// Traversal stack used for the dominator tree DFS.
    worklist: Vec<Visit>,
}

impl Mem2Reg {
    /// Executes the Mem2Reg pass over all functions inside the program.
    pub fn run(&mut self, prog: &mut Program, ord: &Order, dom: &Dominance) {
        let entries: Vec<Block> = prog.functions.values().filter_map(|f| f.entry).collect();

        for entry in entries {
            self.collect_candidates(entry, prog, ord);
            if self.candidates.is_empty() {
                continue;
            }

            self.place_phis(entry, prog, ord, dom);
            self.rename_and_substitute(entry, prog, dom);
        }
    }

    /// Phase 1: Detect all promotable allocas within a function.
    fn collect_candidates(&mut self, entry: Block, prog: &Program, ord: &Order) {
        self.candidates.clear();
        self.alloca_index.clear();
        self.non_promotable.clear();

        // Scan all instructions for escaping uses of Allocas
        for &b in ord.rpo(entry) {
            for &v in &prog.blocks[b].insts {
                match &prog.values[v].inst {
                    Inst::Load { .. } => {}
                    Inst::Alloca { ty, .. } => self.candidates.push((v, *ty)),
                    Inst::Store { value, .. } => {
                        if matches!(prog.values[*value].inst, Inst::Alloca { .. }) {
                            self.non_promotable.insert(*value);
                        }
                    }
                    inst => {
                        for op in inst.operands() {
                            if matches!(prog.values[op].inst, Inst::Alloca { .. }) {
                                self.non_promotable.insert(op);
                            }
                        }
                    }
                }
            }
            for op in prog.blocks[b].term.operands() {
                if matches!(prog.values[op].inst, Inst::Alloca { .. }) {
                    self.non_promotable.insert(op);
                }
            }
        }

        // Filter out escaping allocas and build the fast index map
        let mut valid_candidates = Vec::new();
        for (v, ty) in self.candidates.drain(..) {
            if !self.non_promotable.contains(&v) {
                self.alloca_index.insert(v, valid_candidates.len());
                valid_candidates.push((v, ty));
            }
        }
        self.candidates = valid_candidates;
    }

    /// Phase 2: Place Phi nodes at the Iterated Dominance Frontier (IDF) of defs.
    fn place_phis(&mut self, entry: Block, prog: &mut Program, ord: &Order, dom: &Dominance) {
        self.phi_map.clear();
        self.phi_to_alloca.clear();

        for (ai, &(alloca_val, ty)) in self.candidates.iter().enumerate() {
            let mut def_blocks: EntitySet<Block> = EntitySet::default();

            // Gather all blocks that contain a store to this specific alloca
            for &block in ord.rpo(entry) {
                for &val in &prog.blocks[block].insts {
                    if let Inst::Store { ptr, .. } = prog.values[val].inst {
                        if ptr == alloca_val {
                            def_blocks.insert(block);
                            break;
                        }
                    }
                }
            }

            // Fixed-point worklist calculation for IDF
            let mut worklist: Vec<Block> = def_blocks.iter().copied().collect();
            let mut has_phi: EntitySet<Block> = EntitySet::default();

            while let Some(block) = worklist.pop() {
                for &frontier in dom.frontier(block) {
                    if has_phi.insert(frontier) {
                        let phi_val = insert_phi_at_front(frontier, ty, prog);
                        self.phi_map.insert((alloca_val, frontier), phi_val);
                        self.phi_to_alloca.insert(phi_val, ai);

                        // The newly inserted Phi acts as a brand new definition point
                        if def_blocks.insert(frontier) {
                            worklist.push(frontier);
                        }
                    }
                }
            }
        }
    }

    /// Phases 3 & 4: Core SSA Rename and On-The-Fly Value Substitution.
    fn rename_and_substitute(&mut self, entry: Block, prog: &mut Program, dom: &Dominance) {
        self.rename_map.clear();
        self.undo_log.clear();
        self.worklist.clear();

        // Re-align the length of the arena stacks without wiping underlying capacities
        let n = self.candidates.len();
        self.stacks.resize_with(n, Vec::new);
        for i in 0..n {
            self.stacks[i].clear();
        }

        self.worklist.push(Visit::Enter(entry));

        while let Some(visit) = self.worklist.pop() {
            match visit {
                Visit::Leave(undo_start) => {
                    // Roll back variable versions pushed during this block's traversal
                    while self.undo_log.len() > undo_start {
                        let ai = self.undo_log.pop().unwrap();
                        self.stacks[ai].pop();
                    }
                }
                Visit::Enter(block) => {
                    let undo_start = self.undo_log.len();
                    let inst_count = prog.blocks[block].insts.len();

                    for i in 0..inst_count {
                        let val = prog.blocks[block].insts[i];

                        // 1. Substitute operands FIRST. Because we follow dominance order,
                        // any tracked operand here is already finalized.
                        for op in prog.values[val].inst.operands_mut() {
                            if let Some(&new_val) = self.rename_map.get(op) {
                                *op = new_val;
                            }
                        }

                        // 2. Perform the stack-based variable renaming
                        match prog.values[val].inst {
                            Inst::Phi(_) => {
                                if let Some(&ai) = self.phi_to_alloca.get(&val) {
                                    self.stacks[ai].push(val);
                                    self.undo_log.push(ai);
                                }
                            }
                            Inst::Param { .. } => {
                                if let Some(&ai) = self.alloca_index.get(&val) {
                                    self.stacks[ai].push(val);
                                    self.undo_log.push(ai);
                                }
                            }
                            Inst::Store { ptr, value, .. } => {
                                if let Some(&ai) = self.alloca_index.get(&ptr) {
                                    self.stacks[ai].push(value);
                                    self.undo_log.push(ai);
                                    prog.values[val].inst = Inst::Noop;
                                }
                            }
                            Inst::Load { ptr, ty, .. } => {
                                if let Some(&ai) = self.alloca_index.get(&ptr) {
                                    let replacement =
                                        current_or_zero(&self.stacks[ai], ty, block, prog);
                                    self.rename_map.insert(val, replacement);
                                    prog.values[val].inst = Inst::Noop;
                                }
                            }
                            Inst::Alloca { .. } if self.alloca_index.contains_key(&val) => {
                                prog.values[val].inst = Inst::Noop;
                            }
                            _ => {}
                        }
                    }

                    // Substitute terminator operands
                    for op in prog.blocks[block].term.operands_mut() {
                        if let Some(&new_val) = self.rename_map.get(op) {
                            *op = new_val;
                        }
                    }

                    // Populate Upsilon instructions for matching Phis in successor blocks
                    let mut seen_succs: EntitySet<Block> = EntitySet::default();
                    let succs: Vec<_> = prog.blocks[block].term.successors().collect();

                    for succ in succs {
                        if !seen_succs.insert(succ) {
                            continue;
                        }
                        for (ai, &(alloca_val, ty)) in self.candidates.iter().enumerate() {
                            if let Some(&phi_val) = self.phi_map.get(&(alloca_val, succ)) {
                                let current = current_or_zero(&self.stacks[ai], ty, block, prog);
                                let upsilon_idx = prog.blocks[block].insts.len() as u32;
                                let upsilon = prog.values.push(ValueData {
                                    inst: Inst::Upsilon {
                                        phi: phi_val,
                                        value: current,
                                    },
                                    span: Span::DEFAULT,
                                    block,
                                    idx: upsilon_idx,
                                });
                                prog.blocks[block].insts.push(upsilon);
                            }
                        }
                    }

                    // Queue the rollback action, then discover children in DFS order
                    self.worklist.push(Visit::Leave(undo_start));
                    for &child in dom.children(block).iter().rev() {
                        self.worklist.push(Visit::Enter(child));
                    }
                }
            }
        }
    }
}

/// Helper: Inserts a `Phi` instruction at index 0 of the target block.
fn insert_phi_at_front(block: Block, ty: Ty, prog: &mut Program) -> Value {
    for &existing in &prog.blocks[block].insts {
        prog.values[existing].idx += 1;
    }
    let phi_val = prog.values.push(ValueData {
        inst: Inst::Phi(ty),
        span: Span::DEFAULT,
        block,
        idx: 0,
    });
    prog.blocks[block].insts.insert(0, phi_val);
    phi_val
}

/// Helper: Retrieves the current active value from the stack,
/// or constructs a zero-constant fallback for undefined reads.
fn current_or_zero(stack: &[Value], ty: Ty, block: Block, prog: &mut Program) -> Value {
    if let Some(&val) = stack.last() {
        return val;
    }
    let idx = prog.blocks[block].insts.len() as u32;
    let zero = prog.values.push(ValueData {
        inst: Inst::Const { ty, value: 0 },
        span: Span::DEFAULT,
        block,
        idx,
    });
    prog.blocks[block].insts.push(zero);
    zero
}
