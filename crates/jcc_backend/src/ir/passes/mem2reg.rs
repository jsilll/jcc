use crate::ir::{
    analysis::{dom::Dominance, order::Order},
    inst::Inst,
    ty::Ty,
    Block, Program, Value, ValueData,
};

use jcc_codemap::span::Span;
use jcc_entity::EntitySet;

use std::collections::HashMap;

// Algorithm (5 phases, per function):
//
//   1. Promotable alloca detection — scans all instructions; an Alloca is
//      promotable if its only uses are as the ptr in Load/Store. Any other
//      use (passed to a call, stored as a value, used in a terminator) marks
//      it non-promotable.
//
//   2. Phi placement via iterated DF — for each promotable alloca, collect
//      its def-blocks (blocks containing stores), then expand to the iterated
//      dominance frontier (IDF) using a worklist. Insert a Phi(ty) at the
//      front of each IDF block.
//
//   3. SSA rename — iterative DFS over the dominator tree with explicit
//      Enter/Leave stack entries for snapshot/restore of per-alloca value
//      stacks. On Store → alloca: push new value, noop the store. On Load ←
//      alloca: rename load to current value (inserting Const 0 for
//      uninitialized reads, which are UB in C). Before leaving each block:
//      insert Upsilon { phi, value } for every successor phi.
//
//   4. Substitution — single pass over all instructions and terminators,
//      replacing any Value operand that appears in the rename map with its
//      SSA replacement.

#[derive(Default)]
pub struct Mem2Reg {
    /// Promotable allocas and their types, in RPO order.
    candidates: Vec<(Value, Ty)>,

    /// Alloca values that cannot be promoted (escaping uses).
    non_promotable: EntitySet<Value>,

    /// rename_map[load_value] = ssa_replacement_value
    rename_map: HashMap<Value, Value>,

    /// phi_map[(alloca_val, block)] = phi Value inserted at block for alloca_val.
    ///
    /// A single flat map avoids allocating one HashMap per alloca candidate.
    phi_map: HashMap<(Value, Block), Value>,

    /// phi_to_alloca[phi_val] = index into `candidates` for the alloca that phi was inserted for.
    ///
    /// Populated alongside `phi_map`; enables O(1) reverse lookup in the rename pass.
    phi_to_alloca: HashMap<Value, usize>,

    /// alloca_index[alloca_val] = index into `candidates`, for O(1) lookup during rename.
    alloca_index: HashMap<Value, usize>,
}

impl Mem2Reg {
    pub fn run(&mut self, prog: &mut Program, ord: &Order, dom: &Dominance) {
        let entries: Vec<Block> = prog.functions.values().filter_map(|f| f.entry).collect();

        for entry in entries {
            self.collect_candidates(entry, prog, ord);
            if self.candidates.is_empty() {
                continue;
            }

            self.place_phis(entry, prog, ord, dom);
            self.rename_pass(entry, prog, dom);
            if self.rename_map.is_empty() {
                continue;
            }

            // Flatten all chains in rename_map so every entry points directly
            // to its root.  After this, the substitution pass below pays at
            // most one HashMap lookup per operand instead of walking a chain.
            let keys: Vec<Value> = self.rename_map.keys().copied().collect();
            for key in keys {
                let root = {
                    let mut root = key;
                    while let Some(&next) = self.rename_map.get(&root) {
                        root = next;
                    }
                    root
                };
                self.rename_map.insert(key, root);
            }

            for block in ord.rpo(entry) {
                for i in 0..prog.blocks[block].insts.len() {
                    let val = prog.blocks[block].insts[i];
                    for op in prog.values[val].inst.operands_mut() {
                        if let Some(&root) = self.rename_map.get(op) {
                            *op = root;
                        }
                    }
                }
                for op in prog.blocks[block].term.operand_mut() {
                    if let Some(&root) = self.rename_map.get(op) {
                        *op = root;
                    }
                }
            }
        }
    }

    fn collect_candidates(&mut self, entry: Block, prog: &Program, ord: &Order) {
        self.candidates.clear();
        self.non_promotable.clear();

        for b in ord.rpo(entry) {
            for &v in &prog.blocks[b].insts {
                match &prog.values[v].inst {
                    Inst::Load { .. } => {}
                    Inst::Alloca { ty, .. } => {
                        self.candidates.push((v, *ty));
                    }
                    Inst::Store { value, .. } => {
                        // Storing the address of an alloca makes it non-promotable.
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

        self.candidates
            .retain(|(v, _)| !self.non_promotable.contains(v));

        // Rebuild the index after filtering out non-promotable entries.
        self.alloca_index.clear();
        for (i, &(v, _)) in self.candidates.iter().enumerate() {
            self.alloca_index.insert(v, i);
        }
    }

    fn place_phis(&mut self, entry: Block, prog: &mut Program, ord: &Order, dom: &Dominance) {
        self.phi_map.clear();
        self.phi_to_alloca.clear();

        for (ai, &(alloca_val, ty)) in self.candidates.iter().enumerate() {
            // Collect blocks that contain stores to this alloca.
            let mut def_blocks: EntitySet<Block> = EntitySet::default();
            for block in ord.rpo(entry) {
                for &val in &prog.blocks[block].insts {
                    if let Inst::Store { ptr, .. } = prog.values[val].inst {
                        if ptr == alloca_val {
                            def_blocks.insert(block);
                            break;
                        }
                    }
                }
            }

            // Iterate to fixed point: phi placements expand the def-block set.
            let mut worklist: Vec<Block> = def_blocks.iter().copied().collect();
            let mut has_phi: EntitySet<Block> = EntitySet::default();

            while let Some(block) = worklist.pop() {
                for frontier in dom.frontier(block) {
                    if has_phi.insert(frontier) {
                        let phi_val = insert_phi_at_front(frontier, ty, prog);
                        self.phi_map.insert((alloca_val, frontier), phi_val);
                        self.phi_to_alloca.insert(phi_val, ai);
                        // The frontier is now also a def-block via the phi.
                        if def_blocks.insert(frontier) {
                            worklist.push(frontier);
                        }
                    }
                }
            }
        }
    }

    fn rename_pass(&mut self, entry: Block, prog: &mut Program, dom: &Dominance) {
        self.rename_map.clear();
        let n = self.candidates.len();

        // Per-alloca stack of current SSA values (None = no value yet / undef).
        let mut stacks: Vec<Vec<Option<Value>>> = vec![Vec::new(); n];

        enum Visit {
            Enter(Block),
            /// Stack-length snapshot taken before entering the corresponding block.
            Leave(Vec<usize>),
        }

        let mut worklist: Vec<Visit> = vec![Visit::Enter(entry)];

        while let Some(visit) = worklist.pop() {
            match visit {
                Visit::Leave(snapshot) => {
                    for (stack, &len) in stacks.iter_mut().zip(&snapshot) {
                        stack.truncate(len);
                    }
                }

                Visit::Enter(block) => {
                    let snapshot: Vec<usize> = stacks.iter().map(|s| s.len()).collect();
                    worklist.push(Visit::Leave(snapshot));

                    // Push dominator-tree children so they are visited after this block.
                    for child in dom.children(block).collect::<Vec<_>>().into_iter().rev() {
                        worklist.push(Visit::Enter(child));
                    }

                    // Process instructions in this block.
                    //
                    // We iterate by index rather than cloning the Vec to avoid
                    // an allocation; the Vec may grow (upsilons appended later),
                    // but we only want to visit the instructions that existed
                    // when we entered the block, so we snapshot the count first.
                    let inst_count = prog.blocks[block].insts.len();
                    for i in 0..inst_count {
                        let val = prog.blocks[block].insts[i];
                        match prog.values[val].inst {
                            Inst::Phi(_) => {
                                // Find which alloca this phi belongs to.
                                if let Some(&ai) = self.phi_to_alloca.get(&val) {
                                    stacks[ai].push(Some(val));
                                }
                            }
                            Inst::Param { .. } => {
                                // Params act as pre-initialized virtual allocas.
                                if let Some(&ai) = self.alloca_index.get(&val) {
                                    stacks[ai].push(Some(val));
                                }
                            }
                            Inst::Store { ptr, value, .. } => {
                                if let Some(&ai) = self.alloca_index.get(&ptr) {
                                    stacks[ai].push(Some(value));
                                    prog.values[val].inst = Inst::Noop;
                                }
                            }
                            Inst::Load { ptr, ty, .. } => {
                                if let Some(&ai) = self.alloca_index.get(&ptr) {
                                    let replacement =
                                        current_or_zero(&mut stacks[ai], ty, block, prog);
                                    self.rename_map.insert(val, replacement);
                                    prog.values[val].inst = Inst::Noop;
                                }
                            }

                            // Noop promoted allocas right here, avoiding a
                            // separate pass over the instruction list.
                            Inst::Alloca { .. } if self.alloca_index.contains_key(&val) => {
                                prog.values[val].inst = Inst::Noop;
                            }
                            _ => {}
                        }
                    }

                    // Insert Upsilons for phi nodes in each successor.
                    let mut seen_succs: EntitySet<Block> = EntitySet::default();
                    let succs: Vec<_> = prog.blocks[block].term.successors().collect();
                    for succ in succs {
                        if !seen_succs.insert(succ) {
                            continue;
                        }
                        for (ai, &(alloca_val, ty)) in self.candidates.iter().enumerate() {
                            if let Some(&phi_val) = self.phi_map.get(&(alloca_val, succ)) {
                                let current = current_or_zero(&mut stacks[ai], ty, block, prog);
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
                }
            }
        }
    }
}

/// Inserts a `Phi(ty)` at the front of `block`, shifting existing instruction indices.
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

/// Returns the current SSA value on top of `stack`, or synthesises a zero
/// constant (for uninitialized reads, which are UB in C).
fn current_or_zero(stack: &mut [Option<Value>], ty: Ty, block: Block, prog: &mut Program) -> Value {
    if let Some(Some(val)) = stack.last().copied() {
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
