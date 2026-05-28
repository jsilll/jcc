use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use jcc_entity::EntitySet;

use crate::{
    ir::{
        analysis::{liveness::Liveness, order::Order},
        inst::{BinaryOp, Inst},
        ty::Ty,
        Block, Program, Value,
    },
    x86_64::regs::{class_of, RegClass, CALLEE_SAVED_FLOAT, FLOAT_REGS, INT_REGS},
};

use super::regs::{PReg, FLOAT_ARG_REGS, INT_ARG_REGS};

/// Where a value lives after register allocation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Location {
    Reg(PReg),
    /// Byte offset from %rbp (always negative for locals).
    Stack(i32),
}

/// Output of the register allocator for one function.
pub struct RegAlloc {
    /// Physical location assigned to each SSA value.
    pub assignment: HashMap<Value, Location>,
    /// Total stack frame size in bytes (already aligned to 16 bytes).
    pub stack_size: u32,
    /// Callee-saved GP registers that are actually used by this function.
    pub used_callee_saved: Vec<PReg>,
    /// Callee-saved XMM registers (xmm8–xmm15) actually used by this function.
    pub used_callee_saved_float: Vec<PReg>,
}

// ---------------------------------------------------------------------------
// Interference graph
// ---------------------------------------------------------------------------

struct InterferenceGraph {
    // Map from value index to the set of interfering value indices.
    adj: HashMap<u32, HashSet<u32>>,
}

impl InterferenceGraph {
    fn new() -> Self {
        Self {
            adj: HashMap::new(),
        }
    }

    fn add_edge(&mut self, a: Value, b: Value) {
        if a == b {
            return;
        }
        self.adj.entry(a.as_u32()).or_default().insert(b.as_u32());
        self.adj.entry(b.as_u32()).or_default().insert(a.as_u32());
    }

    fn neighbors(&self, v: Value) -> impl Iterator<Item = Value> + '_ {
        self.adj
            .get(&v.as_u32())
            .into_iter()
            .flat_map(|s| s.iter().map(|&i| Value::from_u32(i)))
    }

    fn neighbors_sorted(&self, v: Value) -> Vec<Value> {
        let mut neighbors: Vec<_> = self.neighbors(v).collect();
        neighbors.sort_by_key(|value| value.as_u32());
        neighbors
    }
}

// ---------------------------------------------------------------------------
// Build interference graph via backward simulation
// ---------------------------------------------------------------------------

#[warn(clippy::too_many_arguments)]
fn build_interference_graph(
    entry: Block,
    prog: &Program,
    order: &Order,
    liveness: &Liveness,
) -> InterferenceGraph {
    use crate::ir::analysis::liveness::{inst_uses, term_uses};

    let mut graph = InterferenceGraph::new();

    for block in order.rpo(entry).into_iter() {
        let mut live: EntitySet<Value> = liveness.live_out[block].clone();

        // Terminator uses become live before the block's last instruction.
        for u in term_uses(&prog.blocks[block].term) {
            live.insert(u);
        }

        // Separate phi defs from regular instructions.
        let insts: Vec<Value> = prog.blocks[block].insts.clone();
        let (phi_vals, non_phi_vals): (Vec<_>, Vec<_>) = insts
            .iter()
            .partition(|&&v| matches!(prog.values[v].inst, Inst::Phi(_)));

        // Walk non-phi instructions in reverse.
        for &val in non_phi_vals.iter().rev() {
            let inst = &prog.values[val].inst;

            if inst.ty() != Ty::Void {
                // val is defined here: it interferes with everything currently live.
                for &u in live.iter() {
                    graph.add_edge(val, u);
                }
                live.remove(&val);

                // For two-address x86 code generation, the result register
                // cannot overlap with any source operand (since we emit
                // "mov lhs, dst; op rhs, dst" and rhs must still be readable
                // after the move). Add explicit interference with all uses.
                for u in inst_uses(inst) {
                    graph.add_edge(val, u);
                }
            }

            for u in inst_uses(inst) {
                live.insert(u);
            }
        }

        // Phi defs are simultaneous at the block start.
        // They all interfere with each other and with values live at that point.
        for &phi in &phi_vals {
            for &u in live.iter() {
                graph.add_edge(phi, u);
            }
            for &other_phi in &phi_vals {
                graph.add_edge(phi, other_phi);
            }
        }
    }

    graph
}

// ---------------------------------------------------------------------------
// Chaitin-Briggs graph coloring (per register class)
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn color_class(
    values: &[Value],
    regs: &[PReg],
    prog: &Program,
    graph: &InterferenceGraph,
    call_live: &HashSet<u32>,
    // Values that cannot be placed in %rax/%rdx — they are live across
    // div/rem instructions which implicitly clobber those registers.
    div_clobber_forbidden: &HashSet<u32>,
    precolors: &HashMap<u32, PReg>,
    assignment: &mut HashMap<Value, Location>,
    stack_offset: &mut i32,
) {
    let k = regs.len();

    // Only consider registers that are callee-saved for call-live values.
    let callee_saved: Vec<PReg> = regs
        .iter()
        .copied()
        .filter(|r| !r.is_caller_saved())
        .collect();

    // Collect values not yet assigned (precolored values skip the worklist).
    let mut remaining: BTreeSet<u32> = values
        .iter()
        .filter(|&&v| !precolors.contains_key(&v.as_u32()))
        .map(|v| v.as_u32())
        .collect();

    // Track per-value degree restricted to the current class.
    let mut degree: HashMap<u32, usize> = remaining
        .iter()
        .map(|&vi| {
            let v = Value::from_u32(vi);
            let d = graph
                .neighbors(v)
                .filter(|u| remaining.contains(&u.as_u32()) || precolors.contains_key(&u.as_u32()))
                .count();
            (vi, d)
        })
        .collect();

    let mut stack: Vec<u32> = Vec::new();

    // Simplify + potential-spill loop.
    let mut simplify_queue: VecDeque<u32> = remaining
        .iter()
        .filter(|&&vi| degree.get(&vi).copied().unwrap_or(0) < k)
        .copied()
        .collect();

    while !remaining.is_empty() {
        // Simplify: remove low-degree nodes first.
        while let Some(vi) = simplify_queue.pop_front() {
            if !remaining.contains(&vi) {
                continue;
            }
            remaining.remove(&vi);
            stack.push(vi);

            let v = Value::from_u32(vi);
            for u in graph.neighbors_sorted(v) {
                let ui = u.as_u32();
                if remaining.contains(&ui) {
                    if let Some(d) = degree.get_mut(&ui) {
                        if *d > 0 {
                            *d -= 1;
                        }
                        if *d + 1 == k {
                            simplify_queue.push_back(ui);
                        }
                    }
                }
            }
        }

        // Potential spill: pick the node with the highest degree.
        if !remaining.is_empty() {
            let &spill = remaining
                .iter()
                .max_by_key(|&&vi| (degree.get(&vi).copied().unwrap_or(0), std::cmp::Reverse(vi)))
                .unwrap();
            remaining.remove(&spill);
            stack.push(spill);

            let v = Value::from_u32(spill);
            for u in graph.neighbors_sorted(v) {
                let ui = u.as_u32();
                if remaining.contains(&ui) {
                    if let Some(d) = degree.get_mut(&ui) {
                        if *d > 0 {
                            *d -= 1;
                        }
                        if *d + 1 == k {
                            simplify_queue.push_back(ui);
                        }
                    }
                }
            }
        }
    }

    // Select: assign colors in reverse simplification order.
    while let Some(vi) = stack.pop() {
        let v = Value::from_u32(vi);
        let ty = prog.values[v].inst.ty();

        // Collect colors already used by neighbors.
        let mut used: HashSet<usize> = HashSet::new();
        for u in graph.neighbors_sorted(v) {
            if let Some(&Location::Reg(r)) = assignment.get(&u) {
                if r.class() == class_of(ty) {
                    if let Some(idx) = regs.iter().position(|&x| x == r) {
                        used.insert(idx);
                    }
                }
            }
            if let Some(&r) = precolors.get(&u.as_u32()) {
                if r.class() == class_of(ty) {
                    if let Some(idx) = regs.iter().position(|&x| x == r) {
                        used.insert(idx);
                    }
                }
            }
        }

        // For values live across calls, restrict to callee-saved registers.
        let base_regs: &[PReg] = if call_live.contains(&vi) && !callee_saved.is_empty() {
            &callee_saved
        } else {
            regs
        };

        // For values live across div/rem, exclude %rax and %rdx which are
        // implicitly clobbered by idiv (quotient/sign-extension/remainder).
        // Also exclude %rcx which is clobbered by shift instructions when a
        // value is live across a shift.
        let chosen = if div_clobber_forbidden.contains(&vi) {
            base_regs
                .iter()
                .copied()
                .filter(|r| !matches!(r, PReg::Rax | PReg::Rdx | PReg::Rcx))
                .find(|r| {
                    regs.iter()
                        .position(|x| x == r)
                        .is_some_and(|i| !used.contains(&i))
                })
                .or_else(|| {
                    // Fall back to any register (including rax/rdx/rcx) if none available.
                    base_regs.iter().copied().find(|r| {
                        regs.iter()
                            .position(|x| x == r)
                            .is_some_and(|i| !used.contains(&i))
                    })
                })
        } else {
            base_regs.iter().copied().find(|r| {
                regs.iter()
                    .position(|x| x == r)
                    .is_some_and(|i| !used.contains(&i))
            })
        };

        match chosen {
            Some(r) => {
                assignment.insert(v, Location::Reg(r));
            }
            None => {
                // Actual spill: assign a stack slot (8-byte aligned).
                *stack_offset -= 8;
                assignment.insert(v, Location::Stack(*stack_offset));
            }
        }
    }

    // Apply pre-colors.
    for (&vi, &r) in precolors {
        let v = Value::from_u32(vi);
        if values.contains(&v) {
            assignment.insert(v, Location::Reg(r));
        }
    }
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

pub fn allocate(entry: Block, prog: &Program, order: &Order, liveness: &Liveness) -> RegAlloc {
    let mut assignment: HashMap<Value, Location> = HashMap::new();
    let mut stack_offset: i32 = 0;

    // Collect all values reachable in this function.
    let mut int_values: Vec<Value> = Vec::new();
    let mut float_values: Vec<Value> = Vec::new();

    // Precolored values: Param instructions are bound to ABI arg registers.
    let mut int_precolors: HashMap<u32, PReg> = HashMap::new();
    let mut float_precolors: HashMap<u32, PReg> = HashMap::new();

    // Values that are live across at least one call site.
    let mut call_live: HashSet<u32> = HashSet::new();

    // Values that cannot be placed in %rax/%rdx/%rcx because they are live
    // across a div/rem or shift instruction that implicitly clobbers those
    // registers (cltd clobbers %rdx; dividend goes in %rax; shift count uses %rcx).
    let mut div_clobber_forbidden: HashSet<u32> = HashSet::new();

    for block in order.rpo(entry).into_iter() {
        let live_out = &liveness.live_out[block];

        for &val in &prog.blocks[block].insts {
            let inst = &prog.values[val].inst;
            let ty = inst.ty();
            if ty == Ty::Void {
                continue;
            }

            match ty {
                Ty::F32 | Ty::F64 => float_values.push(val),
                _ => int_values.push(val),
            }

            // Precolor parameters.
            if let Inst::Param { ty, index } = inst {
                match class_of(*ty) {
                    RegClass::Int => {
                        if let Some(&r) = INT_ARG_REGS.get(*index as usize) {
                            int_precolors.insert(val.as_u32(), r);
                        }
                    }
                    RegClass::Float => {
                        if let Some(&r) = FLOAT_ARG_REGS.get(*index as usize) {
                            float_precolors.insert(val.as_u32(), r);
                        }
                    }
                }
            }

            // Mark values live across calls.
            if matches!(inst, Inst::Call { .. } | Inst::IndirectCall { .. }) {
                for &live_val in live_out.iter() {
                    let live_ty = prog.values[live_val].inst.ty();
                    if live_ty != Ty::Void {
                        call_live.insert(live_val.as_u32());
                    }
                }
            }
        }
    }

    // Backward scan per block to find values live at each call/div/rem/shift instruction.
    // live_out[block] only covers block boundaries; we need intra-block liveness too.
    {
        use crate::ir::analysis::liveness::{inst_uses, term_uses};
        for block in order.rpo(entry).into_iter() {
            let mut live: EntitySet<Value> = liveness.live_out[block].clone();

            for u in term_uses(&prog.blocks[block].term) {
                live.insert(u);
            }

            let insts: Vec<Value> = prog.blocks[block].insts.clone();
            let (_, non_phi_vals): (Vec<_>, Vec<_>) = insts
                .iter()
                .partition(|&&v| matches!(prog.values[v].inst, Inst::Phi(_)));

            for &val in non_phi_vals.iter().rev() {
                let inst = &prog.values[val].inst;

                let is_div_rem = matches!(
                    inst,
                    Inst::Binary {
                        op: BinaryOp::SDiv | BinaryOp::UDiv | BinaryOp::SRem | BinaryOp::URem,
                        ..
                    }
                );
                let is_shift = matches!(
                    inst,
                    Inst::Binary {
                        op: BinaryOp::Shl | BinaryOp::Shr | BinaryOp::AShr,
                        ..
                    }
                );
                let is_call = matches!(inst, Inst::Call { .. } | Inst::IndirectCall { .. });

                if is_call {
                    for &live_val in live.iter() {
                        if live_val != val && prog.values[live_val].inst.ty() != Ty::Void {
                            call_live.insert(live_val.as_u32());
                        }
                    }
                }

                if is_div_rem || is_shift {
                    // Values in `live` at the definition point are live across this
                    // instruction. They cannot occupy registers clobbered by idiv/shift.
                    for &live_val in live.iter() {
                        if live_val != val {
                            div_clobber_forbidden.insert(live_val.as_u32());
                        }
                    }
                    // The result of a shift must not land in %rcx because we use
                    // %rcx to hold the shift count during emission.  For div/rem the
                    // result is always moved out of %rax/%rdx afterwards, so only
                    // shifts need this extra constraint.
                    if is_shift {
                        div_clobber_forbidden.insert(val.as_u32());
                    }
                }

                if inst.ty() != Ty::Void {
                    live.remove(&val);
                }
                for u in inst_uses(inst) {
                    live.insert(u);
                }
            }
        }
    }

    let graph = build_interference_graph(entry, prog, order, liveness);
    int_precolors
        .retain(|value, _| !call_live.contains(value) && !div_clobber_forbidden.contains(value));
    float_precolors
        .retain(|value, _| !call_live.contains(value) && !div_clobber_forbidden.contains(value));

    color_class(
        &int_values,
        INT_REGS,
        prog,
        &graph,
        &call_live,
        &div_clobber_forbidden,
        &int_precolors,
        &mut assignment,
        &mut stack_offset,
    );
    color_class(
        &float_values,
        FLOAT_REGS,
        prog,
        &graph,
        &call_live,
        &div_clobber_forbidden,
        &float_precolors,
        &mut assignment,
        &mut stack_offset,
    );

    // Determine which callee-saved GP registers are actually used.
    let used_callee_saved: Vec<PReg> = super::regs::CALLEE_SAVED_INT
        .iter()
        .copied()
        .filter(|r| assignment.values().any(|loc| *loc == Location::Reg(*r)))
        .collect();

    // Determine which callee-saved XMM registers (xmm8–xmm15) are used.
    let used_callee_saved_float: Vec<PReg> = CALLEE_SAVED_FLOAT
        .iter()
        .copied()
        .filter(|r| assignment.values().any(|loc| *loc == Location::Reg(*r)))
        .collect();

    // Reserve space in the frame for callee-saved registers above spill slots.
    // Layout (from %rbp downwards):
    //   [%rbp - 8*i]        = callee_saved_int[i-1]     (8 bytes each)
    //   [%rbp - 8n - 16*j]  = callee_saved_float[j-1]   (16 bytes each, movdqu)
    //   spill slots start below both areas (Stack(-8) adjusted by total callee area)
    let int_callee_area = used_callee_saved.len() as i32 * 8;
    let float_callee_area = used_callee_saved_float.len() as i32 * 16;
    let total_callee_area = int_callee_area + float_callee_area;
    for loc in assignment.values_mut() {
        if let Location::Stack(off) = loc {
            *off -= total_callee_area;
        }
    }

    // Total stack frame = spill area + callee-save area, aligned to 16.
    // After pushq %rbp + movq %rsp, %rbp: %rsp is 16-byte aligned.
    // subq $stack_size keeps alignment when stack_size is a multiple of 16.
    let raw_size = (-stack_offset) as u32;
    let total = raw_size + total_callee_area as u32;
    let stack_size = (total + 15) & !15;

    RegAlloc {
        assignment,
        stack_size,
        used_callee_saved,
        used_callee_saved_float,
    }
}
