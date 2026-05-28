use std::{collections::HashSet, fmt::Write};

use crate::{
    ir::{
        analysis::{
            liveness::{inst_uses, term_uses, Liveness},
            order::Order,
        },
        inst::{BinaryOp, ICmpOp, Inst, UnaryOp},
        term::Terminator,
        ty::Ty,
        Block, Function, Program, Value,
    },
    x86_64::{
        regalloc::{allocate, Location, RegAlloc},
        regs::{class_of, PReg, RegClass, FLOAT_ARG_REGS, INT_ARG_REGS},
    },
    IdentInterner,
};

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Emits an operand (register or memory reference) as an AT&T string.
fn fmt_loc(loc: Location, ty: Ty) -> String {
    match loc {
        Location::Reg(r) => match class_of(ty) {
            RegClass::Float => r.name64().to_string(),
            RegClass::Int => r.name_for_ty(ty).to_string(),
        },
        Location::Stack(off) => format!("{}(%rbp)", off),
    }
}

/// Returns the AT&T mov mnemonic suffix appropriate for a type.
fn mov_suffix(ty: Ty) -> &'static str {
    match ty {
        Ty::I1 | Ty::I8 => "b",
        Ty::I16 => "w",
        Ty::I32 => "l",
        Ty::I64 | Ty::Ptr => "q",
        Ty::F32 => "ss",
        Ty::F64 => "sd",
        Ty::Void => "",
    }
}

/// Returns the larger of two integer types (for mixed-type comparisons).
fn larger_int_ty(a: Ty, b: Ty) -> Ty {
    fn rank(t: Ty) -> u8 {
        match t {
            Ty::I1 | Ty::I8 => 1,
            Ty::I16 => 2,
            Ty::I32 => 3,
            _ => 4,
        }
    }
    if rank(a) >= rank(b) {
        a
    } else {
        b
    }
}

/// Returns the AT&T arithmetic suffix for integer operations.
fn int_suffix(ty: Ty) -> &'static str {
    match ty {
        Ty::I1 | Ty::I8 => "b",
        Ty::I16 => "w",
        Ty::I32 => "l",
        Ty::I64 | Ty::Ptr => "q",
        _ => "q",
    }
}

/// Returns the `set` mnemonic (without the leading "set") for an integer comparison.
fn setcc(pred: ICmpOp) -> &'static str {
    match pred {
        ICmpOp::Eq => "e",
        ICmpOp::Ne => "ne",
        ICmpOp::Lt => "l",
        ICmpOp::Le => "le",
        ICmpOp::Gt => "g",
        ICmpOp::Ge => "ge",
        ICmpOp::Ult => "b",
        ICmpOp::Ule => "be",
        ICmpOp::Ugt => "a",
        ICmpOp::Uge => "ae",
    }
}

/// Emit a typed move between two locations.
fn emit_mov(ty: Ty, src: &str, dst: &str, out: &mut String) {
    let suf = mov_suffix(ty);
    if src.contains('(') && dst.contains('(') {
        let scratch = match class_of(ty) {
            RegClass::Float => "%xmm15",
            RegClass::Int => PReg::R11.name_for_ty(ty),
        };
        writeln!(out, "    mov{} {}, {}", suf, src, scratch).unwrap();
        writeln!(out, "    mov{} {}, {}", suf, scratch, dst).unwrap();
        return;
    }
    writeln!(out, "    mov{} {}, {}", suf, src, dst).unwrap();
}

// ---------------------------------------------------------------------------
// Parallel copy resolver
// ---------------------------------------------------------------------------

/// Resolve a set of parallel copies `(src_loc, dst_loc, ty)` without a
/// dedicated temp register by sequentialising them and breaking cycles with
/// %rax (int) or %xmm15 (float) as scratch.
fn emit_parallel_copies(copies: &[(Location, Location, Ty)], out: &mut String) {
    // Deduplicate: skip copies where src == dst.
    let copies: Vec<_> = copies.iter().filter(|(s, d, _)| s != d).collect();
    if copies.is_empty() {
        return;
    }

    // Serialise with a simple sequencer: emit copies in topological order.

    let mut done = vec![false; copies.len()];
    let mut progress = true;

    while progress {
        progress = false;
        for (i, (src, dst, ty)) in copies.iter().enumerate() {
            if done[i] {
                continue;
            }
            // Safe to emit this copy if its destination is not a source of a
            // remaining copy (avoids overwriting a value still needed).
            let dst_is_source = copies
                .iter()
                .enumerate()
                .any(|(j, (s, _, _))| !done[j] && j != i && s == dst);
            if !dst_is_source {
                emit_mov(*ty, &fmt_loc(*src, *ty), &fmt_loc(*dst, *ty), out);
                done[i] = true;
                progress = true;
            }
        }
    }

    // Break any remaining cycles. Each undone copy is part of a cycle.
    // We walk the full cycle, pick a scratch register not in the cycle, save the
    // first element there, then emit the rest of the chain, and finish by copying
    // scratch into the last destination.
    for start in 0..copies.len() {
        if done[start] {
            continue;
        }

        // Follow the cycle chain starting at `start`.
        let mut chain: Vec<usize> = Vec::new();
        let mut cur = start;
        loop {
            chain.push(cur);
            done[cur] = true;
            // Find the next undone copy whose src is this copy's dst.
            let next_src = copies[cur].1; // dst of current = src of next
            let next = copies
                .iter()
                .enumerate()
                .find(|(j, (s, _, _))| !done[*j] && *s == next_src);
            match next {
                Some((j, _)) => cur = j,
                None => break, // back to start (cycle closed)
            }
        }

        let ty = copies[start].2;

        // Collect every location that participates in this cycle (sources & dsts).
        let cycle_locs: std::collections::HashSet<Location> = chain
            .iter()
            .flat_map(|&k| [copies[k].0, copies[k].1])
            .collect();

        // Pick a scratch register not in the cycle.
        let scratch: String = match class_of(ty) {
            RegClass::Float => "%xmm15".to_string(),
            RegClass::Int => {
                use crate::x86_64::regs::INT_REGS;
                let r = INT_REGS
                    .iter()
                    .find(|&&r| !cycle_locs.contains(&Location::Reg(r)))
                    .copied();
                match r {
                    Some(r) => fmt_loc(Location::Reg(r), ty),
                    // All registers are in the cycle — fall back to stack.
                    None => {
                        let (first_src, first_dst, _) = copies[chain[0]];
                        writeln!(out, "    pushq {}", fmt_loc(*first_src, Ty::I64)).unwrap();
                        for k in 1..chain.len() {
                            let (s, d, t) = copies[chain[k]];
                            emit_mov(*t, &fmt_loc(*s, *t), &fmt_loc(*d, *t), out);
                        }
                        writeln!(out, "    popq {}", fmt_loc(*first_dst, Ty::I64)).unwrap();
                        continue;
                    }
                }
            }
        };

        // Standard cycle break: save chain[0].src → scratch, walk chain, scratch → chain[0].dst.
        let (first_src, first_dst, first_ty) = copies[chain[0]];
        emit_mov(*first_ty, &fmt_loc(*first_src, *first_ty), &scratch, out);
        for k in 1..chain.len() {
            let (s, d, t) = copies[chain[k]];
            emit_mov(*t, &fmt_loc(*s, *t), &fmt_loc(*d, *t), out);
        }
        emit_mov(*first_ty, &scratch, &fmt_loc(*first_dst, *first_ty), out);
    }
}

// ---------------------------------------------------------------------------
// Operand lookup helpers
// ---------------------------------------------------------------------------

fn loc(v: Value, alloc: &RegAlloc) -> Location {
    *alloc
        .assignment
        .get(&v)
        .unwrap_or_else(|| panic!("value %{} has no allocation", v.as_u32()))
}

fn loc_str(v: Value, alloc: &RegAlloc, prog: &Program) -> String {
    let ty = prog.values[v].inst.ty();
    fmt_loc(loc(v, alloc), ty)
}

// ---------------------------------------------------------------------------
// Per-instruction emission
// ---------------------------------------------------------------------------

fn emit_inst(
    val: Value,
    prog: &Program,
    alloc: &RegAlloc,
    liveness: &Liveness,
    block: Block,
    out: &mut String,
    interner: &IdentInterner,
) {
    let inst = prog.values[val].inst.clone();
    let ty = inst.ty();
    if is_side_effect_free(&inst) && !value_has_effective_uses(val, prog) {
        return;
    }

    match inst {
        Inst::Noop | Inst::Upsilon { .. } => {
            // Upsilons are handled when emitting the terminator / block boundary.
        }

        Inst::Phi(_) => {
            // Phi values are placed by upsilon copies; no code emitted here.
        }

        Inst::Const { ty, value } => {
            let dst = fmt_loc(loc(val, alloc), ty);
            match ty {
                Ty::F32 => {
                    // Load float constant from a literal pool via %rip-relative address.
                    // We emit it inline; the linker/assembler will place it.
                    let bits = value as u32;
                    writeln!(out, "    movl $0x{:08x}, %eax", bits).unwrap();
                    writeln!(out, "    movd %eax, {}", dst).unwrap();
                }
                Ty::F64 => {
                    let bits = value;
                    writeln!(out, "    movabsq $0x{:016x}, %rax", bits).unwrap();
                    writeln!(out, "    movq %rax, {}", dst).unwrap();
                }
                _ => {
                    let suf = int_suffix(ty);
                    writeln!(out, "    mov{} ${}, {}", suf, value as i64, dst).unwrap();
                }
            }
        }

        Inst::ConstNull(_) => match loc(val, alloc) {
            Location::Reg(r) => {
                writeln!(out, "    xorq {}, {}", r.name64(), r.name64()).unwrap();
            }
            Location::Stack(off) => {
                writeln!(out, "    movq $0, {}(%rbp)", off).unwrap();
            }
        },

        Inst::Param { ty, index } => {
            let dst = loc(val, alloc);
            let src = match class_of(ty) {
                RegClass::Int => INT_ARG_REGS.get(index as usize).copied(),
                RegClass::Float => FLOAT_ARG_REGS.get(index as usize).copied(),
            };
            if let Some(src_reg) = src {
                let src_loc = Location::Reg(src_reg);
                if src_loc != dst {
                    emit_mov(ty, &fmt_loc(src_loc, ty), &fmt_loc(dst, ty), out);
                }
            } else {
                let stack_index = match class_of(ty) {
                    RegClass::Int => index.saturating_sub(INT_ARG_REGS.len() as u32),
                    RegClass::Float => index.saturating_sub(FLOAT_ARG_REGS.len() as u32),
                };
                let src = format!("{}(%rbp)", 16 + stack_index as i32 * 8);
                emit_mov(ty, &src, &fmt_loc(dst, ty), out);
            }
        }

        Inst::GlobalAddr(global) => {
            let name = interner.lookup(prog.globals[global].name);
            let dst = fmt_loc(loc(val, alloc), ty);
            writeln!(out, "    leaq {}(%rip), {}", name, dst).unwrap();
        }

        Inst::Alloca { ty: alloca_ty, .. } => {
            // Non-promoted allocas: the value holds the address of the slot.
            // Assign a stack slot for the pointed-to object.
            // For simplicity, treat the alloca's Location::Stack as its base.
            let base = loc(val, alloc);
            let dst = fmt_loc(base, Ty::Ptr);
            match base {
                Location::Stack(off) => {
                    // The alloca itself lives at off(%rbp); the VALUE is that address.
                    // We need to return the *address*, not the slot contents.
                    // Re-allocate an extra slot for the pointed-to data and return its address.
                    // This is a simplified approach; full support would need the alloca size.
                    let _ = alloca_ty;
                    writeln!(out, "    leaq {}(%rbp), {}", off, dst).unwrap();
                }
                Location::Reg(r) => {
                    // If the alloca result ended up in a register, fill it with the stack address.
                    // (Shouldn't happen for non-promotable allocas, but handle gracefully.)
                    writeln!(out, "    leaq -8(%rsp), {}", r.name64()).unwrap();
                }
            }
        }

        Inst::Load {
            ty: load_ty, ptr, ..
        } => {
            let src_reg = match loc(ptr, alloc) {
                Location::Reg(r) => r.name64().to_string(),
                Location::Stack(off) => {
                    writeln!(out, "    movq {}(%rbp), %rax", off).unwrap();
                    "%rax".to_string()
                }
            };
            let suf = mov_suffix(load_ty);
            // If the result is on the stack we cannot emit a mem-to-mem load.
            // Load into a scratch register then move to the stack slot.
            match loc(val, alloc) {
                Location::Reg(r) => {
                    writeln!(
                        out,
                        "    mov{} ({}), {}",
                        suf,
                        src_reg,
                        fmt_loc(Location::Reg(r), load_ty)
                    )
                    .unwrap();
                }
                Location::Stack(off) => {
                    let scratch = match class_of(load_ty) {
                        RegClass::Float => "%xmm15",
                        RegClass::Int => PReg::R10.name_for_ty(load_ty),
                    };
                    writeln!(out, "    mov{} ({}), {}", suf, src_reg, scratch).unwrap();
                    writeln!(out, "    mov{} {}, {}(%rbp)", suf, scratch, off).unwrap();
                }
            }
        }

        Inst::Store { ptr, value, .. } => {
            let val_ty = prog.values[value].inst.ty();
            let ptr_preg = match loc(ptr, alloc) {
                Location::Reg(r) => r,
                Location::Stack(off) => {
                    writeln!(out, "    movq {}(%rbp), %r11", off).unwrap();
                    PReg::R11
                }
            };
            let suf = mov_suffix(val_ty);
            // If the value is on the stack we cannot emit a mem-to-mem store.
            // Load the value into a scratch register first.
            match loc(value, alloc) {
                Location::Reg(r) => {
                    writeln!(
                        out,
                        "    mov{} {}, ({})",
                        suf,
                        fmt_loc(Location::Reg(r), val_ty),
                        ptr_preg.name64()
                    )
                    .unwrap();
                }
                Location::Stack(off) => {
                    let scratch = match class_of(val_ty) {
                        RegClass::Float => "%xmm15",
                        RegClass::Int => {
                            if ptr_preg == PReg::R10 {
                                PReg::R11.name_for_ty(val_ty)
                            } else {
                                PReg::R10.name_for_ty(val_ty)
                            }
                        }
                    };
                    writeln!(out, "    mov{} {}(%rbp), {}", suf, off, scratch).unwrap();
                    writeln!(out, "    mov{} {}, ({})", suf, scratch, ptr_preg.name64()).unwrap();
                }
            }
        }

        Inst::GetElementPtr { ptr, indices, .. } => {
            let ptr_loc = match loc(ptr, alloc) {
                Location::Reg(r) => r.name64().to_string(),
                Location::Stack(off) => {
                    writeln!(out, "    movq {}(%rbp), %rax", off).unwrap();
                    "%rax".to_string()
                }
            };
            let dst = fmt_loc(loc(val, alloc), Ty::Ptr);
            writeln!(out, "    movq {}, {}", ptr_loc, dst).unwrap();
            // Single index: byte offset (simplified; real GEP needs type sizes).
            if let Some(&idx) = indices.first() {
                let idx_loc = loc_str(idx, alloc, prog);
                writeln!(out, "    addq {}, {}", idx_loc, dst).unwrap();
            }
        }

        Inst::Unary { ty, op, operand } => {
            let src = loc_str(operand, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), ty);
            emit_mov(ty, &src, &dst, out);
            match op {
                UnaryOp::Not => {
                    writeln!(out, "    not{} {}", int_suffix(ty), dst).unwrap();
                }
                UnaryOp::Neg => {
                    writeln!(out, "    neg{} {}", int_suffix(ty), dst).unwrap();
                }
                UnaryOp::FNeg => {
                    // Flip sign bit by XOR with the sign-bit mask.
                    match ty {
                        Ty::F32 => {
                            writeln!(out, "    movl $0x80000000, %eax").unwrap();
                            writeln!(out, "    movd %eax, %xmm15").unwrap();
                            writeln!(out, "    xorps %xmm15, {}", dst).unwrap();
                        }
                        Ty::F64 => {
                            writeln!(out, "    movabsq $0x8000000000000000, %rax").unwrap();
                            writeln!(out, "    movq %rax, %xmm15").unwrap();
                            writeln!(out, "    xorpd %xmm15, {}", dst).unwrap();
                        }
                        _ => {}
                    }
                }
            }
        }

        Inst::Binary { ty, lhs, rhs, op } => {
            let lhs_ty = prog.values[lhs].inst.ty();
            let rhs_ty = prog.values[rhs].inst.ty();

            // Load operands. If an operand is a constant, we may need to load it into a register
            // if the instruction doesn't support immediates (like 64-bit imul, or most two-address ops).
            let mut lhs_loc = loc_str(lhs, alloc, prog);
            let mut rhs_loc = loc_str(rhs, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), ty);

            // Special handling for large immediates or instructions requiring registers.
            if let Inst::Const { value, .. } = prog.values[rhs].inst {
                if value > 0x7FFFFFFF {
                    let scratch = PReg::R11.name64();
                    writeln!(out, "    movabsq ${}, {}", value, scratch).unwrap();
                    rhs_loc = PReg::R11.name_for_ty(rhs_ty).to_string();
                }
            }
            if let Inst::Const { value, .. } = prog.values[lhs].inst {
                if value > 0x7FFFFFFF {
                    let scratch = PReg::R10.name64();
                    writeln!(out, "    movabsq ${}, {}", value, scratch).unwrap();
                    lhs_loc = PReg::R10.name_for_ty(lhs_ty).to_string();
                }
            }

            // Shift count must be in %rcx; always use the 64-bit name for the movq.
            let rhs_loc64 = match loc(rhs, alloc) {
                Location::Reg(r) => r.name64().to_string(),
                Location::Stack(off) => format!("{}(%rbp)", off),
            };

            match (class_of(ty), op) {
                // For simple two-address ops: copy lhs into dst, then apply op(rhs, dst).
                (RegClass::Int, BinaryOp::Add) => {
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    add{} {}, {}", int_suffix(ty), rhs_loc, dst).unwrap();
                }
                (RegClass::Int, BinaryOp::Sub) => {
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    sub{} {}, {}", int_suffix(ty), rhs_loc, dst).unwrap();
                }
                (RegClass::Int, BinaryOp::Mul) => {
                    if ty == Ty::I8 {
                        // 8-bit imul is imulb <reg8/mem8>, implicitly dst = %al * src.
                        // Our IR says dst = lhs * rhs.
                        // We need to move lhs to %al, imulb rhs, then move %al to dst.
                        writeln!(out, "    movb {}, %al", lhs_loc).unwrap();
                        writeln!(out, "    imulb {}", rhs_loc).unwrap();
                        writeln!(out, "    movb %al, {}", dst).unwrap();
                    } else if matches!(loc(val, alloc), Location::Stack(_)) {
                        let scratch = if rhs_loc.contains("%r10") {
                            PReg::R11
                        } else {
                            PReg::R10
                        };
                        let scratch = scratch.name_for_ty(ty);
                        emit_mov(ty, &lhs_loc, scratch, out);
                        writeln!(out, "    imul{} {}, {}", int_suffix(ty), rhs_loc, scratch)
                            .unwrap();
                        emit_mov(ty, scratch, &dst, out);
                    } else {
                        emit_mov(ty, &lhs_loc, &dst, out);
                        writeln!(out, "    imul{} {}, {}", int_suffix(ty), rhs_loc, dst).unwrap();
                    }
                }
                (RegClass::Int, BinaryOp::And) => {
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    and{} {}, {}", int_suffix(ty), rhs_loc, dst).unwrap();
                }
                (RegClass::Int, BinaryOp::Or) => {
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    or{} {}, {}", int_suffix(ty), rhs_loc, dst).unwrap();
                }
                (RegClass::Int, BinaryOp::Xor) => {
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    xor{} {}, {}", int_suffix(ty), rhs_loc, dst).unwrap();
                }
                // Shifts: move lhs to dst, save count to %rcx, then shift.
                // If lhs is in %rcx, move lhs to dst FIRST so the count save
                // does not clobber it. The allocator ensures dst ≠ %rcx.
                // Use the 64-bit rhs name for the movq into %rcx.
                (RegClass::Int, BinaryOp::Shl) => {
                    let lhs_in_rcx = matches!(loc(lhs, alloc), Location::Reg(PReg::Rcx));
                    if lhs_in_rcx {
                        emit_mov(ty, &lhs_loc, &dst, out);
                    }
                    if let Inst::Const { value, .. } = prog.values[rhs].inst {
                        if !lhs_in_rcx {
                            emit_mov(ty, &lhs_loc, &dst, out);
                        }
                        writeln!(out, "    sal{} ${}, {}", int_suffix(ty), value, dst).unwrap();
                    } else {
                        writeln!(out, "    movq {}, %rcx", rhs_loc64).unwrap();
                        if !lhs_in_rcx {
                            emit_mov(ty, &lhs_loc, &dst, out);
                        }
                        writeln!(out, "    sal{} %cl, {}", int_suffix(ty), dst).unwrap();
                    }
                }
                (RegClass::Int, BinaryOp::Shr) => {
                    let lhs_in_rcx = matches!(loc(lhs, alloc), Location::Reg(PReg::Rcx));
                    if lhs_in_rcx {
                        emit_mov(ty, &lhs_loc, &dst, out);
                    }
                    if let Inst::Const { value, .. } = prog.values[rhs].inst {
                        if !lhs_in_rcx {
                            emit_mov(ty, &lhs_loc, &dst, out);
                        }
                        writeln!(out, "    shr{} ${}, {}", int_suffix(ty), value, dst).unwrap();
                    } else {
                        writeln!(out, "    movq {}, %rcx", rhs_loc64).unwrap();
                        if !lhs_in_rcx {
                            emit_mov(ty, &lhs_loc, &dst, out);
                        }
                        writeln!(out, "    shr{} %cl, {}", int_suffix(ty), dst).unwrap();
                    }
                }
                (RegClass::Int, BinaryOp::AShr) => {
                    let lhs_in_rcx = matches!(loc(lhs, alloc), Location::Reg(PReg::Rcx));
                    if lhs_in_rcx {
                        emit_mov(ty, &lhs_loc, &dst, out);
                    }
                    if let Inst::Const { value, .. } = prog.values[rhs].inst {
                        if !lhs_in_rcx {
                            emit_mov(ty, &lhs_loc, &dst, out);
                        }
                        writeln!(out, "    sar{} ${}, {}", int_suffix(ty), value, dst).unwrap();
                    } else {
                        writeln!(out, "    movq {}, %rcx", rhs_loc64).unwrap();
                        if !lhs_in_rcx {
                            emit_mov(ty, &lhs_loc, &dst, out);
                        }
                        writeln!(out, "    sar{} %cl, {}", int_suffix(ty), dst).unwrap();
                    }
                }
                // Div/rem: entirely self-contained (emit_idiv/emit_udiv handle all moves).
                (RegClass::Int, BinaryOp::SDiv) => {
                    emit_idiv(ty, &lhs_loc, &rhs_loc, &dst, false, out);
                }
                (RegClass::Int, BinaryOp::UDiv) => {
                    emit_udiv(ty, &lhs_loc, &rhs_loc, &dst, false, out);
                }
                (RegClass::Int, BinaryOp::SRem) => {
                    emit_idiv(ty, &lhs_loc, &rhs_loc, &dst, true, out);
                }
                (RegClass::Int, BinaryOp::URem) => {
                    emit_udiv(ty, &lhs_loc, &rhs_loc, &dst, true, out);
                }
                (RegClass::Float, BinaryOp::FAdd) => {
                    let suf = if ty == Ty::F32 { "ss" } else { "sd" };
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    add{} {}, {}", suf, rhs_loc, dst).unwrap();
                }
                (RegClass::Float, BinaryOp::FSub) => {
                    let suf = if ty == Ty::F32 { "ss" } else { "sd" };
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    sub{} {}, {}", suf, rhs_loc, dst).unwrap();
                }
                (RegClass::Float, BinaryOp::FMul) => {
                    let suf = if ty == Ty::F32 { "ss" } else { "sd" };
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    mul{} {}, {}", suf, rhs_loc, dst).unwrap();
                }
                (RegClass::Float, BinaryOp::FDiv) => {
                    let suf = if ty == Ty::F32 { "ss" } else { "sd" };
                    emit_mov(ty, &lhs_loc, &dst, out);
                    writeln!(out, "    div{} {}, {}", suf, rhs_loc, dst).unwrap();
                }
                (RegClass::Float, BinaryOp::FRem) => {
                    // x86-64 SSE has no frem; fall back to a comment.
                    writeln!(out, "    # frem not directly supported in SSE").unwrap();
                }
                _ => {
                    writeln!(out, "    # unimplemented binary op {:?}", op).unwrap();
                }
            }
        }

        Inst::ICmp { lhs, rhs, pred } => {
            let lhs_ty = prog.values[lhs].inst.ty();
            let rhs_ty = prog.values[rhs].inst.ty();
            let cmp_ty = larger_int_ty(lhs_ty, rhs_ty);
            let lhs_loc = fmt_loc(loc(lhs, alloc), cmp_ty);
            let rhs_loc = fmt_loc(loc(rhs, alloc), cmp_ty);

            // If both are stack, we must move one to a scratch register.
            if matches!(loc(lhs, alloc), Location::Stack(_))
                && matches!(loc(rhs, alloc), Location::Stack(_))
            {
                let scratch_sized = PReg::R11.name_for_ty(cmp_ty).to_string();
                writeln!(
                    out,
                    "    mov{} {}, {}",
                    mov_suffix(cmp_ty),
                    lhs_loc,
                    scratch_sized
                )
                .unwrap();
                writeln!(
                    out,
                    "    cmp{} {}, {}",
                    int_suffix(cmp_ty),
                    rhs_loc,
                    scratch_sized
                )
                .unwrap();
            } else {
                writeln!(
                    out,
                    "    cmp{} {}, {}",
                    int_suffix(cmp_ty),
                    rhs_loc,
                    lhs_loc
                )
                .unwrap();
            }

            match loc(val, alloc) {
                Location::Reg(r) => {
                    writeln!(out, "    set{} {}", setcc(pred), r.name8()).unwrap();
                    writeln!(out, "    movzbq {}, {}", r.name8(), r.name64()).unwrap();
                }
                Location::Stack(off) => {
                    writeln!(out, "    set{} %r11b", setcc(pred)).unwrap();
                    writeln!(out, "    movzbq %r11b, %r11").unwrap();
                    writeln!(out, "    movq %r11, {}(%rbp)", off).unwrap();
                }
            }
        }

        Inst::FCmp { lhs, rhs, pred } => {
            let lhs_ty = prog.values[lhs].inst.ty();
            let lhs_loc = loc_str(lhs, alloc, prog);
            let rhs_loc = loc_str(rhs, alloc, prog);
            let suf = if lhs_ty == Ty::F32 { "ss" } else { "sd" };

            writeln!(out, "    ucomi{} {}, {}", suf, rhs_loc, lhs_loc).unwrap();

            let dst8 = match loc(val, alloc) {
                Location::Reg(r) => r.name8().to_string(),
                Location::Stack(_) => "%r11b".to_string(),
            };
            use crate::ir::inst::FCmpOp;
            match pred {
                FCmpOp::Oeq | FCmpOp::Ueq => writeln!(out, "    sete {}", dst8).unwrap(),
                FCmpOp::One | FCmpOp::Une => writeln!(out, "    setne {}", dst8).unwrap(),
                FCmpOp::Olt | FCmpOp::Ult => writeln!(out, "    setb {}", dst8).unwrap(),
                FCmpOp::Ole | FCmpOp::Ule => writeln!(out, "    setbe {}", dst8).unwrap(),
                FCmpOp::Ogt | FCmpOp::Ugt => writeln!(out, "    seta {}", dst8).unwrap(),
                FCmpOp::Oge | FCmpOp::Uge => writeln!(out, "    setae {}", dst8).unwrap(),
                FCmpOp::True => writeln!(out, "    movb $1, {}", dst8).unwrap(),
                FCmpOp::False => writeln!(out, "    movb $0, {}", dst8).unwrap(),
                _ => writeln!(out, "    movb $0, {}", dst8).unwrap(),
            }
            match loc(val, alloc) {
                Location::Reg(r) => {
                    writeln!(out, "    movzbq {}, {}", r.name8(), r.name64()).unwrap();
                }
                Location::Stack(off) => {
                    writeln!(out, "    movzbq %r11b, %r11").unwrap();
                    writeln!(out, "    movq %r11, {}(%rbp)", off).unwrap();
                }
            }
        }

        Inst::ZExt { to, value } => {
            let from_ty = prog.values[value].inst.ty();
            let src = loc_str(value, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), to);
            // movzbq/movzwq/movzlq always write to a 64-bit destination register.
            let dst64 = match loc(val, alloc) {
                Location::Reg(r) => r.name64().to_string(),
                Location::Stack(off) => format!("{}(%rbp)", off),
            };
            match (from_ty, to) {
                (Ty::I1 | Ty::I8, Ty::I32) => {
                    // For I8 -> I32, we can use movzbl.
                    // If dst is stack, we might need a scratch reg, but movzbl to memory is unsupported.
                    // However, movzbl src, reg32 is fine.
                    match loc(val, alloc) {
                        Location::Reg(r) => {
                            writeln!(out, "    movzbl {}, {}", src, r.name32()).unwrap();
                        }
                        Location::Stack(_) => {
                            writeln!(out, "    movzbl {}, %r11d", src).unwrap();
                            writeln!(out, "    movl %r11d, {}", dst).unwrap();
                        }
                    }
                }
                (Ty::I1 | Ty::I8, _) => {
                    writeln!(out, "    movzbq {}, {}", src, dst64).unwrap();
                }
                (Ty::I16, Ty::I32) => match loc(val, alloc) {
                    Location::Reg(r) => {
                        writeln!(out, "    movzwl {}, {}", src, r.name32()).unwrap();
                    }
                    Location::Stack(_) => {
                        writeln!(out, "    movzwl {}, %r11d", src).unwrap();
                        writeln!(out, "    movl %r11d, {}", dst).unwrap();
                    }
                },
                (Ty::I16, _) => {
                    writeln!(out, "    movzwq {}, {}", src, dst64).unwrap();
                }
                (Ty::I32, Ty::I64 | Ty::Ptr) => {
                    // movl to a 32-bit register zero-extends to 64-bit.
                    match loc(val, alloc) {
                        Location::Reg(r) => {
                            writeln!(out, "    movl {}, {}", src, r.name32()).unwrap();
                        }
                        Location::Stack(_) => {
                            writeln!(out, "    movl {}, %r11d", src).unwrap();
                            writeln!(out, "    movl %r11d, {}", dst).unwrap();
                            // This only writes 32 bits. We need to zero out the top 32 bits if on stack.
                            // But usually I32 is already zero-extended if in register.
                            // If it's going to stack as I64, we need to be careful.
                            writeln!(
                                out,
                                "    movl $0, {}(%rbp)",
                                match loc(val, alloc) {
                                    Location::Stack(off) => off + 4,
                                    _ => unreachable!(),
                                }
                            )
                            .unwrap();
                        }
                    }
                }
                _ => emit_mov(to, &src, &dst, out),
            }
        }

        Inst::SExt { to, value } => {
            let from_ty = prog.values[value].inst.ty();
            let src = loc_str(value, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), to);
            // movsbq/movswq/movslq always write to a 64-bit destination register.
            let dst64 = match loc(val, alloc) {
                Location::Reg(r) => r.name64().to_string(),
                Location::Stack(off) => format!("{}(%rbp)", off),
            };
            match (from_ty, to) {
                (Ty::I8, Ty::I32) => match loc(val, alloc) {
                    Location::Reg(r) => {
                        writeln!(out, "    movsbl {}, {}", src, r.name32()).unwrap()
                    }
                    Location::Stack(_) => {
                        writeln!(out, "    movsbl {}, %r11d", src).unwrap();
                        writeln!(out, "    movl %r11d, {}", dst).unwrap();
                    }
                },
                (Ty::I8, _) => writeln!(out, "    movsbq {}, {}", src, dst64).unwrap(),
                (Ty::I16, Ty::I32) => match loc(val, alloc) {
                    Location::Reg(r) => {
                        writeln!(out, "    movswl {}, {}", src, r.name32()).unwrap()
                    }
                    Location::Stack(_) => {
                        writeln!(out, "    movswl {}, %r11d", src).unwrap();
                        writeln!(out, "    movl %r11d, {}", dst).unwrap();
                    }
                },
                (Ty::I16, _) => writeln!(out, "    movswq {}, {}", src, dst64).unwrap(),
                (Ty::I32, Ty::I64 | Ty::Ptr) => {
                    writeln!(out, "    movslq {}, {}", src, dst64).unwrap()
                }
                _ => emit_mov(to, &src, &dst, out),
            }
        }

        Inst::Trunc { to, value } => {
            let src = loc(value, alloc);
            let dst = loc(val, alloc);

            // To truncate, we move the lower 'to'-sized part of src to dst.
            let src_str = fmt_loc(src, to);
            let dst_str = fmt_loc(dst, to);

            if src_str != dst_str {
                emit_mov(to, &src_str, &dst_str, out);
            }
        }

        Inst::FTrunc { to: _, value } => {
            let src = loc_str(value, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), Ty::F32);
            writeln!(out, "    cvtsd2ss {}, {}", src, dst).unwrap();
        }

        Inst::FExt { to: _, value } => {
            let src = loc_str(value, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), Ty::F64);
            writeln!(out, "    cvtss2sd {}, {}", src, dst).unwrap();
        }

        Inst::SiToFp { to, value } => {
            let from_ty = prog.values[value].inst.ty();
            let src = loc_str(value, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), to);
            let float_suf = if to == Ty::F32 { "ss" } else { "sd" };
            let int_suf = if from_ty == Ty::I32 { "l" } else { "q" };
            writeln!(out, "    cvtsi2{}{} {}, {}", float_suf, int_suf, src, dst).unwrap();
        }

        Inst::UiToFp { to, value } => {
            // x86-64 has no unsigned-int-to-float for 64-bit; use signed cvtsi2 which
            // is correct for values that fit in the signed range (common case).
            let from_ty = prog.values[value].inst.ty();
            let src = loc_str(value, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), to);
            let float_suf = if to == Ty::F32 { "ss" } else { "sd" };
            let int_suf = if from_ty == Ty::I32 { "l" } else { "q" };
            writeln!(out, "    cvtsi2{}{} {}, {}", float_suf, int_suf, src, dst).unwrap();
        }

        Inst::FpToSi { to, value } => {
            // cvttss2si / cvttsd2si: truncate scalar float to signed integer.
            let src = loc_str(value, alloc, prog);
            let from_ty = prog.values[value].inst.ty();
            let dst = fmt_loc(loc(val, alloc), to);
            let float_suf = if from_ty == Ty::F32 { "ss" } else { "sd" };
            let int_suf = if to == Ty::I32 { "l" } else { "q" };
            writeln!(out, "    cvtt{}2si{} {}, {}", float_suf, int_suf, src, dst).unwrap();
        }

        Inst::FpToUi { to, value } => {
            // No direct unsigned float-to-int in SSE; use signed truncation (correct
            // for values in the representable unsigned range that also fit in i64/i32).
            let src = loc_str(value, alloc, prog);
            let from_ty = prog.values[value].inst.ty();
            let dst = fmt_loc(loc(val, alloc), to);
            let float_suf = if from_ty == Ty::F32 { "ss" } else { "sd" };
            let int_suf = if to == Ty::I32 { "l" } else { "q" };
            writeln!(out, "    cvtt{}2si{} {}, {}", float_suf, int_suf, src, dst).unwrap();
        }

        Inst::Bitcast { to, value }
        | Inst::IntToPtr { to, value }
        | Inst::PtrToInt { to, value } => {
            let src = loc_str(value, alloc, prog);
            let dst = fmt_loc(loc(val, alloc), to);
            emit_mov(to, &src, &dst, out);
        }

        Inst::Select {
            ty: _,
            cond,
            then_val,
            else_val,
        } => {
            // Load condition into %r11 so %rax/%r10 are free as scratch registers.
            let cond_src = match loc(cond, alloc) {
                Location::Reg(r) => r.name64().to_string(),
                Location::Stack(off) => format!("{}(%rbp)", off),
            };
            writeln!(out, "    movq {}, %r11", cond_src).unwrap();
            writeln!(out, "    testq %r11, %r11").unwrap();

            let dst = fmt_loc(loc(val, alloc), ty);

            match class_of(ty) {
                RegClass::Int => {
                    // Move else/then as 64-bit into scratch GP registers, then cmovneq.
                    // Stack slots are always 8 bytes wide so a 64-bit read is safe even
                    // for narrow types; only the low bits matter at the write-back.
                    let else_64 = match loc(else_val, alloc) {
                        Location::Reg(r) => r.name64().to_string(),
                        Location::Stack(off) => format!("{}(%rbp)", off),
                    };
                    let then_64 = match loc(then_val, alloc) {
                        Location::Reg(r) => r.name64().to_string(),
                        Location::Stack(off) => format!("{}(%rbp)", off),
                    };
                    writeln!(out, "    movq {}, %rax", else_64).unwrap();
                    writeln!(out, "    movq {}, %r10", then_64).unwrap();
                    writeln!(out, "    cmovneq %r10, %rax").unwrap();
                    emit_mov(ty, PReg::Rax.name_for_ty(ty), &dst, out);
                }
                RegClass::Float => {
                    // Move float bit-patterns through GP registers to use cmovne, then
                    // write back to the float destination. This avoids branches and
                    // works for both register and memory float locations.
                    if ty == Ty::F32 {
                        let else_fp = fmt_loc(loc(else_val, alloc), Ty::F32);
                        let then_fp = fmt_loc(loc(then_val, alloc), Ty::F32);
                        // Read 32-bit float bit patterns into GP regs.
                        match loc(else_val, alloc) {
                            Location::Reg(r) => {
                                writeln!(out, "    movd {}, %eax", r.name64()).unwrap()
                            }
                            Location::Stack(off) => {
                                writeln!(out, "    movl {}(%rbp), %eax", off).unwrap()
                            }
                        }
                        match loc(then_val, alloc) {
                            Location::Reg(r) => {
                                writeln!(out, "    movd {}, %r10d", r.name64()).unwrap()
                            }
                            Location::Stack(off) => {
                                writeln!(out, "    movl {}(%rbp), %r10d", off).unwrap()
                            }
                        }
                        writeln!(out, "    cmovnel %r10d, %eax").unwrap();
                        match loc(val, alloc) {
                            Location::Reg(r) => {
                                writeln!(out, "    movd %eax, {}", r.name64()).unwrap()
                            }
                            Location::Stack(off) => {
                                writeln!(out, "    movl %eax, {}(%rbp)", off).unwrap()
                            }
                        }
                        let _ = (else_fp, then_fp);
                    } else {
                        // F64: same trick with 64-bit GP regs.
                        match loc(else_val, alloc) {
                            Location::Reg(r) => {
                                writeln!(out, "    movq {}, %rax", r.name64()).unwrap()
                            }
                            Location::Stack(off) => {
                                writeln!(out, "    movq {}(%rbp), %rax", off).unwrap()
                            }
                        }
                        match loc(then_val, alloc) {
                            Location::Reg(r) => {
                                writeln!(out, "    movq {}, %r10", r.name64()).unwrap()
                            }
                            Location::Stack(off) => {
                                writeln!(out, "    movq {}(%rbp), %r10", off).unwrap()
                            }
                        }
                        writeln!(out, "    cmovneq %r10, %rax").unwrap();
                        match loc(val, alloc) {
                            Location::Reg(r) => {
                                writeln!(out, "    movq %rax, {}", r.name64()).unwrap()
                            }
                            Location::Stack(off) => {
                                writeln!(out, "    movq %rax, {}(%rbp)", off).unwrap()
                            }
                        }
                    }
                }
            }
        }

        Inst::Call {
            ty: ret_ty,
            func,
            args,
        } => {
            let func_name = interner.lookup(prog.functions[func].name);
            emit_call(
                val, &args, func_name, ret_ty, prog, alloc, liveness, block, out,
            );
        }

        Inst::IndirectCall {
            ty: ret_ty,
            ptr,
            args,
        } => {
            let ptr_loc = loc_str(ptr, alloc, prog);
            writeln!(out, "    movq {}, %rax", ptr_loc).unwrap();
            emit_call(
                val, &args, "*%rax", ret_ty, prog, alloc, liveness, block, out,
            );
        }
    }
}

/// Emit a signed integer div or mod.
///
/// idiv uses %rax as dividend and clobbers %rdx (sign extension via cltd/cqto).
/// The divisor can be any register or memory operand that is not %rax or %rdx.
/// When rhs is already in such a safe location we use it directly, avoiding
/// any unnecessary clobber of live values in %rcx.
fn emit_idiv(ty: Ty, lhs: &str, rhs: &str, dst: &str, want_rem: bool, out: &mut String) {
    let (eax, edx, ecx, xchg, mov, ext, div) = if ty == Ty::I32 {
        ("%eax", "%edx", "%ecx", "xchgl", "movl", "cltd", "idivl")
    } else {
        ("%rax", "%rdx", "%rcx", "xchgq", "movq", "cqto", "idivq")
    };
    let result = if want_rem { edx } else { eax };

    // Choose the divisor operand. idiv clobbers %rax and %rdx (via cltd), so
    // rhs must not be in either of those after we load lhs into %rax.
    let div_op: String = if rhs != eax && rhs != edx {
        // rhs is safe to use directly (register or memory not clobbered by cltd).
        if lhs != eax {
            writeln!(out, "    {mov} {lhs}, {eax}").unwrap();
        }
        rhs.to_string()
    } else if rhs == eax && lhs == ecx {
        // Both rhs in %eax and lhs in %ecx: xchg to swap them.
        writeln!(out, "    {xchg} {eax}, {ecx}").unwrap();
        // Now %eax = lhs, %ecx = rhs.
        ecx.to_string()
    } else {
        // rhs is in %eax or %edx (will be clobbered by cltd/idiv). Save to %ecx.
        if lhs == ecx {
            // lhs in %ecx would be overwritten; move lhs to %eax first.
            writeln!(out, "    {mov} {lhs}, {eax}").unwrap();
            writeln!(out, "    {mov} {rhs}, {ecx}").unwrap();
        } else {
            writeln!(out, "    {mov} {rhs}, {ecx}").unwrap();
            if lhs != eax {
                writeln!(out, "    {mov} {lhs}, {eax}").unwrap();
            }
        }
        ecx.to_string()
    };

    writeln!(out, "    {ext}").unwrap();
    writeln!(out, "    {div} {div_op}").unwrap();
    if result != dst {
        writeln!(out, "    {mov} {result}, {dst}").unwrap();
    }
}

/// Emit an unsigned integer div or mod.
///
/// Same strategy as emit_idiv but uses xorl/xorq to zero %rdx instead of cltd.
fn emit_udiv(ty: Ty, lhs: &str, rhs: &str, dst: &str, want_rem: bool, out: &mut String) {
    let (eax, edx, ecx, xchg, mov, div) = if ty == Ty::I32 {
        ("%eax", "%edx", "%ecx", "xchgl", "movl", "divl")
    } else {
        ("%rax", "%rdx", "%rcx", "xchgq", "movq", "divq")
    };
    let zero_edx = if ty == Ty::I32 {
        "xorl %edx, %edx"
    } else {
        "xorq %rdx, %rdx"
    };
    let result = if want_rem { edx } else { eax };

    let div_op: String = if rhs != eax && rhs != edx {
        if lhs != eax {
            writeln!(out, "    {mov} {lhs}, {eax}").unwrap();
        }
        rhs.to_string()
    } else if rhs == eax && lhs == ecx {
        writeln!(out, "    {xchg} {eax}, {ecx}").unwrap();
        ecx.to_string()
    } else {
        if lhs == ecx {
            writeln!(out, "    {mov} {lhs}, {eax}").unwrap();
            writeln!(out, "    {mov} {rhs}, {ecx}").unwrap();
        } else {
            writeln!(out, "    {mov} {rhs}, {ecx}").unwrap();
            if lhs != eax {
                writeln!(out, "    {mov} {lhs}, {eax}").unwrap();
            }
        }
        ecx.to_string()
    };

    writeln!(out, "    {zero_edx}").unwrap();
    writeln!(out, "    {div} {div_op}").unwrap();
    if result != dst {
        writeln!(out, "    {mov} {result}, {dst}").unwrap();
    }
}

/// Emit a function call, marshalling args per SysV AMD64.
#[allow(clippy::too_many_arguments)]
fn emit_call(
    result_val: Value,
    args: &[Value],
    target: &str,
    ret_ty: Ty,
    prog: &Program,
    alloc: &RegAlloc,
    _liveness: &Liveness,
    _block: Block,
    out: &mut String,
) {
    // Move arguments into ABI registers (simplified: no stack args).
    let mut int_arg_idx = 0usize;
    let mut float_arg_idx = 0usize;
    let mut copies = Vec::new();
    let mut stack_args = Vec::new();

    for &arg in args {
        let arg_ty = prog.values[arg].inst.ty();
        match class_of(arg_ty) {
            RegClass::Int => {
                if let Some(&r) = INT_ARG_REGS.get(int_arg_idx) {
                    copies.push((loc(arg, alloc), Location::Reg(r), arg_ty));
                } else {
                    stack_args.push((arg, arg_ty));
                }
                int_arg_idx += 1;
            }
            RegClass::Float => {
                if let Some(&r) = FLOAT_ARG_REGS.get(float_arg_idx) {
                    copies.push((loc(arg, alloc), Location::Reg(r), arg_ty));
                } else {
                    stack_args.push((arg, arg_ty));
                }
                float_arg_idx += 1;
            }
        }
    }

    let stack_padding = if stack_args.len() % 2 == 0 { 0 } else { 8 };
    if stack_padding != 0 {
        writeln!(out, "    subq $8, %rsp").unwrap();
    }
    let mut protected = HashSet::new();
    for (src, _, _) in &copies {
        if let Location::Reg(r) = src {
            protected.insert(*r);
        }
    }
    for &(arg, _) in &stack_args {
        if let Location::Reg(r) = loc(arg, alloc) {
            protected.insert(r);
        }
    }
    let scratch = [PReg::Rax, PReg::R10, PReg::R11]
        .into_iter()
        .find(|r| !protected.contains(r))
        .unwrap_or(PReg::Rax);
    for &(arg, arg_ty) in stack_args.iter().rev() {
        let src = loc_str(arg, alloc, prog);
        match arg_ty {
            Ty::I1 | Ty::I8 | Ty::I16 | Ty::I32 => match loc(arg, alloc) {
                Location::Reg(r) => {
                    writeln!(out, "    pushq {}", r.name64()).unwrap();
                }
                Location::Stack(_) => {
                    writeln!(
                        out,
                        "    mov{} {}, {}",
                        mov_suffix(arg_ty),
                        src,
                        scratch.name32()
                    )
                    .unwrap();
                    writeln!(out, "    pushq {}", scratch.name64()).unwrap();
                }
            },
            Ty::I64 | Ty::Ptr => {
                writeln!(out, "    pushq {}", src).unwrap();
            }
            Ty::F32 => {
                writeln!(out, "    subq $8, %rsp").unwrap();
                writeln!(out, "    movss {}, (%rsp)", src).unwrap();
            }
            Ty::F64 => {
                writeln!(out, "    subq $8, %rsp").unwrap();
                writeln!(out, "    movsd {}, (%rsp)", src).unwrap();
            }
            Ty::Void => {}
        }
    }
    emit_parallel_copies(&copies, out);

    writeln!(out, "    call {}", target).unwrap();

    let stack_bytes = stack_args.len() * 8 + stack_padding;
    if stack_bytes != 0 {
        writeln!(out, "    addq ${}, %rsp", stack_bytes).unwrap();
    }

    // Move return value from ABI register into the result location.
    if ret_ty != Ty::Void {
        let dst = fmt_loc(loc(result_val, alloc), ret_ty);
        let src = match class_of(ret_ty) {
            RegClass::Int => PReg::Rax.name_for_ty(ret_ty),
            RegClass::Float => PReg::Xmm0.name64(),
        };
        if dst != src {
            emit_mov(ret_ty, src, &dst, out);
        }
    }
}

// ---------------------------------------------------------------------------
// Upsilon / phi-copy collection
// ---------------------------------------------------------------------------

/// Collect the parallel copies that must execute before leaving `from_block`
/// along the edge to `to_block` (phi-upsilon semantics).
fn collect_phi_copies(
    from_block: Block,
    to_block: Block,
    prog: &Program,
    alloc: &RegAlloc,
) -> Vec<(Location, Location, Ty)> {
    let mut copies: Vec<(Location, Location, Ty)> = Vec::new();

    for &val in &prog.blocks[from_block].insts {
        if let Inst::Upsilon { phi, value } = prog.values[val].inst {
            // Only include upsilons whose phi lives in to_block.
            if prog.values[phi].block == to_block && value_has_effective_uses(phi, prog) {
                let src = loc(value, alloc);
                let dst = loc(phi, alloc);
                let ty = prog.values[value].inst.ty();
                copies.push((src, dst, ty));
            }
        }
    }

    copies
}

fn is_side_effect_free(inst: &Inst) -> bool {
    !matches!(
        inst,
        Inst::Store { .. } | Inst::Call { .. } | Inst::IndirectCall { .. } | Inst::Upsilon { .. }
    )
}

fn value_has_effective_uses(value: Value, prog: &Program) -> bool {
    let mut visiting = HashSet::new();
    value_has_effective_uses_inner(value, prog, &mut visiting)
}

fn value_has_effective_uses_inner(
    value: Value,
    prog: &Program,
    visiting: &mut HashSet<u32>,
) -> bool {
    if !visiting.insert(value.as_u32()) {
        return false;
    }

    let mut found = false;
    'blocks: for (_, block) in prog.blocks.iter() {
        for &inst in &block.insts {
            let inst_data = &prog.values[inst].inst;
            let used = match inst_data {
                Inst::Upsilon {
                    phi,
                    value: incoming,
                } if *incoming == value => value_has_effective_uses_inner(*phi, prog, visiting),
                _ => inst_uses(inst_data).contains(&value),
            };
            if used {
                found = true;
                break 'blocks;
            }
        }

        if term_uses(&block.term).contains(&value) {
            found = true;
            break;
        }
    }

    visiting.remove(&value.as_u32());
    found
}

// ---------------------------------------------------------------------------
// Block terminator emission
// ---------------------------------------------------------------------------

/// Restore callee-saved registers and tear down the stack frame before `ret`.
fn emit_epilogue(alloc: &RegAlloc, out: &mut String) {
    // Restore callee-saved GP registers.
    for (i, &r) in alloc.used_callee_saved.iter().enumerate() {
        let off = -((i as i32 + 1) * 8);
        writeln!(out, "    movq {}(%rbp), {}", off, r.name64()).unwrap();
    }
    // Restore callee-saved XMM registers (stored below the GP save area).
    let int_area = alloc.used_callee_saved.len() as i32 * 8;
    for (i, &r) in alloc.used_callee_saved_float.iter().enumerate() {
        let off = -(int_area + (i as i32 + 1) * 16);
        writeln!(out, "    movdqu {}(%rbp), {}", off, r.name64()).unwrap();
    }
    writeln!(out, "    leave").unwrap();
}

fn emit_term(
    block: Block,
    prog: &Program,
    alloc: &RegAlloc,
    func: Function,
    out: &mut String,
    interner: &IdentInterner,
) {
    let func_name = interner.lookup(prog.functions[func].name);

    match &prog.blocks[block].term.clone() {
        Terminator::Ret(None) => {
            emit_epilogue(alloc, out);
            writeln!(out, "    ret").unwrap();
        }
        Terminator::Ret(Some(v)) => {
            let ty = prog.values[*v].inst.ty();
            let src = loc_str(*v, alloc, prog);
            let ret_reg = match class_of(ty) {
                RegClass::Int => PReg::Rax.name_for_ty(ty),
                RegClass::Float => PReg::Xmm0.name64(),
            };
            if src != ret_reg {
                emit_mov(ty, &src, ret_reg, out);
            }
            emit_epilogue(alloc, out);
            writeln!(out, "    ret").unwrap();
        }
        Terminator::Br(dest) => {
            let copies = collect_phi_copies(block, *dest, prog, alloc);
            emit_parallel_copies(&copies, out);
            writeln!(out, "    jmp .L{}_{}", func_name, dest.as_u32()).unwrap();
        }
        Terminator::CondBr {
            cond,
            then_block,
            else_block,
        } => {
            // Use 64-bit test — booleans are always zero-extended after setcc+movzbq,
            // and 32-bit writes zero-extend on x86-64 too, so the full register is safe.
            let cond_reg64 = match loc(*cond, alloc) {
                Location::Reg(r) => r.name64().to_string(),
                Location::Stack(off) => {
                    writeln!(out, "    movq {}(%rbp), %rax", off).unwrap();
                    "%rax".to_string()
                }
            };
            writeln!(out, "    testq {}, {}", cond_reg64, cond_reg64).unwrap();

            // Emit phi copies for both edges before the branch.
            // Because they're on separate edges we can just emit them in sequence.
            let then_copies = collect_phi_copies(block, *then_block, prog, alloc);
            let else_copies = collect_phi_copies(block, *else_block, prog, alloc);

            if then_copies.is_empty() && else_copies.is_empty() {
                writeln!(out, "    jne .L{}_{}", func_name, then_block.as_u32()).unwrap();
                writeln!(out, "    jmp .L{}_{}", func_name, else_block.as_u32()).unwrap();
            } else {
                // Emit a local jump to copy-then-jump stubs.
                writeln!(
                    out,
                    "    jne .L{}_{}_{}_then",
                    func_name,
                    block.as_u32(),
                    then_block.as_u32()
                )
                .unwrap();
                // Fall through to else copies.
                emit_parallel_copies(&else_copies, out);
                writeln!(out, "    jmp .L{}_{}", func_name, else_block.as_u32()).unwrap();
                writeln!(
                    out,
                    ".L{}_{}_{}_then:",
                    func_name,
                    block.as_u32(),
                    then_block.as_u32()
                )
                .unwrap();
                emit_parallel_copies(&then_copies, out);
                writeln!(out, "    jmp .L{}_{}", func_name, then_block.as_u32()).unwrap();
            }
        }
        Terminator::Switch {
            value,
            default,
            cases,
        } => {
            let val_ty = prog.values[*value].inst.ty();
            let val_loc = loc_str(*value, alloc, prog);
            for (case_val, case_block) in cases {
                let copies = collect_phi_copies(block, *case_block, prog, alloc);
                match val_ty {
                    Ty::I64 | Ty::Ptr => {
                        let imm = *case_val as i64;
                        if i32::try_from(imm).is_ok() {
                            writeln!(out, "    cmp{} ${}, {}", int_suffix(val_ty), imm, val_loc)
                                .unwrap();
                        } else {
                            writeln!(out, "    movabsq ${}, %r11", imm).unwrap();
                            writeln!(out, "    cmpq %r11, {}", val_loc).unwrap();
                        }
                    }
                    Ty::I32 => {
                        let imm = *case_val as u32 as i32;
                        writeln!(out, "    cmpl ${}, {}", imm, val_loc).unwrap();
                    }
                    Ty::I16 => {
                        let imm = *case_val as u16 as i16;
                        writeln!(out, "    cmpw ${}, {}", imm, val_loc).unwrap();
                    }
                    Ty::I1 | Ty::I8 => {
                        let imm = *case_val as u8 as i8;
                        writeln!(out, "    cmpb ${}, {}", imm, val_loc).unwrap();
                    }
                    _ => unreachable!("switch condition must be an integer type"),
                }
                if copies.is_empty() {
                    writeln!(out, "    je .L{}_{}", func_name, case_block.as_u32()).unwrap();
                } else {
                    writeln!(
                        out,
                        "    jne .L{}_{}_{}_skip",
                        func_name,
                        block.as_u32(),
                        case_block.as_u32()
                    )
                    .unwrap();
                    emit_parallel_copies(&copies, out);
                    writeln!(out, "    jmp .L{}_{}", func_name, case_block.as_u32()).unwrap();
                    writeln!(
                        out,
                        ".L{}_{}_{}_skip:",
                        func_name,
                        block.as_u32(),
                        case_block.as_u32()
                    )
                    .unwrap();
                }
            }
            let def_copies = collect_phi_copies(block, *default, prog, alloc);
            emit_parallel_copies(&def_copies, out);
            writeln!(out, "    jmp .L{}_{}", func_name, default.as_u32()).unwrap();
        }
        Terminator::Unreachable => {
            writeln!(out, "    ud2").unwrap();
        }
    }
}

// ---------------------------------------------------------------------------
// Function emission
// ---------------------------------------------------------------------------

fn emit_function(
    func: Function,
    prog: &Program,
    order: &Order,
    liveness: &Liveness,
    alloc: &RegAlloc,
    interner: &IdentInterner,
    out: &mut String,
) {
    let func_data = &prog.functions[func];
    let entry = match func_data.entry {
        Some(e) => e,
        None => return, // declaration only, no body
    };

    let func_name = interner.lookup(func_data.name);

    if func_data.is_global {
        writeln!(out, "    .globl {}", func_name).unwrap();
    }
    writeln!(out, "    .type {}, @function", func_name).unwrap();
    writeln!(out, "{}:", func_name).unwrap();

    // Prologue.
    writeln!(out, "    pushq %rbp").unwrap();
    writeln!(out, "    movq %rsp, %rbp").unwrap();

    if alloc.stack_size > 0 {
        writeln!(out, "    subq ${}, %rsp", alloc.stack_size).unwrap();
    }

    // Save callee-saved GP registers just below %rbp.
    for (i, &r) in alloc.used_callee_saved.iter().enumerate() {
        let off = -((i as i32 + 1) * 8);
        writeln!(out, "    movq {}, {}(%rbp)", r.name64(), off).unwrap();
    }
    // Save callee-saved XMM registers below the GP save area (16 bytes each).
    let int_area = alloc.used_callee_saved.len() as i32 * 8;
    for (i, &r) in alloc.used_callee_saved_float.iter().enumerate() {
        let off = -(int_area + (i as i32 + 1) * 16);
        writeln!(out, "    movdqu {}, {}(%rbp)", r.name64(), off).unwrap();
    }

    // Emit blocks in RPO.
    for block in order.rpo(entry).into_iter() {
        writeln!(out, ".L{}_{}:", func_name, block.as_u32()).unwrap();

        for &val in &prog.blocks[block].insts {
            emit_inst(val, prog, alloc, liveness, block, out, interner);
        }

        emit_term(block, prog, alloc, func, out, interner);
    }

    writeln!(out, "    .size {}, . - {}", func_name, func_name).unwrap();
    writeln!(out).unwrap();
}

// ---------------------------------------------------------------------------
// Program emission
// ---------------------------------------------------------------------------

pub fn emit_program(prog: &Program, order: &Order, interner: &IdentInterner) -> String {
    let mut out = String::new();

    writeln!(out, "    .text").unwrap();

    for (func, func_data) in prog.functions.iter() {
        let entry = match func_data.entry {
            Some(e) => e,
            None => continue,
        };

        let liveness = Liveness::compute(entry, prog, &Default::default(), order);
        let alloc = allocate(entry, prog, order, &liveness);

        emit_function(func, prog, order, &liveness, &alloc, interner, &mut out);
    }

    // Globals section.
    if !prog.globals.is_empty() {
        writeln!(out, "    .data").unwrap();
        for (_, global) in prog.globals.iter() {
            let Some(init) = global.init else {
                continue;
            };
            let name = interner.lookup(global.name);
            if global.is_global {
                writeln!(out, "    .globl {}", name).unwrap();
            }
            writeln!(out, "    .align {}", global.ty.size_bytes()).unwrap();
            writeln!(out, "{}:", name).unwrap();
            match global.ty.size_bytes() {
                1 => writeln!(out, "    .byte {}", init).unwrap(),
                2 => writeln!(out, "    .word {}", init).unwrap(),
                4 => writeln!(out, "    .long {}", init).unwrap(),
                _ => writeln!(out, "    .quad {}", init).unwrap(),
            }
        }
    }

    out
}
