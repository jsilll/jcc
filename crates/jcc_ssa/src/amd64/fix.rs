use std::collections::HashMap;

use crate::{
    amd64::{Func, InstKind},
    infra::inset::InsertionSet,
};

use super::{BinaryOp, Inst, Operand, Program, Reg};

// ---------------------------------------------------------------------------
// AMD64Fixer
// ---------------------------------------------------------------------------

pub struct AMD64Fixer {
    offset: i32,
    inset: InsertionSet<Func>,
    offsets: HashMap<u32, i32>,
}

impl Default for AMD64Fixer {
    fn default() -> Self {
        Self::new()
    }
}

impl AMD64Fixer {
    pub fn new() -> Self {
        Self {
            offset: 0,
            offsets: HashMap::new(),
            inset: InsertionSet::default(),
        }
    }

    pub fn fix(mut self, program: &mut Program) {
        let mut blocks = Vec::new();
        program.funcs.iter_mut().for_each(|func| {
            func.snapshot_blocks(&mut blocks);
            for b in blocks.iter() {
                let n_insts = func.block(*b).insts.len();
                for i_idx in 0..n_insts {
                    let inst = func.block(*b).insts[i_idx];
                    self.fix_instr(func.inst_mut(inst));
                }
                self.inset.apply(func, *b);
            }
            if let Some(block) = func.blocks.first() {
                if let Some(inst) = block.insts.first() {
                    let inst = func.inst_mut(*inst);
                    if let InstKind::Alloca(ref mut size) = &mut inst.kind {
                        self.offset = (self.offset + 15) & !15;
                        *size = self.offset;
                    }
                }
            }
        });
    }

    #[inline]
    fn fix_operand(&mut self, oper: &mut Operand) {
        if let Operand::Pseudo(id) = oper {
            *oper = Operand::Stack(*self.offsets.entry(*id).or_insert_with(|| {
                self.offset += 4;
                -self.offset
            }));
        }
    }

    fn fix_instr(&mut self, inst: &mut Inst) {
        match &mut inst.kind {
            InstKind::Nop
            | InstKind::Ret
            | InstKind::Cdq
            | InstKind::Jmp(_)
            | InstKind::Alloca(_)
            | InstKind::Call(_)
            | InstKind::Dealloca(_)
            | InstKind::JmpCC { .. } => {}
            InstKind::Push(oper) => {
                self.fix_operand(oper);
            }
            InstKind::Idiv(oper) => {
                self.fix_operand(oper);
                if let Operand::Imm(_) = oper {
                    // NOTE
                    //
                    // The `idiv` doesn't support
                    // immediate values, so we need to move it
                    // to a register.
                    //
                    // idivl $3
                    //
                    // movl $3, %r10d
                    // idivl %r10d
                    let tmp = *oper;
                    *oper = Operand::Reg(Reg::Rg10);
                    self.inset
                        .before(inst.idx, Inst::mov(tmp, Operand::Reg(Reg::Rg10), inst.span));
                }
            }
            InstKind::Mov { src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                if matches!(src, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) {
                    // NOTE
                    //
                    // The `mov` instruction
                    // doesn't support moving from memory
                    // to memory, so we need to use a register
                    // as a temporary.
                    //
                    // movl -4(%rbp), -8(%rbp)
                    //
                    // movl -4(%rbp), %r10d
                    // movl %r10d, -8(%rbp)
                    let tmp = *dst;
                    *dst = Operand::Reg(Reg::Rg10);
                    self.inset
                        .after(inst.idx, Inst::mov(Operand::Reg(Reg::Rg10), tmp, inst.span));
                }
            }
            InstKind::Cmp { lhs, rhs } => {
                self.fix_operand(lhs);
                self.fix_operand(rhs);
                if matches!(rhs, Operand::Imm(_)) {
                    // NOTE
                    //
                    // The `cmp` instruction doesn't support
                    // an immediate value as the second operand,
                    // so we need to move the immediate value
                    // to a register.
                    //
                    // cmpl -4(%rbp), $3
                    //
                    // movl $3, %r11d
                    // cmpl -4(%rbp), %r11d
                    let tmp = *rhs;
                    *rhs = Operand::Reg(Reg::Rg11);
                    self.inset
                        .before(inst.idx, Inst::mov(tmp, Operand::Reg(Reg::Rg11), inst.span));
                } else if matches!(lhs, Operand::Stack(_)) && matches!(rhs, Operand::Stack(_)) {
                    // NOTE
                    //
                    // The `cmp` instruction doesn't support
                    // comparing memory to memory, so we need to
                    // use a register as a temporary.
                    //
                    // cmpl -4(%rbp), -8(%rbp)
                    //
                    // movl -4(%rbp), %r10d
                    // cmpl %r10d, -8(%rbp)
                    let tmp = *lhs;
                    *lhs = Operand::Reg(Reg::Rg10);
                    self.inset
                        .before(inst.idx, Inst::mov(tmp, Operand::Reg(Reg::Rg10), inst.span));
                }
            }
            InstKind::Test { lhs: src, rhs: dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                if matches!(src, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) {
                    // NOTE
                    //
                    // The `test` instruction doesn't support
                    // testing memory to memory, so we need to
                    // use a register as a temporary.
                    //
                    // testl -4(%rbp), -8(%rbp)
                    //
                    // movl -4(%rbp), %r10d
                    // testl %r10d, -8(%rbp)
                    let tmp = *src;
                    *src = Operand::Reg(Reg::Rg10);
                    self.inset
                        .before(inst.idx, Inst::mov(tmp, Operand::Reg(Reg::Rg10), inst.span));
                } else if matches!(src, Operand::Imm(_)) && matches!(dst, Operand::Imm(_)) {
                    // NOTE
                    //
                    // The `test` instruction doesn't support
                    // immediate values as operands, so we need to
                    // move the immediate value to a register.
                    //
                    // testl $1, $2
                    //
                    // movl $2, %r10d
                    // testl $1, %r10d
                    let tmp = *dst;
                    *dst = Operand::Reg(Reg::Rg10);
                    self.inset
                        .before(inst.idx, Inst::mov(tmp, Operand::Reg(Reg::Rg10), inst.span));
                }
            }
            InstKind::SetCC { dst, .. } => {
                self.fix_operand(dst);
            }
            InstKind::Unary { dst, .. } => self.fix_operand(dst),
            InstKind::Binary { op, src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                match *op {
                    BinaryOp::Mul => {
                        if let Operand::Stack(_) = dst {
                            // NOTE
                            //
                            // The `imul` can't use
                            // a memory address as the destination
                            // operand, so we need to use a register
                            // as a temporary.
                            //
                            // imull $3, -4(%rbp)
                            //
                            // movl -4(%rbp), %r11d
                            // imull $3, %r11d
                            // movl %r11d, -4(%rbp)
                            let tmp = *dst;
                            *dst = Operand::Reg(Reg::Rg11);
                            self.inset.before(
                                inst.idx,
                                Inst::mov(tmp, Operand::Reg(Reg::Rg11), inst.span),
                            );
                            self.inset.after(
                                inst.idx,
                                Inst::mov(Operand::Reg(Reg::Rg11), tmp, inst.span),
                            );
                        }
                    }
                    BinaryOp::Shl | BinaryOp::Shr => {
                        if !matches!(src, Operand::Imm(_)) && !matches!(src, Operand::Reg(Reg::Rcx))
                        {
                            // NOTE
                            //
                            // The shift instructions only supports
                            // dynamic shift counts in the CL register, so
                            // we need to move the shift count to CL.
                            //
                            // shl $1, %rax
                            //
                            // movl $1, %cl
                            // shll %cl, %rax
                            let tmp = *src;
                            *src = Operand::Reg(Reg::Rcx);
                            self.inset.before(
                                inst.idx,
                                Inst::mov(tmp, Operand::Reg(Reg::Rcx), inst.span),
                            );
                        }
                    }
                    _ => {
                        if matches!(src, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) {
                            // NOTE
                            //
                            // Binary instructions don't support
                            // moving from memory to memory, so we need to
                            // use a register as a temporary.
                            //
                            // addl -4(%rbp), -8(%rbp)
                            //
                            // movl -4(%rbp), %r10d
                            // addl %r10d, -8(%rbp)
                            let tmp = *src;
                            *src = Operand::Reg(Reg::Rg10);
                            self.inset.before(
                                inst.idx,
                                Inst::mov(tmp, Operand::Reg(Reg::Rg10), inst.span),
                            );
                        }
                    }
                }
            }
        }
    }
}
