use std::collections::HashMap;

use crate::{
    amd64::{Func, InstKind, SymbolTable, Type},
    infra::inset::InsertionSet,
};

use super::{BinaryOp, Inst, Operand, Reg};
use crate::amd64::Program;

// ---------------------------------------------------------------------------
// AMD64Fixer
// ---------------------------------------------------------------------------

pub struct AMD64Fixer<'a> {
    offset: i64,
    table: &'a SymbolTable,
    inset: InsertionSet<Func>,
    offsets: HashMap<u32, i64>,
}

impl<'a> AMD64Fixer<'a> {
    pub fn new(table: &'a SymbolTable) -> Self {
        Self {
            table,
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
            if let Some(block) = func.blocks[1..].first() {
                if let Some(inst) = block.insts.first() {
                    let inst = func.inst_mut(*inst);
                    if let InstKind::Binary {
                        op: BinaryOp::Sub,
                        dst: Operand::Reg(Reg::Rsp),
                        src: Operand::Imm(ref mut size),
                    } = &mut inst.kind
                    {
                        self.offset = (self.offset + 15) & !15;
                        *size = self.offset;
                    }
                }
            }
        });
    }

    fn fix_instr(&mut self, inst: &mut Inst) {
        match &mut inst.kind {
            InstKind::Nop
            | InstKind::Ret
            | InstKind::Cdq
            | InstKind::Jmp(_)
            | InstKind::Call(_)
            | InstKind::JmpCC { .. } => {}
            InstKind::SetCC { dst, .. } | InstKind::Unary { dst, .. } => {
                self.fix_operand(dst);
            }
            InstKind::Push(dst) => {
                self.fix_operand(dst);
                if matches!(dst, Operand::Imm(v) if *v < i32::MIN as i64 || *v > i32::MAX as i64) {
                    // NOTE
                    //
                    // The quadword versions of our three binary arithmetic instructions
                    // (addq, imulq, and subq) can’t handle immediate values that don’t fit into an
                    // int, and neither can cmpq or pushq. If the source of any of these instructions
                    // is a constant outside the range of int, we’ll need to copy it into R10 before
                    // we can use it.
                    let tmp = *dst;
                    *dst = Operand::Reg(Reg::Rg10);
                    self.inset.before(
                        inst.idx,
                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                    );
                }
            }
            InstKind::Div(oper) | InstKind::Idiv(oper) => {
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
                    self.inset.before(
                        inst.idx,
                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                    );
                }
            }
            InstKind::Movsx { src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                if matches!(src, Operand::Imm(_)) {
                    // NOTE
                    //
                    // The `movsx` instruction
                    // Can’t use an immediate value as a source.
                    //
                    // movsx $10, %eax
                    //
                    // movl $10, %r10d
                    // movsx %r10d, %eax
                    let tmp = *src;
                    *src = Operand::Reg(Reg::Rg10);
                    self.inset
                        .before(inst.idx, Inst::mov(inst.ty, tmp, *src, inst.span));
                }
                if matches!(dst, Operand::Stack(_)) {
                    // NOTE
                    //
                    // The `movsx` instruction
                    // Can’t use a memory address as a destination.
                    //
                    // movsx %eax, -16(%rbp)
                    //
                    // movsx %eax, %r10d
                    // movq %r10d, -16(%rbp)
                    let tmp = *dst;
                    *dst = Operand::Reg(Reg::Rg10);
                    self.inset
                        .after(inst.idx, Inst::mov(inst.ty, *dst, tmp, inst.span));
                }
            }
            InstKind::Movzx { src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                match dst {
                    Operand::Reg(_) => {
                        // NOTE
                        //
                        // If the destination is a register,
                        // we can just use a normal movl instruction.
                        inst.ty = Type::Long;
                        inst.kind = InstKind::Mov {
                            src: *src,
                            dst: *dst,
                        };
                    }
                    Operand::Stack(_) => {
                        // NOTE
                        //
                        // If the destination is a memory address,
                        // we need to use a temporary register.
                        let dst = *dst;
                        inst.ty = Type::Long;
                        inst.kind = InstKind::Mov {
                            src: *src,
                            dst: Operand::Reg(Reg::Rg11),
                        };
                        self.inset.after(
                            inst.idx,
                            Inst::mov(Type::Quad, Operand::Reg(Reg::Rg11), dst, inst.span),
                        );
                    }
                    _ => {}
                }
            }
            InstKind::Mov { src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                if matches!(dst, Operand::Stack(_) | Operand::Data(_))
                    && matches!(src, Operand::Stack(_) | Operand::Data(_))
                {
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
                    self.inset.after(
                        inst.idx,
                        Inst::mov(inst.ty, Operand::Reg(Reg::Rg10), tmp, inst.span),
                    );
                }
                if let Operand::Imm(c) = src {
                    if *c < i32::MIN as i64 || *c > i32::MAX as i64 {
                        match inst.ty {
                            Type::Byte => {}
                            Type::Long => {
                                // NOTE
                                //
                                // Since `movl` can’t use 8-byte immediate values, the assembler
                                // automatically truncates these values to 32 bits. When this happens
                                // the GNU assembler issues a warning, although the LLVM assembler doesn’t.
                                //
                                // To avoid these warnings, we truncate 8-byte immediate values in movl instructions ourselves.
                                //
                                // movl $4294967299, %r10d
                                //
                                // movl $3, %r10d
                                *c = *c as i32 as i64;
                            }
                            Type::Quad => {
                                if matches!(dst, Operand::Stack(_) | Operand::Data(_)) {
                                    // NOTE
                                    //
                                    // The `movq` instruction can move these very large immediate
                                    // values into registers, but not directly into memory.
                                    //
                                    // movq $4294967295, -16(%rbp)
                                    //
                                    // movq $4294967295, %r10
                                    // movq %r10, -16(%rbp)
                                    let tmp = *src;
                                    *src = Operand::Reg(Reg::Rg10);
                                    self.inset.before(
                                        inst.idx,
                                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                                    );
                                }
                            }
                        }
                    }
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
                    self.inset.before(
                        inst.idx,
                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg11), inst.span),
                    );
                }
                if matches!(lhs, Operand::Imm(v) if *v < i32::MIN as i64 || *v > i32::MAX as i64) {
                    // NOTE
                    //
                    // The quadword versions of our three binary arithmetic instructions
                    // (addq, imulq, and subq) can’t handle immediate values that don’t fit into an
                    // int, and neither can cmpq or pushq. If the source of any of these instructions
                    // is a constant outside the range of int, we’ll need to copy it into R10 before
                    let tmp = *lhs;
                    *lhs = Operand::Reg(Reg::Rg10);
                    self.inset.before(
                        inst.idx,
                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                    );
                }
                if matches!((&lhs, &rhs), (Operand::Stack(_), Operand::Stack(_))) {
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
                    self.inset.before(
                        inst.idx,
                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                    );
                }
            }
            InstKind::Test { lhs, rhs } => {
                self.fix_operand(lhs);
                self.fix_operand(rhs);
                if matches!(lhs, Operand::Stack(_)) && matches!(rhs, Operand::Stack(_)) {
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
                    let tmp = *lhs;
                    *lhs = Operand::Reg(Reg::Rg10);
                    self.inset.before(
                        inst.idx,
                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                    );
                } else if matches!(lhs, Operand::Imm(_)) && matches!(rhs, Operand::Imm(_)) {
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
                    let tmp = *rhs;
                    *rhs = Operand::Reg(Reg::Rg10);
                    self.inset.before(
                        inst.idx,
                        Inst::mov(Type::Long, tmp, Operand::Reg(Reg::Rg10), inst.span),
                    );
                }
            }
            InstKind::Binary { op, src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                if matches!(src, Operand::Imm(v) if *v < i32::MIN as i64 || *v > i32::MAX as i64) {
                    // NOTE
                    //
                    // The quadword versions of our three binary arithmetic instructions
                    // (addq, imulq, and subq) can’t handle immediate values that don’t fit into an
                    // int, and neither can cmpq or pushq. If the source of any of these instructions
                    // is a constant outside the range of int, we’ll need to copy it into R10 before
                    // we can use it.
                    let tmp = *src;
                    *src = Operand::Reg(Reg::Rg10);
                    self.inset.before(
                        inst.idx,
                        Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                    );
                }
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
                                Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg11), inst.span),
                            );
                            self.inset.after(
                                inst.idx,
                                Inst::mov(inst.ty, Operand::Reg(Reg::Rg11), tmp, inst.span),
                            );
                        }
                    }
                    BinaryOp::Shl | BinaryOp::Shr | BinaryOp::Sar => {
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
                                Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rcx), inst.span),
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
                                Inst::mov(inst.ty, tmp, Operand::Reg(Reg::Rg10), inst.span),
                            );
                        }
                    }
                }
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Auxiliary methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn fix_operand(&mut self, oper: &mut Operand) {
        if let Operand::Pseudo(id) = oper {
            let entry = &self.table.pseudos[*id as usize];
            *oper = Operand::Stack(*self.offsets.entry(*id).or_insert_with(|| {
                self.offset += entry.ty.align();
                -self.offset
            }));
        }
    }
}
