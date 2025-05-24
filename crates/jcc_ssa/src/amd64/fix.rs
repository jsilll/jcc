use std::collections::HashMap;

use super::{BinaryOp, Block, Inst, Operand, Program, Reg};

// ---------------------------------------------------------------------------
// AMD64Fixer
// ---------------------------------------------------------------------------

pub struct AMD64Fixer {
    offset: u32,
    offsets: HashMap<u32, u32>,
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
        }
    }

    pub fn fix(mut self, program: &mut Program) {
        let mut idx = 0;
        program.0.blocks.iter_mut().for_each(|block| {
            while idx < block.instrs.len() {
                self.fix_instr(block, &mut idx);
                idx += 1;
            }
            idx = 0;
        });
        if let Some(block) = program.0.blocks.first_mut() {
            if let Some(Inst::Alloca(size)) = block.instrs.first_mut() {
                *size = self.offset;
            }
        }
    }

    #[inline]
    fn fix_operand(&mut self, oper: &mut Operand) {
        if let Operand::Pseudo(id) = oper {
            *oper = Operand::Stack(*self.offsets.entry(*id).or_insert_with(|| {
                self.offset += 4;
                self.offset
            }));
        }
    }

    fn fix_instr(&mut self, block: &mut Block, idx: &mut usize) {
        let instr = &mut block.instrs[*idx];
        match instr {
            &mut Inst::Ret | Inst::Cdq | Inst::Jmp(_) | Inst::Alloca(_) | Inst::JmpCC { .. } => {}
            Inst::Idiv(oper) => {
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
                    block.instrs.insert(
                        *idx,
                        Inst::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg10),
                        },
                    );
                    *idx += 1;
                }
            }
            Inst::Mov { src, dst } => {
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
                    block.instrs.insert(
                        *idx + 1,
                        Inst::Mov {
                            src: Operand::Reg(Reg::Rg10),
                            dst: tmp,
                        },
                    );
                    *idx += 1;
                }
            }
            Inst::Cmp { lhs, rhs } => {
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
                    block.instrs.insert(
                        *idx,
                        Inst::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg11),
                        },
                    );
                    *idx += 1;
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
                    block.instrs.insert(
                        *idx,
                        Inst::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg10),
                        },
                    );
                    *idx += 1;
                }
            }
            Inst::Test { lhs: src, rhs: dst } => {
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
                    block.instrs.insert(
                        *idx,
                        Inst::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg10),
                        },
                    );
                    *idx += 1;
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
                    block.instrs.insert(
                        *idx,
                        Inst::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg10),
                        },
                    );
                    *idx += 1;
                }
            }
            Inst::SetCC { dst, .. } => {
                self.fix_operand(dst);
            }
            Inst::Unary { dst, .. } => self.fix_operand(dst),
            Inst::Binary { op, src, dst } => {
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
                            block.instrs.insert(
                                *idx,
                                Inst::Mov {
                                    src: tmp,
                                    dst: Operand::Reg(Reg::Rg11),
                                },
                            );
                            *idx += 2;
                            block.instrs.insert(
                                *idx,
                                Inst::Mov {
                                    src: Operand::Reg(Reg::Rg11),
                                    dst: tmp,
                                },
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
                            block.instrs.insert(
                                *idx,
                                Inst::Mov {
                                    src: tmp,
                                    dst: Operand::Reg(Reg::Rcx),
                                },
                            );
                            *idx += 1;
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
                            block.instrs.insert(
                                *idx,
                                Inst::Mov {
                                    src: tmp,
                                    dst: Operand::Reg(Reg::Rg10),
                                },
                            );
                            *idx += 1;
                        }
                    }
                }
            }
        }
    }
}
