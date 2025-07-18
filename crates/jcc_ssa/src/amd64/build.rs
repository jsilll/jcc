use super::{BinaryOp, Block, BlockRef, CondCode, FnDef, Inst, Operand, Program, Reg, UnaryOp};

use std::collections::HashMap;

const ARG_REGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];

// ---------------------------------------------------------------------------
// Root function
// ---------------------------------------------------------------------------

pub fn build(ssa: &crate::Program) -> Program {
    let mut prog = Program::default();
    ssa.iter_funcs().for_each(|func| {
        if ssa.func(func).blocks.is_empty() {
            // Skip empty functions
            return;
        }
        prog.funcs.push(AMD64FuncBuilder::new(ssa, func).build());
    });
    prog
}

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64FuncBuilder<'a> {
    ssa: &'a crate::Program<'a>,
    func: crate::FuncRef,
    fn_def: FnDef,
    block: BlockRef,
    arg_count: u32,
    pseudo_count: u32,
    blocks: HashMap<crate::BlockRef, BlockRef>,
    operands: HashMap<crate::InstRef, Operand>,
}

impl<'a> AMD64FuncBuilder<'a> {
    pub fn new(ssa: &'a crate::Program, func: crate::FuncRef) -> Self {
        let mut blocks = HashMap::new();
        let actual_func = ssa.func(func);
        let mut fn_def = FnDef::new(actual_func.name, actual_func.span);
        let block = fn_def.push_block(Block::default());
        blocks.insert(ssa.func(func).blocks[0], block);
        Self {
            ssa,
            func,
            block,
            blocks,
            fn_def,
            arg_count: 0,
            pseudo_count: 0,
            operands: HashMap::new(),
        }
    }

    pub fn build(mut self) -> FnDef {
        self.append_to_block(Inst::Alloca(0), self.ssa.func(self.func).span);
        self.ssa.func(self.func).blocks.iter().for_each(|b| {
            self.block = self.get_or_make_block(*b);
            self.fn_def.get_block_mut(self.block).label = Some(self.ssa.block(*b).name);
            self.ssa.block(*b).insts.iter().for_each(|inst| {
                self.visit_inst(*inst);
            });
        });
        self.fn_def
    }

    #[inline]
    fn make_pseudo(&mut self) -> Operand {
        let tmp = Operand::Pseudo(self.pseudo_count);
        self.pseudo_count += 1;
        tmp
    }

    #[inline]
    fn get_operand(&self, inst: &crate::InstRef) -> Operand {
        *self.operands.get(inst).expect("expected a variable")
    }

    #[inline]
    fn get_or_make_operand(&mut self, inst: crate::InstRef) -> Operand {
        *self.operands.entry(inst).or_insert_with(|| {
            let tmp = Operand::Pseudo(self.pseudo_count);
            self.pseudo_count += 1;
            tmp
        })
    }

    #[inline]
    fn get_or_make_block(&mut self, block: crate::BlockRef) -> BlockRef {
        *self
            .blocks
            .entry(block)
            .or_insert_with(|| self.fn_def.push_block(Block::default()))
    }

    #[inline]
    fn append_to_block(&mut self, instr: Inst, span: crate::SourceSpan) {
        let block = self.fn_def.get_block_mut(self.block);
        block.instrs.push(instr);
        block.spans.push(span);
    }

    fn visit_inst(&mut self, i: crate::InstRef) {
        let inst = self.ssa.inst(i);
        match &inst.kind {
            crate::InstKind::Select { .. } => todo!("handle select"),
            crate::InstKind::Static(_) => todo!("handle static address"),
            crate::InstKind::Nop | crate::InstKind::Phi => {}
            crate::InstKind::Alloca => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
            }
            crate::InstKind::Const(c) => {
                self.operands.insert(i, Operand::Imm(*c));
            }
            crate::InstKind::Jump(block) => {
                let block = self.get_or_make_block(*block);
                self.append_to_block(Inst::Jmp(block), inst.span);
            }
            crate::InstKind::Identity(val) => {
                self.operands.insert(i, self.get_operand(val));
            }
            crate::InstKind::Ret(val) => {
                self.append_to_block(
                    Inst::Mov {
                        src: self.get_operand(val),
                        dst: Operand::Reg(Reg::Rax),
                    },
                    inst.span,
                );
                self.append_to_block(Inst::Ret, inst.span);
            }
            crate::InstKind::Load(ptr) => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                self.append_to_block(
                    Inst::Mov {
                        src: self.get_operand(ptr),
                        dst,
                    },
                    inst.span,
                );
            }
            crate::InstKind::Store { ptr, val } => {
                self.append_to_block(
                    Inst::Mov {
                        src: self.get_operand(val),
                        dst: self.get_operand(ptr),
                    },
                    inst.span,
                );
            }
            crate::InstKind::Upsilon { phi, val } => {
                let dst = self.get_or_make_operand(*phi);
                self.append_to_block(
                    Inst::Mov {
                        dst,
                        src: self.get_operand(val),
                    },
                    inst.span,
                );
            }
            crate::InstKind::Branch { cond, then, other } => {
                let cond = self.get_operand(cond);
                let then = self.get_or_make_block(*then);
                let other = self.get_or_make_block(*other);
                self.append_to_block(
                    Inst::Test {
                        lhs: cond,
                        rhs: cond,
                    },
                    inst.span,
                );
                self.append_to_block(
                    Inst::JmpCC {
                        cond_code: CondCode::NotEqual,
                        target: then,
                    },
                    inst.span,
                );
                self.append_to_block(Inst::Jmp(other), inst.span);
            }
            crate::InstKind::Switch {
                cond,
                default,
                cases,
            } => {
                let cond = self.get_operand(cond);
                cases.iter().for_each(|(val, block)| {
                    let block = self.get_or_make_block(*block);
                    self.append_to_block(
                        Inst::Cmp {
                            lhs: cond,
                            rhs: Operand::Imm(*val),
                        },
                        inst.span,
                    );
                    self.append_to_block(
                        Inst::JmpCC {
                            cond_code: CondCode::Equal,
                            target: block,
                        },
                        inst.span,
                    );
                });
                let default = self.get_or_make_block(*default);
                self.append_to_block(Inst::Jmp(default), inst.span);
            }
            crate::InstKind::Unary { op, val } => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                let src = self.get_operand(val);
                match op {
                    crate::UnaryOp::Not => self.build_unary(UnaryOp::Not, src, dst, inst.span),
                    crate::UnaryOp::Neg => self.build_unary(UnaryOp::Neg, src, dst, inst.span),
                    crate::UnaryOp::Inc => self.build_unary(UnaryOp::Inc, src, dst, inst.span),
                    crate::UnaryOp::Dec => self.build_unary(UnaryOp::Dec, src, dst, inst.span),
                }
            }
            crate::InstKind::Binary { op, lhs, rhs } => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                let lhs = self.get_operand(lhs);
                let rhs = self.get_operand(rhs);
                match op {
                    crate::BinaryOp::Or => {
                        self.build_binary(BinaryOp::Or, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::And => {
                        self.build_binary(BinaryOp::And, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Xor => {
                        self.build_binary(BinaryOp::Xor, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Shl => {
                        self.build_binary(BinaryOp::Shl, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Shr => {
                        self.build_binary(BinaryOp::Shr, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Add => {
                        self.build_binary(BinaryOp::Add, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Sub => {
                        self.build_binary(BinaryOp::Sub, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Mul => {
                        self.build_binary(BinaryOp::Mul, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Equal => {
                        self.build_comparison(CondCode::Equal, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::NotEqual => {
                        self.build_comparison(CondCode::NotEqual, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::LessThan => {
                        self.build_comparison(CondCode::LessThan, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::LessEqual => {
                        self.build_comparison(CondCode::LessEqual, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::GreaterThan => {
                        self.build_comparison(CondCode::GreaterThan, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::GreaterEqual => {
                        self.build_comparison(CondCode::GreaterEqual, lhs, rhs, dst, inst.span)
                    }
                    crate::BinaryOp::Div => self.build_div_or_rem(true, lhs, rhs, dst, inst.span),
                    crate::BinaryOp::Rem => self.build_div_or_rem(false, lhs, rhs, dst, inst.span),
                }
            }
            crate::InstKind::Arg => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                match ARG_REGS.get(self.arg_count as usize) {
                    Some(&reg) => {
                        self.append_to_block(
                            Inst::Mov {
                                src: Operand::Reg(reg),
                                dst,
                            },
                            inst.span,
                        );
                    }
                    None => {
                        let n_stack_arg = self.arg_count - ARG_REGS.len() as u32;
                        let offset = 16 + n_stack_arg * 8;
                        self.append_to_block(
                            Inst::Mov {
                                src: Operand::Stack(offset as i32),
                                dst,
                            },
                            inst.span,
                        );
                    }
                }
                self.arg_count += 1;
            }
            crate::InstKind::Call { func, args } => {
                let stack_args = args.len().saturating_sub(ARG_REGS.len());
                let stack_padding = match stack_args % 2 {
                    0 => 0,
                    1 => {
                        self.append_to_block(Inst::Alloca(8), inst.span);
                        8
                    }
                    _ => unreachable!(),
                };
                for (reg, arg) in ARG_REGS.iter().zip(args.iter()) {
                    let src = self.get_operand(arg);
                    self.append_to_block(
                        Inst::Mov {
                            src,
                            dst: Operand::Reg(*reg),
                        },
                        inst.span,
                    );
                }
                for arg in args.get(ARG_REGS.len()..).unwrap_or(&[]).iter().rev() {
                    let arg = self.get_operand(arg);
                    match arg {
                        Operand::Imm(_) | Operand::Reg(_) => {
                            self.append_to_block(Inst::Push(arg), inst.span);
                        }
                        _ => {
                            self.append_to_block(
                                Inst::Mov {
                                    src: arg,
                                    dst: Operand::Reg(Reg::Rax),
                                },
                                inst.span,
                            );
                            self.append_to_block(Inst::Push(Operand::Reg(Reg::Rax)), inst.span);
                        }
                    }
                }
                let name = self.ssa.func(*func).name;
                self.append_to_block(Inst::Call(name), inst.span);
                let stack_size = stack_args as i32 * 8 + stack_padding;
                if stack_size > 0 {
                    self.append_to_block(Inst::Dealloca(stack_size), inst.span);
                }
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                self.append_to_block(
                    Inst::Mov {
                        src: Operand::Reg(Reg::Rax),
                        dst,
                    },
                    inst.span,
                );
            }
        }
    }

    #[inline]
    fn build_unary(&mut self, op: UnaryOp, src: Operand, dst: Operand, span: crate::SourceSpan) {
        self.append_to_block(Inst::Mov { src, dst }, span);
        self.append_to_block(Inst::Unary { op, dst }, span);
    }

    #[inline]
    fn build_binary(
        &mut self,
        op: BinaryOp,
        lhs: Operand,
        rhs: Operand,
        dst: Operand,
        span: crate::SourceSpan,
    ) {
        self.append_to_block(Inst::Mov { src: lhs, dst }, span);
        self.append_to_block(Inst::Binary { op, src: rhs, dst }, span);
    }

    #[inline]
    fn build_comparison(
        &mut self,
        cond_code: CondCode,
        lhs: Operand,
        rhs: Operand,
        dst: Operand,
        span: crate::SourceSpan,
    ) {
        self.append_to_block(
            Inst::Mov {
                src: Operand::Imm(0),
                dst,
            },
            span,
        );
        self.append_to_block(Inst::Cmp { lhs: rhs, rhs: lhs }, span);
        self.append_to_block(Inst::SetCC { cond_code, dst }, span);
    }

    #[inline]
    fn build_div_or_rem(
        &mut self,
        div: bool,
        lhs: Operand,
        rhs: Operand,
        dst: Operand,
        span: crate::SourceSpan,
    ) {
        self.append_to_block(
            Inst::Mov {
                src: lhs,
                dst: Operand::Reg(Reg::Rax),
            },
            span,
        );
        self.append_to_block(Inst::Cdq, span);
        self.append_to_block(Inst::Idiv(rhs), span);
        self.append_to_block(
            Inst::Mov {
                src: if div {
                    Operand::Reg(Reg::Rax)
                } else {
                    Operand::Reg(Reg::Rdx)
                },
                dst,
            },
            span,
        );
    }
}
