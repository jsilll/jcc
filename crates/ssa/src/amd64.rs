use tacky::amd64;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Root function
// ---------------------------------------------------------------------------

pub fn build(ssa: &crate::Program) -> tacky::amd64::Program {
    let mut prog = amd64::Program::default();
    ssa.funcs_iter().for_each(|func| {
        AMD64FuncBuilder::new(ssa, &mut prog, func).build();
    });
    prog
}

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

struct AMD64FuncBuilder<'a> {
    ssa: &'a crate::Program,
    prog: &'a mut amd64::Program,
    pseudo_count: u32,
    func: crate::FuncRef,
    block: amd64::BlockRef,
    blocks: HashMap<crate::BlockRef, amd64::BlockRef>,
    operands: HashMap<crate::InstRef, amd64::Operand>,
}

impl<'a> AMD64FuncBuilder<'a> {
    pub fn new(
        ssa: &'a crate::Program,
        prog: &'a mut amd64::Program,
        func: crate::FuncRef,
    ) -> Self {
        let mut blocks = HashMap::new();
        let block = prog.0.push_block(amd64::Block::default());
        blocks.insert(ssa.func(func).blocks[0], block);
        Self {
            ssa,
            prog,
            func,
            block,
            blocks,
            pseudo_count: 0,
            operands: HashMap::new(),
        }
    }

    pub fn build(mut self) {
        let span = *self.ssa.func_span(self.func);
        self.append_to_block(amd64::Inst::Alloca(0), span);
        self.ssa.func(self.func).blocks.iter().for_each(|b| {
            self.block = self.get_or_make_block(*b);
            self.prog.0.get_block_mut(self.block).label = Some(*self.ssa.block_name(*b));
            self.ssa.block(*b).insts.iter().for_each(|inst| {
                self.visit_inst(*inst);
            });
        });
    }

    #[inline]
    fn make_pseudo(&mut self) -> amd64::Operand {
        let tmp = amd64::Operand::Pseudo(self.pseudo_count);
        self.pseudo_count += 1;
        tmp
    }

    #[inline]
    fn get_operand(&self, inst: &crate::InstRef) -> amd64::Operand {
        self.operands
            .get(&inst)
            .expect("expected a variable")
            .clone()
    }

    #[inline]
    fn get_or_make_operand(&mut self, inst: crate::InstRef) -> amd64::Operand {
        self.operands
            .entry(inst)
            .or_insert_with(|| {
                let tmp = amd64::Operand::Pseudo(self.pseudo_count);
                self.pseudo_count += 1;
                tmp
            })
            .clone()
    }

    #[inline]
    fn get_or_make_block(&mut self, block: crate::BlockRef) -> amd64::BlockRef {
        self.blocks
            .entry(block)
            .or_insert_with(|| self.prog.0.push_block(amd64::Block::default()))
            .clone()
    }

    #[inline]
    fn append_to_block(&mut self, instr: tacky::amd64::Inst, span: crate::SourceSpan) {
        let block = self.prog.0.get_block_mut(self.block);
        block.instrs.push(instr);
        block.spans.push(span);
    }

    fn visit_inst(&mut self, i: crate::InstRef) {
        let inst = self.ssa.inst(i);
        let span = *self.ssa.inst_span(i);
        match &inst.kind {
            crate::InstKind::Arg => todo!("handle args"),
            crate::InstKind::Select { .. } => todo!("handle select"),
            crate::InstKind::Nop | crate::InstKind::Phi => {}
            crate::InstKind::Alloca => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
            }
            crate::InstKind::Identity(val) => {
                self.operands.insert(i, self.get_operand(&val));
            }
            crate::InstKind::Const(c) => {
                self.operands.insert(i, amd64::Operand::Imm(*c));
            }
            crate::InstKind::Jump(block) => {
                let block = self.get_or_make_block(*block);
                self.append_to_block(amd64::Inst::Jmp(block), span);
            }
            crate::InstKind::Ret(val) => {
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.get_operand(&val),
                        dst: amd64::Operand::Reg(amd64::Reg::Rax),
                    },
                    span,
                );
                self.append_to_block(amd64::Inst::Ret, span);
            }
            crate::InstKind::Load(ptr) => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.get_operand(&ptr),
                        dst,
                    },
                    span,
                );
            }
            crate::InstKind::Store { ptr, val } => {
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.get_operand(&val),
                        dst: self.get_operand(&ptr),
                    },
                    span,
                );
            }
            crate::InstKind::Upsilon { phi, val } => {
                let dst = self.get_or_make_operand(*phi);
                self.append_to_block(
                    amd64::Inst::Mov {
                        dst,
                        src: self.get_operand(&val),
                    },
                    span,
                );
            }
            crate::InstKind::Branch { cond, then, other } => {
                let cond = self.get_operand(&cond);
                let then = self.get_or_make_block(*then);
                let other = self.get_or_make_block(*other);
                self.append_to_block(
                    amd64::Inst::Test {
                        lhs: cond,
                        rhs: cond,
                    },
                    span,
                );
                self.append_to_block(
                    amd64::Inst::JmpCC {
                        cond_code: amd64::CondCode::NotEqual,
                        target: then,
                    },
                    span,
                );
                self.append_to_block(amd64::Inst::Jmp(other), span);
            }
            crate::InstKind::Switch {
                cond,
                default,
                cases,
            } => {
                let cond = self.get_operand(&cond);
                cases.iter().for_each(|(val, block)| {
                    let block = self.get_or_make_block(*block);
                    self.append_to_block(
                        amd64::Inst::Cmp {
                            lhs: cond,
                            rhs: amd64::Operand::Imm(*val),
                        },
                        span,
                    );
                    self.append_to_block(
                        amd64::Inst::JmpCC {
                            cond_code: amd64::CondCode::Equal,
                            target: block,
                        },
                        span,
                    );
                });
                let default = self.get_or_make_block(*default);
                self.append_to_block(amd64::Inst::Jmp(default), span);
            }
            crate::InstKind::Unary { op, val } => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                let src = self.get_operand(&val);
                match op {
                    crate::UnaryOp::Not => self.build_unary(amd64::UnaryOp::Not, src, dst, span),
                    crate::UnaryOp::Neg => self.build_unary(amd64::UnaryOp::Neg, src, dst, span),
                    crate::UnaryOp::Inc => self.build_unary(amd64::UnaryOp::Inc, src, dst, span),
                    crate::UnaryOp::Dec => self.build_unary(amd64::UnaryOp::Dec, src, dst, span),
                }
            }
            crate::InstKind::Binary { op, lhs, rhs } => {
                let dst = self.make_pseudo();
                self.operands.insert(i, dst);
                let lhs = self.get_operand(&lhs);
                let rhs = self.get_operand(&rhs);
                match op {
                    crate::BinaryOp::Or => {
                        self.build_binary(amd64::BinaryOp::Or, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::And => {
                        self.build_binary(amd64::BinaryOp::And, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Xor => {
                        self.build_binary(amd64::BinaryOp::Xor, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Shl => {
                        self.build_binary(amd64::BinaryOp::Shl, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Shr => {
                        self.build_binary(amd64::BinaryOp::Shr, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Add => {
                        self.build_binary(amd64::BinaryOp::Add, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Sub => {
                        self.build_binary(amd64::BinaryOp::Sub, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Mul => {
                        self.build_binary(amd64::BinaryOp::Mul, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Equal => {
                        self.build_comparison(amd64::CondCode::Equal, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::NotEqual => {
                        self.build_comparison(amd64::CondCode::NotEqual, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::LessThan => {
                        self.build_comparison(amd64::CondCode::LessThan, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::LessEqual => {
                        self.build_comparison(amd64::CondCode::LessEqual, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::GreaterThan => {
                        self.build_comparison(amd64::CondCode::GreaterThan, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::GreaterEqual => {
                        self.build_comparison(amd64::CondCode::GreaterEqual, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Div => self.build_div_or_rem(true, lhs, rhs, dst, span),
                    crate::BinaryOp::Rem => self.build_div_or_rem(false, lhs, rhs, dst, span),
                }
            }
        }
    }

    #[inline]
    fn build_unary(
        &mut self,
        op: amd64::UnaryOp,
        src: amd64::Operand,
        dst: amd64::Operand,
        span: crate::SourceSpan,
    ) {
        self.append_to_block(amd64::Inst::Mov { src, dst }, span);
        self.append_to_block(amd64::Inst::Unary { op, dst }, span);
    }

    #[inline]
    fn build_binary(
        &mut self,
        op: amd64::BinaryOp,
        lhs: amd64::Operand,
        rhs: amd64::Operand,
        dst: amd64::Operand,
        span: crate::SourceSpan,
    ) {
        self.append_to_block(amd64::Inst::Mov { src: lhs, dst }, span);
        self.append_to_block(amd64::Inst::Binary { op, src: rhs, dst }, span);
    }

    #[inline]
    fn build_comparison(
        &mut self,
        cond_code: amd64::CondCode,
        lhs: amd64::Operand,
        rhs: amd64::Operand,
        dst: amd64::Operand,
        span: crate::SourceSpan,
    ) {
        self.append_to_block(
            amd64::Inst::Mov {
                src: amd64::Operand::Imm(0),
                dst,
            },
            span,
        );
        self.append_to_block(amd64::Inst::Cmp { lhs: rhs, rhs: lhs }, span);
        self.append_to_block(amd64::Inst::SetCC { cond_code, dst }, span);
    }

    #[inline]
    fn build_div_or_rem(
        &mut self,
        div: bool,
        lhs: amd64::Operand,
        rhs: amd64::Operand,
        dst: amd64::Operand,
        span: crate::SourceSpan,
    ) {
        self.append_to_block(
            amd64::Inst::Mov {
                src: lhs,
                dst: amd64::Operand::Reg(amd64::Reg::Rax),
            },
            span,
        );
        self.append_to_block(amd64::Inst::Cdq, span);
        self.append_to_block(amd64::Inst::Idiv(rhs), span);
        self.append_to_block(
            amd64::Inst::Mov {
                src: if div {
                    amd64::Operand::Reg(amd64::Reg::Rax)
                } else {
                    amd64::Operand::Reg(amd64::Reg::Rdx)
                },
                dst,
            },
            span,
        );
    }
}
