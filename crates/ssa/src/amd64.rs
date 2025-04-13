use tacky::amd64;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64Builder<'a> {
    ssa: &'a crate::Program,
    stack_size: u32,
    pseudo_count: u32,
    prog: amd64::Program,
    block: amd64::BlockRef,
    variables: HashMap<crate::InstRef, amd64::Operand>,
}

impl<'a> AMD64Builder<'a> {
    pub fn new(ssa: &'a crate::Program) -> Self {
        let mut prog = amd64::Program::default();
        let block = prog.0.push_block(amd64::Block::default());
        Self {
            ssa,
            prog,
            block,
            stack_size: 0,
            pseudo_count: 0,
            variables: HashMap::new(),
        }
    }

    pub fn build(mut self) -> tacky::amd64::Program {
        for (f, func) in self.ssa.funcs_iter2() {
            self.append_to_block(amd64::Inst::Alloca(0), *self.ssa.func_span(f));
            for idx in 0..func.blocks.len() {
                for inst in &self.ssa.block(func.blocks[idx]).insts {
                    self.visit_inst(*inst);
                }
            }
        }
        self.prog
    }

    #[inline]
    fn make_pseudo(&mut self) -> amd64::Operand {
        let tmp = amd64::Operand::Pseudo(self.pseudo_count);
        self.pseudo_count += 1;
        tmp
    }

    #[inline]
    fn get_variable(&self, inst: &crate::InstRef) -> amd64::Operand {
        self.variables
            .get(&inst)
            .expect("expected a variable")
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
        match inst.kind {
            crate::InstKind::Nop => {}
            crate::InstKind::Arg => todo!("handle args"),
            crate::InstKind::Phi => todo!("handle phis"),
            crate::InstKind::Jump(_) => todo!("handle jumps"),
            crate::InstKind::Branch { .. } => todo!("handle branches"),
            crate::InstKind::Upsilon { .. } => todo!("handle upsilons"),
            crate::InstKind::Const(c) => {
                self.variables.insert(i, amd64::Operand::Imm(c));
            }
            crate::InstKind::Identity(val) => {
                self.variables.insert(i, self.get_variable(&val));
            }
            crate::InstKind::Alloca => {
                self.variables
                    .insert(i, amd64::Operand::Stack(self.stack_size));
                self.stack_size += inst.ty.size();
            }
            crate::InstKind::Ret(val) => {
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.get_variable(&val),
                        dst: amd64::Operand::Reg(amd64::Reg::Rax),
                    },
                    span,
                );
                self.append_to_block(amd64::Inst::Ret, span);
            }
            crate::InstKind::Load(ptr) => {
                let dst = self.make_pseudo();
                self.variables.insert(i, dst);
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.get_variable(&ptr),
                        dst,
                    },
                    span,
                );
            }
            crate::InstKind::Store { ptr, val } => {
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.get_variable(&val),
                        dst: self.get_variable(&ptr),
                    },
                    span,
                );
            }
            crate::InstKind::Unary { op, val } => {
                let dst = self.make_pseudo();
                self.variables.insert(i, dst);
                let src = self.get_variable(&val);
                match op {
                    crate::UnaryOp::Not => self.build_unary(amd64::UnaryOp::Not, src, dst, span),
                    crate::UnaryOp::Neg => self.build_unary(amd64::UnaryOp::Neg, src, dst, span),
                    crate::UnaryOp::Inc => self.build_unary(amd64::UnaryOp::Inc, src, dst, span),
                    crate::UnaryOp::Dec => self.build_unary(amd64::UnaryOp::Dec, src, dst, span),
                }
            }
            crate::InstKind::Binary { op, lhs, rhs } => {
                let dst = self.make_pseudo();
                self.variables.insert(i, dst);
                let lhs = self.get_variable(&lhs);
                let rhs = self.get_variable(&rhs);
                match op {
                    crate::BinaryOp::Add => {
                        self.build_binary(amd64::BinaryOp::Add, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Sub => {
                        self.build_binary(amd64::BinaryOp::Sub, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Mul => {
                        self.build_binary(amd64::BinaryOp::Mul, lhs, rhs, dst, span)
                    }
                    crate::BinaryOp::Div => self.build_div_or_rem(true, lhs, rhs, dst, span),
                    crate::BinaryOp::Rem => self.build_div_or_rem(false, lhs, rhs, dst, span),
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
                    crate::BinaryOp::Equal
                    | crate::BinaryOp::NotEqual
                    | crate::BinaryOp::LessThan
                    | crate::BinaryOp::LessEqual
                    | crate::BinaryOp::GreaterThan
                    | crate::BinaryOp::GreaterEqual => todo!("handle cmp"),
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
