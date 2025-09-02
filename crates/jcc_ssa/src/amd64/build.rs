use crate::amd64::{
    BinaryOp, Block, BlockRef, CondCode, Func, Inst, InstIdx, Operand, Program, Reg, StaticVar,
    StaticVarRef, UnaryOp,
};

use std::collections::HashMap;

const ARG_REGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

pub struct Builder<'a> {
    ssa: &'a crate::Program<'a>,
    func: Func,
    amd64: Program,
    block: BlockRef,
    arg_count: u32,
    pseudo_count: u32,
    blocks: HashMap<crate::BlockRef, BlockRef>,
    operands: HashMap<crate::InstRef, Operand>,
}

impl<'a> Builder<'a> {
    pub fn new(ssa: &'a crate::Program) -> Self {
        Self {
            ssa,
            arg_count: 0,
            pseudo_count: 0,
            func: Func::default(),
            amd64: Program::default(),
            block: BlockRef::default(),
            blocks: HashMap::new(),
            operands: HashMap::new(),
        }
    }

    pub fn build(mut self) -> Program {
        self.ssa.iter_static_vars().for_each(|v| {
            self.amd64.new_static_var(StaticVar {
                name: v.name,
                init: v.init,
                span: v.span,
                is_global: v.is_global,
            });
        });
        self.ssa.iter_funcs().for_each(|f| {
            if f.blocks.is_empty() {
                return;
            }
            self.func.name = f.name;
            self.func.span = f.span;
            self.func.is_global = f.is_global;
            f.blocks.iter().enumerate().for_each(|(idx, b)| {
                let block = self.ssa.block(*b);
                self.block = self.get_or_make_block(*b);
                let _ = self.func.block_mut(self.block).label.insert(block.name);
                if idx == 0 {
                    self.insert_inst(Inst::alloca(0, f.span));
                }
                block.insts.iter().for_each(|inst| {
                    self.visit_inst(*inst);
                });
            });
            self.amd64.funcs.push(std::mem::take(&mut self.func));
            self.arg_count = 0;
            self.pseudo_count = 0;
            self.blocks.clear();
            self.operands.clear();
        });
        self.amd64
    }

    fn visit_inst(&mut self, inst_ref: crate::InstRef) {
        let inst = self.ssa.inst(inst_ref);
        match inst.kind {
            crate::InstKind::Select { .. } => todo!("handle select"),
            crate::InstKind::Nop | crate::InstKind::Phi => {}
            crate::InstKind::Alloca => {
                let dst = self.make_pseudo();
                self.operands.insert(inst_ref, dst);
            }
            crate::InstKind::Arg => {
                let dst = self.make_pseudo();
                self.operands.insert(inst_ref, dst);
                if let Some(&reg) = ARG_REGS.get(self.arg_count as usize) {
                    self.insert_inst(Inst::mov(Operand::Reg(reg), dst, inst.span));
                } else {
                    let n_stack_arg = self.arg_count - ARG_REGS.len() as u32;
                    let offset = 16 + n_stack_arg * 8;
                    self.insert_inst(Inst::mov(Operand::Stack(offset as i32), dst, inst.span));
                }
                self.arg_count += 1;
            }
            crate::InstKind::Jump(block) => {
                let block = self.get_or_make_block(block);
                self.insert_inst(Inst::jmp(block, inst.span));
            }
            crate::InstKind::Const(c) => {
                self.operands.insert(inst_ref, Operand::Imm(c));
            }
            crate::InstKind::Identity(val) => {
                self.operands.insert(inst_ref, self.get_operand(val));
            }
            crate::InstKind::Static(v) => {
                self.operands
                    .insert(inst_ref, Operand::Data(StaticVarRef(v.0)));
            }
            crate::InstKind::Load(ptr) => {
                let dst = self.make_pseudo();
                self.operands.insert(inst_ref, dst);
                self.insert_inst(Inst::mov(self.get_operand(ptr), dst, inst.span));
            }
            crate::InstKind::Ret(val) => {
                let src = self.get_operand(val);
                self.insert_inst(Inst::mov(src, Operand::Reg(Reg::Rax), inst.span));
                self.insert_inst(Inst::ret(inst.span));
            }
            crate::InstKind::Store { ptr, val } => {
                let src = self.get_operand(val);
                let dst = self.get_operand(ptr);
                self.insert_inst(Inst::mov(src, dst, inst.span));
            }
            crate::InstKind::Upsilon { phi, val } => {
                let src = self.get_operand(val);
                let dst = self.get_or_make_operand(phi);
                self.insert_inst(Inst::mov(src, dst, inst.span));
            }
            crate::InstKind::Branch { cond, then, other } => {
                let cond = self.get_operand(cond);
                let then = self.get_or_make_block(then);
                let other = self.get_or_make_block(other);
                self.insert_inst(Inst::test(cond, cond, inst.span));
                self.insert_inst(Inst::jmpcc(CondCode::NotEqual, then, inst.span));
                self.insert_inst(Inst::jmp(other, inst.span));
            }
            crate::InstKind::Switch {
                cond,
                default,
                ref cases,
            } => {
                let cond = self.get_operand(cond);
                cases.iter().for_each(|(v, block)| {
                    let block = self.get_or_make_block(*block);
                    self.insert_inst(Inst::cmp(cond, Operand::Imm(*v), inst.span));
                    self.insert_inst(Inst::jmpcc(CondCode::Equal, block, inst.span));
                });
                let default = self.get_or_make_block(default);
                self.insert_inst(Inst::jmp(default, inst.span));
            }
            crate::InstKind::Call { func, ref args } => {
                let stack_args = args.len().saturating_sub(ARG_REGS.len());
                let stack_padding = if stack_args % 2 == 0 {
                    0
                } else {
                    self.insert_inst(Inst::alloca(8, inst.span));
                    8
                };
                for (reg, arg) in ARG_REGS.iter().zip(args.iter()) {
                    let src = self.get_operand(*arg);
                    self.insert_inst(Inst::mov(src, Operand::Reg(*reg), inst.span));
                }
                for arg in args.get(ARG_REGS.len()..).unwrap_or(&[]).iter().rev() {
                    let arg = self.get_operand(*arg);
                    match arg {
                        Operand::Imm(_) | Operand::Reg(_) => {
                            self.insert_inst(Inst::push(arg, inst.span));
                        }
                        _ => {
                            self.insert_inst(Inst::mov(arg, Operand::Reg(Reg::Rax), inst.span));
                            self.insert_inst(Inst::push(Operand::Reg(Reg::Rax), inst.span));
                        }
                    }
                }
                let name = self.ssa.func(func).name;
                self.insert_inst(Inst::call(name, inst.span));
                let stack_size = stack_args as i32 * 8 + stack_padding;
                if stack_size > 0 {
                    self.insert_inst(Inst::dealloca(stack_size, inst.span));
                }
                let dst = self.make_pseudo();
                self.operands.insert(inst_ref, dst);
                self.insert_inst(Inst::mov(Operand::Reg(Reg::Rax), dst, inst.span));
            }
            crate::InstKind::Unary { op, val } => {
                let dst = self.make_pseudo();
                let src = self.get_operand(val);
                self.operands.insert(inst_ref, dst);
                match op {
                    crate::UnaryOp::Not => self.build_unary(UnaryOp::Not, src, dst, inst.span),
                    crate::UnaryOp::Neg => self.build_unary(UnaryOp::Neg, src, dst, inst.span),
                    crate::UnaryOp::Inc => self.build_unary(UnaryOp::Inc, src, dst, inst.span),
                    crate::UnaryOp::Dec => self.build_unary(UnaryOp::Dec, src, dst, inst.span),
                }
            }
            crate::InstKind::Binary { op, lhs, rhs } => {
                let dst = self.make_pseudo();
                let lhs = self.get_operand(lhs);
                let rhs = self.get_operand(rhs);
                self.operands.insert(inst_ref, dst);
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
        }
    }

    // ---------------------------------------------------------------------------
    // Factory methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn build_unary(&mut self, op: UnaryOp, src: Operand, dst: Operand, span: crate::SourceSpan) {
        self.insert_inst(Inst::mov(src, dst, span));
        self.insert_inst(Inst::unary(op, dst, span));
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
        self.insert_inst(Inst::mov(lhs, dst, span));
        self.insert_inst(Inst::binary(op, rhs, dst, span));
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
        self.insert_inst(Inst::mov(Operand::Imm(0), dst, span));
        self.insert_inst(Inst::cmp(rhs, lhs, span));
        self.insert_inst(Inst::setcc(cond_code, dst, span));
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
        self.insert_inst(Inst::mov(lhs, Operand::Reg(Reg::Rax), span));
        self.insert_inst(Inst::cdq(span));
        self.insert_inst(Inst::idiv(rhs, span));
        self.insert_inst(Inst::mov(
            if div {
                Operand::Reg(Reg::Rax)
            } else {
                Operand::Reg(Reg::Rdx)
            },
            dst,
            span,
        ));
    }

    // ---------------------------------------------------------------------------
    // Auxiliary methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn make_pseudo(&mut self) -> Operand {
        let tmp = Operand::Pseudo(self.pseudo_count);
        self.pseudo_count += 1;
        tmp
    }

    #[inline]
    fn insert_inst(&mut self, mut inst: Inst) {
        let len = self.func.block(self.block).insts.len();
        inst.idx = InstIdx(len as u32);
        let inst = self.func.new_inst(inst);
        let block = self.func.block_mut(self.block);
        block.insts.push(inst);
    }

    #[inline]
    fn get_operand(&self, inst: crate::InstRef) -> Operand {
        *self.operands.get(&inst).expect("expected a variable")
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
            .or_insert_with(|| self.func.new_block(Block::default()))
    }
}
