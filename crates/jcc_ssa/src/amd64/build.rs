use jcc_sourcemap::SourceSpan;

use crate::{
    amd64::{
        BinaryOp, Block, BlockRef, CondCode, Func, FuncEntry, Inst, InstIdx, Operand, Program,
        PseudoEntry, Reg, SymbolTable, Type, UnaryOp,
    },
    ir,
};

use std::collections::HashMap;

const ARG_REGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];

// ---------------------------------------------------------------------------
// BuilderResult
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct BuilderResult {
    pub program: Program,
    pub table: SymbolTable,
}

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

pub struct Builder<'a> {
    ssa: &'a ir::Program<'a>,
    func: Func,
    block: BlockRef,
    arg_count: u32,
    pseudo_count: u32,
    result: BuilderResult,
    blocks: HashMap<ir::BlockRef, BlockRef>,
    operands: HashMap<ir::InstRef, Operand>,
}

impl<'a> Builder<'a> {
    pub fn new(ssa: &'a ir::Program) -> Self {
        Self {
            ssa,
            arg_count: 0,
            pseudo_count: 0,
            func: Func::default(),
            blocks: HashMap::new(),
            operands: HashMap::new(),
            block: BlockRef::default(),
            result: BuilderResult::default(),
        }
    }

    pub fn build(mut self) -> BuilderResult {
        self.ssa.iter_static_vars().for_each(|v| {
            self.result.program.new_static_var(v.clone());
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
                    self.insert_inst(Inst::alloc_stack(0, f.span))
                }
                block.insts.iter().for_each(|inst| {
                    self.visit_inst(*inst);
                });
            });
            self.result
                .program
                .funcs
                .push(std::mem::take(&mut self.func));
            self.arg_count = 0;
            self.blocks.clear();
            self.operands.clear();
            self.result.table.funcs.insert(
                f.name,
                FuncEntry {
                    frame_size: 0, // TODO
                    is_static: !f.is_global,
                },
            );
        });
        self.result
    }

    fn visit_inst(&mut self, inst_ref: ir::InstRef) {
        let inst = self.ssa.inst(inst_ref);
        match inst.kind {
            ir::InstKind::Nop | ir::InstKind::Phi => {}
            ir::InstKind::Select { .. } => todo!("handle select"),
            ir::InstKind::Alloca(ty) => {
                let dst = self.make_pseudo(ty.try_into().unwrap());
                self.operands.insert(inst_ref, dst);
            }
            ir::InstKind::Arg => {
                let ty = inst.ty.try_into().unwrap();
                let dst = self.make_pseudo(inst.ty.try_into().unwrap());
                self.operands.insert(inst_ref, dst);
                match ARG_REGS.get(self.arg_count as usize) {
                    Some(&reg) => {
                        self.insert_inst(Inst::mov(ty, Operand::Reg(reg), dst, inst.span));
                    }
                    None => {
                        let n_stack_arg = self.arg_count - ARG_REGS.len() as u32;
                        let offset = (16 + n_stack_arg * 8) as i64;
                        self.insert_inst(Inst::mov(ty, Operand::Stack(offset), dst, inst.span));
                    }
                }
                self.arg_count += 1;
            }
            ir::InstKind::Jump(block) => {
                let block = self.get_or_make_block(block);
                self.insert_inst(Inst::jmp(block, inst.span));
            }
            ir::InstKind::Const(ir::ConstValue::Int64(c)) => {
                self.operands.insert(inst_ref, Operand::Imm(c));
            }
            ir::InstKind::Const(ir::ConstValue::Int32(c)) => {
                self.operands.insert(inst_ref, Operand::Imm(c as i64));
            }
            ir::InstKind::Identity(val) => {
                self.operands.insert(inst_ref, self.get_operand(val));
            }
            ir::InstKind::Static(v) => {
                self.operands
                    .insert(inst_ref, Operand::Data(ir::StaticVarRef(v.0)));
            }
            ir::InstKind::SignExtend(val) => {
                let dst = self.make_pseudo(Type::Quad);
                let src = self.get_operand(val);
                self.operands.insert(inst_ref, dst);
                self.insert_inst(Inst::movsx(src, dst, inst.span));
            }
            ir::InstKind::Truncate(val) => {
                let dst = self.make_pseudo(Type::Long);
                let src = self.get_operand(val);
                self.operands.insert(inst_ref, dst);
                self.insert_inst(Inst::mov(Type::Long, src, dst, inst.span));
            }
            ir::InstKind::Load(ptr) => {
                let ty = inst.ty.try_into().unwrap();
                let dst = self.make_pseudo(ty);
                self.operands.insert(inst_ref, dst);
                self.insert_inst(Inst::mov(ty, self.get_operand(ptr), dst, inst.span));
            }
            ir::InstKind::Ret(val) => {
                let ty = self.ssa.inst(val).ty;
                let src = self.get_operand(val);
                self.insert_inst(Inst::mov(
                    ty.try_into().unwrap(),
                    src,
                    Operand::Reg(Reg::Rax),
                    inst.span,
                ));
                self.insert_inst(Inst::ret(inst.span));
            }
            ir::InstKind::Store { ptr, val } => {
                let ty = self.ssa.inst(val).ty;
                let src = self.get_operand(val);
                let dst = self.get_operand(ptr);
                self.insert_inst(Inst::mov(ty.try_into().unwrap(), src, dst, inst.span));
            }
            ir::InstKind::Upsilon { phi, val } => {
                let src = self.get_operand(val);
                let dst = self.get_or_make_pseudo(phi, inst.ty.try_into().unwrap());
                self.insert_inst(Inst::mov(inst.ty.try_into().unwrap(), src, dst, inst.span));
            }
            ir::InstKind::Branch { cond, then, other } => {
                let ty = self.ssa.inst(cond).ty.try_into().unwrap();
                let cond = self.get_operand(cond);
                let then = self.get_or_make_block(then);
                let other = self.get_or_make_block(other);
                self.insert_inst(Inst::test(ty, cond, cond, inst.span));
                self.insert_inst(Inst::jmpcc(CondCode::Ne, then, inst.span));
                self.insert_inst(Inst::jmp(other, inst.span));
            }
            ir::InstKind::Switch {
                cond,
                default,
                ref cases,
            } => {
                let oper = self.get_operand(cond);
                let ty = self.ssa.inst(cond).ty.try_into().unwrap();
                cases.iter().for_each(|(v, block)| {
                    let v = match v {
                        ir::ConstValue::Int64(v) => *v,
                        ir::ConstValue::Int32(v) => *v as i64,
                    };
                    let block = self.get_or_make_block(*block);
                    self.insert_inst(Inst::cmp(ty, oper, Operand::Imm(v), inst.span));
                    self.insert_inst(Inst::jmpcc(CondCode::Eq, block, inst.span));
                });
                let default = self.get_or_make_block(default);
                self.insert_inst(Inst::jmp(default, inst.span));
            }
            ir::InstKind::Unary { op, val } => {
                let dst = self.make_pseudo(inst.ty.try_into().unwrap());
                let src = self.get_operand(val);
                self.operands.insert(inst_ref, dst);
                let val_ty = self.ssa.inst(val).ty.try_into().unwrap();
                match op {
                    ir::UnaryOp::Not => {
                        self.build_unary(val_ty, UnaryOp::Not, src, dst, inst.span);
                    }
                    ir::UnaryOp::Neg => self.build_unary(val_ty, UnaryOp::Neg, src, dst, inst.span),
                    ir::UnaryOp::Inc => self.build_unary(val_ty, UnaryOp::Inc, src, dst, inst.span),
                    ir::UnaryOp::Dec => self.build_unary(val_ty, UnaryOp::Dec, src, dst, inst.span),
                }
            }
            ir::InstKind::Binary { op, lhs, rhs } => {
                let dst_ty = inst.ty.try_into().unwrap();
                let lhs_ty = self.ssa.inst(lhs).ty.try_into().unwrap();
                let dst = self.make_pseudo(dst_ty);
                let lhs = self.get_operand(lhs);
                let rhs = self.get_operand(rhs);
                self.operands.insert(inst_ref, dst);
                match op {
                    ir::BinaryOp::Div => {
                        self.build_div_or_rem(true, lhs_ty, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Rem => {
                        self.build_div_or_rem(false, lhs_ty, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Or => {
                        self.build_binary(lhs_ty, BinaryOp::Or, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::And => {
                        self.build_binary(lhs_ty, BinaryOp::And, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Xor => {
                        self.build_binary(lhs_ty, BinaryOp::Xor, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Shl => {
                        self.build_binary(lhs_ty, BinaryOp::Shl, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Shr => {
                        self.build_binary(lhs_ty, BinaryOp::Shr, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Add => {
                        self.build_binary(lhs_ty, BinaryOp::Add, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Sub => {
                        self.build_binary(lhs_ty, BinaryOp::Sub, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Mul => {
                        self.build_binary(lhs_ty, BinaryOp::Mul, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::Equal => {
                        self.build_cmp((lhs_ty, dst_ty), CondCode::Eq, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::NotEqual => {
                        self.build_cmp((lhs_ty, dst_ty), CondCode::Ne, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::LessThan => {
                        self.build_cmp((lhs_ty, dst_ty), CondCode::Lt, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::LessEqual => {
                        self.build_cmp((lhs_ty, dst_ty), CondCode::Le, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::GreaterThan => {
                        self.build_cmp((lhs_ty, dst_ty), CondCode::Gt, lhs, rhs, dst, inst.span)
                    }
                    ir::BinaryOp::GreaterEqual => {
                        self.build_cmp((lhs_ty, dst_ty), CondCode::Ge, lhs, rhs, dst, inst.span)
                    }
                }
            }
            ir::InstKind::Call { func, ref args } => {
                let stack_args = args.len().saturating_sub(ARG_REGS.len());
                let stack_padding = if stack_args % 2 == 0 {
                    0
                } else {
                    self.insert_inst(Inst::alloc_stack(8, inst.span));
                    8
                };
                for (reg, arg) in ARG_REGS.iter().zip(args.iter()) {
                    let ty = self.ssa.inst(*arg).ty;
                    let src = self.get_operand(*arg);
                    self.insert_inst(Inst::mov(
                        ty.try_into().unwrap(),
                        src,
                        Operand::Reg(*reg),
                        inst.span,
                    ));
                }
                for arg in args.get(ARG_REGS.len()..).unwrap_or(&[]).iter().rev() {
                    let oper = self.get_operand(*arg);
                    match oper {
                        Operand::Imm(_) | Operand::Reg(_) => {
                            self.insert_inst(Inst::push(oper, inst.span));
                        }
                        oper if matches!(self.ssa.inst(*arg).ty, ir::Ty::Int64) => {
                            self.insert_inst(Inst::push(oper, inst.span));
                        }
                        _ => {
                            let dst = Operand::Reg(Reg::Rax);
                            self.insert_inst(Inst::mov(Type::Long, oper, dst, inst.span));
                            self.insert_inst(Inst::push(dst, inst.span));
                        }
                    }
                }
                let name = self.ssa.func(func).name;
                self.insert_inst(Inst::call(name, inst.span));
                let stack_size = stack_args as i64 * 8 + stack_padding;
                if stack_size > 0 {
                    self.insert_inst(Inst::dealloc_stack(stack_size, inst.span))
                }
                let dst = self.make_pseudo(inst.ty.try_into().unwrap());
                let ty = self.ssa.inst(inst_ref).ty.try_into().unwrap();
                self.operands.insert(inst_ref, dst);
                self.insert_inst(Inst::mov(ty, Operand::Reg(Reg::Rax), dst, inst.span));
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Factory methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn build_unary(&mut self, ty: Type, op: UnaryOp, src: Operand, dst: Operand, span: SourceSpan) {
        self.insert_inst(Inst::mov(ty, src, dst, span));
        self.insert_inst(Inst::unary(ty, op, dst, span));
    }

    #[inline]
    fn build_binary(
        &mut self,
        ty: Type,
        op: BinaryOp,
        lhs: Operand,
        rhs: Operand,
        dst: Operand,
        span: SourceSpan,
    ) {
        self.insert_inst(Inst::mov(ty, lhs, dst, span));
        self.insert_inst(Inst::binary(ty, op, rhs, dst, span));
    }

    #[inline]
    fn build_cmp(
        &mut self,
        tys: (Type, Type),
        cond_code: CondCode,
        lhs: Operand,
        rhs: Operand,
        dst: Operand,
        span: SourceSpan,
    ) {
        self.insert_inst(Inst::cmp(tys.0, rhs, lhs, span));
        self.insert_inst(Inst::mov(tys.1, Operand::Imm(0), dst, span));
        self.insert_inst(Inst::setcc(cond_code, dst, span));
    }

    #[inline]
    fn build_div_or_rem(
        &mut self,
        div: bool,
        ty: Type,
        lhs: Operand,
        rhs: Operand,
        dst: Operand,
        span: SourceSpan,
    ) {
        self.insert_inst(Inst::mov(ty, lhs, Operand::Reg(Reg::Rax), span));
        self.insert_inst(Inst::cdq(ty, span));
        self.insert_inst(Inst::idiv(ty, rhs, span));
        self.insert_inst(Inst::mov(
            ty,
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
    fn make_pseudo(&mut self, ty: Type) -> Operand {
        self.result.table.pseudos.push(PseudoEntry {
            ty,
            is_static: false,
        });
        let pseudo = Operand::Pseudo(self.pseudo_count);
        self.pseudo_count += 1;
        pseudo
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
    fn get_operand(&self, inst: ir::InstRef) -> Operand {
        *self.operands.get(&inst).expect("expected a variable")
    }

    #[inline]
    fn get_or_make_pseudo(&mut self, inst: ir::InstRef, ty: Type) -> Operand {
        *self.operands.entry(inst).or_insert_with(|| {
            self.result.table.pseudos.push(PseudoEntry {
                ty,
                is_static: false,
            });
            let pseudo = Operand::Pseudo(self.pseudo_count);
            self.pseudo_count += 1;
            pseudo
        })
    }

    #[inline]
    fn get_or_make_block(&mut self, block: ir::BlockRef) -> BlockRef {
        *self
            .blocks
            .entry(block)
            .or_insert_with(|| self.func.new_block(Block::default()))
    }
}
