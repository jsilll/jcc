use source_file::SourceSpan;

use std::collections::HashMap;

use super::{BinaryOp, Block, BlockRef, CondCode, FnDef, Inst, Operand, Program, Reg, UnaryOp};

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64Builder<'a> {
    tacky: &'a crate::Program,
}

impl<'a> AMD64Builder<'a> {
    pub fn new(tacky: &'a crate::Program) -> Self {
        Self { tacky }
    }

    pub fn build(self) -> Program {
        Program(AMD64FnDefBuilder::new().build(&self.tacky.0))
    }
}

// ---------------------------------------------------------------------------
// AMD64FnDefBuilder
// ---------------------------------------------------------------------------

struct AMD64FnDefBuilder {
    fn_def: FnDef,
    block: BlockRef,
    block_map: HashMap<crate::BlockRef, BlockRef>,
}

impl AMD64FnDefBuilder {
    fn new() -> Self {
        let mut fn_def = FnDef::default();
        let block = fn_def.push_block(Block::default());
        Self {
            block,
            fn_def,
            block_map: HashMap::new(),
        }
    }

    fn build(mut self, fn_def: &crate::FnDef) -> FnDef {
        self.fn_def.span = fn_def.span;
        self.append_to_block(Inst::Alloca(0), fn_def.span);

        let mut iter = fn_def.blocks_iter_both();
        if let Some((block_ref, block)) = iter.next() {
            self.block_map.insert(block_ref, self.block);
            block.instrs_iter_both().for_each(|(instr, span)| {
                self.build_from_inst(instr, span);
            });
        }
        iter.for_each(|(block_ref, block)| {
            self.block = self.get_or_create_block(block_ref);
            block.instrs_iter_both().for_each(|(instr, span)| {
                self.build_from_inst(instr, span);
            });
        });

        self.block_map
            .into_iter()
            .for_each(|(tacky_ref, block_ref)| {
                let tacky_block = fn_def.get_block(tacky_ref);
                if let Some(label) = tacky_block.label {
                    let block = self.fn_def.get_block_mut(block_ref);
                    block.label = Some(label);
                }
            });

        self.fn_def
    }

    fn append_to_block(&mut self, instr: Inst, span: SourceSpan) {
        let block = self.fn_def.get_block_mut(self.block);
        block.instrs.push(instr);
        block.spans.push(span);
    }

    fn get_or_create_block(&mut self, block: crate::BlockRef) -> BlockRef {
        *self
            .block_map
            .entry(block)
            .or_insert_with(|| self.fn_def.push_block(Block::default()))
    }

    fn build_from_inst(&mut self, instr: &crate::Inst, span: &SourceSpan) {
        match instr {
            crate::Inst::Return(value) => {
                let src = Self::build_from_value(value);
                let dst = Operand::Reg(Reg::Rax);
                self.append_to_block(Inst::Mov { src, dst }, *span);
                self.append_to_block(Inst::Ret, *span);
            }
            crate::Inst::Jump(target) => {
                let target = self.get_or_create_block(*target);
                self.append_to_block(Inst::Jmp(target), *span);
            }
            crate::Inst::Copy { src, dst } => {
                let src = Self::build_from_value(src);
                let dst = Self::build_from_value(dst);
                self.append_to_block(Inst::Mov { src, dst }, *span);
            }
            crate::Inst::JumpIfZero { cond, target }
            | crate::Inst::JumpIfNotZero { cond, target } => {
                let cond = Self::build_from_value(cond);
                let target = self.get_or_create_block(*target);
                self.append_to_block(
                    Inst::Test {
                        lhs: cond,
                        rhs: cond,
                    },
                    *span,
                );
                self.append_to_block(
                    match instr {
                        crate::Inst::JumpIfZero { .. } => Inst::JmpCC {
                            cond_code: CondCode::Equal,
                            target,
                        },
                        crate::Inst::JumpIfNotZero { .. } => Inst::JmpCC {
                            cond_code: CondCode::NotEqual,
                            target,
                        },
                        _ => unreachable!(),
                    },
                    *span,
                );
            }
            crate::Inst::Unary { op, src, dst } => match op {
                crate::UnaryOp::Neg => {
                    self.build_unary_instr(UnaryOp::Neg, src, dst, span);
                }
                crate::UnaryOp::Inc => {
                    self.build_unary_instr(UnaryOp::Inc, src, dst, span);
                }
                crate::UnaryOp::Dec => {
                    self.build_unary_instr(UnaryOp::Dec, src, dst, span);
                }
                crate::UnaryOp::BitNot => {
                    self.build_unary_instr(UnaryOp::Not, src, dst, span);
                }
                crate::UnaryOp::Not => {
                    let src = Self::build_from_value(src);
                    let dst = Self::build_from_value(dst);
                    self.append_to_block(
                        Inst::Mov {
                            src: Operand::Imm(0),
                            dst,
                        },
                        *span,
                    );
                    self.append_to_block(
                        Inst::Cmp {
                            lhs: Operand::Imm(0),
                            rhs: src,
                        },
                        *span,
                    );
                    self.append_to_block(
                        Inst::SetCC {
                            cond_code: CondCode::Equal,
                            dst,
                        },
                        *span,
                    );
                }
            },
            crate::Inst::Binary { op, lhs, rhs, dst } => {
                let lhs = Self::build_from_value(lhs);
                let rhs = Self::build_from_value(rhs);
                let dst = Self::build_from_value(dst);
                match op {
                    crate::BinaryOp::Add => {
                        self.build_binary_instr(BinaryOp::Add, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::Sub => {
                        self.build_binary_instr(BinaryOp::Sub, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::Mul => {
                        self.build_binary_instr(BinaryOp::Mul, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::BitOr => {
                        self.build_binary_instr(BinaryOp::Or, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::BitAnd => {
                        self.build_binary_instr(BinaryOp::And, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::BitXor => {
                        self.build_binary_instr(BinaryOp::Xor, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::BitShl => {
                        self.build_binary_instr(BinaryOp::Shl, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::BitShr => {
                        self.build_binary_instr(BinaryOp::Shr, lhs, rhs, dst, span);
                    }
                    crate::BinaryOp::Div | crate::BinaryOp::Rem => {
                        self.append_to_block(
                            Inst::Mov {
                                src: lhs,
                                dst: Operand::Reg(Reg::Rax),
                            },
                            *span,
                        );
                        self.append_to_block(Inst::Cdq, *span);
                        self.append_to_block(Inst::Idiv(rhs), *span);
                        self.append_to_block(
                            Inst::Mov {
                                src: match op {
                                    crate::BinaryOp::Div => Operand::Reg(Reg::Rax),
                                    crate::BinaryOp::Rem => Operand::Reg(Reg::Rdx),
                                    _ => unreachable!(),
                                },
                                dst,
                            },
                            *span,
                        );
                    }
                    crate::BinaryOp::Equal
                    | crate::BinaryOp::NotEqual
                    | crate::BinaryOp::LessThan
                    | crate::BinaryOp::LessEqual
                    | crate::BinaryOp::GreaterThan
                    | crate::BinaryOp::GreaterEqual => {
                        // TODO: extract this to a function and remove the expect
                        self.append_to_block(Inst::Cmp { lhs: rhs, rhs: lhs }, *span);
                        self.append_to_block(
                            Inst::Mov {
                                src: Operand::Imm(0),
                                dst,
                            },
                            *span,
                        );
                        self.append_to_block(
                            Inst::SetCC {
                                cond_code: CondCode::try_from(*op)
                                    .expect("unexpected binary operator"),
                                dst,
                            },
                            *span,
                        );
                    }
                }
            }
        }
    }

    fn build_unary_instr(
        &mut self,
        op: UnaryOp,
        src: &crate::Value,
        dst: &crate::Value,
        span: &SourceSpan,
    ) {
        let src = Self::build_from_value(src);
        let dst = Self::build_from_value(dst);
        self.append_to_block(Inst::Mov { src, dst }, *span);
        self.append_to_block(Inst::Unary { op, dst }, *span);
    }

    fn build_binary_instr(
        &mut self,
        op: BinaryOp,
        lhs: Operand,
        rhs: Operand,
        dst: Operand,
        span: &SourceSpan,
    ) {
        self.append_to_block(Inst::Mov { src: lhs, dst }, *span);
        self.append_to_block(Inst::Binary { op, src: rhs, dst }, *span);
    }

    fn build_from_value(value: &crate::Value) -> Operand {
        match value {
            crate::Value::Const(c) => Operand::Imm(*c),
            crate::Value::Variable(id) => Operand::Pseudo(*id),
        }
    }
}
