use std::collections::HashMap;

use tacky::amd64;

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64Builder<'a> {
    ssa: &'a crate::Program,
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

    fn visit_inst(&mut self, i: crate::InstRef) {
        let inst = self.ssa.inst(i);
        let span = *self.ssa.inst_span(i);
        match inst.kind {
            crate::InstKind::Const(c) => {
                self.variables.insert(i, amd64::Operand::Imm(c));
            }
            crate::InstKind::Nop => todo!(),
            crate::InstKind::Phi => todo!(),
            crate::InstKind::Arg => todo!(),
            crate::InstKind::Alloca => {
                self.variables.insert(i, amd64::Operand::Stack(0));
            }
            crate::InstKind::Ret(val) => {
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.variables.get(&val).expect("expected a value").clone(),
                        dst: amd64::Operand::Reg(amd64::Reg::Rax),
                    },
                    span,
                );
                self.append_to_block(amd64::Inst::Ret, span);
            }
            crate::InstKind::Jump(_) => todo!(),
            crate::InstKind::Load(ptr) => {
                let dst = amd64::Operand::Pseudo(0);
                self.variables.insert(i, dst);
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self
                            .variables
                            .get(&ptr)
                            .expect("expected a pointer")
                            .clone(),
                        dst,
                    },
                    span,
                );
            }
            crate::InstKind::Identity(_) => todo!(),
            crate::InstKind::Store { ptr, val } => {
                self.append_to_block(
                    amd64::Inst::Mov {
                        src: self.variables.get(&val).expect("expected a value").clone(),
                        dst: self
                            .variables
                            .get(&ptr)
                            .expect("expected a pointer")
                            .clone(),
                    },
                    span,
                );
            }
            crate::InstKind::Upsilon { .. } => todo!(),
            crate::InstKind::Unary { .. } => todo!(),
            crate::InstKind::Binary { .. } => todo!(),
            crate::InstKind::Branch { .. } => todo!(),
        }
    }

    fn append_to_block(&mut self, instr: tacky::amd64::Inst, span: crate::SourceSpan) {
        let block = self.prog.0.get_block_mut(self.block);
        block.instrs.push(instr);
        block.spans.push(span);
    }
}
