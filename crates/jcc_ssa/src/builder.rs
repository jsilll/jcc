use std::num::NonZero;

use jcc_interner::{Interner, Symbol};
use jcc_sourcemap::SourceSpan;

use crate::{Block, BlockRef, FuncRef, Inst, InstRef, Program};

// ---------------------------------------------------------------------------
// IRBuilder
// ---------------------------------------------------------------------------

pub struct IRBuilder<'p> {
    func: FuncRef,
    block: BlockRef,
    pub prog: Program<'p>,
}

impl<'p> IRBuilder<'p> {
    pub fn new(interner: &'p mut Interner) -> Self {
        Self {
            prog: Program::new(interner),
            func: FuncRef(NonZero::new(u32::MAX).unwrap()),
            block: BlockRef(NonZero::new(u32::MAX).unwrap()),
        }
    }

    pub fn build(self) -> Program<'p> {
        self.prog
    }

    #[inline]
    pub fn func(&self) -> FuncRef {
        self.func
    }

    #[inline]
    pub fn block(&self) -> BlockRef {
        self.block
    }

    #[inline]
    pub fn switch_to_func(&mut self, func: FuncRef) {
        self.func = func;
    }

    #[inline]
    pub fn switch_to_block(&mut self, block: BlockRef) {
        self.block = block;
    }

    #[inline]
    pub fn insert_inst_ref(&mut self, inst: InstRef) {
        self.prog.block_mut(self.block).insts.push(inst);
    }

    #[inline]
    pub fn insert_inst(&mut self, mut inst: Inst) -> InstRef {
        inst.block = self.block;
        let inst_ref = self.prog.new_inst(inst);
        self.prog.block_mut(self.block).insts.push(inst_ref);
        inst_ref
    }

    #[inline]
    pub fn insert_inst_skip(&mut self, mut inst: Inst) -> InstRef {
        inst.block = self.block;
        self.prog.new_inst(inst)
    }

    #[inline]
    pub fn new_block(&mut self, name: &str, span: SourceSpan) -> BlockRef {
        let name = self.prog.interner.intern(name);
        self.new_block_interned(name, span)
    }

    #[inline]
    pub fn new_block_interned(&mut self, name: Symbol, span: SourceSpan) -> BlockRef {
        let block = self.prog.new_block(Block {
            name,
            span,
            ..Default::default()
        });
        self.prog.func_mut(self.func).blocks.push(block);
        block
    }
}
