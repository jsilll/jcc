use jcc_interner::{Interner, Symbol};
use jcc_sourcemap::SourceSpan;

use crate::{Block, BlockRef, FuncRef, Inst, InstRef, Program};

// ---------------------------------------------------------------------------
// IRBuilder
// ---------------------------------------------------------------------------

pub struct IRBuilder<'p> {
    block: BlockRef,
    func: Option<FuncRef>,
    pub prog: Program<'p>,
}

impl<'p> IRBuilder<'p> {
    pub fn new(interner: &'p mut Interner) -> Self {
        Self {
            func: None,
            block: BlockRef::default(),
            prog: Program::new(interner),
        }
    }

    pub fn build(self) -> Program<'p> {
        self.prog
    }

    #[inline]
    pub fn block(&self) -> BlockRef {
        self.block
    }

    #[inline]
    pub fn func(&self) -> Option<FuncRef> {
        self.func
    }

    #[inline]
    pub fn clear_func(&mut self) {
        self.func = None;
    }

    #[inline]
    pub fn switch_to_func(&mut self, func: FuncRef) {
        self.func = Some(func);
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
        self.prog
            .func_mut(self.func.expect("expected a function"))
            .blocks
            .push(block);
        block
    }
}
