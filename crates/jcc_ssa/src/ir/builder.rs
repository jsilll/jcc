use jcc_codemap::span::Span;

use crate::{
    ir::{inst::Inst, ty::Ty, Block, BlockData, Function, Program, Value, ValueData},
    Ident, IdentInterner,
};

pub struct Builder<'a> {
    pub program: Program,
    pub interner: &'a mut IdentInterner,
    pub block: Option<Block>,
    pub function: Option<Function>,
}

impl<'a> Builder<'a> {
    pub fn new(interner: &'a mut IdentInterner) -> Self {
        Self {
            interner,
            block: None,
            function: None,
            program: Program::default(),
        }
    }

    pub fn finish(self) -> Program {
        self.program
    }

    /// Pushes a value into the current block.
    pub fn push(&mut self, value: Value) {
        if let Some(block) = self.block {
            let block_data = &mut self.program.blocks[block];
            let index = block_data.insts.len() as u32;
            block_data.insts.push(value);

            let value = &mut self.program.values[value];
            value.index = index;
            value.block = block;
        }
    }

    /// Builds a phi node but doesn't insert it into the current block.
    pub fn build_phi(&mut self, ty: Ty, span: Span) -> Value {
        let inst = Inst::phi(ty);
        let block = self.block.expect("no current block");
        let data = ValueData {
            inst,
            span,
            block,
            index: Default::default(),
        };
        self.program.values.push(data)
    }

    /// Creates a new value in the current block.
    pub fn build_val(&mut self, inst: Inst, span: Span) -> Value {
        let block = self.block.expect("no current block");
        let index = self.program.blocks[block].insts.len() as u32;
        let value = self.program.values.push(ValueData {
            inst,
            span,
            block,
            index,
        });
        self.program.blocks[block].insts.push(value);
        value
    }

    /// Builds a new block in the current function.
    pub fn build_block(&mut self, name: &str, span: Span) -> Block {
        let sym = self.interner.intern(name);
        self.build_block_sym(sym, span)
    }

    /// Builds a new block in the current function.
    pub fn build_block_sym(&mut self, name: Ident, span: Span) -> Block {
        let function = self.function.expect("no current function");
        let block = self.program.blocks.push(BlockData {
            span,
            insts: Vec::new(),
            preds: Vec::new(),
            dom_parent: None,
            dom_children: Vec::new(),
            name,
        });
        self.program.functions[function].blocks.push(block);
        block
    }
}
