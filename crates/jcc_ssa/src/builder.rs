// ---------------------------------------------------------------------------
// IRBuilder
// ---------------------------------------------------------------------------

/// A helper to build IR in a specific function and block.
pub struct IRBuilder<'p> {
    program: &'p mut Program,
    current_block: BlockRef,
}

impl<'p> IRBuilder<'p> {
    pub fn new(program: &'p mut Program, entry_block: BlockRef) -> Self {
        Self {
            program,
            current_block: entry_block,
        }
    }

    pub fn switch_to_block(&mut self, block: BlockRef) {
        self.current_block = block;
    }

    pub fn current_block(&self) -> BlockRef {
        self.current_block
    }

    /// Inserts the instruction at the current position.
    fn insert_inst(&mut self, mut inst: Inst) -> InstRef {
        inst.block = self.current_block;
        let inst_ref = self.program.new_inst(inst);
        self.program
            .block_mut(self.current_block)
            .insts
            .push(inst_ref);
        inst_ref
    }

    // --- Builder methods for instructions ---

    pub fn const_i32(&mut self, val: i64, span: SourceSpan) -> InstRef {
        self.insert_inst(Inst::const_i32(val, span))
    }

    pub fn ret(&mut self, val: InstRef, span: SourceSpan) -> InstRef {
        self.insert_inst(Inst::ret(val, span))
    }

    pub fn add(&mut self, lhs: InstRef, rhs: InstRef, span: SourceSpan) -> InstRef {
        let ty = self.program.inst(lhs).ty; // Assume same type for now
        self.insert_inst(Inst::binary(ty, BinaryOp::Add, lhs, rhs, span))
    }

    // Add other builder methods as needed...
}
