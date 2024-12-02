use std::fmt;

use source_file::SourceSpan;

// TODO: Build an x64 checker:
// - Certain instructions do not allow to use the stack for both operands

// ---------------------------------------------------------------------------
// X64Builder
// ---------------------------------------------------------------------------

pub struct X64Builder<'a> {
    program: &'a crate::Program,
}

impl<'a> X64Builder<'a> {
    pub fn new(program: &'a crate::Program) -> Self {
        Self { program }
    }

    pub fn build(self) -> Program {
        Program(self.build_from_fn_def(&self.program.0))
    }

    fn build_from_fn_def(&self, fn_def: &crate::FnDef) -> FnDef {
        FnDefBuilder::new().build(fn_def)
    }
}

// ---------------------------------------------------------------------------
// FnDefBuilder
// ---------------------------------------------------------------------------

#[derive(Default)]
struct FnDefBuilder {
    instrs: Vec<Instr>,
    instrs_span: Vec<SourceSpan>,
}

impl FnDefBuilder {
    fn new() -> Self {
        Self::default()
    }

    fn build(self, fn_def: &crate::FnDef) -> FnDef {
        FnDef {
            id: fn_def.id,
            span: fn_def.span,
            instrs: self.instrs,
            instrs_span: self.instrs_span,
        }
    }
}

// ---------------------------------------------------------------------------
// x64 IR
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct Program(pub FnDef);

pub struct FnDef {
    pub id: u32,
    pub span: SourceSpan,
    pub instrs: Vec<Instr>,
    pub instrs_span: Vec<SourceSpan>,
}

impl fmt::Debug for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FnDef")
            .field("id", &self.id)
            .field("span", &self.span)
            .field("instrs", &self.instrs)
            .finish()
    }
}

#[derive(Debug)]
pub enum Instr {
    /// A `ret` instruction
    Ret,
    /// A `mov` instruction
    Mov { src: Oper, dst: Oper },
}

#[derive(Debug)]
pub enum Oper {
    /// A register
    Reg,
    /// An immediate value
    Imm(u32),
}
