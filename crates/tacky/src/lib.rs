pub mod x64;

use std::fmt;

pub use source_file;
use source_file::SourceSpan;

// TODO: Build a TACKY checker:
// - The dst of a unary instruction must be a variable.

// ---------------------------------------------------------------------------
// Tacky IR
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
    /// A return instruction.
    Return(Value),
    /// A unary instruction.
    Unary { op: UnaryOp, src: Value, dst: Value },
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    /// A constant integer value.
    Constant(u32),
    /// A variable reference.
    Variable(u32),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    /// The unary minus operator.
    Minus,
    /// The bitwise not operator.
    BitwiseNot,
}
