pub mod amd64;
pub mod arm64;

pub use source_file;
use source_file::SourceSpan;

// TODO: Build a TACKY checker:
// - The dst of a unary instruction must be a variable.

// ---------------------------------------------------------------------------
// Tacky IR
// ---------------------------------------------------------------------------

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Program(pub FnDef);

#[derive(Default, Clone, PartialEq, Eq)]
pub struct FnDef {
    pub id: u32,
    pub span: SourceSpan,
    pub instrs: Vec<Instr>,
    pub instrs_span: Vec<SourceSpan>,
}

impl std::fmt::Debug for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FnDef")
            .field("id", &self.id)
            .field("span", &self.span)
            .field("instrs", &self.instrs)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    /// A return instruction.
    Return(Value),
    /// A unary operation instruction.
    Unary { op: UnaryOp, src: Value, dst: Value },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    /// A constant value.
    Constant(u32),
    /// A variable reference.
    Variable(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// The unary logical not operator.
    Not,
    /// The unary arithmetic negation operator.
    Neg,
}
