pub mod amd64;

pub mod arm64;

pub use source_file;

use source_file::SourceSpan;

// TODO:
// Build a TACKY checker to enforce the following rules:
// - The dst of a unary instruction must always be a variable.

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
    /// A binary operation instruction.
    Binary {
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
        dst: Value,
    },
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
    /// The not operator.
    Not,
    /// The arithmetic negation operator.
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// The add operator.
    Add,
    /// The sub operator.
    Sub,
    /// The mul operator.
    Mul,
    /// The div operator.
    Div,
    /// The rem operator.
    Rem,
    /// The or operator.
    Or,
    /// The and operator.
    And,
    /// The xor operator.
    Xor,
    /// The shift left operator.
    Shl,
    /// The shift right operator.
    Shr,
}
