pub use source_file;

use source_file::SourceSpan;

// ---------------------------------------------------------------------------
// Tacky
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct Program(pub FnDef);

#[derive(Debug)]
pub struct FnDef {
    pub span: SourceSpan,
    pub id: u32,
    pub instrs: Vec<Instr>,
    pub instrs_span: Vec<SourceSpan>,
}

#[derive(Debug)]
pub enum Instr {
    Ret(Value),
    Unary { op: UnaryOp, src: Value, dst: Value },
}

#[derive(Debug)]
pub enum Value {
    Constant(u32),
    Variable(u32),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    BitwiseNot,
}
