pub mod amd64;

pub mod arm64;

pub use source_file;

use source_file::SourceSpan;

// TODO:
// Build a TACKY checker to enforce the following rules:
// - The dst of a unary instruction must always be a variable.
// - Blocks must end with a jump instruction or a return instruction.

// ---------------------------------------------------------------------------
// Tacky IR
// ---------------------------------------------------------------------------

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Program(pub FnDef);

#[derive(Default, Clone, PartialEq, Eq)]
pub struct FnDef {
    pub id: u32,
    pub span: SourceSpan,
    blocks: Vec<Block>,
    labels: Vec<Option<String>>,
}

impl FnDef {
    pub fn new(id: u32, span: SourceSpan) -> Self {
        Self {
            id,
            span,
            blocks: Vec::new(),
            labels: Vec::new(),
        }
    }

    pub fn get_block(&self, block_ref: BlockRef) -> &Block {
        &self.blocks[block_ref.0 as usize]
    }

    pub fn get_block_mut(&mut self, block_ref: BlockRef) -> &mut Block {
        &mut self.blocks[block_ref.0 as usize]
    }

    pub fn push_default_block(&mut self) -> BlockRef {
        self.push_block(Block::default())
    }

    pub fn push_block(&mut self, block: Block) -> BlockRef {
        self.blocks.push(block);
        BlockRef((self.blocks.len() - 1) as u32)
    }
}

impl std::fmt::Debug for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FnDef")
            .field("id", &self.id)
            .field("blocks", &self.blocks)
            .finish()
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub spans: Vec<SourceSpan>,
    pub label : Option<String>,
}

impl Block {
    pub fn with_label(label: impl Into<String>) -> Self {
        Block {
            label: Some(label.into()),
            ..Default::default()
        }
    }
}

impl std::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("label", &self.label)
            .field("instrs", &self.instrs)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockRef(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    /// A return instruction.
    Return(Value),
    /// A copy instruction.
    Copy { src: Value, dst: Value },
    /// A unary operation instruction.
    Unary { op: UnaryOp, src: Value, dst: Value },
    /// A binary operation instruction.
    Binary {
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
        dst: Value,
    },
    /// An unconditional jump instruction.
    Jump { target: BlockRef },
    /// A zero-conditional jump instruction.
    JumpIfZero { cond: Value, target: BlockRef },
    /// A non-zero-conditional jump instruction.
    JumpIfNotZero { cond: Value, target: BlockRef },
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
    /// The neg operator.
    Neg,
    /// The not operator.
    Not,
    /// The bit not operator.
    BitNot,
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
    /// The bit or operator.
    BitOr,
    /// The bit and operator.
    BitAnd,
    /// The bit xor operator.
    BitXor,
    /// The bit shift left operator.
    BitShl,
    /// The bit shift right operator.
    BitShr,
    /// The equal operator.
    Equal,
    /// The not equal operator.
    NotEqual,
    /// The less than operator.
    LessThan,
    /// The less than or equal operator.
    LessEqual,
    /// The greater than operator.
    GreaterThan,
    /// The greater than or equal operator.
    GreaterEqual,
}
