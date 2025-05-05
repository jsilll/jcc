pub mod amd64;

pub mod arm64;

pub use source_file;

use source_file::SourceSpan;

use string_interner::{backend::StringBackend, DefaultHashBuilder, StringInterner};

/// -------------------------------------------------------------------------------
/// Interner
/// -------------------------------------------------------------------------------

pub type Interner = StringInterner<StringBackend<Symbol>, DefaultHashBuilder>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(std::num::NonZeroU32);

impl Default for Symbol {
    #[inline]
    fn default() -> Self {
        unsafe { Self(std::num::NonZeroU32::new_unchecked(1)) }
    }
}

impl string_interner::Symbol for Symbol {
    #[inline]
    fn try_from_usize(index: usize) -> Option<Self> {
        std::num::NonZeroU32::new((index as u32).wrapping_add(1)).map(Self)
    }

    #[inline]
    fn to_usize(self) -> usize {
        self.0.get() as usize - 1
    }
}

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
    blocks: Vec<Block>,
    pub span: SourceSpan,
}

impl FnDef {
    pub fn get_block(&self, block: BlockRef) -> &Block {
        &self.blocks[block.0 as usize]
    }

    pub fn get_block_mut(&mut self, block: BlockRef) -> &mut Block {
        &mut self.blocks[block.0 as usize]
    }

    pub fn push_block(&mut self, block: Block) -> BlockRef {
        self.blocks.push(block);
        BlockRef((self.blocks.len() - 1) as u32)
    }

    pub fn blocks_iter_both(&self) -> impl Iterator<Item = (BlockRef, &Block)> {
        self.blocks
            .iter()
            .enumerate()
            .map(|(idx, block)| (BlockRef(idx as u32), block))
    }
}

impl std::fmt::Debug for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FnDef")
            .field("blocks", &self.blocks)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockRef(u32);

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub instrs: Vec<Inst>,
    pub label: Option<Symbol>,
    pub spans: Vec<SourceSpan>,
}

impl Block {
    pub fn with_label(label: Symbol) -> Self {
        Block {
            label: Some(label),
            ..Default::default()
        }
    }

    pub fn with_instrs(mut self, instrs: &[Inst], span: SourceSpan) -> Self {
        self.instrs.extend_from_slice(instrs);
        self.spans.resize(self.instrs.len(), span);
        self
    }

    pub fn instrs_iter_both(&self) -> impl Iterator<Item = (&Inst, &SourceSpan)> {
        self.instrs.iter().zip(self.spans.iter())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inst {
    /// A return instruction.
    Return(Value),
    /// An unconditional jump instruction.
    Jump(BlockRef),
    /// A copy instruction.
    Copy { src: Value, dst: Value },
    /// A zero-conditional jump instruction.
    JumpIfZero { cond: Value, target: BlockRef },
    /// A non-zero-conditional jump instruction.
    JumpIfNotZero { cond: Value, target: BlockRef },
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
    Const(i64),
    /// A variable reference.
    Variable(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// The not operator.
    Not,
    /// The neg operator.
    Neg,
    /// The inc operator.
    Inc,
    /// The dec operator.
    Dec,
    /// The bit not operator.
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
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
}
