pub mod build;
pub mod emit;
pub mod fix;

pub use build::{build, AMD64FuncBuilder};

use crate::Symbol;

use jcc_sourcemap::SourceSpan;

// ---------------------------------------------------------------------------
// AMD64 IR
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Program {
    funcs: Vec<FnDef>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct FnDef {
    name: Symbol,
    blocks: Vec<Block>,
    pub span: SourceSpan,
}

impl FnDef {
    pub fn new(name: Symbol, span: SourceSpan) -> Self {
        FnDef {
            name,
            span,
            blocks: Vec::new(),
        }
    }

    #[inline]
    pub fn get_block(&self, block_ref: BlockRef) -> &Block {
        &self.blocks[block_ref.0 as usize]
    }

    #[inline]
    pub fn get_block_mut(&mut self, block_ref: BlockRef) -> &mut Block {
        &mut self.blocks[block_ref.0 as usize]
    }

    #[inline]
    pub fn push_block(&mut self, block: Block) -> BlockRef {
        self.blocks.push(block);
        BlockRef((self.blocks.len() - 1) as u32)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockRef(u32);

impl std::fmt::Display for BlockRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub instrs: Vec<Inst>,
    pub spans: Vec<SourceSpan>,
    pub label: Option<Symbol>,
}

impl Block {
    pub fn with_label(label: Symbol) -> Self {
        Block {
            label: Some(label),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inst {
    /// A `ret` instruction.
    Ret,
    /// A `cdq` instruction.
    Cdq,
    /// Stack allocation instruction.
    Alloca(i32),
    /// Stack deallocation instruction.
    Dealloca(i32),
    /// A call instruction.
    Call(Symbol),
    /// A push to stack instruction.
    Push(Operand),
    /// An `idiv` instruction.
    Idiv(Operand),
    /// A `jmp` instruction.
    Jmp(BlockRef),
    /// A `mov` instruction.
    Mov { src: Operand, dst: Operand },
    /// A `cmp` instruction.
    Cmp { lhs: Operand, rhs: Operand },
    /// A `test` instruction.
    Test { lhs: Operand, rhs: Operand },
    /// A conditional set instruction.
    SetCC { cond_code: CondCode, dst: Operand },
    /// A conditional jump instruction.
    JmpCC {
        cond_code: CondCode,
        target: BlockRef,
    },
    /// A unary operation instruction.
    Unary { op: UnaryOp, dst: Operand },
    /// A binary operation instruction.
    Binary {
        op: BinaryOp,
        src: Operand,
        dst: Operand,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    /// An immediate value.
    Imm(i64),
    /// A register.
    Reg(Reg),
    /// A stack operand.
    Stack(i32),
    /// Pseudo register.
    Pseudo(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    /// The RAX register.
    Rax,
    /// The RBX register.
    Rbx,
    /// The RCX register.
    Rcx,
    /// The RDX register.
    Rdx,
    /// The RDI register.
    Rdi,
    /// The RSI register.
    Rsi,
    /// The R8 register.
    R8,
    /// The R9 register.
    R9,
    /// The R10 register.
    Rg10,
    /// The R11 register.
    Rg11,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// The `not` operator.
    Not,
    /// The `neg` operator.
    Neg,
    /// The `inc` operator.
    Inc,
    /// The `dec` operator.
    Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// The `or` operator.
    Or,
    /// The `and` operator.
    And,
    /// The `xor` operator.
    Xor,
    /// The `sal` operator.
    Shl,
    /// The `sar` operator.
    Shr,
    /// The `add` operator.
    Add,
    /// The `sub` operator.
    Sub,
    /// The `mul` operator.
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondCode {
    /// The `sete` condition.
    Equal,
    /// The `setne` condition.
    NotEqual,
    /// The `setl` condition.
    LessThan,
    /// The `setle` condition.
    LessEqual,
    /// The `setg` condition.
    GreaterThan,
    /// The `setge` condition.
    GreaterEqual,
}

impl TryFrom<crate::BinaryOp> for CondCode {
    type Error = ();

    fn try_from(op: crate::BinaryOp) -> Result<Self, Self::Error> {
        match op {
            crate::BinaryOp::Equal => Ok(Self::Equal),
            crate::BinaryOp::NotEqual => Ok(Self::NotEqual),
            crate::BinaryOp::LessThan => Ok(Self::LessThan),
            crate::BinaryOp::LessEqual => Ok(Self::LessEqual),
            crate::BinaryOp::GreaterThan => Ok(Self::GreaterThan),
            crate::BinaryOp::GreaterEqual => Ok(Self::GreaterEqual),
            _ => Err(()),
        }
    }
}
