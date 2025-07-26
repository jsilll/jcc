pub mod build;
pub mod emit;
// pub mod fix;

pub use build::{build, AMD64FuncBuilder};

use crate::Symbol;

use jcc_sourcemap::SourceSpan;

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Program {
    funcs: Vec<Func>,
}

// ---------------------------------------------------------------------------
// Func
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Func {
    name: Symbol,
    span: SourceSpan,
    insts: Vec<Inst>,
    blocks: Vec<Block>,
}

impl Func {
    pub fn new(name: Symbol, span: SourceSpan) -> Self {
        Func {
            name,
            span,
            insts: Vec::new(),
            blocks: Vec::new(),
        }
    }

    #[inline]
    pub fn inst(&self, inst_ref: InstRef) -> &Inst {
        &self.insts[inst_ref.0 as usize]
    }

    #[inline]
    pub fn block(&self, block_ref: BlockRef) -> &Block {
        &self.blocks[block_ref.0 as usize]
    }

    #[inline]
    pub fn inst_mut(&mut self, inst_ref: InstRef) -> &mut Inst {
        &mut self.insts[inst_ref.0 as usize]
    }

    #[inline]
    pub fn block_mut(&mut self, block_ref: BlockRef) -> &mut Block {
        &mut self.blocks[block_ref.0 as usize]
    }

    #[inline]
    pub fn new_inst(&mut self, inst: Inst) -> InstRef {
        self.insts.push(inst);
        InstRef((self.blocks.len() - 1) as u32)
    }

    #[inline]
    pub fn new_block(&mut self, block: Block) -> BlockRef {
        self.blocks.push(block);
        BlockRef((self.blocks.len() - 1) as u32)
    }
}

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockRef(u32);

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub insts: Vec<InstRef>,
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

// ---------------------------------------------------------------------------
// Inst
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InstRef(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Inst {
    pub kind: InstKind,
    pub span: SourceSpan,
}

impl Inst {
    #[inline]
    fn new(kind: InstKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    // ---------------------------------------------------------------------------
    // Factory methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn ret(span: SourceSpan) -> Self {
        Self::new(InstKind::Ret, span)
    }

    #[inline]
    fn cdq(span: SourceSpan) -> Self {
        Self::new(InstKind::Cdq, span)
    }

    #[inline]
    fn call(sym: Symbol, span: SourceSpan) -> Self {
        Self::new(InstKind::Call(sym), span)
    }

    #[inline]
    fn push(op: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::Push(op), span)
    }

    #[inline]
    fn idiv(op: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::Idiv(op), span)
    }

    #[inline]
    fn alloca(size: i32, span: SourceSpan) -> Self {
        Self::new(InstKind::Alloca(size), span)
    }

    #[inline]
    fn dealloca(size: i32, span: SourceSpan) -> Self {
        Self::new(InstKind::Dealloca(size), span)
    }

    #[inline]
    fn jmp(block: BlockRef, span: SourceSpan) -> Self {
        Self::new(InstKind::Jmp(block), span)
    }

    #[inline]
    fn mov(src: Operand, dst: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::Mov { src, dst }, span)
    }

    #[inline]
    fn cmp(lhs: Operand, rhs: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::Cmp { lhs, rhs }, span)
    }

    #[inline]
    fn test(lhs: Operand, rhs: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::Test { lhs, rhs }, span)
    }

    #[inline]
    fn setcc(code: CondCode, dst: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::SetCC { dst, code }, span)
    }

    #[inline]
    fn jmpcc(code: CondCode, target: BlockRef, span: SourceSpan) -> Self {
        Self::new(InstKind::JmpCC { target, code }, span)
    }

    #[inline]
    fn unary(op: UnaryOp, dst: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::Unary { op, dst }, span)
    }

    #[inline]
    fn binary(op: BinaryOp, src: Operand, dst: Operand, span: SourceSpan) -> Self {
        Self::new(InstKind::Binary { op, src, dst }, span)
    }
}

// ---------------------------------------------------------------------------
// Enum Soup
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstKind {
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
    SetCC { code: CondCode, dst: Operand },
    /// A conditional jump instruction.
    JmpCC { code: CondCode, target: BlockRef },
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

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl std::fmt::Display for BlockRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
