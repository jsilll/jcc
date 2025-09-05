pub mod build;
pub mod emit;
pub mod fix;

use crate::{
    infra::{Indexed, IR},
    ConstValue, Symbol,
};

use jcc_sourcemap::SourceSpan;

use std::num::NonZeroU32;

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub struct Program {
    funcs: Vec<Func>,
    static_vars: Vec<StaticVar>,
}

impl Default for Program {
    fn default() -> Self {
        Program {
            funcs: vec![],
            static_vars: vec![Default::default()],
        }
    }
}

impl Program {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn static_var(&self, var_ref: StaticVarRef) -> &StaticVar {
        &self.static_vars[var_ref.0.get() as usize]
    }

    pub fn new_static_var(&mut self, var: StaticVar) -> StaticVarRef {
        // TODO: Reuse slots from `static_vars_free` for better memory efficiency
        let r = StaticVarRef(NonZeroU32::new(self.static_vars.len() as u32).unwrap());
        self.static_vars.push(var);
        r
    }

    pub fn iter_static_vars(&self) -> impl Iterator<Item = StaticVarRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.static_vars.len())
            .map(|i| unsafe { StaticVarRef(NonZeroU32::new_unchecked(i as u32)) })
    }

    pub fn iter_static_vars_with_ref(
        &self,
    ) -> impl Iterator<Item = (StaticVarRef, &StaticVar)> + '_ {
        self.iter_static_vars().map(|r| (r, self.static_var(r)))
    }
}

// ---------------------------------------------------------------------------
// StaticVar
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StaticVarRef(NonZeroU32);

#[derive(Debug, Default, Clone)]
pub struct StaticVar {
    pub name: Symbol,
    pub is_global: bool,
    pub span: SourceSpan,
    pub init: Option<ConstValue>,
}

// ---------------------------------------------------------------------------
// Func
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Func {
    pub name: Symbol,
    pub is_global: bool,
    pub span: SourceSpan,
    insts: Vec<Inst>,
    blocks: Vec<Block>,
}

impl Default for Func {
    fn default() -> Self {
        Func {
            name: Default::default(),
            span: Default::default(),
            is_global: Default::default(),
            insts: vec![Default::default()],
            blocks: vec![Default::default()],
        }
    }
}

impl IR for Func {
    type Inst = Inst;
    type InstRef = InstRef;
    type BlockRef = BlockRef;

    #[inline]
    fn is_nop(&self, inst: Self::InstRef) -> bool {
        matches!(self.inst(inst).kind, InstKind::Nop)
    }

    #[inline]
    fn new_inst(&mut self, inst: Self::Inst) -> Self::InstRef {
        Self::new_inst(self, inst)
    }

    #[inline]
    fn block_insts(&self, block: Self::BlockRef) -> &[Self::InstRef] {
        &self.blocks[block.0.get() as usize].insts
    }

    #[inline]
    fn swap_block_insts(&mut self, block: Self::BlockRef, insts: &mut Vec<Self::InstRef>) {
        std::mem::swap(&mut self.blocks[block.0.get() as usize].insts, insts);
    }
}

impl Func {
    pub fn new(name: Symbol, is_global: bool, span: SourceSpan) -> Self {
        Func {
            name,
            span,
            is_global,
            insts: vec![Default::default()],
            blocks: vec![Default::default()],
        }
    }

    // ---------------------------------------------------------------------------
    // Accessors
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn inst(&self, inst_ref: InstRef) -> &Inst {
        &self.insts[inst_ref.0.get() as usize]
    }

    #[inline]
    pub fn block(&self, block_ref: BlockRef) -> &Block {
        &self.blocks[block_ref.0.get() as usize]
    }

    #[inline]
    pub fn inst_mut(&mut self, inst_ref: InstRef) -> &mut Inst {
        &mut self.insts[inst_ref.0.get() as usize]
    }

    #[inline]
    pub fn block_mut(&mut self, block_ref: BlockRef) -> &mut Block {
        &mut self.blocks[block_ref.0.get() as usize]
    }

    // ---------------------------------------------------------------------------
    // Creation
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn new_inst(&mut self, inst: Inst) -> InstRef {
        // TODO: Reuse slots from `insts_free` for better memory efficiency
        let r = InstRef(NonZeroU32::new(self.insts.len() as u32).unwrap());
        self.insts.push(inst);
        r
    }

    #[inline]
    pub fn new_block(&mut self, block: Block) -> BlockRef {
        // TODO: Reuse slots from `blocks_free` for better memory efficiency
        let r = BlockRef(NonZeroU32::new(self.blocks.len() as u32).unwrap());
        self.blocks.push(block);
        r
    }

    // ---------------------------------------------------------------------------
    // Deletion
    // ---------------------------------------------------------------------------

    // TODO: Implement deletion methods if necessary.

    // ---------------------------------------------------------------------------
    // Iterators
    // ---------------------------------------------------------------------------

    pub fn iter_insts(&self) -> impl Iterator<Item = InstRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.insts.len()).map(|i| unsafe { InstRef(NonZeroU32::new_unchecked(i as u32)) })
    }

    pub fn iter_blocks(&self) -> impl Iterator<Item = BlockRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.blocks.len()).map(|i| unsafe { BlockRef(NonZeroU32::new_unchecked(i as u32)) })
    }

    // ---------------------------------------------------------------------------
    // Snapshot
    // ---------------------------------------------------------------------------

    fn snapshot_blocks(&self, buf: &mut Vec<BlockRef>) {
        buf.clear();
        for i in 1..self.blocks.len() {
            buf.push(BlockRef(NonZeroU32::new(i as u32).unwrap()));
        }
    }
}

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockRef(NonZeroU32);

impl Default for BlockRef {
    fn default() -> Self {
        Self(NonZeroU32::new(u32::MAX).unwrap())
    }
}

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
pub struct InstRef(NonZeroU32);

#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct InstIdx(pub(crate) u32);

impl From<InstIdx> for u32 {
    #[inline]
    fn from(idx: InstIdx) -> Self {
        idx.0
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Inst {
    pub idx: InstIdx,
    pub kind: InstKind,
    pub span: SourceSpan,
}

impl Indexed for Inst {
    #[inline]
    fn set_idx(&mut self, idx: u32) {
        self.idx = InstIdx(idx);
    }
}

impl Inst {
    #[inline]
    fn new(kind: InstKind, span: SourceSpan) -> Self {
        Self {
            kind,
            span,
            ..Default::default()
        }
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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum InstKind {
    /// A no-operation instruction.
    #[default]
    Nop,
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
    /// A register.
    Reg(Reg),
    /// A stack operand.
    Stack(i32),
    /// Pseudo register.
    Pseudo(u32),
    /// An immediate value.
    Imm(ConstValue),
    /// A static variable reference.
    Data(StaticVarRef),
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
