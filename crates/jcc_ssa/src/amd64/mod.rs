pub mod build;
pub mod emit;
pub mod fix;

use crate::{
    infra::{Indexed, IR},
    ir, Ident,
};

use jcc_codemap::span::Span;
use jcc_entity::PrimaryMap;

use std::{collections::HashMap, num::NonZeroU32};

#[derive(Default)]
pub struct Program {
    funcs: Vec<Func>,
    vars: PrimaryMap<ir::Global, ir::GlobalData>,
}

// ---------------------------------------------------------------------------
// Func
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Func {
    pub name: Ident,
    pub is_global: bool,
    pub span: Span,
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
    pub fn new(name: Ident, is_global: bool, span: Span) -> Self {
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
    // Helpers
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

impl std::fmt::Display for BlockRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub insts: Vec<InstRef>,
    pub label: Option<Ident>,
}

impl Block {
    pub fn with_label(label: Ident) -> Self {
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
pub struct InstIdx(u32);

impl From<InstIdx> for u32 {
    #[inline]
    fn from(idx: InstIdx) -> Self {
        idx.0
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Inst {
    pub ty: Type,
    pub idx: InstIdx,
    pub kind: InstKind,
    pub span: Span,
}

impl Indexed for Inst {
    #[inline]
    fn set_idx(&mut self, idx: u32) {
        self.idx = InstIdx(idx);
    }
}

impl Inst {
    #[inline]
    fn new(ty: Type, kind: InstKind, span: Span) -> Self {
        Self {
            ty,
            kind,
            span,
            ..Default::default()
        }
    }

    // ---------------------------------------------------------------------------
    // Factory methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn ret(span: Span) -> Self {
        Self::new(Type::default(), InstKind::Ret, span)
    }

    #[inline]
    fn cdq(ty: Type, span: Span) -> Self {
        Self::new(ty, InstKind::Cdq, span)
    }

    #[inline]
    fn call(sym: Ident, span: Span) -> Self {
        Self::new(Type::default(), InstKind::Call(sym), span)
    }

    #[inline]
    fn push(op: Operand, span: Span) -> Self {
        Self::new(Type::Quad, InstKind::Push(op), span)
    }

    #[inline]
    fn jmp(block: BlockRef, span: Span) -> Self {
        Self::new(Type::default(), InstKind::Jmp(block), span)
    }

    #[inline]
    fn idiv(ty: Type, dst: Operand, span: Span) -> Self {
        Self::new(ty, InstKind::Idiv(dst), span)
    }

    #[inline]
    fn test(ty: Type, lhs: Operand, rhs: Operand, span: Span) -> Self {
        Self::new(ty, InstKind::Test { lhs, rhs }, span)
    }

    #[inline]
    fn movsx(src: Operand, dst: Operand, span: Span) -> Self {
        Self::new(Type::Quad, InstKind::Movsx { src, dst }, span)
    }

    #[inline]
    fn setcc(code: CondCode, dst: Operand, span: Span) -> Self {
        Self::new(Type::default(), InstKind::SetCC { dst, code }, span)
    }

    #[inline]
    fn jmpcc(code: CondCode, target: BlockRef, span: Span) -> Self {
        Self::new(Type::default(), InstKind::JmpCC { target, code }, span)
    }

    #[inline]
    fn mov(ty: Type, src: Operand, dst: Operand, span: Span) -> Self {
        Self::new(ty, InstKind::Mov { src, dst }, span)
    }

    #[inline]
    fn cmp(ty: Type, lhs: Operand, rhs: Operand, span: Span) -> Self {
        Self::new(ty, InstKind::Cmp { lhs, rhs }, span)
    }

    #[inline]
    fn unary(ty: Type, op: UnaryOp, dst: Operand, span: Span) -> Self {
        Self::new(ty, InstKind::Unary { op, dst }, span)
    }

    #[inline]
    fn binary(ty: Type, op: BinaryOp, src: Operand, dst: Operand, span: Span) -> Self {
        Self::new(ty, InstKind::Binary { op, src, dst }, span)
    }

    #[inline]
    fn alloc_stack(size: i64, span: Span) -> Self {
        assert!(size >= 0 && size % 8 == 0);
        Self::new(
            Type::Quad,
            InstKind::Binary {
                op: BinaryOp::Sub,
                src: Operand::Imm(size),
                dst: Operand::Reg(Reg::Rsp),
            },
            span,
        )
    }

    #[inline]
    fn dealloc_stack(size: i64, span: Span) -> Self {
        assert!(size >= 0 && size % 8 == 0);
        Self::new(
            Type::Quad,
            InstKind::Binary {
                op: BinaryOp::Add,
                src: Operand::Imm(size),
                dst: Operand::Reg(Reg::Rsp),
            },
            span,
        )
    }
}

// ---------------------------------------------------------------------------
// Enum Soup
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// 8-bit type.
    #[default]
    Byte,
    /// 32-bit type.
    Long,
    /// 64-bit type.
    Quad,
}

impl Type {
    #[inline]
    pub fn align(&self) -> i64 {
        match self {
            Type::Byte => 1,
            Type::Long => 4,
            Type::Quad => 8,
        }
    }

    #[inline]
    pub fn as_str(&self) -> &'static str {
        match self {
            Type::Byte => "b",
            Type::Long => "l",
            Type::Quad => "q",
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TryFrom<ir::ty::Ty> for Type {
    type Error = ();
    fn try_from(ty: ir::ty::Ty) -> Result<Self, Self::Error> {
        match ty {
            ir::ty::Ty::I1 => Ok(Self::Long),
            ir::ty::Ty::I32 => Ok(Self::Long),
            ir::ty::Ty::I64 => Ok(Self::Quad),
            ir::ty::Ty::Ptr => Ok(Self::Quad),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum InstKind {
    /// A no-operation instruction.
    #[default]
    Nop,
    /// A `ret` instruction.
    Ret,
    /// A `cdq` instruction.
    Cdq,
    /// A call instruction.
    Call(Ident),
    /// A push to stack instruction.
    Push(Operand),
    /// A `jmp` instruction.
    Jmp(BlockRef),
    /// An `idiv` instruction.
    Idiv(Operand),
    /// A `cmp` instruction.
    Cmp { lhs: Operand, rhs: Operand },
    /// A `test` instruction.
    Test { lhs: Operand, rhs: Operand },
    /// A `movsx` instruction.
    Movsx { src: Operand, dst: Operand },
    /// A conditional set instruction.
    SetCC { code: CondCode, dst: Operand },
    /// A conditional jump instruction.
    JmpCC { code: CondCode, target: BlockRef },
    /// A `mov` instruction.
    Mov { src: Operand, dst: Operand },
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
    Stack(i64),
    /// Pseudo register.
    Pseudo(u32),
    /// A static variable reference.
    Data(ir::Global),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    /// The SP register.
    Rsp,
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

impl Reg {
    pub fn as_str_8(&self) -> &'static str {
        match self {
            Reg::Rsp => "spl",
            Reg::Rax => "al",
            Reg::Rbx => "bl",
            Reg::Rcx => "cl",
            Reg::Rdx => "dl",
            Reg::Rdi => "dil",
            Reg::Rsi => "sil",
            Reg::R8 => "r8b",
            Reg::R9 => "r9b",
            Reg::Rg10 => "r10b",
            Reg::Rg11 => "r11b",
        }
    }

    pub fn as_str_32(&self) -> &'static str {
        match self {
            Reg::Rsp => "esp",
            Reg::Rax => "eax",
            Reg::Rbx => "ebx",
            Reg::Rcx => "ecx",
            Reg::Rdx => "edx",
            Reg::Rdi => "edi",
            Reg::Rsi => "esi",
            Reg::R8 => "r8d",
            Reg::R9 => "r9d",
            Reg::Rg10 => "r10d",
            Reg::Rg11 => "r11d",
        }
    }

    pub fn as_str_64(&self) -> &'static str {
        match self {
            Reg::Rsp => "rsp",
            Reg::Rax => "rax",
            Reg::Rbx => "rbx",
            Reg::Rcx => "rcx",
            Reg::Rdx => "rdx",
            Reg::Rdi => "rdi",
            Reg::Rsi => "rsi",
            Reg::R8 => "r8",
            Reg::R9 => "r9",
            Reg::Rg10 => "r10",
            Reg::Rg11 => "r11",
        }
    }
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
    Eq,
    /// The `setne` condition.
    Ne,
    /// The `setl` condition.
    Lt,
    /// The `setle` condition.
    Le,
    /// The `setg` condition.
    Gt,
    /// The `setge` condition.
    Ge,
}

impl std::fmt::Display for CondCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl CondCode {
    fn as_str(&self) -> &'static str {
        match self {
            CondCode::Eq => "e",
            CondCode::Ne => "ne",
            CondCode::Lt => "l",
            CondCode::Le => "le",
            CondCode::Gt => "g",
            CondCode::Ge => "ge",
        }
    }
}

impl TryFrom<ir::inst::ICmpOp> for CondCode {
    type Error = ();
    fn try_from(op: ir::inst::ICmpOp) -> Result<Self, Self::Error> {
        match op {
            ir::inst::ICmpOp::Eq => Ok(Self::Eq),
            ir::inst::ICmpOp::Ne => Ok(Self::Ne),
            ir::inst::ICmpOp::Lt => Ok(Self::Lt),
            ir::inst::ICmpOp::Le => Ok(Self::Le),
            ir::inst::ICmpOp::Gt => Ok(Self::Gt),
            ir::inst::ICmpOp::Ge => Ok(Self::Ge),
            _ => Err(()),
        }
    }
}

// ---------------------------------------------------------------------------
// Symbol Table
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    pseudos: Vec<PseudoEntry>,
    funcs: HashMap<Ident, FuncEntry>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct PseudoEntry {
    pub ty: Type,
    pub is_static: bool,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FuncEntry {
    pub is_static: bool,
    pub frame_size: i64,
}
