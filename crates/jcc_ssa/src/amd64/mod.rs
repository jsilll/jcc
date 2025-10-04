pub mod build;
pub mod emit;
pub mod fix;

use crate::{
    infra::{Indexed, IR},
    ConstValue, Symbol,
};

use jcc_sourcemap::SourceSpan;

use std::{collections::HashMap, num::NonZeroU32};

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

pub struct Program {
    funcs: Vec<Func>,
    vars: Vec<StaticVar>,
}

impl Default for Program {
    fn default() -> Self {
        Program {
            funcs: vec![],
            vars: vec![Default::default()],
        }
    }
}

impl Program {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn static_var(&self, var_ref: StaticVarRef) -> &StaticVar {
        &self.vars[var_ref.0.get() as usize]
    }

    pub fn new_static_var(&mut self, var: StaticVar) -> StaticVarRef {
        // TODO: Reuse slots from `static_vars_free` for better memory efficiency
        let r = StaticVarRef(NonZeroU32::new(self.vars.len() as u32).unwrap());
        self.vars.push(var);
        r
    }

    pub fn iter_static_vars(&self) -> impl Iterator<Item = StaticVarRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.vars.len()).map(|i| unsafe { StaticVarRef(NonZeroU32::new_unchecked(i as u32)) })
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
    pub align: u32,
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
    fn new(ty: Type, kind: InstKind, span: SourceSpan) -> Self {
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
    fn ret(span: SourceSpan) -> Self {
        Self::new(Type::default(), InstKind::Ret, span)
    }

    #[inline]
    fn cdq(ty: Type, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Cdq, span)
    }

    #[inline]
    fn call(sym: Symbol, span: SourceSpan) -> Self {
        Self::new(Type::default(), InstKind::Call(sym), span)
    }

    #[inline]
    fn push(op: Operand, span: SourceSpan) -> Self {
        Self::new(Type::Quad, InstKind::Push(op), span)
    }

    #[inline]
    fn jmp(block: BlockRef, span: SourceSpan) -> Self {
        Self::new(Type::default(), InstKind::Jmp(block), span)
    }

    #[inline]
    fn idiv(ty: Type, dst: Operand, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Idiv(dst), span)
    }

    #[inline]
    fn test(ty: Type, lhs: Operand, rhs: Operand, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Test { lhs, rhs }, span)
    }

    #[inline]
    fn movsx(src: Operand, dst: Operand, span: SourceSpan) -> Self {
        Self::new(Type::Quad, InstKind::Movsx { src, dst }, span)
    }

    #[inline]
    fn setcc(code: CondCode, dst: Operand, span: SourceSpan) -> Self {
        Self::new(Type::default(), InstKind::SetCC { dst, code }, span)
    }

    #[inline]
    fn jmpcc(code: CondCode, target: BlockRef, span: SourceSpan) -> Self {
        Self::new(Type::default(), InstKind::JmpCC { target, code }, span)
    }

    #[inline]
    fn mov(ty: Type, src: Operand, dst: Operand, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Mov { src, dst }, span)
    }

    #[inline]
    fn cmp(ty: Type, lhs: Operand, rhs: Operand, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Cmp { lhs, rhs }, span)
    }

    #[inline]
    fn unary(ty: Type, op: UnaryOp, dst: Operand, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Unary { op, dst }, span)
    }

    #[inline]
    fn binary(ty: Type, op: BinaryOp, src: Operand, dst: Operand, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Binary { op, src, dst }, span)
    }

    #[inline]
    fn alloc_stack(size: i64, span: SourceSpan) -> Self {
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
    fn dealloc_stack(size: i64, span: SourceSpan) -> Self {
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

impl TryFrom<crate::Type> for Type {
    type Error = ();
    fn try_from(ty: crate::Type) -> Result<Self, Self::Error> {
        match ty {
            crate::Type::Int8 => Ok(Self::Byte),
            crate::Type::Int32 => Ok(Self::Long),
            crate::Type::Int64 => Ok(Self::Quad),
            crate::Type::IntPtr => Ok(Self::Quad),
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
    Call(Symbol),
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
    Data(StaticVarRef),
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

impl TryFrom<crate::BinaryOp> for CondCode {
    type Error = ();
    fn try_from(op: crate::BinaryOp) -> Result<Self, Self::Error> {
        match op {
            crate::BinaryOp::Equal => Ok(Self::Eq),
            crate::BinaryOp::NotEqual => Ok(Self::Ne),
            crate::BinaryOp::LessThan => Ok(Self::Lt),
            crate::BinaryOp::LessEqual => Ok(Self::Le),
            crate::BinaryOp::GreaterThan => Ok(Self::Gt),
            crate::BinaryOp::GreaterEqual => Ok(Self::Ge),
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
    funcs: HashMap<Symbol, FuncEntry>,
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
