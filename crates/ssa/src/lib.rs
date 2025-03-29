pub mod effects;

pub mod insertion;

pub mod verify;

pub mod amd64;

pub use source_file;

pub use string_interner;

use effects::{AbstractHeap, FastEffects};

use source_file::SourceSpan;
use string_interner::{DefaultStringInterner, DefaultSymbol};

use std::{
    collections::{HashMap, HashSet},
    fmt,
    marker::PhantomData,
    num::NonZeroU32,
};

// ---------------------------------------------------------------------------
// Type
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    #[default]
    Void,
    Int32,
    Int64,
    IntPtr,
}

// ---------------------------------------------------------------------------
// Inst
// ---------------------------------------------------------------------------

pub type InstRef = EntityRef<Inst>;

#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct InstIdx(pub(crate) u32);

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Inst {
    pub ty: Type,
    pub kind: InstKind,
    pub(crate) idx: InstIdx,
    // TODO: pub block: BlockRef,
}

impl Inst {
    pub fn new(ty: Type, kind: InstKind) -> Self {
        Self {
            ty,
            kind,
            idx: InstIdx::default(),
        }
    }

    pub fn phi(ty: Type) -> Self {
        Self::new(ty, InstKind::Phi)
    }

    pub fn get_arg(ty: Type) -> Self {
        Self::new(ty, InstKind::Arg)
    }

    pub fn alloca() -> Self {
        Self::new(Type::IntPtr, InstKind::Alloca)
    }

    pub fn ret(val: InstRef) -> Self {
        Self::new(Type::Void, InstKind::Ret(val))
    }

    pub fn load(ptr: InstRef) -> Self {
        Self::new(Type::Int32, InstKind::Load(ptr))
    }

    pub fn const_i32(val: i64) -> Self {
        Self::new(Type::Int32, InstKind::Const(val))
    }

    pub fn const_i64(val: i64) -> Self {
        Self::new(Type::Int64, InstKind::Const(val))
    }

    pub fn store(ptr: InstRef, val: InstRef) -> Self {
        Self::new(Type::Void, InstKind::Store { ptr, val })
    }

    pub fn upsilon(phi: InstRef, val: InstRef) -> Self {
        Self::new(Type::Void, InstKind::Upsilon { phi, val })
    }

    pub fn add_i32(lhs: InstRef, rhs: InstRef) -> Self {
        Self::new(
            Type::Int32,
            InstKind::Binary {
                lhs,
                rhs,
                op: BinaryOp::Add,
            },
        )
    }

    pub fn add_i64(lhs: InstRef, rhs: InstRef) -> Self {
        Self::new(
            Type::Int64,
            InstKind::Binary {
                lhs,
                rhs,
                op: BinaryOp::Add,
            },
        )
    }

    pub fn is_const(&self, val: i64) -> bool {
        match self.kind {
            InstKind::Const(v) => v == val,
            _ => false,
        }
    }

    pub fn has_side_effects(&self) -> bool {
        match self.kind {
            InstKind::Ret(_) | InstKind::Jump(_) | InstKind::Branch { .. } => true,
            _ => false,
        }
    }

    pub fn get_args(&self, args: &mut Vec<InstRef>) {
        match &self.kind {
            InstKind::Upsilon { phi, val } => args.extend([*phi, *val]),
            InstKind::Binary { lhs, rhs, .. } => args.extend([*lhs, *rhs]),
            InstKind::Ret(val)
            | InstKind::Identity(val)
            | InstKind::Unary { val, .. }
            | InstKind::Branch { val, .. } => args.push(*val),
            _ => {}
        }
    }

    pub fn get_effects(&self, prog: &Program, effects: &mut FastEffects) {
        match self.kind {
            InstKind::Phi => effects.reads.push(prog.heaps.ssa_state),
            InstKind::Upsilon { .. } => effects.writes.push(prog.heaps.ssa_state),
            InstKind::Jump(_) | InstKind::Ret(_) | InstKind::Branch { .. } => {
                effects.writes.push(prog.heaps.control)
            }
            _ => {}
        }
    }

    pub fn replace_args(&mut self, map: &HashMap<InstRef, InstRef>) {
        match &mut self.kind {
            InstKind::Ret(val)
            | InstKind::Identity(val)
            | InstKind::Unary { val, .. }
            | InstKind::Branch { val, .. } => {
                if let Some(v) = map.get(val) {
                    *val = *v;
                }
            }
            InstKind::Upsilon { phi, val } => {
                if let Some(v) = map.get(phi) {
                    *phi = *v;
                }
                if let Some(v) = map.get(val) {
                    *val = *v;
                }
            }
            InstKind::Binary { lhs, rhs, .. } => {
                if let Some(v) = map.get(lhs) {
                    *lhs = *v;
                }
                if let Some(v) = map.get(rhs) {
                    *rhs = *v;
                }
            }
            _ => {}
        }
    }

    pub fn into_identity(&mut self, val: InstRef) {
        self.kind = InstKind::Identity(val)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum InstKind {
    #[default]
    /// A no-op instruction.
    Nop,
    /// A phi instruction.
    Phi,
    /// A get argument instruction.
    Arg,
    /// A stack allocation instruction.
    Alloca,
    /// A constant instruction.
    Const(i64),
    /// A return instruction.
    Ret(InstRef),
    /// A jump instruction.
    Jump(BlockRef),
    /// A load instruction.
    Load(InstRef),
    /// An identity instruction.
    Identity(InstRef),
    /// A store instruction.
    Store { ptr: InstRef, val: InstRef },
    /// An upsilon instruction.
    Upsilon { val: InstRef, phi: InstRef },
    /// A unary instruction.
    Unary { op: UnaryOp, val: InstRef },
    /// A binary instruction.
    Binary {
        op: BinaryOp,
        lhs: InstRef,
        rhs: InstRef,
    },
    /// A branch instruction.
    Branch {
        val: InstRef,
        then: BlockRef,
        other: BlockRef,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// The add operaotr.
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
    /// The bit shift left operator.
    BitShl,
    /// The bit shift right operator.
    BitShr,
}

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

pub type BlockRef = EntityRef<Block>;

// #[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
// pub struct BlockIdx(u32);

#[derive(Debug, Default, Clone)]
pub struct Block {
    // TODO: pub index: BlockIdx,
    pub insts: Vec<InstRef>,
    pub succs: Vec<BlockRef>,
}

// ---------------------------------------------------------------------------
// Func
// ---------------------------------------------------------------------------

pub type FuncRef = EntityRef<Func>;

#[derive(Debug, Default, Clone)]
pub struct Func {
    pub blocks: Vec<BlockRef>,
}

// ---------------------------------------------------------------------------
// BaseHeaps
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct BaseHeaps {
    pub world: AbstractHeap,
    pub memory: AbstractHeap,
    pub control: AbstractHeap,
    pub ssa_state: AbstractHeap,
}

impl Default for BaseHeaps {
    fn default() -> Self {
        Self {
            world: AbstractHeap::new(1, 8),
            memory: AbstractHeap::new(2, 3),
            control: AbstractHeap::new(4, 5),
            ssa_state: AbstractHeap::new(6, 7),
        }
    }
}

impl BaseHeaps {
    pub fn new() -> Self {
        Default::default()
    }
}

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

pub struct Program<'a> {
    heaps: BaseHeaps,
    interner: &'a mut DefaultStringInterner,
    insts: Vec<Inst>,
    insts_span: Vec<SourceSpan>,
    insts_free: HashSet<InstRef>,
    blocks: Vec<Block>,
    blocks_span: Vec<SourceSpan>,
    blocks_name: Vec<DefaultSymbol>,
    blocks_free: HashSet<BlockRef>,
    funcs: Vec<Func>,
    funcs_span: Vec<SourceSpan>,
    funcs_name: Vec<DefaultSymbol>,
    funcs_free: HashSet<FuncRef>,
}

impl<'a> Program<'a> {
    pub fn new(interner: &'a mut DefaultStringInterner) -> Self {
        let question_mark_symbol = interner.get_or_intern_static("?");
        Self {
            interner,
            heaps: BaseHeaps::new(),
            insts_free: HashSet::new(),
            funcs_free: HashSet::new(),
            blocks_free: HashSet::new(),
            insts: vec![Default::default()],
            funcs: vec![Default::default()],
            blocks: vec![Default::default()],
            insts_span: vec![Default::default()],
            funcs_span: vec![Default::default()],
            blocks_span: vec![Default::default()],
            funcs_name: vec![question_mark_symbol],
            blocks_name: vec![question_mark_symbol],
        }
    }

    pub fn inst(&self, inst: InstRef) -> &Inst {
        &self.insts[inst.0.get() as usize]
    }

    pub fn block(&self, block: BlockRef) -> &Block {
        &self.blocks[block.0.get() as usize]
    }

    pub fn func(&self, func: FuncRef) -> &Func {
        &self.funcs[func.0.get() as usize]
    }

    pub fn inst_span(&self, inst: InstRef) -> &SourceSpan {
        &self.insts_span[inst.0.get() as usize]
    }

    pub fn block_span(&self, block: BlockRef) -> &SourceSpan {
        &self.blocks_span[block.0.get() as usize]
    }

    pub fn func_span(&self, func: FuncRef) -> &SourceSpan {
        &self.funcs_span[func.0.get() as usize]
    }

    pub fn block_name(&self, block: BlockRef) -> &DefaultSymbol {
        &self.blocks_name[block.0.get() as usize]
    }

    pub fn func_name(&self, func: FuncRef) -> &DefaultSymbol {
        &self.funcs_name[func.0.get() as usize]
    }

    pub fn inst_mut(&mut self, inst: InstRef) -> &mut Inst {
        &mut self.insts[inst.0.get() as usize]
    }

    pub fn block_mut(&mut self, block: BlockRef) -> &mut Block {
        &mut self.blocks[block.0.get() as usize]
    }

    pub fn func_mut(&mut self, func: FuncRef) -> &mut Func {
        &mut self.funcs[func.0.get() as usize]
    }

    pub fn delete_inst(&mut self, inst: InstRef) {
        self.insts_free.insert(inst);
    }

    pub fn delete_block(&mut self, block: BlockRef) {
        self.blocks_free.insert(block);
    }

    pub fn delete_func(&mut self, func: FuncRef) {
        self.funcs_free.insert(func);
    }

    pub fn new_inst(&mut self, inst: Inst) -> InstRef {
        let idx = InstRef::new(self.insts.len());
        self.insts.push(inst);
        self.insts_span.push(Default::default());
        idx
    }

    pub fn new_block(&mut self, name: &str) -> BlockRef {
        let idx = BlockRef::new(self.blocks.len());
        self.blocks.push(Default::default());
        self.blocks_span.push(Default::default());
        self.blocks_name.push(self.interner.get_or_intern(name));
        idx
    }

    pub fn new_func(&mut self, name: &str) -> FuncRef {
        let idx = FuncRef::new(self.funcs.len());
        self.funcs.push(Default::default());
        self.funcs_span.push(Default::default());
        self.funcs_name.push(self.interner.get_or_intern(name));
        idx
    }

    pub fn new_block_interned(&mut self, name: DefaultSymbol) -> BlockRef {
        let idx = BlockRef::new(self.blocks.len());
        self.blocks.push(Default::default());
        self.blocks_span.push(Default::default());
        self.blocks_name.push(name);
        idx
    }

    pub fn new_func_interned(&mut self, name: DefaultSymbol) -> FuncRef {
        let idx = FuncRef::new(self.funcs.len());
        self.funcs.push(Default::default());
        self.funcs_span.push(Default::default());
        self.funcs_name.push(name);
        idx
    }

    pub fn new_inst_with_span(&mut self, inst: Inst, span: SourceSpan) -> InstRef {
        let r = InstRef::new(self.insts.len());
        self.insts.push(inst);
        self.insts_span.push(span);
        r
    }

    pub fn new_block_with_span(&mut self, name: &str, span: SourceSpan) -> BlockRef {
        let r = BlockRef::new(self.blocks.len());
        self.blocks.push(Default::default());
        self.blocks_span.push(span);
        self.blocks_name.push(self.interner.get_or_intern(name));
        r
    }

    pub fn new_func_with_span(&mut self, name: &str, span: SourceSpan) -> FuncRef {
        let r = FuncRef::new(self.funcs.len());
        self.funcs.push(Default::default());
        self.funcs_span.push(span);
        self.funcs_name.push(self.interner.get_or_intern(name));
        r
    }

    pub fn new_block_with_span_interned(
        &mut self,
        name: DefaultSymbol,
        span: SourceSpan,
    ) -> BlockRef {
        let r = BlockRef::new(self.blocks.len());
        self.blocks.push(Default::default());
        self.blocks_span.push(span);
        self.blocks_name.push(name);
        r
    }

    pub fn new_func_with_span_interned(
        &mut self,
        name: DefaultSymbol,
        span: SourceSpan,
    ) -> FuncRef {
        let r = FuncRef::new(self.funcs.len());
        self.funcs.push(Default::default());
        self.funcs_span.push(span);
        self.funcs_name.push(name);
        r
    }

    pub fn insts_iter(&self) -> impl Iterator<Item = InstRef> {
        // TODO: Skip deleted insts
        unsafe { (1..self.insts.len()).map(|i| InstRef::new_unchecked(i)) }
    }

    pub fn blocks_iter(&self) -> impl Iterator<Item = BlockRef> {
        // TODO: Skip deleted blocks
        unsafe { (1..self.blocks.len()).map(|i| BlockRef::new_unchecked(i)) }
    }

    pub fn funcs_iter(&self) -> impl Iterator<Item = FuncRef> {
        // TODO: Skip deleted funcs
        unsafe { (1..self.funcs.len()).map(|i| FuncRef::new_unchecked(i)) }
    }

    pub fn insts_iter2(&self) -> impl Iterator<Item = (InstRef, &Inst)> {
        // TODO: Skip deleted insts
        self.insts_iter().zip(self.insts.iter().skip(1))
    }

    pub fn blocks_iter2(&self) -> impl Iterator<Item = (BlockRef, &Block)> {
        // TODO: Skip deleted blocks
        self.blocks_iter().zip(self.blocks.iter().skip(1))
    }

    pub fn funcs_iter2(&self) -> impl Iterator<Item = (FuncRef, &Func)> {
        // TODO: Skip deleted funcs
        self.funcs_iter().zip(self.funcs.iter().skip(1))
    }

    pub fn get_phi_args(&self, phi: InstRef, args: &mut Vec<InstRef>) {
        // TODO: build a function that builds this for all phis??
        for (idx, inst) in self.insts.iter().enumerate().skip(1) {
            if let InstKind::Upsilon { phi: p, .. } = &inst.kind {
                if *p == phi {
                    args.push(InstRef::new(idx))
                }
            }
        }
    }

    pub fn get_predecessors(&self, pred: &mut HashMap<BlockRef, Vec<BlockRef>>) {
        for (idx, block) in self.blocks.iter().enumerate() {
            for succ in &block.succs {
                pred.entry(*succ)
                    .or_insert(Vec::new())
                    .push(BlockRef::new(idx));
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl fmt::Display for InstRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0.get())
    }
}

impl fmt::Display for BlockRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0.get())
    }
}

impl fmt::Display for FuncRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f{}", self.0.get())
    }
}

impl fmt::Display for InstKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstKind::Ret(i) => write!(f, "Ret({})", i),
            InstKind::Jump(b) => write!(f, "Jump({})", b),
            InstKind::Load(ptr) => write!(f, "Load({})", ptr),
            InstKind::Identity(i) => write!(f, "Identity({})", i),
            InstKind::Store { val, ptr } => write!(f, "Store {{ ptr: {}, val: {} }}", ptr, val),
            InstKind::Upsilon { val, phi } => {
                write!(f, "Upsilon {{ val: {}, phi: {} }}", val, phi)
            }
            InstKind::Unary { op, val } => {
                write!(f, "{:?}({})", op, val)
            }
            InstKind::Binary { op, lhs, rhs } => {
                write!(f, "{:?}({}, {})", op, lhs, rhs)
            }
            InstKind::Branch {
                val: cond,
                then,
                other,
            } => {
                write!(
                    f,
                    "Branch {{ cond: {}, then: {}, other: {} }}",
                    cond, then, other
                )
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, func) in self.funcs_name.iter().zip(self.funcs.iter()).skip(1) {
            write!(f, "define @{}", self.interner.resolve(*name).unwrap_or("?"))?;
            for block in &func.blocks {
                let name = *self.block_name(*block);
                write!(f, "\n{}:", self.interner.resolve(name).unwrap_or("?"))?;
                for i in &self.block(*block).insts {
                    let inst = self.inst(*i);
                    write!(f, "\n  {:?} {} = {}", inst.ty, i, inst.kind)?;
                }
                if !self.block(*block).succs.is_empty() {
                    write!(
                        f,
                        "\n successors: {}",
                        self.block(*block)
                            .succs
                            .iter()
                            .map(|b| self.interner.resolve(*self.block_name(*b)).unwrap_or("?"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
            }
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// EntityRef<T>
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct EntityRef<T>(NonZeroU32, PhantomData<T>);

impl<T> EntityRef<T> {
    pub fn new(idx: usize) -> Self {
        Self(
            NonZeroU32::new(idx as u32).expect("expected a positive index"),
            PhantomData,
        )
    }

    unsafe fn new_unchecked(idx: usize) -> Self {
        Self(NonZeroU32::new_unchecked(idx as u32), PhantomData)
    }
}

impl<T> Copy for EntityRef<T> {}
impl<T> Clone for EntityRef<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T> Eq for EntityRef<T> {}
impl<T> PartialEq for EntityRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> std::hash::Hash for EntityRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
