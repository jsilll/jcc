pub mod verify;

pub mod effects;

pub mod insertion;

pub mod amd64;

pub use jcc_interner as interner;
pub use jcc_sourcemap as sourcemap;

use effects::{AbstractHeap, FastEffects};

use jcc_interner::{Interner, Symbol};
use jcc_sourcemap::SourceSpan;

use std::{
    collections::{HashMap, HashSet},
    fmt,
    num::NonZeroU32,
};

// ---------------------------------------------------------------------------
// Type
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    #[default]
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::Void => 0,
            Type::Int8 => 1,
            Type::Int16 => 2,
            Type::Int32 => 4,
            Type::Int64 => 8,
            Type::IntPtr => 8,
        }
    }
}

// ---------------------------------------------------------------------------
// Inst
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstRef(NonZeroU32);

#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct InstIdx(pub(crate) u32);

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Inst {
    pub ty: Type,
    pub kind: InstKind,
    pub(crate) idx: InstIdx,
    // pub block: BlockRef,
}

impl Inst {
    fn new(ty: Type, kind: InstKind) -> Self {
        Self {
            ty,
            kind,
            idx: InstIdx::default(),
        }
    }

    pub fn phi(ty: Type) -> Self {
        Self::new(ty, InstKind::Phi)
    }

    pub fn arg(ty: Type) -> Self {
        Self::new(ty, InstKind::Arg)
    }

    pub fn nop() -> Self {
        Self::new(Type::Void, InstKind::Nop)
    }

    pub fn alloca() -> Self {
        Self::new(Type::IntPtr, InstKind::Alloca)
    }

    pub fn ret(val: InstRef) -> Self {
        Self::new(Type::Void, InstKind::Ret(val))
    }

    pub fn const_i32(val: i64) -> Self {
        Self::new(Type::Int32, InstKind::Const(val))
    }

    pub fn const_i64(val: i64) -> Self {
        Self::new(Type::Int64, InstKind::Const(val))
    }

    pub fn load(ptr: InstRef) -> Self {
        Self::new(Type::Int32, InstKind::Load(ptr))
    }

    pub fn jump(block: BlockRef) -> Self {
        Self::new(Type::Void, InstKind::Jump(block))
    }

    pub fn store(ptr: InstRef, val: InstRef) -> Self {
        Self::new(Type::Void, InstKind::Store { ptr, val })
    }

    pub fn upsilon(phi: InstRef, val: InstRef) -> Self {
        Self::new(Type::Void, InstKind::Upsilon { phi, val })
    }

    pub fn unary(ty: Type, op: UnaryOp, val: InstRef) -> Self {
        Self::new(ty, InstKind::Unary { op, val })
    }

    pub fn binary(ty: Type, op: BinaryOp, lhs: InstRef, rhs: InstRef) -> Self {
        Self::new(ty, InstKind::Binary { op, lhs, rhs })
    }

    pub fn select(ty: Type, cond: InstRef, then: InstRef, other: InstRef) -> Self {
        Self::new(ty, InstKind::Select { cond, then, other })
    }

    pub fn branch(val: InstRef, then: BlockRef, other: BlockRef) -> Self {
        Self::new(
            Type::Void,
            InstKind::Branch {
                cond: val,
                then,
                other,
            },
        )
    }

    pub fn switch(cond: InstRef, default: BlockRef, cases: Vec<(i64, BlockRef)>) -> Self {
        Self::new(
            Type::Void,
            InstKind::Switch {
                cond,
                cases,
                default,
            },
        )
    }

    pub fn call(func: FuncRef, args: Vec<InstRef>) -> Self {
        Self::new(Type::Void, InstKind::Call { func, args })
    }

    pub fn is_const(&self, val: i64) -> bool {
        match self.kind {
            InstKind::Const(v) => v == val,
            _ => false,
        }
    }

    pub fn has_side_effects(&self) -> bool {
        matches!(
            self.kind,
            InstKind::Ret(_)
                | InstKind::Jump(_)
                | InstKind::Branch { .. }
                | InstKind::Switch { .. }
        )
    }

    pub fn get_args(&self, args: &mut Vec<InstRef>) {
        match &self.kind {
            InstKind::Nop
            | InstKind::Phi
            | InstKind::Arg
            | InstKind::Alloca
            | InstKind::Jump(_)
            | InstKind::Const(_) => {}
            InstKind::Ret(val)
            | InstKind::Load(val)
            | InstKind::Identity(val)
            | InstKind::Unary { val, .. }
            | InstKind::Select { cond: val, .. }
            | InstKind::Branch { cond: val, .. }
            | InstKind::Switch { cond: val, .. } => args.push(*val),
            InstKind::Store { ptr, val } => args.extend([*ptr, *val]),
            InstKind::Upsilon { phi, val } => args.extend([*phi, *val]),
            InstKind::Binary { lhs, rhs, .. } => args.extend([*lhs, *rhs]),
            InstKind::Call { args: vals, .. } => args.extend(vals),
        }
    }

    pub fn get_effects(&self, prog: &Program, effects: &mut FastEffects) {
        // TODO: Complete
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
        // TODO: Complete
        match &mut self.kind {
            InstKind::Ret(val)
            | InstKind::Identity(val)
            | InstKind::Unary { val, .. }
            | InstKind::Branch { cond: val, .. } => {
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
    Upsilon { phi: InstRef, val: InstRef },
    /// A unary instruction.
    Unary { op: UnaryOp, val: InstRef },
    /// A binary instruction.
    Binary {
        op: BinaryOp,
        lhs: InstRef,
        rhs: InstRef,
    },
    /// A select instruction.
    Select {
        cond: InstRef,
        then: InstRef,
        other: InstRef,
    },
    /// A branch instruction.
    Branch {
        cond: InstRef,
        then: BlockRef,
        other: BlockRef,
    },
    /// A switch instruction.
    Switch {
        cond: InstRef,
        default: BlockRef,
        cases: Vec<(i64, BlockRef)>,
    },
    /// A function call instruction.
    Call { func: FuncRef, args: Vec<InstRef> },
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// The bit or operator.
    Or,
    /// The bit and operator.
    And,
    /// The bit xor operator.
    Xor,
    /// The bit shift left operator.
    Shl,
    /// The bit shift right operator.
    Shr,
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

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockRef(NonZeroU32);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncRef(NonZeroU32);

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

pub struct Program {
    heaps: BaseHeaps,
    interner: Interner,
    insts: Vec<Inst>,
    insts_span: Vec<SourceSpan>,
    insts_free: HashSet<InstRef>,
    blocks: Vec<Block>,
    blocks_name: Vec<Symbol>,
    blocks_span: Vec<SourceSpan>,
    blocks_free: HashSet<BlockRef>,
    funcs: Vec<Func>,
    funcs_name: Vec<Symbol>,
    funcs_span: Vec<SourceSpan>,
    funcs_free: HashSet<FuncRef>,
}

impl Program {
    pub fn new(mut interner: Interner) -> Self {
        let question_mark_symbol = interner.intern("?");
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

    pub fn take_interner(self) -> Interner {
        self.interner
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

    pub fn block_name(&self, block: BlockRef) -> &Symbol {
        &self.blocks_name[block.0.get() as usize]
    }

    pub fn func_name(&self, func: FuncRef) -> &Symbol {
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
        let r = InstRef(NonZeroU32::new(self.insts.len() as u32).unwrap());
        self.insts.push(inst);
        self.insts_span.push(Default::default());
        r
    }

    pub fn new_block(&mut self, name: &str) -> BlockRef {
        let r = BlockRef(NonZeroU32::new(self.blocks.len() as u32).unwrap());
        self.blocks.push(Default::default());
        self.blocks_span.push(Default::default());
        self.blocks_name.push(self.interner.intern(name));
        r
    }

    pub fn new_func(&mut self, name: &str) -> FuncRef {
        let r = FuncRef(NonZeroU32::new(self.funcs.len() as u32).unwrap());
        self.funcs.push(Default::default());
        self.funcs_span.push(Default::default());
        self.funcs_name.push(self.interner.intern(name));
        r
    }

    pub fn new_block_interned(&mut self, name: Symbol) -> BlockRef {
        let r = BlockRef(NonZeroU32::new(self.blocks.len() as u32).unwrap());
        self.blocks.push(Default::default());
        self.blocks_span.push(Default::default());
        self.blocks_name.push(name);
        r
    }

    pub fn new_func_interned(&mut self, name: Symbol) -> FuncRef {
        let r = FuncRef(NonZeroU32::new(self.funcs.len() as u32).unwrap());
        self.funcs.push(Default::default());
        self.funcs_span.push(Default::default());
        self.funcs_name.push(name);
        r
    }

    pub fn new_inst_with_span(&mut self, inst: Inst, span: SourceSpan) -> InstRef {
        let r = InstRef(NonZeroU32::new(self.insts.len() as u32).unwrap());
        self.insts.push(inst);
        self.insts_span.push(span);
        r
    }

    pub fn new_block_with_span(&mut self, name: &str, span: SourceSpan) -> BlockRef {
        let r = BlockRef(NonZeroU32::new(self.blocks.len() as u32).unwrap());
        self.blocks.push(Default::default());
        self.blocks_span.push(span);
        self.blocks_name.push(self.interner.intern(name));
        r
    }

    pub fn new_func_with_span(&mut self, name: &str, span: SourceSpan) -> FuncRef {
        let r = FuncRef(NonZeroU32::new(self.funcs.len() as u32).unwrap());
        self.funcs.push(Default::default());
        self.funcs_span.push(span);
        self.funcs_name.push(self.interner.intern(name));
        r
    }

    pub fn new_block_with_span_interned(&mut self, name: Symbol, span: SourceSpan) -> BlockRef {
        let r = BlockRef(NonZeroU32::new(self.blocks.len() as u32).unwrap());
        self.blocks.push(Default::default());
        self.blocks_span.push(span);
        self.blocks_name.push(name);
        r
    }

    pub fn new_func_with_span_interned(&mut self, name: Symbol, span: SourceSpan) -> FuncRef {
        let r = FuncRef(NonZeroU32::new(self.funcs.len() as u32).unwrap());
        self.funcs.push(Default::default());
        self.funcs_span.push(span);
        self.funcs_name.push(name);
        r
    }

    pub fn insts_iter(&self) -> impl Iterator<Item = InstRef> {
        // TODO: Skip deleted insts
        unsafe { (1..self.insts.len()).map(|i| InstRef(NonZeroU32::new_unchecked(i as u32))) }
    }

    pub fn blocks_iter(&self) -> impl Iterator<Item = BlockRef> {
        // TODO: Skip deleted blocks
        unsafe { (1..self.blocks.len()).map(|i| BlockRef(NonZeroU32::new_unchecked(i as u32))) }
    }

    pub fn funcs_iter(&self) -> impl Iterator<Item = FuncRef> {
        // TODO: Skip deleted funcs
        unsafe { (1..self.funcs.len()).map(|i| FuncRef(NonZeroU32::new_unchecked(i as u32))) }
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
                    args.push(InstRef(NonZeroU32::new(idx as u32).unwrap()));
                }
            }
        }
    }

    pub fn get_predecessors(&self, pred: &mut HashMap<BlockRef, Vec<BlockRef>>) {
        for (idx, block) in self.blocks.iter().enumerate() {
            for succ in &block.succs {
                pred.entry(*succ)
                    .or_default()
                    .push(BlockRef(NonZeroU32::new(idx as u32).unwrap()));
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
            InstKind::Nop => write!(f, "Nop"),
            InstKind::Phi => write!(f, "Phi"),
            InstKind::Arg => write!(f, "Arg"),
            InstKind::Alloca => write!(f, "Alloca"),
            InstKind::Ret(i) => write!(f, "Ret({})", i),
            InstKind::Jump(b) => write!(f, "Jump({})", b),
            InstKind::Const(i) => write!(f, "Const({})", i),
            InstKind::Load(ptr) => write!(f, "Load({})", ptr),
            InstKind::Identity(i) => write!(f, "Identity({})", i),
            InstKind::Store { val, ptr } => write!(f, "Store {{ ptr: {}, val: {} }}", ptr, val),
            InstKind::Upsilon { phi, val } => {
                write!(f, "Upsilon {{ phi: {}, val: {} }}", phi, val)
            }
            InstKind::Unary { op, val } => {
                write!(f, "{:?}({})", op, val)
            }
            InstKind::Binary { op, lhs, rhs } => {
                write!(f, "{:?}({}, {})", op, lhs, rhs)
            }
            InstKind::Select { cond, then, other } => {
                write!(
                    f,
                    "Select {{ cond: {}, then: {}, other: {} }}",
                    cond, then, other
                )
            }
            InstKind::Branch { cond, then, other } => {
                write!(
                    f,
                    "Branch {{ cond: {}, then: {}, other: {} }}",
                    cond, then, other
                )
            }
            InstKind::Switch {
                cond,
                cases,
                default,
            } => {
                write!(
                    f,
                    "Switch {{ cond: {}, default: {}, cases: {:?} }}",
                    cond, default, cases
                )
            }
            InstKind::Call { func, args } => {
                write!(f, "Call {{ func: {}, args: {:?} }}", func, args)
            }
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, func) in self.funcs_name.iter().zip(self.funcs.iter()).skip(1) {
            write!(f, "define @{}", self.interner.lookup(*name))?;
            for block in &func.blocks {
                let name = *self.block_name(*block);
                write!(f, "\n{}:", self.interner.lookup(name))?;
                for i in &self.block(*block).insts {
                    let inst = self.inst(*i);
                    // TODO: properly print the names of blocks instead of 'b<idx>'
                    write!(f, "\n  {:?} {} = {}", inst.ty, i, inst.kind)?;
                }
                if !self.block(*block).succs.is_empty() {
                    write!(
                        f,
                        "\n successors: {}",
                        self.block(*block)
                            .succs
                            .iter()
                            .map(|b| self.interner.lookup(*self.block_name(*b)))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
            }
        }
        Ok(())
    }
}
