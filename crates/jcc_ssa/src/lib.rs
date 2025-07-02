pub mod amd64;
pub mod effects;
pub mod insertion;
pub mod verify;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Inst {
    pub ty: Type,
    pub kind: InstKind,
    pub block: BlockRef,
    pub span: SourceSpan,
    pub(crate) idx: InstIdx,
}

impl Default for Inst {
    fn default() -> Self {
        Self {
            ty: Default::default(),
            idx: Default::default(),
            kind: Default::default(),
            span: Default::default(),
            block: BlockRef(NonZeroU32::new(u32::MAX).unwrap()),
        }
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
    pub fn nop(span: SourceSpan) -> Self {
        Self::new(Type::Void, InstKind::Nop, span)
    }

    #[inline]
    pub fn alloca(span: SourceSpan) -> Self {
        Self::new(Type::IntPtr, InstKind::Alloca, span)
    }

    #[inline]
    pub fn phi(ty: Type, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Phi, span)
    }

    #[inline]
    pub fn arg(ty: Type, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Arg, span)
    }

    #[inline]
    pub fn ret(val: InstRef, span: SourceSpan) -> Self {
        Self::new(Type::Void, InstKind::Ret(val), span)
    }

    #[inline]
    pub fn const_i32(val: i64, span: SourceSpan) -> Self {
        Self::new(Type::Int32, InstKind::Const(val), span)
    }

    #[inline]
    pub fn const_i64(val: i64, span: SourceSpan) -> Self {
        Self::new(Type::Int64, InstKind::Const(val), span)
    }

    #[inline]
    pub fn load(ptr: InstRef, span: SourceSpan) -> Self {
        Self::new(Type::Int32, InstKind::Load(ptr), span)
    }

    #[inline]
    pub fn jump(block: BlockRef, span: SourceSpan) -> Self {
        Self::new(Type::Void, InstKind::Jump(block), span)
    }

    #[inline]
    pub fn static_addr(var: Symbol, span: SourceSpan) -> Self {
        Self::new(Type::IntPtr, InstKind::StaticAddr(var), span)
    }

    #[inline]
    pub fn store(ptr: InstRef, val: InstRef, span: SourceSpan) -> Self {
        Self::new(Type::Void, InstKind::Store { ptr, val }, span)
    }

    #[inline]
    pub fn upsilon(phi: InstRef, val: InstRef, span: SourceSpan) -> Self {
        Self::new(Type::Void, InstKind::Upsilon { phi, val }, span)
    }

    #[inline]
    pub fn call(func: FuncRef, args: Vec<InstRef>, span: SourceSpan) -> Self {
        Self::new(Type::Void, InstKind::Call { func, args }, span)
    }

    #[inline]
    pub fn unary(ty: Type, op: UnaryOp, val: InstRef, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Unary { op, val }, span)
    }

    #[inline]
    pub fn branch(cond: InstRef, then: BlockRef, other: BlockRef, span: SourceSpan) -> Self {
        Self::new(Type::Void, InstKind::Branch { cond, then, other }, span)
    }

    #[inline]
    pub fn binary(ty: Type, op: BinaryOp, lhs: InstRef, rhs: InstRef, span: SourceSpan) -> Self {
        Self::new(ty, InstKind::Binary { op, lhs, rhs }, span)
    }

    #[inline]
    pub fn select(
        ty: Type,
        cond: InstRef,
        then: InstRef,
        other: InstRef,
        span: SourceSpan,
    ) -> Self {
        Self::new(ty, InstKind::Select { cond, then, other }, span)
    }

    #[inline]
    pub fn switch(
        cond: InstRef,
        default: BlockRef,
        cases: Vec<(i64, BlockRef)>,
        span: SourceSpan,
    ) -> Self {
        Self::new(
            Type::Void,
            InstKind::Switch {
                cond,
                cases,
                default,
            },
            span,
        )
    }

    // ---------------------------------------------------------------------------
    // Utility methods
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn is_const(&self, val: i64) -> bool {
        matches!(self.kind, InstKind::Const(v) if v == val)
    }

    pub fn has_side_effects(&self) -> bool {
        matches!(
            self.kind,
            InstKind::Ret(_)
                | InstKind::Jump(_)
                | InstKind::Branch { .. }
                | InstKind::Switch { .. }
                | InstKind::Store { .. }
                | InstKind::Call { .. }
        )
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

    pub fn get_args(&self, args: &mut Vec<InstRef>) {
        match &self.kind {
            InstKind::Ret(val)
            | InstKind::Load(val)
            | InstKind::Identity(val)
            | InstKind::Unary { val, .. } => args.push(*val),
            InstKind::Call { args: vals, .. } => args.extend(vals),
            InstKind::Store { ptr, val } => args.extend([*ptr, *val]),
            InstKind::Upsilon { phi, val } => args.extend([*phi, *val]),
            InstKind::Binary { lhs, rhs, .. } => args.extend([*lhs, *rhs]),
            InstKind::Select { cond, then, other } => args.extend([*cond, *then, *other]),
            InstKind::Branch { cond, .. } | InstKind::Switch { cond, .. } => args.push(*cond),
            InstKind::Nop
            | InstKind::Phi
            | InstKind::Arg
            | InstKind::Alloca
            | InstKind::Const(_)
            | InstKind::Jump(_)
            | InstKind::StaticAddr(_) => {}
        }
    }

    pub fn replace_args(&mut self, map: &HashMap<InstRef, InstRef>) {
        let replace = |r: &mut InstRef| {
            if let Some(new_ref) = map.get(r) {
                *r = *new_ref;
            }
        };

        match &mut self.kind {
            InstKind::Ret(val)
            | InstKind::Load(val)
            | InstKind::Identity(val)
            | InstKind::Unary { val, .. } => replace(val),
            InstKind::Store { ptr, val } => {
                replace(ptr);
                replace(val);
            }
            InstKind::Upsilon { phi, val } => {
                replace(phi);
                replace(val);
            }
            InstKind::Binary { lhs, rhs, .. } => {
                replace(lhs);
                replace(rhs);
            }
            InstKind::Select { cond, then, other } => {
                replace(cond);
                replace(then);
                replace(other);
            }
            InstKind::Branch { cond, .. } | InstKind::Switch { cond, .. } => replace(cond),
            InstKind::Call { args, .. } => {
                for arg in args {
                    replace(arg);
                }
            }
            InstKind::Nop
            | InstKind::Phi
            | InstKind::Arg
            | InstKind::Alloca
            | InstKind::Jump(_)
            | InstKind::Const(_)
            | InstKind::StaticAddr(_) => {}
        }
    }
}

// ---------------------------------------------------------------------------
// InstKind
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum InstKind {
    #[default]
    Nop,
    Phi,
    Arg,
    Alloca,
    Const(i64),
    Ret(InstRef),
    Jump(BlockRef),
    Load(InstRef),
    Identity(InstRef),
    StaticAddr(Symbol),
    Store {
        ptr: InstRef,
        val: InstRef,
    },
    Upsilon {
        phi: InstRef,
        val: InstRef,
    },
    Unary {
        op: UnaryOp,
        val: InstRef,
    },
    Binary {
        op: BinaryOp,
        lhs: InstRef,
        rhs: InstRef,
    },
    Select {
        cond: InstRef,
        then: InstRef,
        other: InstRef,
    },
    Branch {
        cond: InstRef,
        then: BlockRef,
        other: BlockRef,
    },
    Switch {
        cond: InstRef,
        default: BlockRef,
        cases: Vec<(i64, BlockRef)>,
    },
    Call {
        func: FuncRef,
        args: Vec<InstRef>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    Neg,
    Inc,
    Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Or,
    And,
    Xor,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockRef(NonZeroU32);

#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct BlockIdx(pub(crate) u32);

#[derive(Debug, Default, Clone)]
pub struct Block {
    pub name: Symbol,
    pub span: SourceSpan,
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
    pub name: Symbol,
    pub is_global: bool,
    pub span: SourceSpan,
    pub blocks: Vec<BlockRef>,
}

// ---------------------------------------------------------------------------
// StaticVar
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone)]
pub struct StaticVar {
    pub ty: Type,
    pub is_global: bool,
    pub span: SourceSpan,
    pub init: Option<i64>,
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

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

pub struct Program {
    heaps: BaseHeaps,
    insts: Vec<Inst>,
    funcs: Vec<Func>,
    blocks: Vec<Block>,
    insts_free: HashSet<InstRef>,
    funcs_free: HashSet<FuncRef>,
    blocks_free: HashSet<BlockRef>,
    pub interner: Interner,
    pub static_vars: HashMap<Symbol, StaticVar>,
}

impl Program {
    pub fn new(interner: Interner) -> Self {
        Self {
            interner,
            heaps: Default::default(),
            insts_free: HashSet::new(),
            funcs_free: HashSet::new(),
            static_vars: HashMap::new(),
            blocks_free: HashSet::new(),
            insts: vec![Default::default()],
            funcs: vec![Default::default()],
            blocks: vec![Default::default()],
        }
    }

    pub fn take_interner(self) -> Interner {
        self.interner
    }

    // ---------------------------------------------------------------------------
    // Accessors
    // ---------------------------------------------------------------------------

    pub fn inst(&self, inst: InstRef) -> &Inst {
        &self.insts[inst.0.get() as usize]
    }

    pub fn func(&self, func: FuncRef) -> &Func {
        &self.funcs[func.0.get() as usize]
    }

    pub fn block(&self, block: BlockRef) -> &Block {
        &self.blocks[block.0.get() as usize]
    }

    pub fn inst_mut(&mut self, inst: InstRef) -> &mut Inst {
        &mut self.insts[inst.0.get() as usize]
    }

    pub fn func_mut(&mut self, func: FuncRef) -> &mut Func {
        &mut self.funcs[func.0.get() as usize]
    }

    pub fn block_mut(&mut self, block: BlockRef) -> &mut Block {
        &mut self.blocks[block.0.get() as usize]
    }

    // ---------------------------------------------------------------------------
    // Creation
    // ---------------------------------------------------------------------------

    pub fn new_inst(&mut self, inst: Inst) -> InstRef {
        // TODO: Reuse slots from `insts_free` for better memory efficiency
        let r = InstRef(NonZeroU32::new(self.insts.len() as u32).unwrap());
        self.insts.push(inst);
        r
    }

    pub fn new_func(&mut self, func: Func) -> FuncRef {
        // TODO: Reuse slots from `funcs_free` for better memory efficiency
        let r = FuncRef(NonZeroU32::new(self.funcs.len() as u32).unwrap());
        self.funcs.push(func);
        r
    }

    pub fn new_block(&mut self, block: Block) -> BlockRef {
        // TODO: Reuse slots from `blocks_free` for better memory efficiency
        let r = BlockRef(NonZeroU32::new(self.blocks.len() as u32).unwrap());
        self.blocks.push(block);
        r
    }

    // ---------------------------------------------------------------------------
    // Deletion
    // ---------------------------------------------------------------------------

    pub fn delete_inst(&mut self, inst_ref: InstRef) {
        self.insts_free.insert(inst_ref);
    }

    pub fn delete_func(&mut self, func_ref: FuncRef) {
        self.funcs_free.insert(func_ref);
    }

    pub fn delete_block(&mut self, block_ref: BlockRef) {
        self.blocks_free.insert(block_ref);
    }

    // ---------------------------------------------------------------------------
    // Iterators
    // ---------------------------------------------------------------------------

    pub fn iter_insts(&self) -> impl Iterator<Item = InstRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.insts.len())
            .map(|i| unsafe { InstRef(NonZeroU32::new_unchecked(i as u32)) })
            .filter(move |r| !self.insts_free.contains(r))
    }

    pub fn iter_funcs(&self) -> impl Iterator<Item = FuncRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.funcs.len())
            .map(|i| unsafe { FuncRef(NonZeroU32::new_unchecked(i as u32)) })
            .filter(move |r| !self.funcs_free.contains(r))
    }

    pub fn iter_blocks(&self) -> impl Iterator<Item = BlockRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.blocks.len())
            .map(|i| unsafe { BlockRef(NonZeroU32::new_unchecked(i as u32)) })
            .filter(move |r| !self.blocks_free.contains(r))
    }

    pub fn iter_insts_with_refs(&self) -> impl Iterator<Item = (InstRef, &Inst)> {
        self.iter_insts().map(move |r| (r, self.inst(r)))
    }

    pub fn iter_funcs_with_refs(&self) -> impl Iterator<Item = (FuncRef, &Func)> {
        self.iter_funcs().map(move |r| (r, self.func(r)))
    }

    pub fn iter_blocks_with_refs(&self) -> impl Iterator<Item = (BlockRef, &Block)> {
        self.iter_blocks().map(move |r| (r, self.block(r)))
    }

    // ---------------------------------------------------------------------------
    // Complexity Queries
    // ---------------------------------------------------------------------------

    pub fn get_predecessors(&self, pred: &mut HashMap<BlockRef, Vec<BlockRef>>) {
        pred.clear();
        for (block_ref, block) in self.iter_blocks_with_refs() {
            for succ in &block.succs {
                pred.entry(*succ).or_default().push(block_ref);
            }
        }
    }

    pub fn get_phi_args(&self, phi: InstRef, args: &mut Vec<InstRef>) {
        // This is inefficient (O(N_insts)) but kept as per user request.
        for (idx, inst) in self.insts.iter().enumerate().skip(1) {
            if let InstKind::Upsilon { phi: p, .. } = &inst.kind {
                if *p == phi {
                    // This check is important to avoid yielding deleted upsilons
                    let inst_ref = InstRef(NonZeroU32::new(idx as u32).unwrap());
                    if !self.insts_free.contains(&inst_ref) {
                        args.push(inst_ref);
                    }
                }
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
            InstKind::Nop => write!(f, "nop"),
            InstKind::Phi => write!(f, "phi"),
            InstKind::Arg => write!(f, "arg"),
            InstKind::Alloca => write!(f, "alloca"),
            InstKind::Ret(i) => write!(f, "ret {}", i),
            InstKind::Jump(b) => write!(f, "jump {}", b),
            InstKind::Const(i) => write!(f, "const {}", i),
            InstKind::Load(ptr) => write!(f, "load {}", ptr),
            InstKind::Identity(i) => write!(f, "identity {}", i),
            InstKind::StaticAddr(name) => write!(f, "static_addr {:?}", name),
            InstKind::Store { val, ptr } => write!(f, "store {}, {}", ptr, val),
            InstKind::Upsilon { phi, val } => write!(f, "upsilon {}, {}", phi, val),
            InstKind::Unary { op, val } => write!(f, "{:?} {}", op, val),
            InstKind::Binary { op, lhs, rhs } => write!(f, "{:?} {}, {}", op, lhs, rhs),
            InstKind::Select { cond, then, other } => {
                write!(f, "select {}, {}, {}", cond, then, other)
            }
            InstKind::Branch { cond, then, other } => {
                write!(f, "branch {}, {}, {}", cond, then, other)
            }
            InstKind::Switch {
                cond,
                cases,
                default,
            } => {
                write!(
                    f,
                    "switch {}, default: {}, cases: {:?}",
                    cond, default, cases
                )
            }
            InstKind::Call { func, args } => {
                let args_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "call {}({})", func, args_str)
            }
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, var) in self.static_vars.iter() {
            let name_str = self.interner.lookup(*name);
            let linkage = if var.is_global { "global" } else { "static" };
            writeln!(f, "{} {} = {:?};", linkage, name_str, var.init)?;
        }
        if !self.static_vars.is_empty() {
            writeln!(f)?;
        }
        for (func_ref, func) in self.iter_funcs_with_refs() {
            let func_name = self.interner.lookup(func.name);
            writeln!(f, "define @{}({}) {{", func_name, func_ref)?;

            for block_ref in &func.blocks {
                if self.blocks_free.contains(block_ref) {
                    continue;
                }
                let block = self.block(*block_ref);
                let block_name = self.interner.lookup(block.name);
                writeln!(f, "{}: ; preds: TODO", block_name)?;
                for inst_ref in &block.insts {
                    let inst = self.inst(*inst_ref);
                    if inst.ty == Type::Void {
                        writeln!(f, "  {};", inst.kind)?;
                    } else {
                        writeln!(f, "  {} = {:?} {};", inst_ref, inst.ty, inst.kind)?;
                    }
                }
            }
            writeln!(f, "}}")?;
        }
        Ok(())
    }
}
