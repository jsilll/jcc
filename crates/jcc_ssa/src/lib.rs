pub mod amd64;
pub mod builder;
pub mod effects;
pub mod insertion;
pub mod verify;

pub use jcc_interner as interner;
pub use jcc_sourcemap as sourcemap;

use effects::{AbstractHeap, FastEffects};
use interner::{Interner, Symbol};
use sourcemap::SourceSpan;

use std::{collections::HashMap, fmt, num::NonZeroU32};

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
    pub block: BlockRef,
    pub span: SourceSpan,
    pub(crate) idx: InstIdx,
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
    pub fn static_addr(var: StaticVarRef, span: SourceSpan) -> Self {
        Self::new(Type::IntPtr, InstKind::Static(var), span)
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
            | InstKind::Static(_) => {}
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
            | InstKind::Static(_) => {}
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
    Load(InstRef),
    Jump(BlockRef),
    Identity(InstRef),
    Static(StaticVarRef),
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

impl Default for BlockRef {
    fn default() -> Self {
        Self(NonZeroU32::new(u32::MAX).unwrap())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct BlockIdx(u32);

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

impl Default for FuncRef {
    fn default() -> Self {
        Self(NonZeroU32::new(u32::MAX).unwrap())
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StaticVarRef(NonZeroU32);

#[derive(Debug, Default, Clone)]
pub struct StaticVar {
    pub ty: Type,
    pub name: Symbol,
    pub is_global: bool,
    pub init: Option<i64>,
    pub span: SourceSpan,
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

pub struct Program<'a> {
    pub interner: &'a mut Interner,
    heaps: BaseHeaps,
    insts: Vec<Inst>,
    insts_free: Vec<InstRef>,
    funcs: Vec<Func>,
    funcs_free: Vec<FuncRef>,
    blocks: Vec<Block>,
    blocks_free: Vec<BlockRef>,
    static_vars: Vec<StaticVar>,
    static_vars_free: Vec<StaticVarRef>,
}

impl<'a> Program<'a> {
    pub fn new(interner: &'a mut Interner) -> Self {
        Self {
            interner,
            heaps: Default::default(),
            insts_free: Vec::new(),
            funcs_free: Vec::new(),
            blocks_free: Vec::new(),
            static_vars_free: Vec::new(),
            insts: vec![Default::default()],
            funcs: vec![Default::default()],
            blocks: vec![Default::default()],
            static_vars: vec![Default::default()],
        }
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

    pub fn static_var(&self, var: StaticVarRef) -> &StaticVar {
        &self.static_vars[var.0.get() as usize]
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

    pub fn static_var_mut(&mut self, var: StaticVarRef) -> &mut StaticVar {
        &mut self.static_vars[var.0.get() as usize]
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

    pub fn new_static_var(&mut self, var: StaticVar) -> StaticVarRef {
        // TODO: Reuse slots from `static_vars_free` for better memory efficiency
        let r = StaticVarRef(NonZeroU32::new(self.static_vars.len() as u32).unwrap());
        self.static_vars.push(var);
        r
    }

    // ---------------------------------------------------------------------------
    // Deletion
    // ---------------------------------------------------------------------------

    pub fn delete_inst(&mut self, inst_ref: InstRef) {
        self.insts_free.push(inst_ref);
    }

    pub fn delete_func(&mut self, func_ref: FuncRef) {
        self.funcs_free.push(func_ref);
    }

    pub fn delete_block(&mut self, block_ref: BlockRef) {
        self.blocks_free.push(block_ref);
    }

    pub fn delete_static_var(&mut self, var_ref: StaticVarRef) {
        self.static_vars_free.push(var_ref);
    }

    // ---------------------------------------------------------------------------
    // Iterators
    // ---------------------------------------------------------------------------

    // TODO: There's some issues with this iterators:
    // 1. Find a better a way to skip deleted entries (maybe a use an Option)
    // 2. The _with_refs version do not need to index into the vector, they can
    // just iterate through it with an enumerate()

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

    pub fn iter_static_vars(&self) -> impl Iterator<Item = StaticVarRef> + '_ {
        // Start from 1 to skip the default function at index 0
        (1..self.static_vars.len())
            .map(|i| unsafe { StaticVarRef(NonZeroU32::new_unchecked(i as u32)) })
            .filter(move |r| !self.static_vars_free.contains(r))
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

    pub fn iter_static_vars_with_refs(&self) -> impl Iterator<Item = (StaticVarRef, &StaticVar)> {
        self.iter_static_vars()
            .map(move |r| (r, self.static_var(r)))
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

impl fmt::Display for StaticVarRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0.get())
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "not"),
            UnaryOp::Neg => write!(f, "neg"),
            UnaryOp::Inc => write!(f, "inc"),
            UnaryOp::Dec => write!(f, "dec"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::Shl => write!(f, "shl"),
            BinaryOp::Shr => write!(f, "shr"),
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Mul => write!(f, "mul"),
            BinaryOp::Div => write!(f, "div"),
            BinaryOp::Rem => write!(f, "rem"),
            BinaryOp::Equal => write!(f, "eq"),
            BinaryOp::NotEqual => write!(f, "neq"),
            BinaryOp::LessThan => write!(f, "lt"),
            BinaryOp::LessEqual => write!(f, "leq"),
            BinaryOp::GreaterThan => write!(f, "gt"),
            BinaryOp::GreaterEqual => write!(f, "geq"),
        }
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
            InstKind::Static(v) => write!(f, "static {}", v),
            InstKind::Identity(i) => write!(f, "identity {}", i),
            InstKind::Unary { op, val } => write!(f, "{} {}", op, val),
            InstKind::Store { val, ptr } => write!(f, "store {}, {}", ptr, val),
            InstKind::Upsilon { phi, val } => write!(f, "upsilon {}, {}", phi, val),
            InstKind::Binary { op, lhs, rhs } => write!(f, "{} {}, {}", op, lhs, rhs),
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

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (var_ref, var) in self.iter_static_vars_with_refs() {
            let name_str = self.interner.lookup(var.name);
            let linkage = if var.is_global { "global" } else { "static" };
            let init = match var.init {
                None => "_".to_string(),
                Some(v) => v.to_string(),
            };
            writeln!(
                f,
                "@{}({}) = {} {:?} {}",
                name_str, var_ref, linkage, var.ty, init
            )?;
        }
        for (func_ref, func) in self.iter_funcs_with_refs() {
            let func_name = self.interner.lookup(func.name);
            write!(f, "define @{}({}) ", func_name, func_ref)?;
            if func.blocks.is_empty() {
                writeln!(f)?;
            } else {
                writeln!(f, "{{")?;
                for block_ref in &func.blocks {
                    if self.blocks_free.contains(block_ref) {
                        continue;
                    }
                    let block = self.block(*block_ref);
                    let block_name = self.interner.lookup(block.name);
                    writeln!(f, "{}({}): ; preds: []", block_name, block_ref)?;
                    for inst_ref in &block.insts {
                        let inst = self.inst(*inst_ref);
                        writeln!(f, "  {} = {:?} {}", inst_ref, inst.ty, inst.kind)?;
                    }
                }
                writeln!(f, "}}")?;
            }
        }
        Ok(())
    }
}
