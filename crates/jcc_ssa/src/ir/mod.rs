pub mod builder;

use jcc_codemap::span::Span;
use jcc_interner::{Interner, Symbol};

use std::{collections::HashMap, fmt, num::NonZeroU32};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TargetOs {
    #[default]
    Linux,
    Macos,
}

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

pub struct Program<'a> {
    pub interner: &'a mut Interner,
    insts: Vec<Inst>,
    blocks: Vec<Block>,
    funcs: Vec<Option<Func>>,
    static_vars: Vec<Option<StaticVar>>,
}

impl<'a> Program<'a> {
    pub fn new(interner: &'a mut Interner) -> Self {
        Self {
            interner,
            insts: vec![Default::default()],
            funcs: vec![Default::default()],
            blocks: vec![Default::default()],
            static_vars: vec![Default::default()],
        }
    }

    // ---------------------------------------------------------------------------
    // Accessors
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn inst(&self, inst: InstRef) -> &Inst {
        &self.insts[inst.0.get() as usize]
    }

    #[inline]
    pub fn inst_mut(&mut self, inst: InstRef) -> &mut Inst {
        &mut self.insts[inst.0.get() as usize]
    }

    #[inline]
    pub fn block(&self, block: BlockRef) -> &Block {
        &self.blocks[block.0.get() as usize]
    }

    #[inline]
    pub fn block_mut(&mut self, block: BlockRef) -> &mut Block {
        &mut self.blocks[block.0.get() as usize]
    }

    #[inline]
    pub fn func(&self, func: FuncRef) -> &Func {
        self.funcs
            .get(func.0.get() as usize)
            .and_then(|f| f.as_ref())
            .expect("invalid function reference")
    }

    #[inline]
    pub fn func_mut(&mut self, func: FuncRef) -> &mut Func {
        self.funcs
            .get_mut(func.0.get() as usize)
            .and_then(|f| f.as_mut())
            .expect("invalid function reference")
    }

    #[inline]
    pub fn static_var(&self, var: StaticVarRef) -> &StaticVar {
        self.static_vars
            .get(var.0.get() as usize)
            .and_then(|v| v.as_ref())
            .expect("invalid static var reference")
    }

    #[inline]
    pub fn static_var_mut(&mut self, var: StaticVarRef) -> &mut StaticVar {
        self.static_vars
            .get_mut(var.0.get() as usize)
            .and_then(|v| v.as_mut())
            .expect("invalid static var reference")
    }

    // ---------------------------------------------------------------------------
    // Creation
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn new_inst(&mut self, inst: Inst) -> InstRef {
        let r = InstRef(NonZeroU32::new(self.insts.len() as u32).unwrap());
        self.insts.push(inst);
        r
    }

    #[inline]
    pub fn new_block(&mut self, block: Block) -> BlockRef {
        let r = BlockRef(NonZeroU32::new(self.blocks.len() as u32).unwrap());
        self.blocks.push(block);
        r
    }

    #[inline]
    pub fn new_func(&mut self, func: Func) -> FuncRef {
        let r = FuncRef(NonZeroU32::new(self.funcs.len() as u32).unwrap());
        self.funcs.push(Some(func));
        r
    }

    #[inline]
    pub fn new_static_var(&mut self, var: StaticVar) -> StaticVarRef {
        let r = StaticVarRef(NonZeroU32::new(self.static_vars.len() as u32).unwrap());
        self.static_vars.push(Some(var));
        r
    }

    // ---------------------------------------------------------------------------
    // Iterators
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn iter_funcs(&self) -> impl Iterator<Item = &Func> {
        self.funcs.iter().skip(1).filter_map(move |f| match f {
            None => None,
            Some(func) => Some(func),
        })
    }

    #[inline]
    pub fn iter_funcs_with_ref(&self) -> impl Iterator<Item = (FuncRef, &Func)> {
        self.funcs.iter().skip(1).enumerate().filter_map(|(i, f)| {
            f.as_ref().map(|func| {
                (
                    FuncRef(unsafe { NonZeroU32::new_unchecked(i as u32 + 1) }),
                    func,
                )
            })
        })
    }

    #[inline]
    pub fn iter_static_vars(&self) -> impl Iterator<Item = &StaticVar> {
        self.static_vars
            .iter()
            .skip(1)
            .filter_map(move |v| match v {
                None => None,
                Some(var) => Some(var),
            })
    }

    #[inline]
    pub fn iter_static_vars_with_ref(&self) -> impl Iterator<Item = (StaticVarRef, &StaticVar)> {
        self.static_vars
            .iter()
            .skip(1)
            .enumerate()
            .filter_map(|(i, v)| {
                v.as_ref().map(|var| {
                    (
                        StaticVarRef(unsafe { NonZeroU32::new_unchecked(i as u32 + 1) }),
                        var,
                    )
                })
            })
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
    pub ty: Ty,
    pub span: Span,
    pub kind: InstKind,
    pub block: BlockRef,
    pub(crate) idx: InstIdx,
}

impl Inst {
    #[inline]
    fn new(ty: Ty, kind: InstKind, span: Span) -> Self {
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
    pub fn nop(span: Span) -> Self {
        Self::new(Ty::Void, InstKind::Nop, span)
    }

    #[inline]
    pub fn alloca(ty: Ty, span: Span) -> Self {
        Self::new(Ty::IntPtr, InstKind::Alloca(ty), span)
    }

    #[inline]
    pub fn phi(ty: Ty, span: Span) -> Self {
        Self::new(ty, InstKind::Phi, span)
    }

    #[inline]
    pub fn arg(ty: Ty, span: Span) -> Self {
        Self::new(ty, InstKind::Arg, span)
    }

    #[inline]
    pub fn ret(val: InstRef, span: Span) -> Self {
        Self::new(Ty::Void, InstKind::Ret(val), span)
    }

    #[inline]
    pub fn identity(ty: Ty, val: InstRef, span: Span) -> Self {
        Self::new(ty, InstKind::Identity(val), span)
    }

    #[inline]
    pub fn truncate(val: InstRef, span: Span) -> Self {
        Self::new(Ty::Int32, InstKind::Truncate(val), span)
    }

    #[inline]
    pub fn sign_extend(val: InstRef, span: Span) -> Self {
        Self::new(Ty::Int64, InstKind::SignExtend(val), span)
    }

    #[inline]
    pub fn const_i32(val: i32, span: Span) -> Self {
        Self::new(Ty::Int32, InstKind::Const(ConstValue::Int32(val)), span)
    }

    #[inline]
    pub fn const_i64(val: i64, span: Span) -> Self {
        Self::new(Ty::Int64, InstKind::Const(ConstValue::Int64(val)), span)
    }

    #[inline]
    pub fn jump(block: BlockRef, span: Span) -> Self {
        Self::new(Ty::Void, InstKind::Jump(block), span)
    }

    #[inline]
    pub fn load(ty: Ty, ptr: InstRef, span: Span) -> Self {
        Self::new(ty, InstKind::Load(ptr), span)
    }

    #[inline]
    pub fn static_addr(var: StaticVarRef, span: Span) -> Self {
        Self::new(Ty::IntPtr, InstKind::Static(var), span)
    }

    #[inline]
    pub fn store(ptr: InstRef, val: InstRef, span: Span) -> Self {
        Self::new(Ty::Void, InstKind::Store { ptr, val }, span)
    }

    #[inline]
    pub fn call(ty: Ty, func: FuncRef, args: Vec<InstRef>, span: Span) -> Self {
        Self::new(ty, InstKind::Call { func, args }, span)
    }

    #[inline]
    pub fn unary(ty: Ty, op: UnaryOp, val: InstRef, span: Span) -> Self {
        Self::new(ty, InstKind::Unary { op, val }, span)
    }

    #[inline]
    pub fn upsilon(ty: Ty, phi: InstRef, val: InstRef, span: Span) -> Self {
        Self::new(ty, InstKind::Upsilon { phi, val }, span)
    }

    #[inline]
    pub fn branch(cond: InstRef, then: BlockRef, other: BlockRef, span: Span) -> Self {
        Self::new(Ty::Void, InstKind::Branch { cond, then, other }, span)
    }

    #[inline]
    pub fn binary(ty: Ty, op: BinaryOp, lhs: InstRef, rhs: InstRef, span: Span) -> Self {
        Self::new(ty, InstKind::Binary { op, lhs, rhs }, span)
    }

    #[inline]
    pub fn select(ty: Ty, cond: InstRef, then: InstRef, other: InstRef, span: Span) -> Self {
        Self::new(ty, InstKind::Select { cond, then, other }, span)
    }

    #[inline]
    pub fn switch(
        cond: InstRef,
        default: BlockRef,
        cases: Vec<(ConstValue, BlockRef)>,
        span: Span,
    ) -> Self {
        Self::new(
            Ty::Void,
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
    pub fn is_const_i32(&self, val: i32) -> bool {
        matches!(self.kind, InstKind::Const(ConstValue::Int32(v)) if v == val)
    }

    #[inline]
    pub fn is_const_i64(&self, val: i64) -> bool {
        matches!(self.kind, InstKind::Const(ConstValue::Int64(v)) if v == val)
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

    // pub fn get_effects(&self, prog: &Program, effects: &mut FastEffects) {
    //     match self.kind {
    //         InstKind::Phi => effects.reads.push(prog.heaps.ssa_state),
    //         InstKind::Upsilon { .. } => effects.writes.push(prog.heaps.ssa_state),
    //         InstKind::Jump(_) | InstKind::Ret(_) | InstKind::Branch { .. } => {
    //             effects.writes.push(prog.heaps.control)
    //         }
    //         _ => {}
    //     }
    // }

    pub fn get_args(&self, args: &mut Vec<InstRef>) {
        match &self.kind {
            InstKind::Ret(val)
            | InstKind::Load(val)
            | InstKind::Identity(val)
            | InstKind::Truncate(val)
            | InstKind::SignExtend(val)
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
            | InstKind::Alloca(_)
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
            | InstKind::Truncate(val)
            | InstKind::SignExtend(val)
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
            | InstKind::Alloca(_)
            | InstKind::Jump(_)
            | InstKind::Const(_)
            | InstKind::Static(_) => {}
        }
    }
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
    pub span: Span,
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
    pub span: Span,
    pub blocks: Vec<BlockRef>,
}

// ---------------------------------------------------------------------------
// StaticVar
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StaticVarRef(pub(crate) NonZeroU32);

#[derive(Debug, Default, Clone)]
pub struct StaticVar {
    pub ty: Ty,
    pub name: Symbol,
    pub is_global: bool,
    pub span: Span,
    pub init: Option<ConstValue>,
}

impl StaticVar {
    #[inline]
    pub fn int32(name: Symbol, is_global: bool, init: Option<i32>, span: Span) -> Self {
        Self {
            span,
            name,
            is_global,
            ty: Ty::Int32,
            init: init.map(ConstValue::Int32),
        }
    }

    #[inline]
    pub fn int64(name: Symbol, is_global: bool, init: Option<i64>, span: Span) -> Self {
        Self {
            span,
            name,
            is_global,
            ty: Ty::Int64,
            init: init.map(ConstValue::Int64),
        }
    }
}

// ---------------------------------------------------------------------------
// Ty
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty {
    #[default]
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
}

impl Ty {
    pub fn align(&self) -> u32 {
        match self {
            Ty::Int8 => 1,
            Ty::Int16 => 2,
            Ty::Int32 => 4,
            Ty::Int64 => 8,
            Ty::IntPtr => 8,
            Ty::Void => panic!("void has no alignment"),
        }
    }
}

// ---------------------------------------------------------------------------
// ConstValue
// --------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum ConstValue {
    /// A 32-bit const value.
    Int32(i32),
    /// A 64-bit const value.
    Int64(i64),
}

impl ConstValue {
    #[inline]
    pub fn is_zero(&self) -> bool {
        matches!(ConstValue::Int32(0), ConstValue::Int64(0))
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
    Alloca(Ty),
    Ret(InstRef),
    Load(InstRef),
    Jump(BlockRef),
    Const(ConstValue),
    Identity(InstRef),
    Truncate(InstRef),
    SignExtend(InstRef),
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
        cases: Vec<(ConstValue, BlockRef)>,
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
            InstKind::Const(ConstValue::Int32(i)) => write!(f, "{}", i),
            InstKind::Const(ConstValue::Int64(i)) => write!(f, "{}", i),
            InstKind::Nop => write!(f, "nop"),
            InstKind::Phi => write!(f, "phi"),
            InstKind::Arg => write!(f, "arg"),
            InstKind::Alloca(ty) => write!(f, "alloca {:?}", ty),
            InstKind::Ret(i) => write!(f, "ret {}", i),
            InstKind::Jump(b) => write!(f, "jump {}", b),
            InstKind::Load(ptr) => write!(f, "load {}", ptr),
            InstKind::Static(v) => write!(f, "static {}", v),
            InstKind::Identity(i) => write!(f, "identity {}", i),
            InstKind::Truncate(i) => write!(f, "truncate {}", i),
            InstKind::SignExtend(i) => write!(f, "sext {}", i),
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
        for (var_ref, var) in self.iter_static_vars_with_ref() {
            let name_str = self.interner.lookup(var.name);
            let linkage = if var.is_global { "global" } else { "static" };
            let init = match var.init {
                None => "_".to_string(),
                Some(ConstValue::Int32(v)) => v.to_string(),
                Some(ConstValue::Int64(v)) => v.to_string(),
            };
            writeln!(
                f,
                "@{}({}) = {} {:?} {}",
                name_str, var_ref, linkage, var.ty, init
            )?;
        }
        for (func_ref, func) in self.iter_funcs_with_ref() {
            let func_name = self.interner.lookup(func.name);
            write!(f, "define @{}({}) ", func_name, func_ref)?;
            if func.blocks.is_empty() {
                writeln!(f)?;
            } else {
                writeln!(f, "{{")?;
                for block_ref in &func.blocks {
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
