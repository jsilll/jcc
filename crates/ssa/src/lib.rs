pub mod effects;

pub mod insertion;

pub mod opt;

pub use source_file;

use effects::{AbstractHeap, FastEffects};

use source_file::SourceSpan;

use std::{
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU32,
};

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

impl<T> Hash for EntityRef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

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

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum InstKind {
    #[default]
    Nop,
    Phi,
    GetArg,
    Const(i64),
    Ret(InstRef),
    Jump(BlockRef),
    Identity(InstRef),
    Upsilon {
        val: InstRef,
        phi: InstRef,
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
    Branch {
        val: InstRef,
        then: BlockRef,
        other: BlockRef,
    },
}

pub type InstRef = EntityRef<Inst>;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Inst {
    // TODO: index in block
    // TODO: index of block
    // TODO: resulting variable name
    pub ty: Type,
    pub kind: InstKind,
}

impl Inst {
    pub fn phi(ty: Type) -> Self {
        Self {
            ty,
            kind: InstKind::Phi,
        }
    }

    pub fn ret(val: InstRef) -> Self {
        Self {
            ty: Type::Void,
            kind: InstKind::Ret(val),
        }
    }

    pub fn const_i32(val: i64) -> Self {
        Self {
            ty: Type::Int32,
            kind: InstKind::Const(val),
        }
    }

    pub fn const_i64(val: i64) -> Self {
        Self {
            ty: Type::Int64,
            kind: InstKind::Const(val),
        }
    }

    pub fn upsilon(phi: InstRef, val: InstRef) -> Self {
        Self {
            ty: Type::Void,
            kind: InstKind::Upsilon { phi, val },
        }
    }

    pub fn add_i32(lhs: InstRef, rhs: InstRef) -> Self {
        Self {
            ty: Type::Int32,
            kind: InstKind::Binary {
                lhs,
                rhs,
                op: BinaryOp::Add,
            },
        }
    }

    pub fn is_const(&self, val: i64) -> bool {
        match self.kind {
            InstKind::Const(v) => v == val,
            _ => false,
        }
    }

    pub fn get_args(&self) -> Option<Vec<InstRef>> {
        match &self.kind {
            InstKind::Upsilon { phi, val } => Some(vec![*phi, *val]),

            InstKind::Binary { lhs, rhs, .. } => Some(vec![*lhs, *rhs]),

            InstKind::Ret(val) | InstKind::Identity(val) | InstKind::Unary { val, .. } => {
                Some(vec![*val])
            }

            InstKind::Nop
            | InstKind::Phi
            | InstKind::GetArg
            | InstKind::Const(_)
            | InstKind::Jump(_)
            | InstKind::Branch { .. } => None,
        }
    }

    pub fn get_effects(&self, program: &Program) -> FastEffects {
        let mut effects = FastEffects::new();
        match self.kind {
            InstKind::Phi => effects.reads.add(program.heaps.ssa_state),

            InstKind::Upsilon { .. } => effects.writes.add(program.heaps.ssa_state),

            InstKind::Jump(_) | InstKind::Ret(_) | InstKind::Branch { .. } => {
                effects.writes.add(program.heaps.control)
            }

            InstKind::Nop
            | InstKind::GetArg
            | InstKind::Const(_)
            | InstKind::Identity(_)
            | InstKind::Unary { .. }
            | InstKind::Binary { .. } => {}
        }
        effects
    }

    pub fn into_identity(&mut self, val: InstRef) {
        self.kind = InstKind::Identity(val)
    }

    pub fn replace_args(&mut self, map: &HashMap<InstRef, InstRef>) {
        match &mut self.kind {
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

            InstKind::Ret(val)
            | InstKind::Identity(val)
            | InstKind::Unary { val, .. }
            | InstKind::Branch { val, .. } => {
                if let Some(v) = map.get(val) {
                    *val = *v;
                }
            }

            InstKind::Nop
            | InstKind::Phi
            | InstKind::GetArg
            | InstKind::Const(_)
            | InstKind::Jump(_) => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

pub type BlockRef = EntityRef<Block>;

#[derive(Debug, Default, Clone)]
pub struct Block {
    // TODO: further split this?
    // TODO: index in function
    pub insts: Vec<InstRef>,
    pub succ: Vec<BlockRef>,
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
// Program
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct BaseHeaps {
    pub world: AbstractHeap,
    pub memory: AbstractHeap,
    pub control: AbstractHeap,
    pub ssa_state: AbstractHeap,
}

impl BaseHeaps {
    pub fn new() -> Self {
        Self {
            world: AbstractHeap::new(1, 8),
            memory: AbstractHeap::new(2, 3),
            control: AbstractHeap::new(4, 5),
            ssa_state: AbstractHeap::new(6, 7),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    heaps: BaseHeaps,

    funcs: Vec<Func>,
    funcs_name: Vec<String>,
    // funcs_free: Vec<FuncRef>, TODO
    blocks: Vec<Block>,
    blocks_name: Vec<String>,
    // blocks_free: Vec<BlockRef>, TODO
    insts: Vec<Inst>,
    insts_span: Vec<SourceSpan>,
    // insts_free: Vec<InstRef>, TODO
}

impl Program {
    pub fn new() -> Self {
        Self {
            heaps: BaseHeaps::new(),

            funcs: vec![Default::default()],
            funcs_name: vec![Default::default()],
            // funcs_free: Vec::new(), TODO
            blocks: vec![Default::default()],
            blocks_name: vec![Default::default()],
            // blocks_free: Vec::new(), TODO
            insts: vec![Default::default()],
            insts_span: vec![Default::default()],
            // insts_free: Vec::new(), TODO
        }
    }

    pub fn get_func(&self, func: FuncRef) -> &Func {
        &self.funcs[func.0.get() as usize]
    }

    pub fn get_block(&self, block: BlockRef) -> &Block {
        &self.blocks[block.0.get() as usize]
    }

    pub fn get_inst(&self, inst: InstRef) -> &Inst {
        &self.insts[inst.0.get() as usize]
    }

    pub fn get_func_mut(&mut self, func: FuncRef) -> &mut Func {
        &mut self.funcs[func.0.get() as usize]
    }

    pub fn get_block_mut(&mut self, block: BlockRef) -> &mut Block {
        &mut self.blocks[block.0.get() as usize]
    }

    pub fn get_inst_mut(&mut self, inst: InstRef) -> &mut Inst {
        &mut self.insts[inst.0.get() as usize]
    }

    pub fn get_func_name(&self, func: FuncRef) -> &str {
        &self.funcs_name[func.0.get() as usize]
    }

    pub fn get_block_name(&self, block: BlockRef) -> &str {
        &self.blocks_name[block.0.get() as usize]
    }

    pub fn get_funcs_iter(&self) -> impl Iterator<Item = FuncRef> {
        (1..self.funcs.len()).map(|i| FuncRef::new(i))
    }

    pub fn get_blocks_iter(&self) -> impl Iterator<Item = BlockRef> {
        (1..self.blocks.len()).map(|i| BlockRef::new(i))
    }

    pub fn add_func(&mut self, name: &str) -> FuncRef {
        let idx = FuncRef::new(self.funcs.len());
        self.funcs.push(Default::default());
        self.funcs_name.push(name.to_owned());
        idx
    }

    pub fn add_block(&mut self, name: &str) -> BlockRef {
        let idx = BlockRef::new(self.blocks.len());
        self.blocks.push(Default::default());
        self.blocks_name.push(name.to_owned());
        idx
    }

    pub fn add_inst(&mut self, inst: Inst) -> InstRef {
        let idx = InstRef::new(self.insts.len());
        self.insts.push(inst);
        self.insts_span.push(Default::default());
        idx
    }

    pub fn add_inst_with_span(&mut self, inst: Inst, span: SourceSpan) -> InstRef {
        let idx = InstRef::new(self.insts.len());
        self.insts.push(inst);
        self.insts_span.push(span);
        idx
    }

    pub fn get_phi_args(&self, phi: InstRef) -> Vec<InstRef> {
        // TODO: build a function that builds this for all phis??
        let mut res = Vec::new();
        for (idx, inst) in self.insts.iter().enumerate().skip(1) {
            if let InstKind::Upsilon { phi: p, .. } = &inst.kind {
                if *p == phi {
                    res.push(InstRef::new(idx))
                }
            }
        }
        res
    }

    pub fn get_predecessors(&self) -> HashMap<BlockRef, Vec<BlockRef>> {
        // TODO: should this be here??
        let mut res = HashMap::new();

        for (idx, block) in self.blocks.iter().enumerate() {
            for succ in &block.succ {
                res.entry(*succ)
                    .or_insert(Vec::new())
                    .push(BlockRef::new(idx));
            }
        }

        res
    }

    pub fn validate(&self) -> bool {
        // TODO: Check SSA dominance rule: each use must be dominated by its definition
        // This would require implementing dominance calculation first
        true
    }
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl fmt::Display for FuncRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f{}", self.0.get())
    }
}

impl fmt::Display for BlockRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0.get())
    }
}

impl fmt::Display for InstRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0.get())
    }
}

impl fmt::Display for InstKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstKind::Ret(i) => write!(f, "Ret({})", i),

            InstKind::Jump(b) => write!(f, "Jump({})", b),

            InstKind::Identity(i) => write!(f, "Identity({})", i),

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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, func) in self.funcs_name.iter().zip(self.funcs.iter()).skip(1) {
            writeln!(f, "define @{}", name)?;
            for block in &func.blocks {
                writeln!(f, "{}:", self.get_block_name(*block))?;
                for i in self.get_block(*block).insts.iter() {
                    let inst = self.get_inst(*i);
                    writeln!(f, "  {:?} {} = {}", inst.ty, i, inst.kind)?;
                }
            }
        }
        Ok(())
    }
}
