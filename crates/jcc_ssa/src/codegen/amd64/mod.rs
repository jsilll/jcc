pub mod build;
pub mod inst;
pub mod reg;

use crate::{
    codegen::amd64::{
        inst::{MInst, MInstKind},
        reg::{GpReg, XmmReg},
    },
    ir::{self, ty::Ty},
    Ident,
};

use jcc_codemap::span::Span;
use jcc_entity::{entity_impl, PrimaryMap, SparseSet};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MBlock(u32);
entity_impl!(MBlock, "mbb");

#[derive(Default)]
pub struct MProgram {
    pub functions: Vec<MFunction>,
    pub blocks: PrimaryMap<MBlock, MBlockData>,
    pub globals: PrimaryMap<ir::Global, ir::GlobalData>,
}

#[derive(Default)]
pub struct MFunction {
    pub span: Span,
    pub name: Ident,
    pub is_global: bool,
    pub vreg_gp_count: u32,
    pub vreg_xmm_count: u32,
    pub entry: Option<MBlock>,
}

#[derive(Default)]
pub struct MBlockData {
    pub span: Span,
    pub name: Ident,
    pub insts: Vec<MInst>,
    pub succs: Vec<MBlock>,
    pub preds: Vec<MBlock>,
}

impl MBlockData {
    /// Create a new machine block
    pub fn new(name: Ident, span: Span) -> Self {
        Self {
            span,
            name,
            ..Default::default()
        }
    }
}

impl MFunction {
    /// Create a new machine function
    pub fn new(name: Ident, is_global: bool, span: Span) -> Self {
        Self {
            span,
            name,
            is_global,
            ..Default::default()
        }
    }

    /// Create a new virtual GP register
    pub fn new_gp_vreg(&mut self) -> GpReg {
        let reg = GpReg::Virt(self.vreg_gp_count);
        self.vreg_gp_count += 1;
        reg
    }

    /// Create a new virtual XMM register
    pub fn new_xmm_vreg(&mut self) -> XmmReg {
        let reg = XmmReg::Virt(self.vreg_xmm_count);
        self.vreg_xmm_count += 1;
        reg
    }

    /// Returns an iterator over reachable machine blocks starting at `entry`.
    ///
    /// Blocks are yielded once in depth-first discovery order. Successors from
    /// `MBlockData::succs` and jump targets from terminator-like instructions
    /// (`Jmp`, `Jcc`) are both considered. If the function has no entry block,
    /// the iterator is empty.
    fn blocks<'a>(&self, blocks: &'a PrimaryMap<MBlock, MBlockData>) -> MBlockIter<'a> {
        let mut stack = Vec::new();
        if let Some(entry) = self.entry {
            stack.push(entry);
        }
        MBlockIter {
            stack,
            blocks,
            seen: SparseSet::new(),
        }
    }
}

struct MBlockIter<'a> {
    stack: Vec<MBlock>,
    seen: SparseSet<MBlock>,
    blocks: &'a PrimaryMap<MBlock, MBlockData>,
}

impl Iterator for MBlockIter<'_> {
    type Item = MBlock;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(block) = self.stack.pop() {
            if !self.seen.insert(block) {
                continue;
            }
            for inst in self.blocks[block].insts.iter().rev() {
                match &inst.kind {
                    MInstKind::Jmp(target) | MInstKind::Jcc { target, .. } => {
                        self.stack.push(*target)
                    }
                    _ => {}
                }
            }
            return Some(block);
        }
        None
    }
}

impl std::fmt::Display for MProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (handle, data) in self.globals.iter() {
            write!(f, "{} = ", handle)?;
            if data.is_global {
                write!(f, "global ")?;
            } else {
                write!(f, "constant ")?;
            }
            write!(f, "{} ", data.ty)?;

            match data.init {
                None => writeln!(f, "zeroinitializer")?,
                Some(val) => match data.ty {
                    Ty::F64 => writeln!(f, "{}", f64::from_bits(val))?,
                    Ty::F32 => writeln!(f, "{}", f32::from_bits(val as u32))?,
                    _ => writeln!(f, "{}", val)?,
                },
            }
        }

        if !self.globals.is_empty() {
            writeln!(f)?;
        }

        for function in &self.functions {
            writeln!(f, "define @{:?} {{", function.name)?;
            for (idx, block) in function.blocks(&self.blocks).enumerate() {
                if idx > 0 {
                    writeln!(f)?;
                }
                let data = &self.blocks[block];
                writeln!(f, "{}:", block)?;
                for inst in &data.insts {
                    writeln!(f, "  {}", inst.kind)?;
                }
            }
            writeln!(f, "}}")?
        }

        Ok(())
    }
}
