pub mod build;
pub mod inst;
pub mod reg;

use crate::{
    codegen::amd64::{
        inst::MInst,
        reg::{Reg, RegClass},
    },
    ir::{self, ty::Ty},
    Ident,
};

use jcc_codemap::span::Span;
use jcc_entity::{entity_impl, PrimaryMap};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MBlock(u32);
entity_impl!(MBlock, "mbb");

#[derive(Default)]
pub struct MProgram {
    pub functions: Vec<MFunction>,
    pub globals: PrimaryMap<ir::Global, ir::GlobalData>,
}

#[derive(Default)]
pub struct MFunction {
    pub span: Span,
    pub name: Ident,
    pub is_global: bool,
    pub vreg_count: u32,
    pub blocks: PrimaryMap<MBlock, MBlockData>,
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

    /// Create a new virtual register
    pub fn new_vreg(&mut self, class: RegClass) -> Reg {
        let reg = Reg::Virt(class, self.vreg_count);
        self.vreg_count += 1;
        reg
    }
}

impl std::fmt::Display for MFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "define @{:?} {{", self.name)?;
        for (id, block) in self.blocks.iter() {
            if id.0 > 0 {
                writeln!(f)?;
            }
            writeln!(f, "{}:", id)?;
            for inst in &block.insts {
                writeln!(f, "  {}", inst.kind)?;
            }
        }
        writeln!(f, "}}")
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
            write!(f, "{function}")?;
        }

        Ok(())
    }
}
