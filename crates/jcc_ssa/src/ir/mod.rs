pub mod builder;
pub mod inst;
pub mod ty;

use jcc_codemap::span::Span;
use jcc_entity::{entity_impl, PrimaryMap};

use crate::{
    ir::{inst::Inst, ty::Ty},
    Ident,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(u32);
entity_impl!(Value, "%");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(u32);
entity_impl!(Block, "bb");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Global(u32);
entity_impl!(Global, "@global");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(u32);
entity_impl!(Function, "@function");

#[derive(Default)]
pub struct Program {
    pub values: PrimaryMap<Value, ValueData>,
    pub blocks: PrimaryMap<Block, BlockData>,
    pub globals: PrimaryMap<Global, GlobalData>,
    pub functions: PrimaryMap<Function, FunctionData>,
}

#[derive(Debug, Clone)]
pub struct ValueData {
    pub idx: u32,
    pub span: Span,
    pub inst: Inst,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct BlockData {
    pub span: Span,
    pub name: Ident,
    pub insts: Vec<Value>,
    pub preds: Vec<Block>,
    pub dom_children: Vec<Block>,
    pub dom_parent: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct GlobalData {
    pub ty: Ty,
    pub span: Span,
    pub name: Ident,
    pub is_global: bool,
    pub init: Option<u64>,
}

#[derive(Debug, Clone)]
pub struct FunctionData {
    pub span: Span,
    pub name: Ident,
    pub is_global: bool,
    pub blocks: Vec<Block>,
}

impl std::fmt::Display for Program {
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

        for (handle, data) in self.functions.iter() {
            writeln!(f, "define {} {{", handle)?;
            for (idx, block) in data.blocks.iter().enumerate() {
                if idx > 0 {
                    writeln!(f)?;
                }
                writeln!(f, "{}:", block)?;

                for value in &self.blocks[*block].insts {
                    let data = &self.values[*value];
                    write!(f, "  ")?;
                    if data.inst.ty() != Ty::Void {
                        write!(f, "{} = ", value)?;
                    }
                    writeln!(f, "{}", data.inst)?;
                }
            }
            writeln!(f, "}}")?;
        }

        Ok(())
    }
}
