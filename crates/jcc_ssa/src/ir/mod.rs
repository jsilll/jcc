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
    pub span: Span,
    pub inst: Inst,
    pub index: u32,
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
        // Print Globals
        for (global_handle, global_data) in self.globals.iter() {
            write!(f, "{} = ", global_handle)?;
            if global_data.is_global {
                write!(f, "global ")?;
            } else {
                write!(f, "constant ")?;
            }
            write!(f, "{} ", global_data.ty)?;

            match global_data.init {
                None => writeln!(f, "zeroinitializer")?,
                Some(val) => match global_data.ty {
                    Ty::F64 => write!(f, "{}", f64::from_bits(val))?,
                    Ty::F32 => write!(f, "{}", f32::from_bits(val as u32))?,
                    _ => write!(f, "{}", val)?,
                },
            }
        }

        if !self.globals.is_empty() {
            writeln!(f)?;
        }

        // Print Functions
        for (func_handle, func_data) in self.functions.iter() {
            writeln!(f, "define {} {{", func_handle)?;

            // Iterate over blocks in the function
            for (i, block_handle) in func_data.blocks.iter().enumerate() {
                let block_data = &self.blocks[*block_handle];

                // Print Block Label
                // If it's the first block, LLVM usually omits the label,
                // but printing it explicitly is safer for debugging.
                if i > 0 {
                    writeln!(f)?;
                }
                writeln!(f, "{}:", block_handle)?;

                // Iterate over instructions (Values) in the block
                for value_handle in &block_data.insts {
                    let value_data = &self.values[*value_handle];

                    // If the instruction produces a value (not void), print "%n = "
                    write!(f, "  ")?;
                    if value_data.inst.ty() != Ty::Void {
                        write!(f, "{} = ", value_handle)?;
                    }

                    // Print the instruction itself
                    writeln!(f, "{}", value_data.inst)?;
                }
            }

            writeln!(f, "}}")?;
            writeln!(f)?;
        }

        Ok(())
    }
}
