pub mod builder;
pub mod inst;
pub mod pretty;
pub mod term;
pub mod ty;

use jcc_codemap::span::Span;
use jcc_entity::{entity_impl, PrimaryMap, SparseSet};

use crate::{
    ir::{
        inst::Inst,
        pretty::{ProgramPretty, ProgramPrettyEmitter},
        term::Terminator,
        ty::Ty,
    },
    Ident, IdentInterner,
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

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ProgramPrettyEmitter::with_handles(self, f).emit()
    }
}

impl Program {
    pub fn pretty<'a>(&'a self, interner: &'a IdentInterner) -> ProgramPretty<'a> {
        ProgramPretty::new(self, interner)
    }
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
    pub term: Terminator,
    pub insts: Vec<Value>,
    pub preds: Vec<Block>,
    pub dom_children: Vec<Block>,
    pub dom_parent: Option<Block>,
}

impl BlockData {
    pub fn new(name: Ident, span: Span) -> Self {
        Self {
            name,
            span,
            insts: Vec::new(),
            preds: Vec::new(),
            dom_parent: None,
            dom_children: Vec::new(),
            term: Terminator::Unreachable,
        }
    }
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
    pub entry: Option<Block>,
}

impl FunctionData {
    /// Create a new function data
    pub fn new(name: Ident, is_global: bool, span: Span) -> Self {
        Self {
            name,
            span,
            is_global,
            entry: None,
        }
    }

    /// Returns an iterator over reachable IR blocks starting at `entry`.
    ///
    /// Blocks are yielded once in depth-first discovery order. Branch targets
    /// from each block terminator are all considered. If the function has no
    /// entry block, the iterator is empty.
    pub fn blocks<'a>(&'a self, prog: &'a Program) -> BlockIter<'a> {
        let mut stack = Vec::new();
        if let Some(entry) = self.entry {
            stack.push(entry);
        }
        BlockIter {
            prog,
            stack,
            seen: SparseSet::new(),
        }
    }
}

pub struct BlockIter<'a> {
    prog: &'a Program,
    stack: Vec<Block>,
    seen: SparseSet<Block>,
}

impl Iterator for BlockIter<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(block) = self.stack.pop() {
            if !self.seen.insert(block) {
                continue;
            }
            match &self.prog.blocks[block].term {
                Terminator::Ret(_) | Terminator::Unreachable => {}
                Terminator::Br(target) => self.stack.push(*target),
                Terminator::CondBr {
                    then_block,
                    else_block,
                    ..
                } => {
                    self.stack.push(*else_block);
                    self.stack.push(*then_block);
                }
                Terminator::Switch { default, cases, .. } => {
                    self.stack.push(*default);
                    for (_, target) in cases.iter().rev() {
                        self.stack.push(*target);
                    }
                }
            }
            return Some(block);
        }
        None
    }
}
