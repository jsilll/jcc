pub mod analysis;
pub mod builder;
pub mod inst;
pub mod pretty;
pub mod term;
pub mod ty;

use jcc_codemap::span::Span;
use jcc_entity::{entity_impl, EntitySet, PrimaryMap};

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
}

impl BlockData {
    pub fn new(name: Ident, span: Span) -> Self {
        Self {
            name,
            span,
            insts: Vec::new(),
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

    /// Returns an iterator over reachable IR blocks in preorder.
    ///
    /// If the function has no entry block, the iterator is empty.
    pub fn blocks_pre<'a>(&'a self, prog: &'a Program) -> BlocksPreIter<'a> {
        let mut stack = Vec::new();
        if let Some(entry) = self.entry {
            stack.push(entry);
        }
        BlocksPreIter {
            prog,
            stack,
            seen: EntitySet::default(),
        }
    }

    /// Returns an iterator over reachable IR blocks in postorder.
    ///
    /// If the function has no entry block, the iterator is empty.
    pub fn blocks_post<'a>(&'a self, prog: &'a Program) -> BlocksPostIter<'a> {
        let mut stack = Vec::new();
        if let Some(entry) = self.entry {
            stack.push((entry, false));
        }
        BlocksPostIter {
            prog,
            stack,
            seen: EntitySet::default(),
        }
    }
}

pub struct BlocksPreIter<'a> {
    prog: &'a Program,
    stack: Vec<Block>,
    seen: EntitySet<Block>,
}

impl Iterator for BlocksPreIter<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(block) = self.stack.pop() {
            if !self.seen.insert(block) {
                continue;
            }
            self.stack
                .extend(self.prog.blocks[block].term.successors().rev());
            return Some(block);
        }
        None
    }
}

pub struct BlocksPostIter<'a> {
    prog: &'a Program,
    seen: EntitySet<Block>,
    stack: Vec<(Block, bool)>,
}

impl Iterator for BlocksPostIter<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((block, expanded)) = self.stack.pop() {
            if expanded {
                return Some(block);
            }
            if !self.seen.insert(block) {
                continue;
            }
            self.stack.push((block, true));
            for succ in self.prog.blocks[block].term.successors().rev() {
                self.stack.push((succ, false));
            }
        }
        None
    }
}
