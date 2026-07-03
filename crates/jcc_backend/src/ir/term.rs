use crate::ir::{Block, Value};

use std::iter::FusedIterator;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Terminator {
    /// Unconditional branch to a block.
    Br(Block),

    /// Return a value from the function.
    Ret(Option<Value>),

    /// Conditional branch based on a boolean.
    CondBr {
        cond: Value,
        then_block: Block,
        else_block: Block,
    },

    /// Switch statement for multi-way branching.
    Switch {
        value: Value,
        default: Block,
        cases: Vec<(u64, Block)>,
    },

    /// Hints that code execution cannot reach this point.
    Unreachable,
}

impl Terminator {
    /// Creates an unreachable terminator.
    pub fn unreachable() -> Self {
        Self::Unreachable
    }

    /// Creates an unconditional branch terminator.
    pub fn br(dest: Block) -> Self {
        Self::Br(dest)
    }

    /// Creates a return terminator.
    pub fn ret(val: Option<Value>) -> Self {
        Self::Ret(val)
    }

    /// Creates a conditional branch terminator.
    pub fn cond_br(cond: Value, then_block: Block, else_block: Block) -> Self {
        Self::CondBr {
            cond,
            then_block,
            else_block,
        }
    }

    /// Creates a switch terminator.
    pub fn switch(value: Value, cases: Vec<(u64, Block)>, default: Block) -> Self {
        Self::Switch {
            value,
            cases,
            default,
        }
    }

    /// Returns an iterator to the operands of this terminator.
    pub fn operands(&self) -> impl Iterator<Item = Value> {
        match self {
            Terminator::Ret(Some(v)) => Some(*v),
            Terminator::CondBr { cond, .. } => Some(*cond),
            Terminator::Switch { value, .. } => Some(*value),
            _ => None,
        }
        .into_iter()
    }

    /// Returns a mutable iterator to the operands of this terminator.
    pub fn operands_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        match self {
            Terminator::Ret(Some(v)) => Some(v),
            Terminator::CondBr { cond, .. } => Some(cond),
            Terminator::Switch { value, .. } => Some(value),
            _ => None,
        }
        .into_iter()
    }

    /// Returns the successor blocks of this terminator.
    pub fn successors(&self) -> Successors<'_> {
        match self {
            Terminator::Ret(_) | Terminator::Unreachable => Successors::Empty,
            Terminator::Br(dest) => Successors::One(Some(*dest)),
            Terminator::CondBr {
                then_block,
                else_block,
                ..
            } => Successors::Two {
                first: Some(*then_block),
                second: Some(*else_block),
            },
            Terminator::Switch { cases, default, .. } => Successors::Switch {
                cases: cases.iter(),
                default: Some(*default),
            },
        }
    }
}

// -----------------------------------------------------------------------
// Successors
// -----------------------------------------------------------------------

pub enum Successors<'a> {
    /// No successors.
    Empty,

    /// One successor block.
    One(Option<Block>),

    /// Two successor blocks.
    Two {
        first: Option<Block>,
        second: Option<Block>,
    },

    /// Multiple successor blocks from a switch.
    Switch {
        default: Option<Block>,
        cases: std::slice::Iter<'a, (u64, Block)>,
    },
}

impl<'a> FusedIterator for Successors<'a> {}

impl<'a> Iterator for Successors<'a> {
    type Item = Block;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::One(block) => block.take(),
            Self::Two { first, second } => first.take().or_else(|| second.take()),
            Self::Switch { cases, default } => {
                cases.next().map(|(_, b)| *b).or_else(|| default.take())
            }
        }
    }
}

impl<'a> DoubleEndedIterator for Successors<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::One(block) => block.take(),
            Self::Two { first, second } => second.take().or_else(|| first.take()),
            Self::Switch { cases, default } => default
                .take()
                .or_else(|| cases.next_back().map(|(_, b)| *b)),
        }
    }
}
