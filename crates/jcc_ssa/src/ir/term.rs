use crate::ir::{Block, Value};

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
}

impl std::fmt::Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Unreachable => write!(f, "unreachable"),
            Terminator::Br(dest) => write!(f, "br {}", dest),
            Terminator::Ret(val) => match val {
                Some(v) => write!(f, "ret {}", v),
                None => write!(f, "ret void"),
            },
            Terminator::CondBr {
                cond,
                then_block,
                else_block,
            } => write!(f, "br i1 {}, {}, {}", cond, then_block, else_block),
            Terminator::Switch {
                value,
                default,
                cases,
            } => {
                write!(f, "switch {} [ default: {}", value, default)?;
                for (val, blk) in cases {
                    write!(f, ", {}: {}", val, blk)?;
                }
                write!(f, " ]")
            }
        }
    }
}
