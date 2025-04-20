pub mod build;

pub mod fix;

pub mod emit;

use source_file::SourceSpan;

use crate::Symbol;

// ---------------------------------------------------------------------------
// AMD64 IR
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Program(pub FnDef);

#[derive(Default, Clone, PartialEq, Eq)]
pub struct FnDef {
    blocks: Vec<Block>,
    pub span: SourceSpan,
}

impl FnDef {
    pub fn get_block(&self, block_ref: BlockRef) -> &Block {
        &self.blocks[block_ref.0 as usize]
    }

    pub fn get_block_mut(&mut self, block_ref: BlockRef) -> &mut Block {
        &mut self.blocks[block_ref.0 as usize]
    }

    pub fn push_block(&mut self, block: Block) -> BlockRef {
        self.blocks.push(block);
        BlockRef((self.blocks.len() - 1) as u32)
    }
}

impl std::fmt::Debug for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FnDef")
            .field("blocks", &self.blocks)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockRef(u32);

impl std::fmt::Display for BlockRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub instrs: Vec<Inst>,
    pub spans: Vec<SourceSpan>,
    pub label: Option<Symbol>,
}

impl Block {
    pub fn with_label(label: Symbol) -> Self {
        Block {
            label: Some(label),
            ..Default::default()
        }
    }
}

impl std::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("label", &self.label)
            .field("instrs", &self.instrs)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inst {
    /// A `ret` instruction.
    Ret,
    /// A `cdq` instruction.
    Cdq,
    /// Stack allocation instruction.
    Alloca(u32),
    /// An `idiv` instruction.
    Idiv(Operand),
    /// A `jmp` instruction.
    Jmp(BlockRef),
    /// A `mov` instruction.
    Mov { src: Operand, dst: Operand },
    /// A `cmp` instruction.
    Cmp { lhs: Operand, rhs: Operand },
    /// A `test` instruction.
    Test { lhs: Operand, rhs: Operand },
    /// A conditional set instruction.
    SetCC { cond_code: CondCode, dst: Operand },
    /// A conditional jump instruction.
    JmpCC {
        cond_code: CondCode,
        target: BlockRef,
    },
    /// A unary operation instruction.
    Unary { op: UnaryOp, dst: Operand },
    /// A binary operation instruction.
    Binary {
        op: BinaryOp,
        src: Operand,
        dst: Operand,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    /// An immediate value.
    Imm(i64),
    /// A register.
    Reg(Reg),
    /// A stack operand.
    Stack(u32),
    /// Pseudo register.
    Pseudo(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    /// The RAX register.
    Rax,
    /// The RCX register.
    Rcx,
    /// The RDX register.
    Rdx,
    /// The R10 register.
    Rg10,
    /// The R10 register.
    Rg11,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// The `not` operator.
    Not,
    /// The `neg` operator.
    Neg,
    /// The `inc` operator.
    Inc,
    /// The `dec` operator.
    Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// The `or` operator.
    Or,
    /// The `and` operator.
    And,
    /// The `xor` operator.
    Xor,
    /// The `sal` operator.
    Shl,
    /// The `sar` operator.
    Shr,
    /// The `add` operator.
    Add,
    /// The `sub` operator.
    Sub,
    /// The `mul` operator.
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondCode {
    /// The `sete` condition.
    Equal,
    /// The `setne` condition.
    NotEqual,
    /// The `setl` condition.
    LessThan,
    /// The `setle` condition.
    LessEqual,
    /// The `setg` condition.
    GreaterThan,
    /// The `setge` condition.
    GreaterEqual,
}

impl TryFrom<crate::BinaryOp> for CondCode {
    type Error = ();

    fn try_from(op: crate::BinaryOp) -> Result<Self, Self::Error> {
        match op {
            crate::BinaryOp::Equal => Ok(Self::Equal),
            crate::BinaryOp::NotEqual => Ok(Self::NotEqual),
            crate::BinaryOp::LessThan => Ok(Self::LessThan),
            crate::BinaryOp::LessEqual => Ok(Self::LessEqual),
            crate::BinaryOp::GreaterThan => Ok(Self::GreaterThan),
            crate::BinaryOp::GreaterEqual => Ok(Self::GreaterEqual),
            _ => Err(()),
        }
    }
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Reg::Rax => write!(f, "rax"),
            Reg::Rcx => write!(f, "rcx"),
            Reg::Rdx => write!(f, "rdx"),
            Reg::Rg10 => write!(f, "r10"),
            Reg::Rg11 => write!(f, "r11"),
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "not"),
            UnaryOp::Neg => write!(f, "neg"),
            UnaryOp::Inc => write!(f, "inc"),
            UnaryOp::Dec => write!(f, "dec"),
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::Shl => write!(f, "sal"),
            BinaryOp::Shr => write!(f, "sar"),
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Mul => write!(f, "mul"),
        }
    }
}

impl std::fmt::Display for CondCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CondCode::Equal => write!(f, "eq"),
            CondCode::NotEqual => write!(f, "ne"),
            CondCode::LessThan => write!(f, "lt"),
            CondCode::LessEqual => write!(f, "le"),
            CondCode::GreaterThan => write!(f, "gt"),
            CondCode::GreaterEqual => write!(f, "ge"),
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operand::Reg(reg) => write!(f, "{}", reg),
            Operand::Imm(value) => write!(f, "{}", value),
            Operand::Stack(offset) => write!(f, "[{}]", offset),
            Operand::Pseudo(pseudo) => write!(f, "%{}", pseudo),
        }
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Inst::Ret => write!(f, "ret"),
            Inst::Cdq => write!(f, "cdq"),
            Inst::Alloca(size) => write!(f, "alloca {}", size),
            Inst::Idiv(operand) => write!(f, "idiv {}", operand),
            Inst::Jmp(block_ref) => write!(f, "jmp {}", block_ref),
            Inst::Mov { src, dst } => write!(f, "mov {}, {}", src, dst),
            Inst::Cmp { lhs, rhs } => write!(f, "cmp {}, {}", lhs, rhs),
            Inst::Test { lhs, rhs } => write!(f, "test {}, {}", lhs, rhs),
            Inst::SetCC { cond_code, dst } => write!(f, "set{} {}", cond_code, dst),
            Inst::JmpCC { cond_code, target } => {
                write!(f, "jmpcc {}, {}", cond_code, target)
            }
            Inst::Unary { op, dst } => write!(f, "{} {}", op, dst),
            Inst::Binary { op, src, dst } => write!(f, "{} {}, {}", op, src, dst),
        }
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(_) = &self.label {
            write!(f, "<block label>:")?;
        }
        for instr in self.instrs.iter() {
            write!(f, "\n  {}", instr)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for block in &self.blocks {
            write!(f, "{}", block)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
