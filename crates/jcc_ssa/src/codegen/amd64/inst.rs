use crate::{
    codegen::amd64::{reg::Reg, MBlock},
    Ident,
};

use jcc_codemap::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// Immediate value
    Imm(i64),
    /// Register operand
    Reg(Reg),
    /// Memory operand
    Mem(AMode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size {
    /// 1 Byte (8 bits)
    Byte = 1,
    /// 2 Bytes (16 bits)
    Word = 2,
    /// 4 Bytes (32 bits)
    DWord = 4,
    /// 8 Bytes (64 bits)
    QWord = 8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Increment
    Inc,
    /// Decrement
    Dec,
    /// Bitwise NOT
    Not,
    /// Two's complement negation
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    /// Bitwise OR
    Or,
    /// Bitwise AND
    And,
    /// Bitwise XOR
    Xor,
    /// Shift Left
    Shl,
    /// Logical Shift Right
    Shr,
    /// Arithmetic Shift Right
    Sar,
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondCode {
    /// The `sete` condition.
    E,
    /// The `seta` condition.
    A,
    /// The `setae` condition.
    Ae,
    /// The `setb` condition.
    B,
    /// The `setbe` condition.
    Be,
    /// The `setl` condition.
    Lt,
    /// The `setle` condition.
    Le,
    /// The `setg` condition.
    Gt,
    /// The `setge` condition.
    Ge,
    /// The `setne` condition.
    Ne,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AMode {
    /// RIP addressing
    Rip(Ident),
    /// Stack addressing
    Stack(u32),
    /// Standard memory operand
    Mem {
        scale: u8,
        disp: i32,
        idx: Option<Reg>,
        base: Option<Reg>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MInst {
    pub span: Span,
    pub kind: MInstKind,
}

impl MInst {
    /// Return from function
    pub fn ret(span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Ret,
        }
    }

    /// Sign-extend EAX into EDX:EAX
    pub fn cdq(span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Cdq,
        }
    }

    /// Function call
    pub fn call(name: Ident, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Call(name),
        }
    }

    /// Unconditional Jump
    pub fn jmp(block: MBlock, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Jmp(block),
        }
    }

    /// Pop from stack
    pub fn pop(size: Size, dst: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Pop { size, dst },
        }
    }

    /// Push to stack
    pub fn push(size: Size, src: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Push { size, src },
        }
    }

    /// Conditional Jump
    pub fn jcc(cond: CondCode, target: MBlock, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Jcc { cond, target },
        }
    }

    /// Set byte on condition
    pub fn setcc(cond: CondCode, dst: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Setcc { cond, dst },
        }
    }

    /// Load Effective Address
    pub fn lea(size: Size, addr: AMode, dst: Reg, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Lea { size, addr, dst },
        }
    }

    /// Unary operation
    pub fn unary(size: Size, op: UnaryOp, dst: Reg, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Unary { size, op, dst },
        }
    }

    /// Comparison operation
    pub fn cmp(size: Size, lhs: Operand, rhs: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Cmp { size, lhs, rhs },
        }
    }

    /// Movement of data
    pub fn mov(size: Size, src: Operand, dst: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Mov { size, src, dst },
        }
    }

    /// Integer division
    pub fn div(size: Size, signed: bool, divisor: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Div {
                size,
                signed,
                divisor,
            },
        }
    }

    /// Binary operation
    pub fn binary(size: Size, op: BinOp, src: Operand, dst: Reg, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Binary { op, src, dst, size },
        }
    }

    /// Conditional Movement of data
    pub fn cmov(size: Size, cond: CondCode, src: Operand, dst: Reg, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Cmov {
                src,
                dst,
                cond,
                size,
            },
        }
    }

    /// Movement of data with sign-extension
    pub fn movsx(src_size: Size, dst_size: Size, src: Operand, dst: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Movsx {
                src,
                dst,
                src_size,
                dst_size,
            },
        }
    }

    /// Movement of data with zero-extension
    pub fn movzx(src_size: Size, dst_size: Size, src: Operand, dst: Operand, span: Span) -> Self {
        MInst {
            span,
            kind: MInstKind::Movzx {
                src,
                dst,
                src_size,
                dst_size,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MInstKind {
    /// Return from function
    Ret,

    /// Sign-extend EAX into EDX:EAX
    Cdq,

    /// Function call
    Call(Ident),

    /// Unconditional Jump
    Jmp(MBlock),

    /// Pop from stack
    Pop { size: Size, dst: Operand },

    /// Push to stack
    Push { size: Size, src: Operand },

    /// Conditional Jump
    Jcc { cond: CondCode, target: MBlock },

    /// Set byte on condition
    Setcc { cond: CondCode, dst: Operand },

    /// Load Effective Address
    Lea { dst: Reg, size: Size, addr: AMode },

    /// Unary operation
    Unary { dst: Reg, size: Size, op: UnaryOp },

    /// Comparison operation
    Cmp {
        size: Size,
        lhs: Operand,
        rhs: Operand,
    },

    /// Movement of data
    Mov {
        size: Size,
        dst: Operand,
        src: Operand,
    },

    /// Integer division
    Div {
        size: Size,
        signed: bool,
        divisor: Operand,
    },

    /// Binary operation
    Binary {
        dst: Reg,
        op: BinOp,
        size: Size,
        src: Operand,
    },

    /// Conditional Move
    Cmov {
        dst: Reg,
        size: Size,
        src: Operand,
        cond: CondCode,
    },

    /// Movement of data with sign-extension
    Movsx {
        dst: Operand,
        src: Operand,
        src_size: Size,
        dst_size: Size,
    },

    /// Movement of data with zero-extension
    Movzx {
        dst: Operand,
        src: Operand,
        src_size: Size,
        dst_size: Size,
    },
}

impl std::fmt::Display for Size {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Size::Byte => write!(f, "b"),
            Size::Word => write!(f, "w"),
            Size::DWord => write!(f, "l"),
            Size::QWord => write!(f, "q"),
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Reg(r) => write!(f, "{}", r),
            Operand::Mem(m) => write!(f, "{}", m),
            Operand::Imm(i) => write!(f, "${}", i),
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Inc => write!(f, "inc"),
            UnaryOp::Dec => write!(f, "dec"),
            UnaryOp::Not => write!(f, "not"),
            UnaryOp::Neg => write!(f, "neg"),
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Or => write!(f, "or"),
            BinOp::And => write!(f, "and"),
            BinOp::Xor => write!(f, "xor"),
            BinOp::Shl => write!(f, "shl"),
            BinOp::Shr => write!(f, "shr"),
            BinOp::Sar => write!(f, "sar"),
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "imul"),
        }
    }
}

impl std::fmt::Display for CondCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CondCode::E => write!(f, "e"),
            CondCode::A => write!(f, "a"),
            CondCode::Ae => write!(f, "ae"),
            CondCode::B => write!(f, "b"),
            CondCode::Be => write!(f, "be"),
            CondCode::Lt => write!(f, "l"),
            CondCode::Le => write!(f, "le"),
            CondCode::Gt => write!(f, "g"),
            CondCode::Ge => write!(f, "ge"),
            CondCode::Ne => write!(f, "ne"),
        }
    }
}

impl std::fmt::Display for AMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AMode::Stack(i) => write!(f, "stack[{i}]"),
            AMode::Rip(label) => write!(f, "[rip + {:?}]", label),
            AMode::Mem {
                idx,
                base,
                disp,
                scale,
            } => {
                write!(f, "[")?;
                if let Some(b) = base {
                    write!(f, "{}", b)?;
                }
                if let Some(i) = idx {
                    if base.is_some() {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}*{}", i, scale)?;
                }
                if *disp != 0 {
                    if base.is_some() || idx.is_some() {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}", disp)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl std::fmt::Display for MInstKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MInstKind::Ret => write!(f, "ret"),
            MInstKind::Cdq => write!(f, "cdq"),
            MInstKind::Jmp(b) => write!(f, "jmp {}", b),
            MInstKind::Call(l) => write!(f, "call {:?}", l),
            MInstKind::Pop { size, dst } => write!(f, "pop{} {}", size, dst),
            MInstKind::Push { size, src } => write!(f, "push{} {}", size, src),
            MInstKind::Setcc { cond, dst } => write!(f, "set{} {}", cond, dst),
            MInstKind::Jcc { cond, target } => write!(f, "j{} {}", cond, target),
            MInstKind::Unary { dst, size, op } => write!(f, "{}{} {}", op, size, dst),
            MInstKind::Cmp { size, lhs, rhs } => write!(f, "cmp{} {}, {}", size, rhs, lhs),
            MInstKind::Mov { size, dst, src } => write!(f, "mov{} {}, {}", size, src, dst),
            MInstKind::Lea { size, dst, addr } => write!(f, "lea{} {}, {}", size, addr, dst),
            MInstKind::Binary { op, size, dst, src } => {
                write!(f, "{}{} {}, {}", op, size, src, dst)
            }
            MInstKind::Div {
                size,
                signed,
                divisor,
            } => {
                let op = if *signed { "idiv" } else { "div" };
                write!(f, "{}{} {}", op, size, divisor)
            }
            MInstKind::Cmov {
                src,
                dst,
                size,
                cond,
            } => write!(f, "cmov{}{} {}, {}", cond, size, src, dst),
            MInstKind::Movsx {
                dst,
                src,
                src_size,
                dst_size,
            } => write!(f, "movs{}{} {}, {}", src_size, dst_size, src, dst),
            MInstKind::Movzx {
                dst,
                src,
                src_size,
                dst_size,
            } => write!(f, "movz{}{} {}, {}", src_size, dst_size, src, dst),
        }
    }
}
