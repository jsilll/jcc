use crate::ir::{ty::Ty, Block, Function, Global, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// Bitwise NOT
    Not,
    /// Arithmetic negation
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ICmpOp {
    // Equal
    Eq,
    // Not equal
    Ne,
    // Signed less than
    Lt,
    // Signed less than or equal
    Le,
    // Signed greater than
    Gt,
    // Signed greater than or equal
    Ge,
    // Unsigned less than
    Ult,
    // Unsigned less than or equal
    Ule,
    // Unsigned greater than
    Ugt,
    // Unsigned greater than or equal
    Uge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Addition
    Add,
    // Subtraction
    Sub,
    // Multiplication
    Mul,
    // Signed division
    Div,
    // Signed remainder
    Rem,
    // Unsigned division
    UDiv,
    // Unsigned remainder
    URem,
    // Bitwise AND
    And,
    // Bitwise OR
    Or,
    // Bitwise XOR
    Xor,
    // Shift left
    Shl,
    // Logical shift right (zero fill)
    Shr,
    // Arithmetic shift right (sign extension)
    AShr,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum Inst {
    #[default]
    Noop,

    /// A Phi node represents a merge point for data.
    /// In this variation, the Phi node itself does not know its inputs.
    /// It simply defines a type and waits for `Upsilon` instructions to feed it.
    Phi(Ty),

    /// Upsilon acts as a "setter" for a Phi node.
    /// It must be placed in a predecessor block and "pushes" `value`
    /// into the `phi` node located in the successor block.
    Upsilon { phi: Value, value: Value },

    /// Null constant pointer.
    ConstNull(Ty),

    /// Address of a global variable.
    GlobalAddr(Global),

    /// Function parameter definition.
    Param { ty: Ty, index: u32 },

    /// Constant integer value.
    ConstInt { ty: Ty, value: i64 },

    /// Stack allocation. Returns a pointer to the allocated type.
    Alloca { ty: Ty, align: u32 },

    /// Load a value from memory.
    Load { ty: Ty, ptr: Value, align: u32 },

    /// Store a value to memory.
    Store {
        ptr: Value,
        align: u32,
        value: Value,
    },

    /// Calculate the address of a sub-element of an aggregate data structure.
    GetElementPtr {
        ty: Ty,
        ptr: Value,
        indices: Vec<Value>,
    },

    /// Unary operations.
    Unary { ty: Ty, op: UnaryOp, operand: Value },

    /// Binary operations.
    Binary {
        ty: Ty,
        lhs: Value,
        rhs: Value,
        op: BinaryOp,
    },

    /// Integer comparison.
    Icmp {
        lhs: Value,
        rhs: Value,
        pred: ICmpOp,
    },

    /// Select between two values based on a boolean condition (like a ternary operator).
    Select {
        ty: Ty,
        cond: Value,
        then_val: Value,
        else_val: Value,
    },

    /// Conversion: truncate integer to smaller type.
    Trunc { to: Ty, value: Value },

    /// Conversion: zero-extend integer to larger type.
    Zext { to: Ty, value: Value },

    /// Conversion: sign-extend integer to larger type.
    Sext { to: Ty, value: Value },

    /// Conversion: bitcast between types of the same size.
    Bitcast { to: Ty, value: Value },

    /// Conversion: Casts an integer to a pointer.
    IntToPtr { to: Ty, value: Value },

    /// Conversion: Casts a pointer to an integer.
    PtrToInt { to: Ty, value: Value },

    // Function Call: Direct function call.
    Call {
        ty: Ty, // Return type
        func: Function,
        args: Vec<Value>,
    },

    /// Function Call: Indirect function call.
    IndirectCall {
        ty: Ty,
        ptr: Value,
        args: Vec<Value>,
    },

    /// Terminator: Hints to the optimizer that code execution cannot reach this point.
    Unreachable,

    /// Terminator: Unconditional branch to a block.
    Br(Block),

    /// Terminator: Return a value from the function (or void).
    Ret(Option<Value>),

    /// Terminator: Conditional branch based on a boolean value.
    CondBr {
        cond: Value,
        then_block: Block,
        else_block: Block,
    },

    /// Terminator: Switch statement for multi-way branching.
    Switch {
        value: Value,
        default: Block,
        cases: Vec<(i64, Block)>,
    },
}

impl Inst {
    /// Creates a no-operation instruction.
    pub fn noop() -> Self {
        Self::Noop
    }

    /// Creates a Phi node waiting for incoming values.
    pub fn phi(ty: Ty) -> Self {
        Self::Phi(ty)
    }

    /// Creates an assignment to a Phi node in a successor block.
    pub fn upsilon(phi: Value, value: Value) -> Self {
        Self::Upsilon { phi, value }
    }

    /// Creates a null constant pointer of the specified type.
    pub fn const_null(ty: Ty) -> Self {
        Self::ConstNull(ty)
    }

    /// Creates an instruction to get the address of a global variable.
    pub fn global_addr(global: Global) -> Self {
        Self::GlobalAddr(global)
    }

    /// Creates a function parameter definition.
    pub fn param(ty: Ty, index: u32) -> Self {
        Self::Param { ty, index }
    }

    /// Creates a constant integer instruction.
    pub fn const_int(ty: Ty, value: i64) -> Self {
        Self::ConstInt { ty, value }
    }

    /// Creates a stack allocation instruction.
    pub fn alloca(ty: Ty, align: u32) -> Self {
        Self::Alloca { ty, align }
    }

    /// Creates a load instruction to read a value from memory.
    pub fn load(ty: Ty, ptr: Value, align: u32) -> Self {
        Self::Load { ty, ptr, align }
    }

    /// Creates a store instruction to write a value to memory.
    pub fn store(ptr: Value, value: Value, align: u32) -> Self {
        Self::Store { ptr, value, align }
    }

    /// Creates a GetElementPtr instruction to compute the address of a sub-element.
    ///
    /// # Note
    ///
    /// `indices` is passed as a generic iterable to make usage cleaner (e.g., passing a slice or a Vec).
    pub fn gep<I>(ty: Ty, ptr: Value, indices: impl IntoIterator<Item = Value>) -> Self {
        Self::GetElementPtr {
            ty,
            ptr,
            indices: indices.into_iter().collect(),
        }
    }

    /// Creates a unary operation instruction.
    pub fn unary(op: UnaryOp, ty: Ty, operand: Value) -> Self {
        Self::Unary { ty, op, operand }
    }

    /// Creates an integer comparison instruction.
    pub fn icmp(pred: ICmpOp, lhs: Value, rhs: Value) -> Self {
        Self::Icmp { lhs, rhs, pred }
    }

    /// Creates a binary operation instruction.
    pub fn binary(op: BinaryOp, ty: Ty, lhs: Value, rhs: Value) -> Self {
        Self::Binary { ty, lhs, rhs, op }
    }

    /// Creates a select instruction to choose between two values based on a condition.
    pub fn select(ty: Ty, cond: Value, then_val: Value, else_val: Value) -> Self {
        Self::Select {
            ty,
            cond,
            then_val,
            else_val,
        }
    }

    /// Creates a truncate instruction to reduce an integer's size.
    pub fn trunc(to: Ty, value: Value) -> Self {
        Self::Trunc { to, value }
    }

    /// Creates a zero-extend instruction to increase an integer's size.
    pub fn zext(to: Ty, value: Value) -> Self {
        Self::Zext { to, value }
    }

    /// Creates a sign-extend instruction to increase an integer's size.
    pub fn sext(to: Ty, value: Value) -> Self {
        Self::Sext { to, value }
    }

    /// Creates a bitcast instruction to convert between types of the same size.
    pub fn bitcast(to: Ty, value: Value) -> Self {
        Self::Bitcast { to, value }
    }

    /// Creates an instruction to cast an integer to a pointer.
    pub fn int_to_ptr(to: Ty, value: Value) -> Self {
        Self::IntToPtr { to, value }
    }

    /// Creates an instruction to cast a pointer to an integer.
    pub fn ptr_to_int(to: Ty, value: Value) -> Self {
        Self::PtrToInt { to, value }
    }

    /// Creates a direct function call instruction.
    pub fn call(ty: Ty, func: Function, args: Vec<Value>) -> Self {
        Self::Call { ty, func, args }
    }

    /// Creates an indirect function call instruction.
    pub fn indirect_call(ty: Ty, ptr: Value, args: Vec<Value>) -> Self {
        Self::IndirectCall { ty, ptr, args }
    }

    /// Creates an unreachable instruction to indicate code cannot be reached.
    pub fn unreachable() -> Self {
        Self::Unreachable
    }

    /// Creates an unconditional branch instruction to a target block.
    pub fn br(dest: Block) -> Self {
        Self::Br(dest)
    }

    /// Creates a return instruction to exit a function.
    pub fn ret(val: Option<Value>) -> Self {
        Self::Ret(val)
    }

    /// Creates a conditional branch instruction based on a condition.
    pub fn cond_br(cond: Value, then_block: Block, else_block: Block) -> Self {
        Self::CondBr {
            cond,
            then_block,
            else_block,
        }
    }

    /// Creates a switch instruction for multi-way branching.
    pub fn switch(value: Value, default: Block, cases: Vec<(i64, Block)>) -> Self {
        Self::Switch {
            value,
            default,
            cases,
        }
    }

    /// Returns the type produced by this instruction.
    ///
    /// Instructions that do not yield a value (e.g. stores, branches, terminators) return `Ty::Void`
    pub fn ty(&self) -> Ty {
        match self {
            Inst::Noop
            | Inst::Unreachable
            | Inst::Br(_)
            | Inst::Ret(_)
            | Inst::Store { .. }
            | Inst::CondBr { .. }
            | Inst::Switch { .. }
            | Inst::Upsilon { .. } => Ty::Void,

            Inst::Icmp { .. } => Ty::I1,
            Inst::GlobalAddr(_) => Ty::Ptr,
            Inst::Alloca { .. } => Ty::Ptr,
            Inst::GetElementPtr { .. } => Ty::Ptr,

            Inst::Phi(ty)
            | Inst::Binary { ty, .. }
            | Inst::Bitcast { to: ty, .. }
            | Inst::Call { ty, .. }
            | Inst::ConstInt { ty, .. }
            | Inst::ConstNull(ty)
            | Inst::IndirectCall { ty, .. }
            | Inst::IntToPtr { to: ty, .. }
            | Inst::Load { ty, .. }
            | Inst::Param { ty, .. }
            | Inst::PtrToInt { to: ty, .. }
            | Inst::Select { ty, .. }
            | Inst::Sext { to: ty, .. }
            | Inst::Trunc { to: ty, .. }
            | Inst::Unary { ty, .. }
            | Inst::Zext { to: ty, .. } => *ty,
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "not"),
            UnaryOp::Neg => write!(f, "neg"),
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Mul => write!(f, "mul"),
            BinaryOp::Div => write!(f, "sdiv"),
            BinaryOp::Rem => write!(f, "srem"),
            BinaryOp::UDiv => write!(f, "udiv"),
            BinaryOp::URem => write!(f, "urem"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::Shl => write!(f, "shl"),
            BinaryOp::Shr => write!(f, "lshr"),
            BinaryOp::AShr => write!(f, "ashr"),
        }
    }
}

impl std::fmt::Display for ICmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ICmpOp::Eq => write!(f, "eq"),
            ICmpOp::Ne => write!(f, "ne"),
            ICmpOp::Lt => write!(f, "slt"),
            ICmpOp::Le => write!(f, "sle"),
            ICmpOp::Gt => write!(f, "sgt"),
            ICmpOp::Ge => write!(f, "sge"),
            ICmpOp::Ult => write!(f, "ult"),
            ICmpOp::Ule => write!(f, "ule"),
            ICmpOp::Ugt => write!(f, "ugt"),
            ICmpOp::Uge => write!(f, "uge"),
        }
    }
}

struct CommaSep<'a, I>(pub &'a I);
impl<'a, I, T> std::fmt::Display for CommaSep<'a, I>
where
    for<'b> &'b I: IntoIterator<Item = &'b T>,
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.0.into_iter();
        if let Some(first) = it.next() {
            write!(f, "{}", first)?;
            for item in it {
                write!(f, ", {}", item)?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Inst::Noop => write!(f, "noop"),
            Inst::Phi(ty) => write!(f, "phi {}", ty),
            Inst::Upsilon { phi, value } => write!(f, "upsilon {} -> {}", value, phi),
            Inst::ConstNull(ty) => write!(f, "null {}", ty),
            Inst::GlobalAddr(g) => write!(f, "global.addr {}", g),
            Inst::Param { ty, index } => write!(f, "param {} #{}", ty, index),
            Inst::ConstInt { ty, value } => write!(f, "const {} {}", ty, value),
            Inst::Alloca { ty, align } => write!(f, "alloca {}, align {}", ty, align),
            Inst::Load { ty, ptr, align } => {
                write!(f, "load {}, {} {}, align {}", ty, Ty::Ptr, ptr, align)
            }
            Inst::Store { ptr, align, value } => {
                write!(f, "store {} {}, align {}", ptr, value, align)
            }
            Inst::GetElementPtr { ty, ptr, indices } => {
                write!(f, "gep {}, {} {}", ty, Ty::Ptr, ptr)?;
                if !indices.is_empty() {
                    write!(f, ", {}", CommaSep(indices))?;
                }
                Ok(())
            }
            Inst::Unary { ty, op, operand } => write!(f, "{} {} {}", op, ty, operand),
            Inst::Binary { ty, lhs, rhs, op } => write!(f, "{} {} {}, {}", op, ty, lhs, rhs),
            Inst::Icmp { lhs, rhs, pred } => write!(f, "icmp {} {}, {}", pred, lhs, rhs),
            Inst::Select {
                ty,
                cond,
                then_val,
                else_val,
            } => {
                write!(
                    f,
                    "select i1 {}, {} {}, {} {}",
                    cond, ty, then_val, ty, else_val
                )
            }
            Inst::Zext { to, value } => write!(f, "zext {} to {}", value, to),
            Inst::Sext { to, value } => write!(f, "sext {} to {}", value, to),
            Inst::Trunc { to, value } => write!(f, "trunc {} to {}", value, to),
            Inst::Bitcast { to, value } => write!(f, "bitcast {} to {}", value, to),
            Inst::IntToPtr { to, value } => write!(f, "inttoptr {} to {}", value, to),
            Inst::PtrToInt { to, value } => write!(f, "ptrtoint {} to {}", value, to),
            Inst::Call { ty, func, args } => {
                write!(f, "call {} {}({})", ty, func, CommaSep(args))
            }
            Inst::IndirectCall { ty, ptr, args } => {
                write!(f, "call_indirect {} {}({})", ty, ptr, CommaSep(args))
            }
            Inst::Unreachable => write!(f, "unreachable"),
            Inst::Br(dest) => write!(f, "br {}", dest),
            Inst::Ret(val) => match val {
                Some(v) => write!(f, "ret {}", v),
                None => write!(f, "ret void"),
            },
            Inst::CondBr {
                cond,
                then_block,
                else_block,
            } => {
                write!(f, "br i1 {}, {}, {}", cond, then_block, else_block)
            }
            Inst::Switch {
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
