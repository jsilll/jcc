use crate::ir::ty::Ty;

/// An allocatable physical register (excludes rsp and rbp).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PReg {
    // General-purpose integer registers
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // SSE registers (used for f32 / f64)
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,
}

/// Register class: integer (GP) or float (XMM).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RegClass {
    Int,
    Float,
}

impl PReg {
    pub fn class(self) -> RegClass {
        match self {
            PReg::Xmm0
            | PReg::Xmm1
            | PReg::Xmm2
            | PReg::Xmm3
            | PReg::Xmm4
            | PReg::Xmm5
            | PReg::Xmm6
            | PReg::Xmm7
            | PReg::Xmm8
            | PReg::Xmm9
            | PReg::Xmm10
            | PReg::Xmm11
            | PReg::Xmm12
            | PReg::Xmm13
            | PReg::Xmm14
            | PReg::Xmm15 => RegClass::Float,
            _ => RegClass::Int,
        }
    }

    /// True if this register is caller-saved under the SysV AMD64 ABI.
    /// GP: rax, rcx, rdx, rsi, rdi, r8–r11.
    /// Float: xmm0–xmm7 only; xmm8–xmm15 are callee-saved.
    pub fn is_caller_saved(self) -> bool {
        matches!(
            self,
            PReg::Rax
                | PReg::Rcx
                | PReg::Rdx
                | PReg::Rsi
                | PReg::Rdi
                | PReg::R8
                | PReg::R9
                | PReg::R10
                | PReg::R11
                | PReg::Xmm0
                | PReg::Xmm1
                | PReg::Xmm2
                | PReg::Xmm3
                | PReg::Xmm4
                | PReg::Xmm5
                | PReg::Xmm6
                | PReg::Xmm7
        )
    }

    /// AT&T 64-bit name (or the XMM name for float regs).
    pub fn name64(self) -> &'static str {
        match self {
            PReg::Rax => "%rax",
            PReg::Rbx => "%rbx",
            PReg::Rcx => "%rcx",
            PReg::Rdx => "%rdx",
            PReg::Rsi => "%rsi",
            PReg::Rdi => "%rdi",
            PReg::R8 => "%r8",
            PReg::R9 => "%r9",
            PReg::R10 => "%r10",
            PReg::R11 => "%r11",
            PReg::R12 => "%r12",
            PReg::R13 => "%r13",
            PReg::R14 => "%r14",
            PReg::R15 => "%r15",
            PReg::Xmm0 => "%xmm0",
            PReg::Xmm1 => "%xmm1",
            PReg::Xmm2 => "%xmm2",
            PReg::Xmm3 => "%xmm3",
            PReg::Xmm4 => "%xmm4",
            PReg::Xmm5 => "%xmm5",
            PReg::Xmm6 => "%xmm6",
            PReg::Xmm7 => "%xmm7",
            PReg::Xmm8 => "%xmm8",
            PReg::Xmm9 => "%xmm9",
            PReg::Xmm10 => "%xmm10",
            PReg::Xmm11 => "%xmm11",
            PReg::Xmm12 => "%xmm12",
            PReg::Xmm13 => "%xmm13",
            PReg::Xmm14 => "%xmm14",
            PReg::Xmm15 => "%xmm15",
        }
    }

    /// Name used for 32-bit sub-register operations (GP only).
    pub fn name32(self) -> &'static str {
        match self {
            PReg::Rax => "%eax",
            PReg::Rbx => "%ebx",
            PReg::Rcx => "%ecx",
            PReg::Rdx => "%edx",
            PReg::Rsi => "%esi",
            PReg::Rdi => "%edi",
            PReg::R8 => "%r8d",
            PReg::R9 => "%r9d",
            PReg::R10 => "%r10d",
            PReg::R11 => "%r11d",
            PReg::R12 => "%r12d",
            PReg::R13 => "%r13d",
            PReg::R14 => "%r14d",
            PReg::R15 => "%r15d",
            _ => self.name64(),
        }
    }

    /// Name used for 16-bit sub-register operations (GP only).
    pub fn name16(self) -> &'static str {
        match self {
            PReg::Rax => "%ax",
            PReg::Rbx => "%bx",
            PReg::Rcx => "%cx",
            PReg::Rdx => "%dx",
            PReg::Rsi => "%si",
            PReg::Rdi => "%di",
            PReg::R8 => "%r8w",
            PReg::R9 => "%r9w",
            PReg::R10 => "%r10w",
            PReg::R11 => "%r11w",
            PReg::R12 => "%r12w",
            PReg::R13 => "%r13w",
            PReg::R14 => "%r14w",
            PReg::R15 => "%r15w",
            _ => self.name64(),
        }
    }

    /// Name used for 8-bit sub-register operations (GP only, low byte).
    pub fn name8(self) -> &'static str {
        match self {
            PReg::Rax => "%al",
            PReg::Rbx => "%bl",
            PReg::Rcx => "%cl",
            PReg::Rdx => "%dl",
            PReg::Rsi => "%sil",
            PReg::Rdi => "%dil",
            PReg::R8 => "%r8b",
            PReg::R9 => "%r9b",
            PReg::R10 => "%r10b",
            PReg::R11 => "%r11b",
            PReg::R12 => "%r12b",
            PReg::R13 => "%r13b",
            PReg::R14 => "%r14b",
            PReg::R15 => "%r15b",
            _ => self.name64(),
        }
    }

    /// Choose the right-sized register name for an IR type.
    pub fn name_for_ty(self, ty: Ty) -> &'static str {
        match ty {
            Ty::I1 | Ty::I8 => self.name8(),
            Ty::I16 => self.name16(),
            Ty::I32 => self.name32(),
            _ => self.name64(),
        }
    }
}

/// Integer registers available for allocation (SysV AMD64), ordered so that
/// caller-saved registers come first (cheaper to allocate for short-lived values).
pub static INT_REGS: &[PReg] = &[
    PReg::Rax,
    PReg::Rcx,
    PReg::Rdx,
    PReg::Rsi,
    PReg::Rdi,
    PReg::R8,
    PReg::R9,
    PReg::R10,
    PReg::R11,
    PReg::Rbx,
    PReg::R12,
    PReg::R13,
    PReg::R14,
    PReg::R15,
];

/// Float registers available for allocation.
pub static FLOAT_REGS: &[PReg] = &[
    PReg::Xmm0,
    PReg::Xmm1,
    PReg::Xmm2,
    PReg::Xmm3,
    PReg::Xmm4,
    PReg::Xmm5,
    PReg::Xmm6,
    PReg::Xmm7,
    PReg::Xmm8,
    PReg::Xmm9,
    PReg::Xmm10,
    PReg::Xmm11,
    PReg::Xmm12,
    PReg::Xmm13,
    PReg::Xmm14,
    PReg::Xmm15,
];

/// SysV AMD64: integer argument registers, in order.
pub static INT_ARG_REGS: &[PReg] = &[
    PReg::Rdi,
    PReg::Rsi,
    PReg::Rdx,
    PReg::Rcx,
    PReg::R8,
    PReg::R9,
];

/// SysV AMD64: float argument registers, in order.
pub static FLOAT_ARG_REGS: &[PReg] = &[
    PReg::Xmm0,
    PReg::Xmm1,
    PReg::Xmm2,
    PReg::Xmm3,
    PReg::Xmm4,
    PReg::Xmm5,
    PReg::Xmm6,
    PReg::Xmm7,
];

/// Returns the register class appropriate for an IR type.
pub fn class_of(ty: Ty) -> RegClass {
    match ty {
        Ty::F32 | Ty::F64 => RegClass::Float,
        _ => RegClass::Int,
    }
}

/// Returns the callee-saved GP registers we may need to preserve.
pub static CALLEE_SAVED_INT: &[PReg] = &[PReg::Rbx, PReg::R12, PReg::R13, PReg::R14, PReg::R15];

/// Callee-saved XMM registers (xmm8–xmm15 per SysV AMD64).
pub static CALLEE_SAVED_FLOAT: &[PReg] = &[
    PReg::Xmm8,
    PReg::Xmm9,
    PReg::Xmm10,
    PReg::Xmm11,
    PReg::Xmm12,
    PReg::Xmm13,
    PReg::Xmm14,
    PReg::Xmm15,
];
