#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    /// A GP register
    Gp(GpReg),
    /// A XMM register
    Xmm(XmmReg),
}

impl Reg {
    pub fn is_gp(&self) -> bool {
        matches!(self, Reg::Gp(_))
    }

    pub fn is_xmm(&self) -> bool {
        matches!(self, Reg::Xmm(_))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GpReg {
    /// A virtual GP register
    Virt(u32),
    /// A physical GP register
    Phys(PhysGpReg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XmmReg {
    /// A virtual XMM register
    Virt(u32),
    /// A physical XMM register
    Phys(PhysXmmReg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhysGpReg {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhysXmmReg {
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

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::Gp(reg) => write!(f, "{}", reg),
            Reg::Xmm(reg) => write!(f, "{}", reg),
        }
    }
}

impl std::fmt::Display for GpReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GpReg::Phys(reg) => write!(f, "{}", reg),
            GpReg::Virt(id) => write!(f, "vgp{}", id),
        }
    }
}

impl std::fmt::Display for XmmReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            XmmReg::Phys(reg) => write!(f, "{}", reg),
            XmmReg::Virt(id) => write!(f, "vxmm{}", id),
        }
    }
}

impl std::fmt::Display for PhysGpReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhysGpReg::Rax => write!(f, "%rax"),
            PhysGpReg::Rcx => write!(f, "%rcx"),
            PhysGpReg::Rdx => write!(f, "%rdx"),
            PhysGpReg::Rbx => write!(f, "%rbx"),
            PhysGpReg::Rsp => write!(f, "%rsp"),
            PhysGpReg::Rbp => write!(f, "%rbp"),
            PhysGpReg::Rsi => write!(f, "%rsi"),
            PhysGpReg::Rdi => write!(f, "%rdi"),
            PhysGpReg::R8 => write!(f, "%r8"),
            PhysGpReg::R9 => write!(f, "%r9"),
            PhysGpReg::R10 => write!(f, "%r10"),
            PhysGpReg::R11 => write!(f, "%r11"),
            PhysGpReg::R12 => write!(f, "%r12"),
            PhysGpReg::R13 => write!(f, "%r13"),
            PhysGpReg::R14 => write!(f, "%r14"),
            PhysGpReg::R15 => write!(f, "%r15"),
        }
    }
}

impl std::fmt::Display for PhysXmmReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhysXmmReg::Xmm0 => write!(f, "%xmm0"),
            PhysXmmReg::Xmm1 => write!(f, "%xmm1"),
            PhysXmmReg::Xmm2 => write!(f, "%xmm2"),
            PhysXmmReg::Xmm3 => write!(f, "%xmm3"),
            PhysXmmReg::Xmm4 => write!(f, "%xmm4"),
            PhysXmmReg::Xmm5 => write!(f, "%xmm5"),
            PhysXmmReg::Xmm6 => write!(f, "%xmm6"),
            PhysXmmReg::Xmm7 => write!(f, "%xmm7"),
            PhysXmmReg::Xmm8 => write!(f, "%xmm8"),
            PhysXmmReg::Xmm9 => write!(f, "%xmm9"),
            PhysXmmReg::Xmm10 => write!(f, "%xmm10"),
            PhysXmmReg::Xmm11 => write!(f, "%xmm11"),
            PhysXmmReg::Xmm12 => write!(f, "%xmm12"),
            PhysXmmReg::Xmm13 => write!(f, "%xmm13"),
            PhysXmmReg::Xmm14 => write!(f, "%xmm14"),
            PhysXmmReg::Xmm15 => write!(f, "%xmm15"),
        }
    }
}
