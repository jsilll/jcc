#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    /// A virtual register
    Virt(VirtReg),
    /// A physical register
    Phys(PhysReg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtReg(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegClass {
    /// General purpose register
    Gp,
    /// Vector register for SIMD operations
    Xmm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhysReg {
    pub idx: u8,
    pub class: RegClass,
}

impl PhysReg {
    pub const fn new(class: RegClass, idx: u8) -> Self {
        Self { class, idx }
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::Virt(v) => write!(f, "%virt{}", v.0),
            Reg::Phys(p) => match p.class {
                RegClass::Gp => match p.idx {
                    0 => write!(f, "%rax"),
                    1 => write!(f, "%rcx"),
                    2 => write!(f, "%rdx"),
                    3 => write!(f, "%rbx"),
                    4 => write!(f, "%rsp"),
                    5 => write!(f, "%rbp"),
                    6 => write!(f, "%rsi"),
                    7 => write!(f, "%rdi"),
                    8 => write!(f, "%r8"),
                    9 => write!(f, "%r9"),
                    10 => write!(f, "%r10"),
                    11 => write!(f, "%r11"),
                    12 => write!(f, "%r12"),
                    13 => write!(f, "%r13"),
                    14 => write!(f, "%r14"),
                    15 => write!(f, "%r15"),
                    idx => write!(f, "%r{idx}"),
                },
                RegClass::Xmm => match p.idx {
                    0 => write!(f, "%xmm0"),
                    1 => write!(f, "%xmm1"),
                    2 => write!(f, "%xmm2"),
                    3 => write!(f, "%xmm3"),
                    4 => write!(f, "%xmm4"),
                    5 => write!(f, "%xmm5"),
                    6 => write!(f, "%xmm6"),
                    7 => write!(f, "%xmm7"),
                    8 => write!(f, "%xmm8"),
                    9 => write!(f, "%xmm9"),
                    10 => write!(f, "%xmm10"),
                    11 => write!(f, "%xmm11"),
                    12 => write!(f, "%xmm12"),
                    13 => write!(f, "%xmm13"),
                    14 => write!(f, "%xmm14"),
                    15 => write!(f, "%xmm15"),
                    idx => write!(f, "%xmm{idx}"),
                },
            },
        }
    }
}

pub const RAX: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 0));
pub const RCX: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 1));
pub const RDX: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 2));
pub const RBX: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 3));
pub const RSP: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 4));
pub const RBP: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 5));
pub const RSI: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 6));
pub const RDI: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 7));
pub const R8: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 8));
pub const R9: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 9));
pub const R10: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 10));
pub const R11: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 11));
pub const R12: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 12));
pub const R13: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 13));
pub const R14: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 14));
pub const R15: Reg = Reg::Phys(PhysReg::new(RegClass::Gp, 15));

pub const XMM0: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 0));
pub const XMM1: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 1));
pub const XMM2: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 2));
pub const XMM3: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 3));
pub const XMM4: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 4));
pub const XMM5: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 5));
pub const XMM6: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 6));
pub const XMM7: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 7));
pub const XMM8: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 8));
pub const XMM9: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 9));
pub const XMM10: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 10));
pub const XMM11: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 11));
pub const XMM12: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 12));
pub const XMM13: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 13));
pub const XMM14: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 14));
pub const XMM15: Reg = Reg::Phys(PhysReg::new(RegClass::Xmm, 15));
