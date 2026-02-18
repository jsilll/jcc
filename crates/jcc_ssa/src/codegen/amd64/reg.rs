#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    /// A virtual register
    Virt(RegClass, u32),
    /// A physical register
    Phys(RegClass, u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegClass {
    /// General purpose register
    Gp,
    /// Vector register for SIMD operations
    Xmm,
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::Virt(class, idx) => {
                let class = match class {
                    RegClass::Gp => "gp",
                    RegClass::Xmm => "xmm",
                };
                write!(f, "%virt{}.{}", idx, class)
            }
            Reg::Phys(class, idx) => match class {
                RegClass::Gp => match idx {
                    0 => write!(f, "%rax"),
                    1 => write!(f, "%rcx"),
                    2 => write!(f, "%rdx"),
                    3 => write!(f, "%rbx"),
                    4 => write!(f, "%rsp"),
                    5 => write!(f, "%rbp"),
                    6 => write!(f, "%rsi"),
                    7 => write!(f, "%rdi"),
                    _ => write!(f, "%r{}", idx),
                },
                RegClass::Xmm => write!(f, "%xmm{}", idx),
            },
        }
    }
}

pub const RAX: Reg = Reg::Phys(RegClass::Gp, 0);
pub const RCX: Reg = Reg::Phys(RegClass::Gp, 1);
pub const RDX: Reg = Reg::Phys(RegClass::Gp, 2);
pub const RBX: Reg = Reg::Phys(RegClass::Gp, 3);
pub const RSP: Reg = Reg::Phys(RegClass::Gp, 4);
pub const RBP: Reg = Reg::Phys(RegClass::Gp, 5);
pub const RSI: Reg = Reg::Phys(RegClass::Gp, 6);
pub const RDI: Reg = Reg::Phys(RegClass::Gp, 7);
pub const R8: Reg = Reg::Phys(RegClass::Gp, 8);
pub const R9: Reg = Reg::Phys(RegClass::Gp, 9);
pub const R10: Reg = Reg::Phys(RegClass::Gp, 10);
pub const R11: Reg = Reg::Phys(RegClass::Gp, 11);
pub const R12: Reg = Reg::Phys(RegClass::Gp, 12);
pub const R13: Reg = Reg::Phys(RegClass::Gp, 13);
pub const R14: Reg = Reg::Phys(RegClass::Gp, 14);
pub const R15: Reg = Reg::Phys(RegClass::Gp, 15);

pub const XMM0: Reg = Reg::Phys(RegClass::Xmm, 0);
pub const XMM1: Reg = Reg::Phys(RegClass::Xmm, 1);
pub const XMM2: Reg = Reg::Phys(RegClass::Xmm, 2);
pub const XMM3: Reg = Reg::Phys(RegClass::Xmm, 3);
pub const XMM4: Reg = Reg::Phys(RegClass::Xmm, 4);
pub const XMM5: Reg = Reg::Phys(RegClass::Xmm, 5);
pub const XMM6: Reg = Reg::Phys(RegClass::Xmm, 6);
pub const XMM7: Reg = Reg::Phys(RegClass::Xmm, 7);
pub const XMM8: Reg = Reg::Phys(RegClass::Xmm, 8);
pub const XMM9: Reg = Reg::Phys(RegClass::Xmm, 9);
pub const XMM10: Reg = Reg::Phys(RegClass::Xmm, 10);
pub const XMM11: Reg = Reg::Phys(RegClass::Xmm, 11);
pub const XMM12: Reg = Reg::Phys(RegClass::Xmm, 12);
pub const XMM13: Reg = Reg::Phys(RegClass::Xmm, 13);
pub const XMM14: Reg = Reg::Phys(RegClass::Xmm, 14);
pub const XMM15: Reg = Reg::Phys(RegClass::Xmm, 15);
