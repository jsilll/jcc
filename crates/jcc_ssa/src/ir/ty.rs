#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty {
    /// Void type
    Void,
    /// 1-bit integer
    I1,
    /// 8-bit integer
    I8,
    /// 16-bit integer
    I16,
    /// 32-bit integer
    I32,
    /// 64-bit integer
    I64,
    /// Pointer type
    Ptr,
}

impl Ty {
    pub fn size_bytes(&self) -> Option<u32> {
        match self {
            Ty::Void => None,
            Ty::I1 => Some(1),
            Ty::I8 => Some(1),
            Ty::I16 => Some(2),
            Ty::I32 => Some(4),
            Ty::I64 => Some(8),
            Ty::Ptr => Some(8),
        }
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Void => write!(f, "void"),
            Ty::I1 => write!(f, "i1"),
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "i64"),
            Ty::Ptr => write!(f, "ptr"),
        }
    }
}
