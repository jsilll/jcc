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
    /// 32-bit floating-point
    F32,
    /// 64-bit floating-point
    F64,
    /// Pointer type
    Ptr,
}

impl Ty {
    pub fn size_bytes(&self) -> u32 {
        match self {
            Ty::Void => 0,
            Ty::I1 => 1,
            Ty::I8 => 1,
            Ty::I16 => 2,
            Ty::I32 => 4,
            Ty::I64 => 8,
            Ty::F32 => 4,
            Ty::F64 => 8,
            Ty::Ptr => 8,
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
            Ty::F32 => write!(f, "f32"),
            Ty::F64 => write!(f, "f64"),
            Ty::Ptr => write!(f, "ptr"),
        }
    }
}
