use crate::ast::{Ty, TyKind};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Constant {
    /// A constant integer value.
    Int(i32),
    /// A constant long integer value.
    Long(i64),
    /// A constant unsigned integer value.
    UInt(u32),
    /// A constant unsigned long integer value.
    ULong(u64),
    /// A constant double precision floating-point value.
    Double(u64),
}

impl Constant {
    /// Converts the constant to a signed variant.
    pub fn to_int(&self) -> Self {
        match self {
            Self::Double(_) => todo!(),
            Self::Int(v) => Self::Int(*v),
            Self::Long(v) => Self::Int(*v as i32),
            Self::UInt(v) => Self::Int(*v as i32),
            Self::ULong(v) => Self::Int(*v as i32),
        }
    }

    /// Converts the constant to a long variant.
    pub fn to_long(&self) -> Self {
        match self {
            Self::Double(_) => todo!(),
            Self::Long(v) => Self::Long(*v),
            Self::Int(v) => Self::Long(*v as i64),
            Self::UInt(v) => Self::Long(*v as i64),
            Self::ULong(v) => Self::Long(*v as i64),
        }
    }

    /// Converts the constant to an unsigned variant.
    pub fn to_uint(&self) -> Self {
        match self {
            Self::Double(_) => todo!(),
            Self::UInt(v) => Self::UInt(*v),
            Self::Int(v) => Self::UInt(*v as u32),
            Self::Long(v) => Self::UInt(*v as u32),
            Self::ULong(v) => Self::UInt(*v as u32),
        }
    }

    /// Converts the constant to an unsigned long variant.
    pub fn to_ulong(&self) -> Self {
        match self {
            Self::Double(_) => todo!(),
            Self::ULong(v) => Self::ULong(*v),
            Self::Int(v) => Self::ULong(*v as u64),
            Self::UInt(v) => Self::ULong(*v as u64),
            Self::Long(v) => Self::ULong(*v as u64),
        }
    }

    /// Converts the constant to the specified type
    pub fn cast(&self, ty: Ty<'_>) -> Self {
        match *ty {
            TyKind::Int => self.to_int(),
            TyKind::Long => self.to_long(),
            TyKind::UInt => self.to_uint(),
            TyKind::ULong => self.to_ulong(),
            _ => *self,
        }
    }

    /// Lowers the constant to tuple with its value and type
    pub fn lower(&self) -> (i64, jcc_ssa::ir::ty::Ty) {
        use jcc_ssa::ir::ty::*;
        match self {
            Self::Double(_) => todo!(),
            Self::Long(v) => (*v, Ty::I64),
            Self::Int(v) => (*v as i64, Ty::I32),
            Self::UInt(v) => (*v as i64, Ty::I32),
            Self::ULong(v) => (*v as i64, Ty::I64),
        }
    }
}
