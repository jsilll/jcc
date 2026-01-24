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

macro_rules! impl_cast {
    ($method_name:ident, $variant:ident, $target_ty:ty, $min:expr, $max:expr) => {
        pub fn $method_name(&self) -> Option<Self> {
            match self {
                Self::Int(v) => Some(Self::$variant(*v as $target_ty)),
                Self::Long(v) => Some(Self::$variant(*v as $target_ty)),
                Self::UInt(v) => Some(Self::$variant(*v as $target_ty)),
                Self::ULong(v) => Some(Self::$variant(*v as $target_ty)),
                Self::Double(v) => {
                    let f = f64::from_bits(*v).trunc();
                    // Strict check: Is the float within the range of the target integer?
                    if f >= ($min as f64) && f <= ($max as f64) {
                        Some(Self::$variant(f as $target_ty))
                    } else {
                        None
                    }
                }
            }
        }
    };
}

impl Constant {
    /// Returns the raw bits of the constant value.
    pub fn to_bits(&self) -> u64 {
        match self {
            Self::ULong(v) => *v,
            Self::Double(v) => *v,
            Self::Int(v) => *v as u64,
            Self::Long(v) => *v as u64,
            Self::UInt(v) => *v as u64,
        }
    }

    /// Lowers the constant to tuple with its value and type.
    pub fn lower(&self) -> (u64, jcc_ssa::ir::ty::Ty) {
        use jcc_ssa::ir::ty::*;
        match self {
            Self::ULong(v) => (*v, Ty::I64),
            Self::Double(v) => (*v, Ty::F64),
            Self::Int(v) => (*v as u64, Ty::I32),
            Self::Long(v) => (*v as u64, Ty::I64),
            Self::UInt(v) => (*v as u64, Ty::I32),
        }
    }

    impl_cast!(to_int, Int, i32, i32::MIN, i32::MAX);
    impl_cast!(to_long, Long, i64, i64::MIN, i64::MAX);
    impl_cast!(to_uint, UInt, u32, u32::MIN, u32::MAX);
    impl_cast!(to_ulong, ULong, u64, u64::MIN, u64::MAX);

    /// Converts the constant to a double precision floating-point value.
    pub fn to_double(&self) -> Option<Self> {
        let val = match self {
            Self::Int(v) => *v as f64,
            Self::UInt(v) => *v as f64,
            Self::Long(v) => *v as f64,
            Self::ULong(v) => *v as f64,
            Self::Double(v) => f64::from_bits(*v),
        };
        Some(Self::Double(val.to_bits()))
    }

    /// Converts the constant to the specified type
    pub fn cast(&self, ty: Ty<'_>) -> Option<Self> {
        match *ty {
            TyKind::Int => self.to_int(),
            TyKind::Long => self.to_long(),
            TyKind::UInt => self.to_uint(),
            TyKind::ULong => self.to_ulong(),
            TyKind::Double => self.to_double(),
            // Ensure we handle cases where casting isn't necessary
            _ => Some(*self),
        }
    }
}
