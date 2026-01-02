use jcc_arena::intern::{InternArena, Interned};
use jcc_ssa::ir::ty::Ty as SsaTy;

// ---------------------------------------------------------------------------
// Ty
// ---------------------------------------------------------------------------

pub type Ty<'ctx> = Interned<'ctx, TyKind<'ctx>>;

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub enum TyKind<'ctx> {
    /// The `void` type.
    #[default]
    Void,
    /// The `int` type.
    Int,
    /// The `unsigned int` type.
    UInt,
    /// The `long` type.
    Long,
    /// The `unsigned long` type.
    ULong,
    /// A pointer type.
    Ptr(Ty<'ctx>),
    /// A function type.
    Func {
        ret: Ty<'ctx>,
        params: Vec<Ty<'ctx>>,
    },
}

impl<'ctx> TyKind<'ctx> {
    /// Returns true if the type is signed.
    pub fn is_signed(&self) -> bool {
        matches!(self, TyKind::Int | TyKind::Long)
    }

    /// Returns the size of the type in bytes.
    pub fn byte_size(&self) -> usize {
        match self {
            TyKind::Void | TyKind::Func { .. } => 0,
            TyKind::Int | TyKind::UInt => 4,
            TyKind::Long | TyKind::ULong => 8,
            TyKind::Ptr(_) => 8,
        }
    }

    /// Returns the return type if the type is a function type.
    pub fn ret(&self) -> Option<Ty<'ctx>> {
        match self {
            TyKind::Func { ret, .. } => Some(*ret),
            _ => None,
        }
    }

    /// Returns the common type between two types, if any.
    pub fn common(lhs: Ty<'ctx>, rhs: Ty<'ctx>) -> Option<Ty<'ctx>> {
        if lhs == rhs {
            Some(lhs)
        } else {
            let lhs_ref = lhs.as_ref();
            let rhs_ref = rhs.as_ref();
            match lhs_ref.byte_size().cmp(&rhs_ref.byte_size()) {
                std::cmp::Ordering::Less => Some(rhs),
                std::cmp::Ordering::Greater => Some(lhs),
                std::cmp::Ordering::Equal => {
                    if lhs_ref.is_signed() {
                        Some(rhs)
                    } else {
                        Some(lhs)
                    }
                }
            }
        }
    }
}

impl From<&TyKind<'_>> for SsaTy {
    fn from(val: &TyKind<'_>) -> Self {
        match val {
            TyKind::Void | TyKind::Func { .. } => SsaTy::Void,
            TyKind::Int => SsaTy::I32,
            TyKind::UInt => SsaTy::I32,
            TyKind::Long => SsaTy::I64,
            TyKind::ULong => SsaTy::I64,
            TyKind::Ptr(_) => SsaTy::Ptr,
        }
    }
}

// ---------------------------------------------------------------------------
// TyCtx
// ---------------------------------------------------------------------------

pub struct TyCtx<'ctx> {
    /// Canonical `void` type.
    pub void_ty: Ty<'ctx>,
    /// Canonical `int` type.
    pub int_ty: Ty<'ctx>,
    /// Canonical `unsigned int` type.
    pub uint_ty: Ty<'ctx>,
    /// Canonical `long` type.
    pub long_ty: Ty<'ctx>,
    /// Canonical `unsigned long` type.
    pub ulong_ty: Ty<'ctx>,
    /// Interned types.
    types: InternArena<TyKind<'ctx>>,
}

impl<'ctx> Default for TyCtx<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> TyCtx<'ctx> {
    pub fn new() -> Self {
        // Safety: The locally allocated references will not outlive the arena
        // since they are contained within TyCtx, which also owns the arena.
        let types = InternArena::new();
        let void_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::Void).unwrap()) };
        let int_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::Int).unwrap()) };
        let uint_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::UInt).unwrap()) };
        let long_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::Long).unwrap()) };
        let ulong_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::ULong).unwrap()) };
        Self {
            types,
            void_ty,
            int_ty,
            uint_ty,
            long_ty,
            ulong_ty,
        }
    }

    /// Creates a pointer type to the given pointee type.
    #[inline]
    pub fn ptr(&'ctx self, pointee: Ty<'ctx>) -> Ty<'ctx> {
        self.types.intern(TyKind::Ptr(pointee)).unwrap()
    }

    /// Creates a function type with the given return type and parameter types.
    #[inline]
    pub fn func(&'ctx self, ret: Ty<'ctx>, params: Vec<Ty<'ctx>>) -> Ty<'ctx> {
        self.types.intern(TyKind::Func { ret, params }).unwrap()
    }
}
