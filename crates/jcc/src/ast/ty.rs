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
    /// The `double` type.
    Double,
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

    /// Returns the return type if the type is a function type.
    pub fn ret(&self) -> Option<Ty<'ctx>> {
        match self {
            TyKind::Func { ret, .. } => Some(*ret),
            _ => None,
        }
    }

    /// Lowers the type to a tuple with the ssa type and signdness
    pub fn lower(&self) -> (SsaTy, bool) {
        match self {
            TyKind::Void => (SsaTy::Void, false),
            TyKind::Int => (SsaTy::I32, true),
            TyKind::Long => (SsaTy::I64, true),
            TyKind::UInt => (SsaTy::I32, false),
            TyKind::ULong => (SsaTy::I64, false),
            TyKind::Double => (SsaTy::F64, false),
            TyKind::Ptr(_) | TyKind::Func { .. } => (SsaTy::Ptr, false),
        }
    }

    /// Returns the common type between two types, if any.
    pub fn common(lhs: Ty<'ctx>, rhs: Ty<'ctx>) -> Option<Ty<'ctx>> {
        if lhs == rhs {
            Some(lhs)
        } else {
            let rhs_size = rhs.lower().0.size_bytes();
            let (lhs_ssa, is_lhs_signed) = lhs.lower();
            match lhs_ssa.size_bytes().cmp(&rhs_size) {
                std::cmp::Ordering::Less => Some(rhs),
                std::cmp::Ordering::Greater => Some(lhs),
                std::cmp::Ordering::Equal => {
                    if is_lhs_signed {
                        Some(rhs)
                    } else {
                        Some(lhs)
                    }
                }
            }
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
    /// Canonical `double` type.
    pub double_ty: Ty<'ctx>,
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
        let double_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::Double).unwrap()) };
        Self {
            types,
            void_ty,
            int_ty,
            uint_ty,
            long_ty,
            ulong_ty,
            double_ty,
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
