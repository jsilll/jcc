use jcc_arena::intern::{InternArena, Interned};
use jcc_ssa::ir::ty::Ty as SsaTy;

// ---------------------------------------------------------------------------
// Ty
// ---------------------------------------------------------------------------

pub type Ty<'ctx> = Interned<'ctx, TyKind<'ctx>>;

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub enum TyKind<'ctx> {
    #[default]
    Void,
    Int,
    Long,
    Ptr(Ty<'ctx>),
    Func {
        ret: Ty<'ctx>,
        params: Vec<Ty<'ctx>>,
    },
}

impl<'ctx> TyKind<'ctx> {
    /// Returns the return type if the type is a function type.
    pub fn ret(&self) -> Option<Ty<'ctx>> {
        match *self {
            TyKind::Func { ret, .. } => Some(ret),
            _ => None,
        }
    }

    /// Returns the common type between two types, if any.
    pub fn common(&self, other: &TyKind<'ctx>, ctx: &TyCtx<'ctx>) -> Option<Ty<'ctx>> {
        match (self, other) {
            (TyKind::Int, TyKind::Int) => Some(ctx.int_ty),
            (TyKind::Long, TyKind::Long) => Some(ctx.long_ty),
            (TyKind::Int, TyKind::Long) | (TyKind::Long, TyKind::Int) => Some(ctx.long_ty),
            _ => None,
        }
    }
}

impl From<&TyKind<'_>> for SsaTy {
    fn from(val: &TyKind<'_>) -> Self {
        match val {
            TyKind::Void => SsaTy::Void,
            TyKind::Int => SsaTy::I32,
            TyKind::Long => SsaTy::I64,
            TyKind::Ptr(_) | TyKind::Func { .. } => SsaTy::Ptr,
        }
    }
}

// ---------------------------------------------------------------------------
// TyCtx
// ---------------------------------------------------------------------------

pub struct TyCtx<'ctx> {
    pub int_ty: Ty<'ctx>,
    pub long_ty: Ty<'ctx>,
    pub void_ty: Ty<'ctx>,
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
        let int_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::Int).unwrap()) };
        let long_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::Long).unwrap()) };
        let void_ty =
            unsafe { std::mem::transmute::<Ty<'_>, Ty<'_>>(types.intern(TyKind::Void).unwrap()) };
        Self {
            types,
            int_ty,
            long_ty,
            void_ty,
        }
    }

    #[inline]
    pub fn ptr(&'ctx self, pointee: Ty<'ctx>) -> Ty<'ctx> {
        self.types.intern(TyKind::Ptr(pointee)).unwrap()
    }

    #[inline]
    pub fn func(&'ctx self, ret: Ty<'ctx>, params: Vec<Ty<'ctx>>) -> Ty<'ctx> {
        self.types.intern(TyKind::Func { ret, params }).unwrap()
    }
}
