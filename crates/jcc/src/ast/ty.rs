use jcc_arena::intern::{InternArena, Interned};

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
