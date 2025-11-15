pub mod control;
pub mod resolve;
pub mod typing;

use crate::ast::{
    ty::{Ty, TyCtx},
    Stmt,
};

use jcc_ssa::ir::ConstValue;

use std::{collections::HashMap, num::NonZeroU32};

// ---------------------------------------------------------------------------
// SemaCtx
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SemaId(pub(crate) NonZeroU32);

pub struct SemaCtx<'ctx> {
    pub tys: &'ctx TyCtx<'ctx>,
    symbols: Vec<Option<SymbolInfo<'ctx>>>,
    pub switches: HashMap<Stmt, SwitchCases>,
}

impl<'ctx> SemaCtx<'ctx> {
    pub fn new(tys: &'ctx TyCtx<'ctx>, symbol_count: usize) -> Self {
        Self {
            tys,
            switches: HashMap::new(),
            symbols: vec![Default::default(); symbol_count],
        }
    }

    #[inline]
    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
    }

    #[inline]
    pub fn symbol(&self, sym: SemaId) -> Option<&SymbolInfo<'ctx>> {
        self.symbols
            .get(sym.0.get() as usize)
            .and_then(|s| s.as_ref())
    }

    #[inline]
    pub fn symbol_mut(&mut self, sym: SemaId) -> &mut Option<SymbolInfo<'ctx>> {
        self.symbols
            .get_mut(sym.0.get() as usize)
            .expect("Invalid symbol index")
    }
}

// ---------------------------------------------------------------------------
// SwitchCases
// ---------------------------------------------------------------------------

// TODO:(perf) Consider flattening the cases into a pool
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SwitchCases {
    pub cases: Vec<Stmt>,
    pub default: Option<Stmt>,
}

// ---------------------------------------------------------------------------
// SymbolInfo
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolInfo<'ctx> {
    pub ty: Ty<'ctx>,
    pub attr: Attribute,
}

impl<'ctx> SymbolInfo<'ctx> {
    #[inline]
    pub fn local(ty: Ty<'ctx>) -> Self {
        Self {
            ty,
            attr: Attribute::Local,
        }
    }

    #[inline]
    pub fn statik(ty: Ty<'ctx>, is_global: bool, init: StaticValue) -> Self {
        Self {
            ty,
            attr: Attribute::Static { is_global, init },
        }
    }

    #[inline]
    pub fn function(ty: Ty<'ctx>, is_global: bool, is_defined: bool) -> Self {
        Self {
            ty,
            attr: Attribute::Function {
                is_global,
                is_defined,
            },
        }
    }

    #[inline]
    pub fn is_global(&self) -> bool {
        match self.attr {
            Attribute::Local => false,
            Attribute::Static { is_global, .. } | Attribute::Function { is_global, .. } => {
                is_global
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Attribute
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Attribute {
    /// A local variable
    #[default]
    Local,
    /// A static variable
    Static { is_global: bool, init: StaticValue },
    /// A function
    Function { is_global: bool, is_defined: bool },
}

// ---------------------------------------------------------------------------
// StaticValue
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StaticValue {
    /// No initializer
    #[default]
    NoInit,
    /// Tentative initializer
    Tentative,
    /// Initialized with a value
    Init(ConstValue),
}
