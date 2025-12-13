pub mod control;
pub mod resolve;
pub mod typing;

use crate::ast::{
    ty::{Ty, TyCtx},
    Stmt,
};

use jcc_entity::{entity_impl, SecondaryMap};
use jcc_ssa::ir::ConstValue;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// SemaCtx
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);
entity_impl!(Symbol, "symbol");

pub struct SemaCtx<'ctx> {
    pub tys: &'ctx TyCtx<'ctx>,
    pub switches: HashMap<Stmt, SwitchCases>,
    pub symbols: SecondaryMap<Symbol, Option<SymbolInfo<'ctx>>>,
}

impl<'ctx> SemaCtx<'ctx> {
    pub fn new(tys: &'ctx TyCtx<'ctx>, capacity: usize) -> Self {
        Self {
            tys,
            switches: HashMap::new(),
            symbols: SecondaryMap::with_capacity(capacity),
        }
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
