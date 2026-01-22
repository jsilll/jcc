pub mod control;
pub mod resolve;
pub mod typing;

use crate::ast::{
    constant::Constant,
    ty::{Ty, TyCtx},
    Stmt,
};

use jcc_entity::{entity_impl, SecondaryMap};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// SemaCtx
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);
entity_impl!(Symbol, "symbol");

pub struct SemaCtx<'ctx> {
    pub ty: &'ctx TyCtx<'ctx>,
    pub switches: HashMap<Stmt, SwitchCases>,
    pub symbols: SecondaryMap<Symbol, Option<SymbolInfo<'ctx>>>,
}

impl<'ctx> SemaCtx<'ctx> {
    pub fn new(ty: &'ctx TyCtx<'ctx>, capacity: usize) -> Self {
        Self {
            ty,
            switches: HashMap::new(),
            symbols: SecondaryMap::with_capacity(capacity),
        }
    }
}

// ---------------------------------------------------------------------------
// SwitchCases
// ---------------------------------------------------------------------------

// TODO:(perf) Consider flattening the cases into a pool
#[derive(Default, Debug, Clone)]
pub struct SwitchCases {
    pub cases: Vec<Stmt>,
    pub default: Option<Stmt>,
}

// ---------------------------------------------------------------------------
// SymbolInfo
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Default, Clone, Copy)]
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

#[derive(Debug, Default, Clone, Copy)]
pub enum StaticValue {
    /// No initializer
    #[default]
    NoInit,
    /// Tentative initializer
    Tentative,
    /// Initialized with a value
    Init(Constant),
}

impl StaticValue {
    /// Returns the integer value if it exists
    pub fn value(&self) -> Option<i64> {
        match self {
            StaticValue::NoInit => None,
            StaticValue::Tentative => Some(0),
            StaticValue::Init(Constant::Long(v)) => Some(*v),
            StaticValue::Init(Constant::Int(v)) => Some(*v as i64),
            StaticValue::Init(Constant::UInt(v)) => Some(*v as i64),
            StaticValue::Init(Constant::ULong(v)) => Some(*v as i64),
            StaticValue::Init(Constant::Double(v)) => Some(*v as i64),
        }
    }
}
