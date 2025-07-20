pub mod control;
pub mod resolve;
pub mod ty;

use crate::ast::{Ast, DeclRef, ExprRef, StmtRef};

use std::{collections::HashMap, num::NonZeroU32};

// ---------------------------------------------------------------------------
// SemaSymbol
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SemaSymbol(pub(crate) NonZeroU32);

impl Default for SemaSymbol {
    fn default() -> Self {
        Self(NonZeroU32::new(u32::MAX).unwrap())
    }
}

// ---------------------------------------------------------------------------
// SemaCtx
// ---------------------------------------------------------------------------

// TODO:(perf) Consider moving types directly into the AST
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaCtx {
    decls_type: Vec<Type>,
    exprs_type: Vec<Type>,
    symbols: Vec<Option<SymbolInfo>>,
    pub switches: HashMap<StmtRef, SwitchCases>,
}

impl SemaCtx {
    pub fn new(ast: &Ast) -> Self {
        Self {
            switches: HashMap::new(),
            symbols: vec![Default::default(); ast.symbols_len() + 1],
            decls_type: vec![Default::default(); ast.decls_len() + 1],
            exprs_type: vec![Default::default(); ast.exprs_len() + 1],
        }
    }

    #[inline]
    pub fn decl_type(&self, decl: DeclRef) -> &Type {
        &self.decls_type[decl.0.get() as usize]
    }

    #[inline]
    pub fn expr_type(&self, expr: ExprRef) -> &Type {
        &self.exprs_type[expr.0.get() as usize]
    }

    #[inline]
    pub fn decl_type_mut(&mut self, decl: DeclRef) -> &mut Type {
        &mut self.decls_type[decl.0.get() as usize]
    }

    #[inline]
    pub fn expr_type_mut(&mut self, expr: ExprRef) -> &mut Type {
        &mut self.exprs_type[expr.0.get() as usize]
    }

    #[inline]
    pub fn symbol(&self, sym: SemaSymbol) -> &Option<SymbolInfo> {
        &self.symbols[sym.0.get() as usize]
    }

    #[inline]
    pub fn symbol_mut(&mut self, sym: SemaSymbol) -> &mut Option<SymbolInfo> {
        &mut self.symbols[sym.0.get() as usize]
    }
}

// ---------------------------------------------------------------------------
// Type
// ---------------------------------------------------------------------------

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    #[default]
    Void,
    Int,
    Func(u32),
}

// ---------------------------------------------------------------------------
// SwitchCases
// ---------------------------------------------------------------------------

// TODO:(perf) Consider flattening the cases into a pool
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SwitchCases {
    pub cases: Vec<StmtRef>,
    pub default: Option<StmtRef>,
}

// ---------------------------------------------------------------------------
// SymbolInfo
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolInfo {
    pub ty: Type,
    pub attr: Attribute,
}

impl SymbolInfo {
    #[inline]
    pub fn local(ty: Type) -> Self {
        Self {
            ty,
            attr: Attribute::Local,
        }
    }

    #[inline]
    pub fn statik(ty: Type, is_global: bool, init: StaticValue) -> Self {
        Self {
            ty,
            attr: Attribute::Static { is_global, init },
        }
    }

    #[inline]
    pub fn function(ty: Type, is_global: bool, is_defined: bool) -> Self {
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
    #[default]
    Local,
    Function {
        is_global: bool,
        is_defined: bool,
    },
    Static {
        is_global: bool,
        init: StaticValue,
    },
}

// ---------------------------------------------------------------------------
// StaticValue
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StaticValue {
    #[default]
    NoInitializer,
    Tentative,
    Initialized(i64),
}
