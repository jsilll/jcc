pub mod control;
pub mod resolve;
pub mod ty;

use crate::ast::{Ast, ConstValue, StmtRef};

use std::{
    collections::HashMap,
    num::{NonZeroU16, NonZeroU32},
    rc::Rc,
};

// ---------------------------------------------------------------------------
// SemaCtx
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq)]
pub struct SemaCtx {
    pub dict: TypeDict,
    symbols: Vec<Option<SymbolInfo>>,
    pub switches: HashMap<StmtRef, SwitchCases>,
}

impl SemaCtx {
    pub fn new(ast: &Ast) -> Self {
        Self {
            dict: TypeDict::new(),
            switches: HashMap::new(),
            symbols: vec![Default::default(); ast.symbols_len() + 1],
        }
    }

    pub fn with_dict(ast: &Ast, dict: TypeDict) -> Self {
        Self {
            dict,
            switches: HashMap::new(),
            symbols: vec![Default::default(); ast.symbols_len() + 1],
        }
    }

    #[inline]
    pub fn symbol(&self, sym: SemaSymbol) -> Option<&SymbolInfo> {
        self.symbols
            .get(sym.0.get() as usize)
            .and_then(|s| s.as_ref())
    }

    #[inline]
    pub fn symbol_mut(&mut self, sym: SemaSymbol) -> &mut Option<SymbolInfo> {
        self.symbols
            .get_mut(sym.0.get() as usize)
            .expect("Invalid symbol index")
    }
}

// ---------------------------------------------------------------------------
// TypeDict
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDict {
    types: Vec<Rc<CompoundType>>,
    cache: HashMap<Rc<CompoundType>, CompoundTypeRef>,
}

impl Default for TypeDict {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeDict {
    #[inline]
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            types: vec![Rc::new(CompoundType::Ptr(Type::default()))],
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.types.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    #[inline]
    pub fn get(&self, ty: CompoundTypeRef) -> &CompoundType {
        &self.types[ty.0.get() as usize]
    }

    #[inline]
    pub fn intern(&mut self, ty: CompoundType) -> CompoundTypeRef {
        if let Some(&r) = self.cache.get(&ty) {
            return r;
        }
        let r = CompoundTypeRef(NonZeroU16::new(self.types.len() as u16).unwrap());
        let ty = Rc::new(ty);
        self.types.push(ty.clone());
        self.cache.insert(ty, r);
        r
    }
}

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
    NoInitializer,
    /// Tentative initializer
    Tentative,
    /// Initialized with a value
    Initialized(ConstValue),
}

// ---------------------------------------------------------------------------
// Type
// ---------------------------------------------------------------------------

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    /// The `void` type
    #[default]
    Void,
    /// The `int` type
    Int,
    /// The `long` type
    Long,
    /// A compound type
    Compound(CompoundTypeRef),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct CompoundTypeRef(NonZeroU16);

impl From<CompoundTypeRef> for Type {
    fn from(ty: CompoundTypeRef) -> Self {
        Type::Compound(ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompoundType {
    /// A pointer type
    Ptr(Type),
    /// A functional type
    Func { ret: Type, params: Vec<Type> },
}
