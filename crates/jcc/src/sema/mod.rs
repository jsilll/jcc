pub mod ty;

pub mod control;

pub mod resolve;

use crate::ast::{Ast, DeclRef, ExprRef, StmtRef};

use std::collections::HashMap;

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
// SemaCtx
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaCtx {
    decls_type: Vec<Type>,
    exprs_type: Vec<Type>,
    pub names: HashMap<ExprRef, DeclRef>,
    pub breaks: HashMap<StmtRef, StmtRef>,
    pub continues: HashMap<StmtRef, StmtRef>,
    pub switches: HashMap<StmtRef, SwitchCases>,
}

impl SemaCtx {
    pub fn new(ast: &Ast) -> Self {
        Self {
            names: HashMap::new(),
            breaks: HashMap::new(),
            switches: HashMap::new(),
            continues: HashMap::new(),
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
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SwitchCases {
    pub cases: Vec<StmtRef>,
    pub default: Option<StmtRef>,
}
