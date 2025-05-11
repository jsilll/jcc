pub mod ty;

pub mod control;

pub mod resolve;

use crate::ast::parse::{DeclRef, ExprRef, StmtRef};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// SemaCtx
// ---------------------------------------------------------------------------

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SemaCtx {
    pub vars: HashMap<ExprRef, DeclRef>,
    pub breaks: HashMap<StmtRef, StmtRef>,
    pub continues: HashMap<StmtRef, StmtRef>,
    pub switches: HashMap<StmtRef, SwitchCases>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SwitchCases {
    pub cases: Vec<StmtRef>,
    pub default: Option<StmtRef>,
}
