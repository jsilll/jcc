pub mod ty;

pub mod control;

pub mod resolve;

use crate::parse::StmtRef;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// SemaCtx
// ---------------------------------------------------------------------------

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SemaCtx {
    pub switches: HashMap<StmtRef, SwitchCases>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SwitchCases {
    pub cases: Vec<StmtRef>,
    pub default: Option<StmtRef>,
}
