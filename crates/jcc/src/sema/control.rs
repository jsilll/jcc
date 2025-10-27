use crate::{
    ast::{Ast, BlockItem, DeclKind, StmtKind, StmtRef},
    sema::SemaCtx,
};

use jcc_ssa::{
    interner::Symbol,
    sourcemap::{diag::Diagnostic, SourceSpan},
};

use std::{cell::Cell, collections::HashMap};

// ---------------------------------------------------------------------------
// ControlResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct ControlResult {
    pub diagnostics: Vec<ControlDiagnostic>,
}

// ---------------------------------------------------------------------------
// ControlDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct ControlDiagnostic {
    pub span: SourceSpan,
    pub kind: ControlDiagnosticKind,
}

// ---------------------------------------------------------------------------
// ControlPass
// ---------------------------------------------------------------------------

pub struct ControlPass<'a> {
    ast: &'a Ast,
    ctx: &'a mut SemaCtx,
    result: ControlResult,
    tracked_stmts: Vec<TrackedStmt>,
    tracked_labels: HashMap<Symbol, TrackedLabel<'a>>,
}

impl<'a> ControlPass<'a> {
    pub fn new(ast: &'a Ast, ctx: &'a mut SemaCtx) -> Self {
        Self {
            ast,
            ctx,
            tracked_stmts: Vec::new(),
            result: ControlResult::default(),
            tracked_labels: HashMap::default(),
        }
    }

    pub fn check(mut self) -> ControlResult {
        self.ast
            .root()
            .iter()
            .for_each(|decl| match self.ast.decl(*decl).kind {
                DeclKind::Var(_) => {}
                DeclKind::Func { body, .. } => {
                    if let Some(body) = body {
                        self.ast.items(body).iter().for_each(|block_item| {
                            if let BlockItem::Stmt(stmt) = block_item {
                                self.visit_stmt(*stmt)
                            }
                        });
                        self.tracked_labels.values().for_each(|e| {
                            if let TrackedLabel::Unresolved(v) = e {
                                v.iter().for_each(|(_, span)| {
                                    self.result.diagnostics.push(ControlDiagnostic {
                                        span: *span,
                                        kind: ControlDiagnosticKind::UndefinedLabel,
                                    });
                                });
                            }
                        });
                        self.tracked_stmts.clear();
                        self.tracked_labels.clear();
                    }
                }
            });
        self.result
    }

    fn visit_stmt(&mut self, stmt_ref: StmtRef) {
        let stmt = self.ast.stmt(stmt_ref);
        match &stmt.kind {
            StmtKind::Empty | StmtKind::Expr(_) | StmtKind::Return(_) => {}
            StmtKind::If {
                then, otherwise, ..
            } => {
                self.visit_stmt(*then);
                if let Some(otherwise) = otherwise {
                    self.visit_stmt(*otherwise);
                }
            }
            StmtKind::While { body, .. } => {
                self.tracked_stmts.push(TrackedStmt::Loop(stmt_ref));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::DoWhile { body, .. } => {
                self.tracked_stmts.push(TrackedStmt::Loop(stmt_ref));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::For { body, .. } => {
                self.tracked_stmts.push(TrackedStmt::Loop(stmt_ref));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::Switch { body, .. } => {
                self.tracked_stmts.push(TrackedStmt::Switch(stmt_ref));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::Compound(stmt) => {
                self.ast
                    .items(*stmt)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(_) => {}
                        BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                    });
            }
            StmtKind::Goto {
                label,
                stmt: target,
            } => {
                let entry = self
                    .tracked_labels
                    .entry(*label)
                    .or_insert(TrackedLabel::Unresolved(vec![(target, stmt.span)]));
                if let TrackedLabel::Resolved(stmt) = entry {
                    target.set(*stmt);
                }
            }
            StmtKind::Label { label, stmt: inner } => {
                let entry = self
                    .tracked_labels
                    .entry(*label)
                    .or_insert(TrackedLabel::Resolved(stmt_ref));
                match entry {
                    TrackedLabel::Unresolved(v) => {
                        v.iter().for_each(|(target, _)| {
                            target.set(stmt_ref);
                        });
                        *entry = TrackedLabel::Resolved(stmt_ref);
                    }
                    TrackedLabel::Resolved(s) => {
                        if stmt_ref != *s {
                            self.result.diagnostics.push(ControlDiagnostic {
                                span: stmt.span,
                                kind: ControlDiagnosticKind::RedeclaredLabel,
                            });
                        }
                    }
                }
                self.visit_stmt(*inner);
            }
            StmtKind::Break(target) => match self.tracked_stmts.last() {
                Some(TrackedStmt::Loop(stmt)) | Some(TrackedStmt::Switch(stmt)) => {
                    target.set(*stmt)
                }
                None => self.result.diagnostics.push(ControlDiagnostic {
                    span: stmt.span,
                    kind: ControlDiagnosticKind::UndefinedLoopOrSwitch,
                }),
            },
            StmtKind::Continue(target) => {
                match self.tracked_stmts.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Loop(stmt) => Some(*stmt),
                    _ => None,
                }) {
                    Some(stmt) => {
                        target.set(stmt);
                    }
                    None => self.result.diagnostics.push(ControlDiagnostic {
                        span: stmt.span,
                        kind: ControlDiagnosticKind::UndefinedLoop,
                    }),
                }
            }
            StmtKind::Case { stmt: inner, .. } => {
                match self.tracked_stmts.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Switch(stmt) => Some(stmt),
                    _ => None,
                }) {
                    Some(switch_stmt) => self
                        .ctx
                        .switches
                        .entry(*switch_stmt)
                        .or_default()
                        .cases
                        .push(stmt_ref),
                    _ => self.result.diagnostics.push(ControlDiagnostic {
                        span: stmt.span,
                        kind: ControlDiagnosticKind::CaseOutsideSwitch,
                    }),
                }
                self.visit_stmt(*inner);
            }
            StmtKind::Default(inner) => {
                match self.tracked_stmts.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Switch(stmt) => Some(stmt),
                    _ => None,
                }) {
                    Some(switch_stmt) => {
                        let switch = self.ctx.switches.entry(*switch_stmt).or_default();
                        match switch.default {
                            None => switch.default = Some(stmt_ref),
                            Some(_) => self.result.diagnostics.push(ControlDiagnostic {
                                span: stmt.span,
                                kind: ControlDiagnosticKind::DuplicateDefault,
                            }),
                        }
                    }
                    _ => self.result.diagnostics.push(ControlDiagnostic {
                        span: stmt.span,
                        kind: ControlDiagnosticKind::CaseOutsideSwitch,
                    }),
                }
                self.visit_stmt(*inner);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrackedStmt {
    /// A loop statement
    Loop(StmtRef),
    /// A switch statement
    Switch(StmtRef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrackedLabel<'a> {
    /// A resolved label
    Resolved(StmtRef),
    /// An unresolved label
    Unresolved(Vec<(&'a Cell<StmtRef>, SourceSpan)>),
}

// ---------------------------------------------------------------------------
// ControlDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum ControlDiagnosticKind {
    UndefinedLoop,
    UndefinedLabel,
    RedeclaredLabel,
    DuplicateDefault,
    CaseOutsideSwitch,
    UndefinedLoopOrSwitch,
}

impl From<ControlDiagnostic> for Diagnostic {
    fn from(diagnostic: ControlDiagnostic) -> Self {
        match diagnostic.kind {
            ControlDiagnosticKind::UndefinedLoop => Diagnostic::error(
                diagnostic.span,
                "undefined loop",
                "this statement is not inside a loop",
            ),
            ControlDiagnosticKind::UndefinedLabel => Diagnostic::error(
                diagnostic.span,
                "undefined label",
                "this label has not been declared",
            ),
            ControlDiagnosticKind::RedeclaredLabel => Diagnostic::error(
                diagnostic.span,
                "redeclared label",
                "this label has already been declared",
            ),
            ControlDiagnosticKind::DuplicateDefault => Diagnostic::error(
                diagnostic.span,
                "duplicate default",
                "this switch statement already has a default case",
            ),
            ControlDiagnosticKind::CaseOutsideSwitch => Diagnostic::error(
                diagnostic.span,
                "case outside switch",
                "this case statement is not inside a switch statement",
            ),
            ControlDiagnosticKind::UndefinedLoopOrSwitch => Diagnostic::error(
                diagnostic.span,
                "undefined loop or switch",
                "this statement is not inside a loop or switch",
            ),
        }
    }
}
