use crate::{
    ast::{Ast, BlockItem, DeclKind, StmtKind, StmtRef},
    sema::SemaCtx,
};

use jcc_ssa::{
    interner::Symbol,
    sourcemap::{diag::Diagnostic, SourceSpan},
};

use std::collections::{HashMap, HashSet};

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

pub struct ControlPass<'ctx> {
    ctx: &'ctx mut SemaCtx,
    result: ControlResult,
    tracked: Vec<TrackedStmt>,
    defined_labels: HashSet<Symbol>,
    unresolved_labels: HashMap<Symbol, Vec<SourceSpan>>,
}

impl<'ctx> ControlPass<'ctx> {
    pub fn new(ctx: &'ctx mut SemaCtx) -> Self {
        Self {
            ctx,
            tracked: Vec::new(),
            result: ControlResult::default(),
            defined_labels: HashSet::default(),
            unresolved_labels: HashMap::default(),
        }
    }

    pub fn check(mut self, ast: &Ast) -> ControlResult {
        ast.root()
            .iter()
            .for_each(|decl| match ast.decl(*decl).kind {
                DeclKind::Var(_) => {}
                DeclKind::Func { body, .. } => {
                    if let Some(body) = body {
                        ast.block_items(body).iter().for_each(|block_item| {
                            if let BlockItem::Stmt(stmt) = block_item {
                                self.visit_stmt(ast, *stmt)
                            }
                        });

                        self.unresolved_labels.values().for_each(|spans| {
                            spans.iter().for_each(|span| {
                                self.result.diagnostics.push(ControlDiagnostic {
                                    span: *span,
                                    kind: ControlDiagnosticKind::UndefinedLabel,
                                });
                            });
                        });

                        self.tracked.clear();
                        self.defined_labels.clear();
                        self.unresolved_labels.clear();
                    }
                }
            });

        self.result
    }

    fn visit_stmt(&mut self, ast: &Ast, stmt_ref: StmtRef) {
        let stmt = ast.stmt(stmt_ref);
        match &stmt.kind {
            StmtKind::Empty | StmtKind::Expr(_) | StmtKind::Return(_) => {}
            StmtKind::If {
                then, otherwise, ..
            } => {
                self.visit_stmt(ast, *then);
                if let Some(otherwise) = otherwise {
                    self.visit_stmt(ast, *otherwise);
                }
            }
            StmtKind::While { body, .. } => {
                self.tracked.push(TrackedStmt::Loop(stmt_ref));
                self.visit_stmt(ast, *body);
                self.tracked.pop();
            }
            StmtKind::DoWhile { body, .. } => {
                self.tracked.push(TrackedStmt::Loop(stmt_ref));
                self.visit_stmt(ast, *body);
                self.tracked.pop();
            }
            StmtKind::For { body, .. } => {
                self.tracked.push(TrackedStmt::Loop(stmt_ref));
                self.visit_stmt(ast, *body);
                self.tracked.pop();
            }
            StmtKind::Switch { body, .. } => {
                self.tracked.push(TrackedStmt::Switch(stmt_ref));
                self.visit_stmt(ast, *body);
                self.tracked.pop();
            }
            StmtKind::Goto(label) => {
                if !self.defined_labels.contains(label) {
                    self.unresolved_labels
                        .entry(*label)
                        .or_default()
                        .push(stmt.span);
                }
            }
            StmtKind::Label { label, stmt: inner } => {
                if !self.defined_labels.insert(*label) {
                    self.result.diagnostics.push(ControlDiagnostic {
                        span: stmt.span,
                        kind: ControlDiagnosticKind::RedeclaredLabel,
                    });
                }
                self.unresolved_labels.remove(label);
                self.visit_stmt(ast, *inner);
            }
            StmtKind::Compound(stmt) => {
                ast.block_items(*stmt)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(_) => {}
                        BlockItem::Stmt(stmt) => self.visit_stmt(ast, *stmt),
                    });
            }
            StmtKind::Break => match self.tracked.last() {
                Some(TrackedStmt::Loop(loop_stmt)) => {
                    self.ctx.breaks.insert(stmt_ref, *loop_stmt);
                }
                Some(TrackedStmt::Switch(switch_stmt)) => {
                    self.ctx.breaks.insert(stmt_ref, *switch_stmt);
                }
                _ => self.result.diagnostics.push(ControlDiagnostic {
                    span: stmt.span,
                    kind: ControlDiagnosticKind::UndefinedLoopOrSwitch,
                }),
            },
            StmtKind::Continue => {
                match self.tracked.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Loop(loop_stmt) => Some(*loop_stmt),
                    _ => None,
                }) {
                    Some(loop_stmt) => {
                        self.ctx.continues.insert(stmt_ref, loop_stmt);
                    }
                    None => self.result.diagnostics.push(ControlDiagnostic {
                        span: stmt.span,
                        kind: ControlDiagnosticKind::UndefinedLoop,
                    }),
                }
            }
            StmtKind::Case { stmt: inner, .. } => {
                match self.tracked.iter().rev().find_map(|stmt| match stmt {
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
                self.visit_stmt(ast, *inner);
            }
            StmtKind::Default(inner) => {
                match self.tracked.iter().rev().find_map(|stmt| match stmt {
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
                self.visit_stmt(ast, *inner);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrackedStmt {
    Loop(StmtRef),
    Switch(StmtRef),
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
