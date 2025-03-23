use crate::{
    parse::{Ast, BlockItem, Stmt, StmtRef},
    sema::SemaCtx,
};

use tacky::{
    source_file::{diag::Diagnostic, SourceSpan},
    string_interner::DefaultSymbol,
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
    stmts: Vec<TrackedStmt>,
    defined_labels: HashSet<DefaultSymbol>,
    unresolved_labels: HashMap<DefaultSymbol, Vec<SourceSpan>>,
}

impl<'ctx> ControlPass<'ctx> {
    pub fn new(ctx: &'ctx mut SemaCtx) -> Self {
        Self {
            ctx,
            stmts: Vec::new(),
            result: ControlResult::default(),
            defined_labels: HashSet::default(),
            unresolved_labels: HashMap::default(),
        }
    }

    pub fn analyze(mut self, ast: &mut Ast) -> ControlResult {
        ast.item_iter().for_each(|item| {
            ast.block_items(ast.item(item).body)
                .to_owned()
                .into_iter()
                .for_each(|block_item| match block_item {
                    BlockItem::Decl(_) => {}
                    BlockItem::Stmt(stmt) => self.analyze_stmt(ast, stmt),
                });
        });
        self.unresolved_labels.values().for_each(|spans| {
            spans.iter().for_each(|span| {
                self.result.diagnostics.push(ControlDiagnostic {
                    span: *span,
                    kind: ControlDiagnosticKind::UndefinedLabel,
                });
            });
        });
        self.result
    }

    fn analyze_stmt(&mut self, ast: &mut Ast, stmt: StmtRef) {
        match ast.stmt_mut(stmt) {
            Stmt::Break(inner) => {
                match self.stmts.last() {
                    Some(TrackedStmt::Loop(loop_stmt)) => *inner = Some(*loop_stmt),
                    Some(TrackedStmt::Switch(switch_stmt)) => *inner = Some(*switch_stmt),
                    _ => self.result.diagnostics.push(ControlDiagnostic {
                        span: *ast.stmt_span(stmt),
                        kind: ControlDiagnosticKind::UndefinedLoopOrSwitch,
                    }),
                }
                return;
            }
            Stmt::Continue(inner) => {
                match self.stmts.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Loop(loop_stmt) => Some(*loop_stmt),
                    _ => None,
                }) {
                    Some(loop_stmt) => *inner = Some(loop_stmt),
                    None => self.result.diagnostics.push(ControlDiagnostic {
                        span: *ast.stmt_span(stmt),
                        kind: ControlDiagnosticKind::UndefinedLoop,
                    }),
                }
                return;
            }
            _ => {}
        }
        match ast.stmt(stmt).clone() {
            Stmt::Empty | Stmt::Expr(_) | Stmt::Return(_) => {}
            Stmt::Break(_) | Stmt::Continue(_) => unreachable!(),
            Stmt::Default(inner) => {
                match self.stmts.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Switch(stmt) => Some(stmt),
                    _ => None,
                }) {
                    Some(switch_stmt) => {
                        let switch = self.ctx.switches.entry(*switch_stmt).or_default();
                        match switch.default {
                            None => switch.default = Some(stmt),
                            Some(_) => self.result.diagnostics.push(ControlDiagnostic {
                                span: *ast.stmt_span(stmt),
                                kind: ControlDiagnosticKind::DuplicateDefault,
                            }),
                        }
                    }
                    _ => self.result.diagnostics.push(ControlDiagnostic {
                        span: *ast.stmt_span(stmt),
                        kind: ControlDiagnosticKind::CaseOutsideSwitch,
                    }),
                }
                self.analyze_stmt(ast, inner);
            }
            Stmt::Case { stmt: inner, .. } => {
                match self.stmts.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Switch(stmt) => Some(stmt),
                    _ => None,
                }) {
                    Some(switch_stmt) => self
                        .ctx
                        .switches
                        .entry(*switch_stmt)
                        .or_default()
                        .cases
                        .push(stmt),
                    _ => self.result.diagnostics.push(ControlDiagnostic {
                        span: *ast.stmt_span(stmt),
                        kind: ControlDiagnosticKind::CaseOutsideSwitch,
                    }),
                }
                self.analyze_stmt(ast, inner);
            }
            Stmt::Goto(label) => {
                if !self.defined_labels.contains(&label) {
                    self.unresolved_labels
                        .entry(label)
                        .or_insert_with(Vec::new)
                        .push(*ast.stmt_span(stmt));
                }
            }
            Stmt::Label { label, stmt: inner } => {
                if !self.defined_labels.insert(label) {
                    self.result.diagnostics.push(ControlDiagnostic {
                        span: *ast.stmt_span(stmt),
                        kind: ControlDiagnosticKind::RedeclaredLabel,
                    });
                }
                self.unresolved_labels.remove(&label);
                self.analyze_stmt(ast, inner);
            }
            Stmt::Compound(stmt) => {
                ast.block_items(stmt)
                    .to_owned()
                    .into_iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(_) => {}
                        BlockItem::Stmt(stmt) => self.analyze_stmt(ast, stmt),
                    });
            }
            Stmt::Switch { body, .. } => {
                self.stmts.push(TrackedStmt::Switch(stmt));
                self.analyze_stmt(ast, body);
                self.stmts.pop();
            }
            Stmt::If {
                then, otherwise, ..
            } => {
                self.analyze_stmt(ast, then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(ast, otherwise);
                }
            }
            Stmt::While { body, .. } => {
                self.stmts.push(TrackedStmt::Loop(stmt));
                self.analyze_stmt(ast, body);
                self.stmts.pop();
            }
            Stmt::DoWhile { body, .. } => {
                self.stmts.push(TrackedStmt::Loop(stmt));
                self.analyze_stmt(ast, body);
                self.stmts.pop();
            }
            Stmt::For { body, .. } => {
                self.stmts.push(TrackedStmt::Loop(stmt));
                self.analyze_stmt(ast, body);
                self.stmts.pop();
            }
        }
    }
}

// ---------------------------------------------------------------------------
// TrackedStmt
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
