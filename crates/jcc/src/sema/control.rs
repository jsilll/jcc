use crate::{
    ast::{Ast, BlockItem, DeclKind, Stmt, StmtKind},
    sema::SemaCtx,
};

use jcc_ssa::{
    codemap::{file::FileId, span::Span, Diagnostic, Label},
    IdentId,
};

use std::{cell::Cell, collections::HashMap};

// ---------------------------------------------------------------------------
// ControlPass
// ---------------------------------------------------------------------------

pub struct ControlPass<'a, 'ctx> {
    /// The AST being analyzed
    ast: &'a Ast<'ctx>,
    /// The semantic analysis context
    ctx: &'a mut SemaCtx<'ctx>,
    /// The result of the control flow analysis
    result: ControlResult,
    /// The stack of currently tracked statements
    tracked_stmts: Vec<TrackedStmt>,
    /// The currently tracked labels
    tracked_labels: HashMap<IdentId, TrackedLabel<'a>>,
}

impl<'a, 'ctx> ControlPass<'a, 'ctx> {
    pub fn new(ast: &'a Ast<'ctx>, ctx: &'a mut SemaCtx<'ctx>) -> Self {
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
            .root
            .iter()
            .for_each(|decl| match self.ast.decl[*decl].kind {
                DeclKind::Var(_) => {}
                DeclKind::Func { body, .. } => {
                    if let Some(body) = body {
                        self.ast.items[body].iter().for_each(|block_item| {
                            if let BlockItem::Stmt(stmt) = block_item {
                                self.visit_stmt(*stmt)
                            }
                        });
                        self.tracked_labels.values().for_each(|e| {
                            if let TrackedLabel::Unresolved(v) = e {
                                v.iter().for_each(|(_, span)| {
                                    self.result.diagnostics.push(ControlDiagnostic {
                                        span: *span,
                                        file: self.ast.file,
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

    fn visit_stmt(&mut self, stmt: Stmt) {
        let data = &self.ast.stmt[stmt];
        match &data.kind {
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
                self.tracked_stmts.push(TrackedStmt::Loop(stmt));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::DoWhile { body, .. } => {
                self.tracked_stmts.push(TrackedStmt::Loop(stmt));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::For { body, .. } => {
                self.tracked_stmts.push(TrackedStmt::Loop(stmt));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::Switch { body, .. } => {
                self.tracked_stmts.push(TrackedStmt::Switch(stmt));
                self.visit_stmt(*body);
                self.tracked_stmts.pop();
            }
            StmtKind::Compound(stmt) => {
                self.ast.items[*stmt]
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
                    .or_insert(TrackedLabel::Unresolved(vec![(target, data.span)]));
                if let TrackedLabel::Resolved(stmt) = entry {
                    target.set(Some(*stmt));
                }
            }
            StmtKind::Break(target) => match self.tracked_stmts.last() {
                Some(TrackedStmt::Loop(stmt)) | Some(TrackedStmt::Switch(stmt)) => {
                    target.set(Some(*stmt))
                }
                None => self.result.diagnostics.push(ControlDiagnostic {
                    span: data.span,
                    file: self.ast.file,
                    kind: ControlDiagnosticKind::UndefinedLoopOrSwitch,
                }),
            },
            StmtKind::Continue(target) => {
                match self.tracked_stmts.iter().rev().find_map(|stmt| match stmt {
                    TrackedStmt::Loop(stmt) => Some(*stmt),
                    _ => None,
                }) {
                    Some(stmt) => {
                        target.set(Some(stmt));
                    }
                    None => self.result.diagnostics.push(ControlDiagnostic {
                        span: data.span,
                        file: self.ast.file,
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
                        .push(stmt),
                    _ => self.result.diagnostics.push(ControlDiagnostic {
                        span: data.span,
                        file: self.ast.file,
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
                            None => switch.default = Some(stmt),
                            Some(_) => self.result.diagnostics.push(ControlDiagnostic {
                                span: data.span,
                                file: self.ast.file,
                                kind: ControlDiagnosticKind::DuplicateDefault,
                            }),
                        }
                    }
                    _ => self.result.diagnostics.push(ControlDiagnostic {
                        span: data.span,
                        file: self.ast.file,
                        kind: ControlDiagnosticKind::CaseOutsideSwitch,
                    }),
                }
                self.visit_stmt(*inner);
            }
            StmtKind::Label { label, stmt: inner } => {
                let entry = self
                    .tracked_labels
                    .entry(*label)
                    .or_insert(TrackedLabel::Resolved(stmt));
                match entry {
                    TrackedLabel::Unresolved(v) => {
                        v.iter().for_each(|(target, _)| {
                            target.set(Some(stmt));
                        });
                        *entry = TrackedLabel::Resolved(stmt);
                    }
                    TrackedLabel::Resolved(s) => {
                        if stmt != *s {
                            self.result.diagnostics.push(ControlDiagnostic {
                                span: data.span,
                                file: self.ast.file,
                                kind: ControlDiagnosticKind::RedeclaredLabel,
                            });
                        }
                    }
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
    Loop(Stmt),
    /// A switch statement
    Switch(Stmt),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrackedLabel<'a> {
    /// A resolved label
    Resolved(Stmt),
    /// An unresolved label
    Unresolved(Vec<(&'a Cell<Option<Stmt>>, Span)>),
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
            ControlDiagnosticKind::UndefinedLoop => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("break/continue outside of loop"),
                )
                .with_note("break and continue statements can only be used inside for, while, or do-while loops"),
            ControlDiagnosticKind::UndefinedLabel => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("label not found"),
                )
                .with_note("no label with this name exists in the current function"),
            ControlDiagnosticKind::RedeclaredLabel => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("duplicate label"),
                )
                .with_note("a label with this name already exists in the current function; labels must be unique within a function"),
            ControlDiagnosticKind::DuplicateDefault => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("duplicate default case"),
                )
                .with_note("switch statements can only have one default case"),
            ControlDiagnosticKind::CaseOutsideSwitch => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("case label outside of switch"),
                )
                .with_note("case labels can only appear inside switch statements"),
            ControlDiagnosticKind::UndefinedLoopOrSwitch => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("break outside of loop or switch"),
                )
                .with_note("break statements can only be used inside loops (for, while, do-while) or switch statements"),
        }
    }
}

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
    pub span: Span,
    pub file: FileId,
    pub kind: ControlDiagnosticKind,
}
