use crate::parse::{Ast, BlockItem, Stmt, StmtRef};

use tacky::{
    source_file::{diag::Diagnostic, SourceSpan},
    string_interner::DefaultSymbol,
};

use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
// LabelerResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LabelerResult {
    pub diagnostics: Vec<LabelerDiagnostic>,
}

// ---------------------------------------------------------------------------
// LabelerDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct LabelerDiagnostic {
    pub span: SourceSpan,
    pub kind: LabelerDiagnosticKind,
}

// ---------------------------------------------------------------------------
// LabelerPass
// ---------------------------------------------------------------------------

pub struct LabelerPass {
    result: LabelerResult,
    defined: HashSet<DefaultSymbol>,
    unresolved: HashMap<DefaultSymbol, SourceSpan>,
}

impl LabelerPass {
    pub fn new() -> Self {
        Self {
            defined: HashSet::default(),
            unresolved: HashMap::default(),
            result: LabelerResult::default(),
        }
    }

    pub fn analyze(mut self, ast: &Ast) -> LabelerResult {
        ast.items().iter().for_each(|item| {
            ast.get_block_items(item.body)
                .iter()
                .for_each(|block_item| match block_item {
                    BlockItem::Stmt(stmt) => self.analyze_stmt(ast, *stmt),
                    _ => {}
                });
        });

        self.unresolved.values().for_each(|span| {
            self.result.diagnostics.push(LabelerDiagnostic {
                span: *span,
                kind: LabelerDiagnosticKind::UndefinedLabel,
            });
        });

        self.result
    }

    fn analyze_stmt(&mut self, ast: &Ast, stmt: StmtRef) {
        match ast.get_stmt(stmt) {
            Stmt::Empty | Stmt::Expr(_) | Stmt::Return(_) => {}
            Stmt::Break(_) => todo!("handle break statements"),
            Stmt::Continue(_) => todo!("handle continue statements"),
            Stmt::Goto(label) => {
                if !self.defined.contains(&label) {
                    self.unresolved.insert(*label, *ast.get_stmt_span(stmt));
                }
            }
            Stmt::Label { label, stmt: inner } => {
                if !self.defined.insert(*label) {
                    self.result.diagnostics.push(LabelerDiagnostic {
                        span: *ast.get_stmt_span(stmt),
                        kind: LabelerDiagnosticKind::RedeclaredLabel,
                    });
                } else {
                    self.unresolved.remove(label);
                }
                self.analyze_stmt(ast, *inner);
            }
            Stmt::Compound(items) => {
                ast.get_block_items(*items)
                    .iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Stmt(stmt) => self.analyze_stmt(ast, *stmt),
                        _ => {}
                    });
            }
            Stmt::If {
                then, otherwise, ..
            } => {
                self.analyze_stmt(ast, *then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(ast, *otherwise);
                }
            }
            Stmt::While { .. } => todo!("handle while statements"),
            Stmt::DoWhile { .. } => todo!("handle do-while statements"),
            Stmt::For { .. } => todo!("handle for statements"),
        }
    }
}

// ---------------------------------------------------------------------------
// LabelerDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum LabelerDiagnosticKind {
    UndefinedLabel,
    RedeclaredLabel,
}

impl From<LabelerDiagnostic> for Diagnostic {
    fn from(diagnostic: LabelerDiagnostic) -> Self {
        match diagnostic.kind {
            LabelerDiagnosticKind::UndefinedLabel => Diagnostic::error(
                diagnostic.span,
                "undefined label",
                "label is not defined in the current scope",
            ),
            LabelerDiagnosticKind::RedeclaredLabel => Diagnostic::error(
                diagnostic.span,
                "redeclared label",
                "label is already defined in the current scope",
            ),
        }
    }
}
