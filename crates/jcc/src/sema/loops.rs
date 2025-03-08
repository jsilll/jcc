use tacky::source_file::{diag::Diagnostic, SourceSpan};

use crate::parse::{Ast, BlockItem, Stmt, StmtRef};

// ---------------------------------------------------------------------------
// LooperResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LooperResult {
    pub diagnostics: Vec<LooperDiagnostic>,
}

// ---------------------------------------------------------------------------
// LooperDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct LooperDiagnostic {
    pub span: SourceSpan,
    pub kind: LooperDiagnosticKind,
}

// ---------------------------------------------------------------------------
// LooperPass
// ---------------------------------------------------------------------------

pub struct LooperPass {
    loops: Vec<StmtRef>,
    result: LooperResult,
}

impl LooperPass {
    pub fn new() -> Self {
        Self {
            loops: Vec::new(),
            result: LooperResult::default(),
        }
    }

    pub fn analyze(mut self, ast: &mut Ast) -> LooperResult {
        ast.items_iter_refs().for_each(|item| {
            ast.get_block_items(ast.get_item(item).body)
                .to_owned()
                .into_iter()
                .for_each(|block_item| match block_item {
                    BlockItem::Decl(_) => {}
                    BlockItem::Stmt(stmt) => self.analyze_stmt(ast, stmt),
                });
        });
        self.result
    }

    fn analyze_stmt(&mut self, ast: &mut Ast, stmt: StmtRef) {
        match ast.get_stmt_mut(stmt) {
            Stmt::Break(inner) | Stmt::Continue(inner) => {
                match self.loops.last() {
                    Some(loop_stmt) => *inner = Some(*loop_stmt),
                    None => {
                        self.result.diagnostics.push(LooperDiagnostic {
                            span: *ast.get_stmt_span(stmt),
                            kind: LooperDiagnosticKind::UndefinedLoop,
                        });
                    }
                }
                return;
            }
            _ => {}
        }
        match ast.get_stmt(stmt).clone() {
            Stmt::Break(_) | Stmt::Continue(_) => unreachable!(),
            Stmt::Empty | Stmt::Goto(_) | Stmt::Expr(_) | Stmt::Return(_) => {}
            Stmt::Default(_) => unimplemented!(),
            Stmt::Label { stmt, .. } => self.analyze_stmt(ast, stmt),
            Stmt::Compound(stmt) => {
                ast.get_block_items(stmt)
                    .to_owned()
                    .into_iter()
                    .for_each(|block_item| match block_item {
                        BlockItem::Decl(_) => {}
                        BlockItem::Stmt(stmt) => self.analyze_stmt(ast, stmt),
                    });
            }
            Stmt::Case { .. } => unimplemented!(),
            Stmt::Switch { .. } => unimplemented!(),
            Stmt::If {
                then, otherwise, ..
            } => {
                self.analyze_stmt(ast, then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(ast, otherwise);
                }
            }
            Stmt::While { body, .. } => {
                self.loops.push(stmt);
                self.analyze_stmt(ast, body);
                self.loops.pop();
            }
            Stmt::DoWhile { body, .. } => {
                self.loops.push(stmt);
                self.analyze_stmt(ast, body);
                self.loops.pop();
            }
            Stmt::For { body, .. } => {
                self.loops.push(stmt);
                self.analyze_stmt(ast, body);
                self.loops.pop();
            }
        }
    }
}

// ---------------------------------------------------------------------------
// LooperDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum LooperDiagnosticKind {
    UndefinedLoop,
}

impl From<LooperDiagnostic> for Diagnostic {
    fn from(diagnostic: LooperDiagnostic) -> Self {
        match diagnostic.kind {
            LooperDiagnosticKind::UndefinedLoop => Diagnostic::error(
                diagnostic.span,
                "undefined loop",
                "this statement is not inside a loop",
            ),
        }
    }
}
