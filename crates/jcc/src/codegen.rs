use crate::parser;

use tacky::{source_file::SourceSpan, FnDef, Instr, Program, UnaryOp, Value};

// ---------------------------------------------------------------------------
// TackyBuilder
// ---------------------------------------------------------------------------

pub struct TackyBuilder<'a> {
    ast: &'a parser::Ast,
}

impl<'a> TackyBuilder<'a> {
    pub fn new(ast: &'a parser::Ast) -> Self {
        Self { ast }
    }

    pub fn build(self) -> Program {
        Program(self.build_from_item(&self.ast.items()[0]))
    }

    fn build_from_item(&self, item: &parser::Item) -> FnDef {
        FnDefBuilder::new(self.ast).build(item)
    }
}

// ---------------------------------------------------------------------------
// FnDefBuilder
// ---------------------------------------------------------------------------

struct FnDefBuilder<'a> {
    ast: &'a parser::Ast,
    instrs: Vec<Instr>,
    instrs_span: Vec<SourceSpan>,
}

impl<'a> FnDefBuilder<'a> {
    fn new(ast: &'a parser::Ast) -> Self {
        Self {
            ast,
            instrs: Vec::new(),
            instrs_span: Vec::new(),
        }
    }

    fn build(mut self, item: &parser::Item) -> FnDef {
        self.build_from_stmt(item.body);
        FnDef {
            span: item.span,
            id: 0,
            instrs: self.instrs,
            instrs_span: self.instrs_span,
        }
    }

    fn build_from_stmt(&mut self, stmt: parser::StmtRef) {
        self.instrs_span.push(*self.ast.get_stmt_span(stmt));
        let stmt = self.ast.get_stmt(stmt);
        match stmt {
            parser::Stmt::Return(expr) => {
                let value = self.build_from_expr(*expr);
                self.instrs.push(Instr::Ret(value));
            }
        }
    }

    fn build_from_expr(&mut self, expr: parser::ExprRef) -> Value {
        let expr = self.ast.get_expr(expr);
        match expr {
            parser::Expr::Constant(value) => Value::Constant(*value),
            parser::Expr::Variable(_) => todo!("handle named variables"),
            parser::Expr::Grouped(mut inner) => {
                while let parser::Expr::Grouped(expr) = self.ast.get_expr(inner) {
                    inner = *expr
                }
                self.build_from_expr(inner)
            }
            parser::Expr::Unary { op, expr } => {
                let src = self.build_from_expr(*expr);
                self.instrs.push(Instr::Unary {
                    op: UnaryOp::from(*op),
                    src,
                    dst: Value::Variable(0),
                });
                Value::Variable(0)
            }
        }
    }
}
