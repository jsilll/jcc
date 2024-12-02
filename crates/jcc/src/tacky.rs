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
    tmp_count: u32,
    instrs: Vec<Instr>,
    instrs_span: Vec<SourceSpan>,
}

impl<'a> FnDefBuilder<'a> {
    fn new(ast: &'a parser::Ast) -> Self {
        Self {
            ast,
            tmp_count: 0,
            instrs: Vec::new(),
            instrs_span: Vec::new(),
        }
    }

    fn build(mut self, item: &parser::Item) -> FnDef {
        self.build_from_stmt(item.body);
        FnDef {
            id: 0, // TODO: assign a unique ID
            span: item.span,
            instrs: self.instrs,
            instrs_span: self.instrs_span,
        }
    }

    fn build_from_stmt(&mut self, stmt: parser::StmtRef) {
        match self.ast.get_stmt(stmt) {
            parser::Stmt::Return(inner) => {
                let value = self.build_from_expr(*inner);
                self.instrs_span.push(*self.ast.get_stmt_span(stmt));
                self.instrs.push(Instr::Return(value));
            }
        }
    }

    fn build_from_expr(&mut self, expr: parser::ExprRef) -> Value {
        match self.ast.get_expr(expr) {
            parser::Expr::Constant(value) => Value::Constant(*value),
            parser::Expr::Variable(_) => todo!("handle named variables"),
            parser::Expr::Grouped(mut inner) => {
                while let parser::Expr::Grouped(expr) = self.ast.get_expr(inner) {
                    inner = *expr
                }
                self.build_from_expr(inner)
            }
            parser::Expr::Unary { op, expr: inner } => {
                let op = UnaryOp::from(*op);
                let src = self.build_from_expr(*inner);
                let dst = self.make_tmp();
                self.instrs_span.push(*self.ast.get_expr_span(expr));
                self.instrs.push(Instr::Unary { op, src, dst });
                dst
            }
        }
    }

    fn make_tmp(&mut self) -> Value {
        let tmp = Value::Variable(self.tmp_count);
        self.tmp_count += 1;
        tmp
    }
}