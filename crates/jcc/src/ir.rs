use crate::parser;

use source_file::SourceSpan;
use string_interner::DefaultSymbol;

// ---------------------------------------------------------------------------
// IrBuilder
// ---------------------------------------------------------------------------

pub struct IrBuilder<'a> {
    ast: &'a parser::Ast,
}

impl<'a> IrBuilder<'a> {
    pub fn new(ast: &'a parser::Ast) -> Self {
        Self { ast }
    }

    pub fn build(self) -> Program {
        Program(self.build_from_fn_def(&self.ast.items[0]))
    }

    fn build_from_fn_def(&self, fn_def: &parser::FnDef) -> FnDef {
        FnDef {
            span: fn_def.span,
            name: fn_def.name,
            body: self.build_from_stmt(fn_def.body),
        }
    }

    fn build_from_stmt(&self, stmt: parser::StmtRef) -> Vec<Instr> {
        let stmt = self.ast.get_stmt(stmt);
        match stmt.kind {
            parser::StmtKind::Return(expr) => vec![
                Instr::Mov {
                    src: self.build_from_expr(expr),
                    dst: Oper::Reg,
                },
                Instr::Ret,
            ],
        }
    }

    fn build_from_expr(&self, expr: parser::ExprRef) -> Oper {
        let expr = self.ast.get_expr(expr);
        match expr.kind {
            parser::ExprKind::Constant(value) => Oper::Imm(value),
            parser::ExprKind::Variable(_) => todo!(),
            parser::ExprKind::Grouped(_) => todo!(),
            parser::ExprKind::Unary { .. } => todo!(),
        }
    }
}

// ---------------------------------------------------------------------------
// IR
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct Program(pub FnDef);

#[derive(Debug)]
pub struct FnDef {
    pub span: SourceSpan,
    pub name: DefaultSymbol,
    pub body: Vec<Instr>,
}

#[derive(Debug)]
pub enum Instr {
    Ret,
    Mov { src: Oper, dst: Oper },
}

#[derive(Debug)]
pub enum Oper {
    Reg,
    Imm(u32),
}
