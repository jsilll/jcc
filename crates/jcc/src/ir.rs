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
        Program(self.build_from_item(&self.ast.items()[0]))
    }

    fn build_from_item(&self, item: &parser::Item) -> FnDef {
        FnDef {
            span: item.span,
            name: item.name,
            body: self.build_from_stmt(item.body),
        }
    }

    fn build_from_stmt(&self, stmt: parser::StmtRef) -> Vec<Instr> {
        let stmt = self.ast.get_stmt(stmt);
        match stmt {
            parser::Stmt::Return(expr) => vec![
                Instr::Mov {
                    src: self.build_from_expr(*expr),
                    dst: Oper::Reg,
                },
                Instr::Ret,
            ],
        }
    }

    fn build_from_expr(&self, expr: parser::ExprRef) -> Oper {
        let expr = self.ast.get_expr(expr);
        match expr {
            parser::Expr::Constant(value) => Oper::Imm(*value),
            parser::Expr::Variable(_) => todo!(),
            parser::Expr::Grouped(_) => todo!(),
            parser::Expr::Unary { .. } => todo!(),
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
