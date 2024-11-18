use crate::parser;

use source_file::SourceSpan;
use string_interner::DefaultSymbol;

// ---------------------------------------------------------------------------
// IrBuilder
// ---------------------------------------------------------------------------

pub struct IrBuilder<'a> {
    program: &'a parser::Program,
}

impl<'a> IrBuilder<'a> {
    pub fn new(program: &'a parser::Program) -> Self {
        Self { program }
    }

    pub fn build(self) -> Program {
        Program(self.build_from_fn_def(&self.program.0))
    }

    fn build_from_fn_def(&self, fn_def: &parser::FnDef) -> FnDef {
        FnDef {
            span: fn_def.span,
            name: fn_def.name,
            body: self.build_from_stmt(&fn_def.body),
        }
    }

    fn build_from_stmt(&self, stmt: &parser::Stmt) -> Vec<Instr> {
        match stmt {
            parser::Stmt::Return(expr) => vec![
                Instr::Mov {
                    src: self.build_from_expr(expr),
                    dst: Oper::Reg,
                },
                Instr::Ret,
            ],
        }
    }

    fn build_from_expr(&self, expr: &parser::Expr) -> Oper {
        match expr {
            parser::Expr::Constant { value, .. } => Oper::Imm(*value),
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
