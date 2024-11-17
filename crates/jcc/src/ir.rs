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
        Program(self.visit_fn_def(&self.program.0))
    }

    fn visit_fn_def(&self, fn_def: &parser::FnDef) -> FnDef {
        FnDef {
            span: fn_def.span,
            name: fn_def.name,
            body: self.visit_stmt(&fn_def.body),
        }
    }

    fn visit_stmt(&self, stmt: &parser::Stmt) -> Vec<Instr> {
        let mut instrs = Vec::new();
        match stmt {
            parser::Stmt::Return(expr) => {
                instrs.push(Instr::Mov {
                    src: self.visit_expr(expr),
                    dst: Oper::Reg,
                });
                instrs.push(Instr::Ret);
            }
        }
        instrs
    }

    fn visit_expr(&self, expr: &parser::Expr) -> Oper {
        match expr {
            parser::Expr::Constant { span: _, value } => Oper::Imm(*value),
        }
    }
}

// ---------------------------------------------------------------------------
// AST
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
