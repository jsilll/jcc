use crate::parse;

use tacky::string_interner::DefaultStringInterner;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Root function
// ---------------------------------------------------------------------------

pub fn build<'a>(ast: &parse::Ast, interner: DefaultStringInterner) -> ssa::Program {
    let mut prog = ssa::Program::new(interner);
    ast.items_iter().for_each(|item| {
        SSAFuncBuilder::new(ast, item, &mut prog).build();
    });
    prog
}

// ---------------------------------------------------------------------------
// SSAFuncBuilder
// ---------------------------------------------------------------------------

struct SSAFuncBuilder<'a> {
    ast: &'a parse::Ast,
    item: parse::ItemRef,
    prog: &'a mut ssa::Program,
    // func: ssa::FuncRef,
    block: ssa::BlockRef,
    variables: HashMap<parse::DeclRef, ssa::InstRef>,
}

impl<'a> SSAFuncBuilder<'a> {
    fn new(ast: &'a parse::Ast, item: parse::ItemRef, prog: &'a mut ssa::Program) -> Self {
        let span = *ast.item_span(item);

        let func = prog.new_func_with_span_interned(ast.item(item).name, span);
        let block = prog.new_block_with_span("entry", span);
        prog.func_mut(func).blocks.push(block);

        Self {
            ast,
            // func,
            prog,
            item,
            block,
            variables: HashMap::new(),
        }
    }

    fn build(mut self) {
        self.ast
            .block_items(self.ast.item(self.item).body)
            .iter()
            .for_each(|item| match item {
                parse::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                parse::BlockItem::Decl(decl) => self.visit_decl(*decl),
            });

        let append_return = match self.ast.block_items(self.ast.item(self.item).body).last() {
            Some(parse::BlockItem::Stmt(stmt)) => match self.ast.stmt(*stmt) {
                parse::Stmt::Return(_) => false,
                _ => true,
            },
            _ => true,
        };

        if append_return {
            let span = *self.ast.item_span(self.item);
            let val = self.prog.new_inst_with_span(ssa::Inst::const_i32(0), span);
            let ret = self.prog.new_inst_with_span(ssa::Inst::ret(val), span);
            self.append_slice_to_block(&[val, ret]);
        }
    }

    #[inline]
    fn append_to_block(&mut self, instr: ssa::InstRef) {
        self.prog.block_mut(self.block).insts.push(instr);
    }

    #[inline]
    fn append_slice_to_block(&mut self, instrs: &[ssa::InstRef]) {
        self.prog
            .block_mut(self.block)
            .insts
            .extend_from_slice(instrs);
    }

    fn visit_decl(&mut self, decl: parse::DeclRef) {
        match self.ast.decl(decl) {
            parse::Decl::Var { init, .. } => {
                let span = *self.ast.decl_span(decl);

                let ptr = self.prog.new_inst_with_span(ssa::Inst::alloca(), span);

                self.append_to_block(ptr);
                self.variables.insert(decl, ptr);

                if let Some(init) = init {
                    let val = self.visit_expr(*init, ExprMode::RightValue);
                    let inst = self
                        .prog
                        .new_inst_with_span(ssa::Inst::store(ptr, val), span);
                    self.append_to_block(inst);
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: parse::StmtRef) {
        match self.ast.stmt(stmt) {
            parse::Stmt::Empty => {}
            parse::Stmt::Break => todo!(),
            parse::Stmt::Continue => todo!(),
            parse::Stmt::Expr(expr) => {
                self.visit_expr(*expr, ExprMode::RightValue);
            }
            parse::Stmt::Return(expr) => {
                let val = self.visit_expr(*expr, ExprMode::RightValue);
                let inst = self.prog.new_inst(ssa::Inst::ret(val));
                self.append_to_block(inst);
            }
            parse::Stmt::Default(_) => todo!(),
            parse::Stmt::Goto(_) => todo!(),
            parse::Stmt::Compound(_) => todo!(),
            parse::Stmt::Case { .. } => todo!(),
            parse::Stmt::Switch { .. } => todo!(),
            parse::Stmt::Label { .. } => todo!(),
            parse::Stmt::If { .. } => todo!(),
            parse::Stmt::While { .. } => todo!(),
            parse::Stmt::DoWhile { .. } => todo!(),
            parse::Stmt::For { .. } => todo!(),
        }
    }

    fn visit_expr(&mut self, expr: parse::ExprRef, mode: ExprMode) -> ssa::InstRef {
        match self.ast.expr(expr) {
            parse::Expr::Const(c) => {
                let inst = self.prog.new_inst(ssa::Inst::const_i32(*c));
                self.append_to_block(inst);
                inst
            }
            parse::Expr::Grouped(mut expr) => {
                while let parse::Expr::Grouped(inner) = self.ast.expr(expr) {
                    expr = *inner;
                }
                self.visit_expr(expr, mode)
            }
            parse::Expr::Var { decl, .. } => {
                let ptr = self
                    .variables
                    .get(&decl.expect("expected a resolved variable"))
                    .copied()
                    .expect("expected a pointer to a variable");
                match mode {
                    ExprMode::LeftValue => ptr,
                    ExprMode::RightValue => {
                        let inst = self.prog.new_inst(ssa::Inst::load(ptr));
                        self.append_to_block(inst);
                        inst
                    }
                }
            }
            parse::Expr::Unary { op, expr } => match op {
                parse::UnaryOp::Neg => {
                    let val = self.visit_expr(*expr, ExprMode::RightValue);
                    let inst = self.prog.new_inst(ssa::Inst::neg(val));
                    self.append_to_block(inst);
                    inst
                }
                parse::UnaryOp::BitNot => todo!(),
                parse::UnaryOp::LogicalNot => todo!(),
                parse::UnaryOp::PreInc => todo!(),
                parse::UnaryOp::PreDec => todo!(),
                parse::UnaryOp::PostInc => todo!(),
                parse::UnaryOp::PostDec => todo!(),
            },
            parse::Expr::Binary { op, lhs, rhs } => match op {
                parse::BinaryOp::LogicalOr => todo!(),
                parse::BinaryOp::LogicalAnd => todo!(),
                parse::BinaryOp::Equal => todo!(),
                parse::BinaryOp::NotEqual => todo!(),
                parse::BinaryOp::LessThan => todo!(),
                parse::BinaryOp::LessEqual => todo!(),
                parse::BinaryOp::GreaterThan => todo!(),
                parse::BinaryOp::GreaterEqual => todo!(),
                parse::BinaryOp::Add => todo!(),
                parse::BinaryOp::Sub => todo!(),
                parse::BinaryOp::Mul => todo!(),
                parse::BinaryOp::Div => todo!(),
                parse::BinaryOp::Rem => todo!(),
                parse::BinaryOp::BitOr => todo!(),
                parse::BinaryOp::BitAnd => todo!(),
                parse::BinaryOp::BitXor => todo!(),
                parse::BinaryOp::BitLsh => todo!(),
                parse::BinaryOp::BitRsh => todo!(),
                parse::BinaryOp::Assign => {
                    let lhs = self.visit_expr(*lhs, ExprMode::LeftValue);
                    let rhs = self.visit_expr(*rhs, ExprMode::RightValue);
                    let store = self.prog.new_inst(ssa::Inst::store(lhs, rhs));
                    self.append_to_block(store);
                    lhs
                }
                parse::BinaryOp::AddAssign => todo!(),
                parse::BinaryOp::SubAssign => todo!(),
                parse::BinaryOp::MulAssign => todo!(),
                parse::BinaryOp::DivAssign => todo!(),
                parse::BinaryOp::RemAssign => todo!(),
                parse::BinaryOp::BitOrAssign => todo!(),
                parse::BinaryOp::BitAndAssign => todo!(),
                parse::BinaryOp::BitXorAssign => todo!(),
                parse::BinaryOp::BitLshAssign => todo!(),
                parse::BinaryOp::BitRshAssign => todo!(),
            },
            parse::Expr::Ternary { .. } => todo!(),
        }
    }
}

// ---------------------------------------------------------------------------
// ExprMode
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprMode {
    LeftValue,
    RightValue,
}
