use crate::{parse, sema};

use tacky::Interner;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Root function
// ---------------------------------------------------------------------------

pub fn build<'a>(ast: &'a parse::Ast, ctx: &'a sema::SemaCtx, interner: Interner) -> ssa::Program {
    let mut prog = ssa::Program::new(interner);
    ast.items_iter().for_each(|item| {
        SSAFuncBuilder::new(ast, ctx, &mut prog, item).build();
    });
    prog
}

// ---------------------------------------------------------------------------
// SSAFuncBuilder
// ---------------------------------------------------------------------------

struct SSAFuncBuilder<'a> {
    ast: &'a parse::Ast,
    ctx: &'a sema::SemaCtx,
    prog: &'a mut ssa::Program,
    item: parse::ItemRef,
    block: ssa::BlockRef,
    // func: ssa::FuncRef,
    vars: HashMap<parse::DeclRef, ssa::InstRef>,
}

impl<'a> SSAFuncBuilder<'a> {
    fn new(
        ast: &'a parse::Ast,
        ctx: &'a sema::SemaCtx,
        prog: &'a mut ssa::Program,
        item: parse::ItemRef,
    ) -> Self {
        let span = *ast.item_span(item);
        let func = prog.new_func_with_span_interned(ast.item(item).name, span);
        let block = prog.new_block_with_span("entry", span);
        prog.func_mut(func).blocks.push(block);
        Self {
            ast,
            ctx,
            prog,
            item,
            block,
            vars: HashMap::new(),
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
        let span = *self.ast.decl_span(decl);
        let ptr = self.prog.new_inst_with_span(ssa::Inst::alloca(), span);
        self.append_to_block(ptr);
        self.vars.insert(decl, ptr);
        if let parse::Decl {
            init: Some(expr), ..
        } = self.ast.decl(decl)
        {
            let val = self.visit_expr(*expr, ExprMode::RightValue);
            let inst = self
                .prog
                .new_inst_with_span(ssa::Inst::store(ptr, val), span);
            self.append_to_block(inst);
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
            parse::Expr::Grouped(expr) => self.visit_expr(*expr, mode),
            parse::Expr::Const(c) => {
                let inst = self.prog.new_inst(ssa::Inst::const_i32(*c));
                self.append_to_block(inst);
                inst
            }
            parse::Expr::Var(_) => {
                let decl = self
                    .ctx
                    .vars
                    .get(&expr)
                    .copied()
                    .expect("expected a variable");
                let ptr = self
                    .vars
                    .get(&decl)
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
                parse::UnaryOp::Neg => self.build_unary_op(ssa::UnaryOp::Neg, *expr),
                parse::UnaryOp::BitNot => self.build_unary_op(ssa::UnaryOp::BitNot, *expr),
                parse::UnaryOp::LogicalNot => self.build_unary_op(ssa::UnaryOp::Not, *expr),
                parse::UnaryOp::PreInc => self.build_prefix_unary_op(ssa::UnaryOp::Inc, *expr),
                parse::UnaryOp::PreDec => self.build_prefix_unary_op(ssa::UnaryOp::Dec, *expr),
                parse::UnaryOp::PostInc => self.build_postfix_unary_op(ssa::UnaryOp::Inc, *expr),
                parse::UnaryOp::PostDec => self.build_postfix_unary_op(ssa::UnaryOp::Dec, *expr),
            },
            parse::Expr::Binary { op, lhs, rhs } => match op {
                parse::BinaryOp::Assign => {
                    let lhs = self.visit_expr(*lhs, ExprMode::LeftValue);
                    let rhs = self.visit_expr(*rhs, ExprMode::RightValue);
                    let store = self.prog.new_inst(ssa::Inst::store(lhs, rhs));
                    self.append_to_block(store);
                    lhs
                }
                parse::BinaryOp::LogicalOr => todo!("implement short-circuiting"),
                parse::BinaryOp::LogicalAnd => todo!("implement short-circuiting"),
                parse::BinaryOp::Equal => self.build_binary_op(ssa::BinaryOp::Equal, *lhs, *rhs),
                parse::BinaryOp::NotEqual => {
                    self.build_binary_op(ssa::BinaryOp::NotEqual, *lhs, *rhs)
                }
                parse::BinaryOp::LessThan => {
                    self.build_binary_op(ssa::BinaryOp::LessThan, *lhs, *rhs)
                }
                parse::BinaryOp::LessEqual => {
                    self.build_binary_op(ssa::BinaryOp::LessEqual, *lhs, *rhs)
                }
                parse::BinaryOp::GreaterThan => {
                    self.build_binary_op(ssa::BinaryOp::GreaterThan, *lhs, *rhs)
                }
                parse::BinaryOp::GreaterEqual => {
                    self.build_binary_op(ssa::BinaryOp::GreaterEqual, *lhs, *rhs)
                }
                parse::BinaryOp::Add => self.build_binary_op(ssa::BinaryOp::Add, *lhs, *rhs),
                parse::BinaryOp::Sub => self.build_binary_op(ssa::BinaryOp::Sub, *lhs, *rhs),
                parse::BinaryOp::Mul => self.build_binary_op(ssa::BinaryOp::Mul, *lhs, *rhs),
                parse::BinaryOp::Div => self.build_binary_op(ssa::BinaryOp::Div, *lhs, *rhs),
                parse::BinaryOp::Rem => self.build_binary_op(ssa::BinaryOp::Rem, *lhs, *rhs),
                parse::BinaryOp::BitOr => self.build_binary_op(ssa::BinaryOp::BitOr, *lhs, *rhs),
                parse::BinaryOp::BitAnd => self.build_binary_op(ssa::BinaryOp::BitAnd, *lhs, *rhs),
                parse::BinaryOp::BitXor => self.build_binary_op(ssa::BinaryOp::BitXor, *lhs, *rhs),
                parse::BinaryOp::BitShl => self.build_binary_op(ssa::BinaryOp::BitShl, *lhs, *rhs),
                parse::BinaryOp::BitShr => self.build_binary_op(ssa::BinaryOp::BitShr, *lhs, *rhs),
                parse::BinaryOp::AddAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::Add, *lhs, *rhs)
                }
                parse::BinaryOp::SubAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::Sub, *lhs, *rhs)
                }
                parse::BinaryOp::MulAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::Mul, *lhs, *rhs)
                }
                parse::BinaryOp::DivAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::Div, *lhs, *rhs)
                }
                parse::BinaryOp::RemAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::Rem, *lhs, *rhs)
                }
                parse::BinaryOp::BitOrAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::BitOr, *lhs, *rhs)
                }
                parse::BinaryOp::BitAndAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::BitAnd, *lhs, *rhs)
                }
                parse::BinaryOp::BitXorAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::BitXor, *lhs, *rhs)
                }
                parse::BinaryOp::BitShlAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::BitShl, *lhs, *rhs)
                }
                parse::BinaryOp::BitShrAssign => {
                    self.build_binary_assign_op(ssa::BinaryOp::BitShr, *lhs, *rhs)
                }
            },
            parse::Expr::Ternary { .. } => todo!(),
        }
    }

    fn build_unary_op(&mut self, op: ssa::UnaryOp, expr: parse::ExprRef) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst(ssa::Inst::unary(ssa::Type::Int32, op, val));
        self.append_to_block(inst);
        inst
    }

    fn build_prefix_unary_op(&mut self, op: ssa::UnaryOp, expr: parse::ExprRef) -> ssa::InstRef {
        let rhs = self.visit_expr(expr, ExprMode::RightValue);
        let inst1 = self
            .prog
            .new_inst(ssa::Inst::unary(ssa::Type::Int32, op, rhs));

        let lhs = self.visit_expr(expr, ExprMode::LeftValue);
        let inst2 = self.prog.new_inst(ssa::Inst::store(lhs, inst1));
        self.append_slice_to_block(&[rhs, inst1, inst2]);
        inst1
    }

    fn build_postfix_unary_op(&mut self, op: ssa::UnaryOp, expr: parse::ExprRef) -> ssa::InstRef {
        let lhs = self.visit_expr(expr, ExprMode::LeftValue);
        let rhs = self.visit_expr(expr, ExprMode::RightValue);
        let inst1 = self
            .prog
            .new_inst(ssa::Inst::unary(ssa::Type::Int32, op, rhs));
        let inst2 = self.prog.new_inst(ssa::Inst::store(lhs, inst1));
        self.append_slice_to_block(&[rhs, inst1, inst2]);
        rhs
    }

    fn build_binary_op(
        &mut self,
        op: ssa::BinaryOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
    ) -> ssa::InstRef {
        let lhs = self.visit_expr(lhs, ExprMode::RightValue);
        let rhs = self.visit_expr(rhs, ExprMode::RightValue);
        let inst1 = self
            .prog
            .new_inst(ssa::Inst::binary(ssa::Type::Int32, op, lhs, rhs));
        let inst2 = self.prog.new_inst(ssa::Inst::store(lhs, inst1));
        self.append_slice_to_block(&[rhs, inst1, inst2]);
        inst1
    }

    fn build_binary_assign_op(
        &mut self,
        op: ssa::BinaryOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
    ) -> ssa::InstRef {
        let vlhs = self.visit_expr(lhs, ExprMode::RightValue);
        let vrhs = self.visit_expr(rhs, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst(ssa::Inst::binary(ssa::Type::Int32, op, vlhs, vrhs));
        let ptr = self.visit_expr(lhs, ExprMode::LeftValue);
        let store = self.prog.new_inst(ssa::Inst::store(ptr, inst));
        self.append_slice_to_block(&[vlhs, vrhs, inst, store]);
        inst
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
