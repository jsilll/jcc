use crate::{parse, sema};

use tacky::string_interner::DefaultStringInterner;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Root function
// ---------------------------------------------------------------------------

pub fn build<'a>(
    ast: &'a parse::Ast,
    ctx: &'a sema::SemaCtx,
    interner: DefaultStringInterner,
) -> ssa::Program {
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
        match self.ast.decl(decl) {
            parse::Decl::Var { init, .. } => {
                let span = *self.ast.decl_span(decl);

                let ptr = self.prog.new_inst_with_span(ssa::Inst::alloca(), span);

                self.append_to_block(ptr);
                self.vars.insert(decl, ptr);

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
            parse::Expr::Grouped(expr) => self.visit_expr(*expr, mode),
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
                parse::UnaryOp::Neg => self.build_unary_op(ssa::Inst::neg, *expr),
                parse::UnaryOp::BitNot => self.build_unary_op(ssa::Inst::bnot, *expr),
                parse::UnaryOp::LogicalNot => self.build_unary_op(ssa::Inst::not, *expr),
                parse::UnaryOp::PreInc => self.build_prefix_unary_op(ssa::Inst::inc, *expr),
                parse::UnaryOp::PreDec => self.build_prefix_unary_op(ssa::Inst::dec, *expr),
                parse::UnaryOp::PostInc => self.build_postfix_unary_op(ssa::Inst::inc, *expr),
                parse::UnaryOp::PostDec => self.build_postfix_unary_op(ssa::Inst::dec, *expr),
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
                parse::BinaryOp::Equal => self.build_binary_op(ssa::Inst::eq, *lhs, *rhs),
                parse::BinaryOp::NotEqual => self.build_binary_op(ssa::Inst::neq, *lhs, *rhs),
                parse::BinaryOp::LessThan => self.build_binary_op(ssa::Inst::lt, *lhs, *rhs),
                parse::BinaryOp::LessEqual => self.build_binary_op(ssa::Inst::leq, *lhs, *rhs),
                parse::BinaryOp::GreaterThan => self.build_binary_op(ssa::Inst::gt, *lhs, *rhs),
                parse::BinaryOp::GreaterEqual => self.build_binary_op(ssa::Inst::geq, *lhs, *rhs),
                parse::BinaryOp::Add => self.build_binary_op(ssa::Inst::add, *lhs, *rhs),
                parse::BinaryOp::Sub => self.build_binary_op(ssa::Inst::sub, *lhs, *rhs),
                parse::BinaryOp::Mul => self.build_binary_op(ssa::Inst::mul, *lhs, *rhs),
                parse::BinaryOp::Div => self.build_binary_op(ssa::Inst::div, *lhs, *rhs),
                parse::BinaryOp::Rem => self.build_binary_op(ssa::Inst::rem, *lhs, *rhs),
                parse::BinaryOp::BitOr => self.build_binary_op(ssa::Inst::bor, *lhs, *rhs),
                parse::BinaryOp::BitAnd => self.build_binary_op(ssa::Inst::band, *lhs, *rhs),
                parse::BinaryOp::BitXor => self.build_binary_op(ssa::Inst::bxor, *lhs, *rhs),
                parse::BinaryOp::BitLsh => self.build_binary_op(ssa::Inst::bshl, *lhs, *rhs),
                parse::BinaryOp::BitRsh => self.build_binary_op(ssa::Inst::bshr, *lhs, *rhs),
                parse::BinaryOp::AddAssign => self.build_binary_assign_op(ssa::Inst::add, *lhs, *rhs),
                parse::BinaryOp::SubAssign => self.build_binary_assign_op(ssa::Inst::sub, *lhs, *rhs),
                parse::BinaryOp::MulAssign => self.build_binary_assign_op(ssa::Inst::mul, *lhs, *rhs),
                parse::BinaryOp::DivAssign => self.build_binary_assign_op(ssa::Inst::div, *lhs, *rhs),
                parse::BinaryOp::RemAssign => self.build_binary_assign_op(ssa::Inst::rem, *lhs, *rhs),
                parse::BinaryOp::BitOrAssign => self.build_binary_assign_op(ssa::Inst::bor, *lhs, *rhs),
                parse::BinaryOp::BitAndAssign => self.build_binary_assign_op(ssa::Inst::band, *lhs, *rhs),
                parse::BinaryOp::BitXorAssign => self.build_binary_assign_op(ssa::Inst::bxor, *lhs, *rhs),
                parse::BinaryOp::BitLshAssign => self.build_binary_assign_op(ssa::Inst::bshl, *lhs, *rhs),
                parse::BinaryOp::BitRshAssign => self.build_binary_assign_op(ssa::Inst::bshr, *lhs, *rhs),
            },
            parse::Expr::Ternary { .. } => todo!(),
        }
    }

    fn build_unary_op<F: FnOnce(ssa::InstRef) -> ssa::Inst>(
        &mut self,
        op: F,
        expr: parse::ExprRef,
    ) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self.prog.new_inst(op(val));
        self.append_slice_to_block(&[val, inst]);
        inst
    }

    fn build_prefix_unary_op<F: FnOnce(ssa::InstRef) -> ssa::Inst>(
        &mut self,
        op: F,
        expr: parse::ExprRef,
    ) -> ssa::InstRef {
        let lhs = self.visit_expr(expr, ExprMode::LeftValue);
        let rhs = self.visit_expr(expr, ExprMode::RightValue);
        let inst1 = self.prog.new_inst(op(rhs));
        let inst2 = self.prog.new_inst(ssa::Inst::store(lhs, inst1));
        self.append_slice_to_block(&[rhs, inst1, inst2]);
        inst1
    }

    fn build_postfix_unary_op<F: FnOnce(ssa::InstRef) -> ssa::Inst>(
        &mut self,
        op: F,
        expr: parse::ExprRef,
    ) -> ssa::InstRef {
        let lhs = self.visit_expr(expr, ExprMode::LeftValue);
        let rhs = self.visit_expr(expr, ExprMode::RightValue);
        let inst1 = self.prog.new_inst(op(rhs));
        let inst2 = self.prog.new_inst(ssa::Inst::store(lhs, inst1));
        self.append_slice_to_block(&[rhs, inst1, inst2]);
        rhs
    }

    fn build_binary_op<F: FnOnce(ssa::InstRef, ssa::InstRef) -> ssa::Inst>(
        &mut self,
        op: F,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
    ) -> ssa::InstRef {
        let lhs = self.visit_expr(lhs, ExprMode::RightValue);
        let rhs = self.visit_expr(rhs, ExprMode::RightValue);
        let inst1 = self.prog.new_inst(op(lhs, rhs));
        let inst2 = self.prog.new_inst(ssa::Inst::store(lhs, inst1));
        self.append_slice_to_block(&[rhs, inst1, inst2]);
        inst1
    }

    fn build_binary_assign_op<F: FnOnce(ssa::InstRef, ssa::InstRef) -> ssa::Inst>(
        &mut self,
        op: F,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
    ) -> ssa::InstRef {
        let vlhs = self.visit_expr(lhs, ExprMode::RightValue);
        let vrhs = self.visit_expr(rhs, ExprMode::RightValue);
        let inst = self.prog.new_inst(op(vlhs, vrhs));
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
