use crate::{parse, sema};

use tacky::{source_file::SourceSpan, Interner};

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
    func: ssa::FuncRef,
    block: ssa::BlockRef,
    item: parse::ItemRef,
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
            func,
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

    #[inline]
    fn get_var_ptr(&self, decl: parse::DeclRef) -> ssa::InstRef {
        self.vars
            .get(&decl)
            .copied()
            .expect("expected a pointer to a variable")
    }

    #[inline]
    fn get_var_decl(&self, expr: parse::ExprRef) -> parse::DeclRef {
        self.ctx
            .vars
            .get(&expr)
            .copied()
            .expect("expected a variable declaration")
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
        let span = self.ast.stmt_span(stmt).clone();
        match self.ast.stmt(stmt) {
            parse::Stmt::Empty => {}
            parse::Stmt::Break => todo!(),
            parse::Stmt::Continue => todo!(),
            parse::Stmt::Expr(expr) => {
                self.visit_expr(*expr, ExprMode::RightValue);
            }
            parse::Stmt::Return(expr) => {
                let val = self.visit_expr(*expr, ExprMode::RightValue);
                let inst = self.prog.new_inst_with_span(ssa::Inst::ret(val), span);
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
        let span = self.ast.expr_span(expr).clone();
        match self.ast.expr(expr) {
            parse::Expr::Grouped(expr) => self.visit_expr(*expr, mode),
            parse::Expr::Const(c) => {
                let inst = self.prog.new_inst_with_span(ssa::Inst::const_i32(*c), span);
                self.append_to_block(inst);
                inst
            }
            parse::Expr::Var(_) => {
                let decl = self.get_var_decl(expr);
                let ptr = self.get_var_ptr(decl);
                match mode {
                    ExprMode::LeftValue => ptr,
                    ExprMode::RightValue => {
                        let inst = self.prog.new_inst_with_span(ssa::Inst::load(ptr), span);
                        self.append_to_block(inst);
                        inst
                    }
                }
            }
            parse::Expr::Unary { op, expr } => match op {
                parse::UnaryOp::Neg => self.build_unary(ssa::UnaryOp::Neg, *expr, span),
                parse::UnaryOp::BitNot => self.build_unary(ssa::UnaryOp::Not, *expr, span),
                parse::UnaryOp::PreInc => self.build_prefix_unary(ssa::UnaryOp::Inc, *expr, span),
                parse::UnaryOp::PreDec => self.build_prefix_unary(ssa::UnaryOp::Dec, *expr, span),
                parse::UnaryOp::PostInc => self.build_postfix_unary(ssa::UnaryOp::Inc, *expr, span),
                parse::UnaryOp::PostDec => self.build_postfix_unary(ssa::UnaryOp::Dec, *expr, span),
                parse::UnaryOp::LogicalNot => {
                    let val = self.visit_expr(*expr, ExprMode::RightValue);

                    let zero = self.prog.new_inst_with_span(ssa::Inst::const_i32(0), span);
                    let cmp = self.prog.new_inst_with_span(
                        ssa::Inst::binary(ssa::Type::Int32, ssa::BinaryOp::Equal, val, zero),
                        span,
                    );

                    self.append_slice_to_block(&[zero, cmp]);
                    cmp
                }
            },
            parse::Expr::Binary { op, lhs, rhs } => match op {
                parse::BinaryOp::LogicalOr => {
                    self.build_short_circuit(LogicalOp::Or, *lhs, *rhs, span)
                }
                parse::BinaryOp::LogicalAnd => {
                    self.build_short_circuit(LogicalOp::And, *lhs, *rhs, span)
                }
                parse::BinaryOp::Equal => self.build_binary(ssa::BinaryOp::Equal, *lhs, *rhs, span),
                parse::BinaryOp::NotEqual => {
                    self.build_binary(ssa::BinaryOp::NotEqual, *lhs, *rhs, span)
                }
                parse::BinaryOp::LessThan => {
                    self.build_binary(ssa::BinaryOp::LessThan, *lhs, *rhs, span)
                }
                parse::BinaryOp::LessEqual => {
                    self.build_binary(ssa::BinaryOp::LessEqual, *lhs, *rhs, span)
                }
                parse::BinaryOp::GreaterThan => {
                    self.build_binary(ssa::BinaryOp::GreaterThan, *lhs, *rhs, span)
                }
                parse::BinaryOp::GreaterEqual => {
                    self.build_binary(ssa::BinaryOp::GreaterEqual, *lhs, *rhs, span)
                }
                parse::BinaryOp::Add => self.build_binary(ssa::BinaryOp::Add, *lhs, *rhs, span),
                parse::BinaryOp::Sub => self.build_binary(ssa::BinaryOp::Sub, *lhs, *rhs, span),
                parse::BinaryOp::Mul => self.build_binary(ssa::BinaryOp::Mul, *lhs, *rhs, span),
                parse::BinaryOp::Div => self.build_binary(ssa::BinaryOp::Div, *lhs, *rhs, span),
                parse::BinaryOp::Rem => self.build_binary(ssa::BinaryOp::Rem, *lhs, *rhs, span),
                parse::BinaryOp::BitOr => self.build_binary(ssa::BinaryOp::Or, *lhs, *rhs, span),
                parse::BinaryOp::BitAnd => self.build_binary(ssa::BinaryOp::And, *lhs, *rhs, span),
                parse::BinaryOp::BitXor => self.build_binary(ssa::BinaryOp::Xor, *lhs, *rhs, span),
                parse::BinaryOp::BitShl => self.build_binary(ssa::BinaryOp::Shl, *lhs, *rhs, span),
                parse::BinaryOp::BitShr => self.build_binary(ssa::BinaryOp::Shr, *lhs, *rhs, span),
                parse::BinaryOp::Assign => {
                    let lhs = self.visit_expr(*lhs, ExprMode::LeftValue);
                    let rhs = self.visit_expr(*rhs, ExprMode::RightValue);
                    let store = self
                        .prog
                        .new_inst_with_span(ssa::Inst::store(lhs, rhs), span);
                    self.append_to_block(store);
                    lhs
                }
                parse::BinaryOp::AddAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Add, *lhs, *rhs, span)
                }
                parse::BinaryOp::SubAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Sub, *lhs, *rhs, span)
                }
                parse::BinaryOp::MulAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Mul, *lhs, *rhs, span)
                }
                parse::BinaryOp::DivAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Div, *lhs, *rhs, span)
                }
                parse::BinaryOp::RemAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Rem, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitOrAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Or, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitAndAssign => {
                    self.build_binary_assign(ssa::BinaryOp::And, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitXorAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Xor, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitShlAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Shl, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitShrAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Shr, *lhs, *rhs, span)
                }
            },
            parse::Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);

                let then_block = self.prog.new_block_with_span("tern.then", span);
                let else_block = self.prog.new_block_with_span("tern.else", span);
                let cont_block = self.prog.new_block_with_span("tern.cont", span);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[then_block, else_block, cont_block]);

                let phi = self
                    .prog
                    .new_inst_with_span(ssa::Inst::phi(ssa::Type::Int32), span);

                let branch = self
                    .prog
                    .new_inst_with_span(ssa::Inst::branch(cond_val, then_block, else_block), span);
                self.append_to_block(branch);

                // === Then Block ===
                self.block = then_block;
                let then_val = self.visit_expr(*then, ExprMode::RightValue);
                let upsilon = self
                    .prog
                    .new_inst_with_span(ssa::Inst::upsilon(phi, then_val), span);
                let jmp_then = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cont_block), span);
                self.append_slice_to_block(&[upsilon, jmp_then]);

                // === Else Block ===
                self.block = else_block;
                let else_val = self.visit_expr(*otherwise, ExprMode::RightValue);
                let upsilon = self
                    .prog
                    .new_inst_with_span(ssa::Inst::upsilon(phi, else_val), span);
                let jmp_else = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cont_block), span);
                self.append_slice_to_block(&[upsilon, jmp_else]);

                // === Merge Block ===
                self.block = cont_block;
                self.append_to_block(phi);
                phi
            }
        }
    }

    fn build_unary(
        &mut self,
        op: ssa::UnaryOp,
        expr: parse::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst_with_span(ssa::Inst::unary(ssa::Type::Int32, op, val), span);
        self.append_to_block(inst);

        inst
    }

    fn build_prefix_unary(
        &mut self,
        op: ssa::UnaryOp,
        expr: parse::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst_with_span(ssa::Inst::unary(ssa::Type::Int32, op, val), span);
        self.append_to_block(inst);

        let ptr = self.visit_expr(expr, ExprMode::LeftValue);
        let store = self
            .prog
            .new_inst_with_span(ssa::Inst::store(ptr, inst), span);
        self.append_to_block(store);

        inst
    }

    fn build_postfix_unary(
        &mut self,
        op: ssa::UnaryOp,
        expr: parse::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst_with_span(ssa::Inst::unary(ssa::Type::Int32, op, val), span);
        self.append_to_block(inst);

        let ptr = self.visit_expr(expr, ExprMode::LeftValue);
        let store = self
            .prog
            .new_inst_with_span(ssa::Inst::store(ptr, inst), span);
        self.append_to_block(store);

        val
    }

    fn build_binary(
        &mut self,
        op: ssa::BinaryOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let lhs = self.visit_expr(lhs, ExprMode::RightValue);
        let rhs = self.visit_expr(rhs, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst_with_span(ssa::Inst::binary(ssa::Type::Int32, op, lhs, rhs), span);
        self.append_to_block(inst);

        inst
    }

    fn build_binary_assign(
        &mut self,
        op: ssa::BinaryOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let l = self.visit_expr(lhs, ExprMode::RightValue);
        let r = self.visit_expr(rhs, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst_with_span(ssa::Inst::binary(ssa::Type::Int32, op, l, r), span);
        self.append_to_block(inst);

        let ptr = self.visit_expr(lhs, ExprMode::LeftValue);
        let store = self
            .prog
            .new_inst_with_span(ssa::Inst::store(ptr, inst), span);
        self.append_to_block(store);

        inst
    }

    fn build_short_circuit(
        &mut self,
        op: LogicalOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let rhs_block = self.prog.new_block_with_span(
            match op {
                LogicalOp::Or => "or.rhs",
                LogicalOp::And => "and.rhs",
            },
            span,
        );
        let cont_block = self.prog.new_block_with_span(
            match op {
                LogicalOp::Or => "or.cont",
                LogicalOp::And => "and.cont",
            },
            span,
        );
        self.prog
            .func_mut(self.func)
            .blocks
            .extend_from_slice(&[rhs_block, cont_block]);

        let phi = self
            .prog
            .new_inst_with_span(ssa::Inst::phi(ssa::Type::Int32), span);

        // === LHS Block ===
        let lhs_val = self.visit_expr(lhs, ExprMode::RightValue);

        let short_circuit_val = self.prog.new_inst_with_span(
            match op {
                LogicalOp::Or => ssa::Inst::const_i32(1),
                LogicalOp::And => ssa::Inst::const_i32(0),
            },
            span,
        );

        let upsilon = self
            .prog
            .new_inst_with_span(ssa::Inst::upsilon(phi, short_circuit_val), span);

        let (true_target, false_target) = match op {
            LogicalOp::Or => (cont_block, rhs_block),
            LogicalOp::And => (rhs_block, cont_block),
        };

        let branch = self
            .prog
            .new_inst_with_span(ssa::Inst::branch(lhs_val, true_target, false_target), span);

        self.append_slice_to_block(&[short_circuit_val, upsilon, branch]);

        // === RHS Block ===
        self.block = rhs_block;
        let rhs_val = self.visit_expr(rhs, ExprMode::RightValue);

        let zero_val = self.prog.new_inst_with_span(ssa::Inst::const_i32(0), span);

        let is_nonzero = self.prog.new_inst_with_span(
            ssa::Inst::binary(ssa::Type::Int32, ssa::BinaryOp::NotEqual, rhs_val, zero_val),
            span,
        );

        let upsilon = self
            .prog
            .new_inst_with_span(ssa::Inst::upsilon(phi, is_nonzero), span);

        let jmp_rhs = self
            .prog
            .new_inst_with_span(ssa::Inst::jump(cont_block), span);

        self.append_slice_to_block(&[zero_val, is_nonzero, upsilon, jmp_rhs]);

        // === Merge Block ===
        self.block = cont_block;
        self.append_to_block(phi);
        phi
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

// ---------------------------------------------------------------------------
// LogicalOp
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LogicalOp {
    Or,
    And,
}
