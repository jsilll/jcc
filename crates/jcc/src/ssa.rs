use crate::{ast::parse, sema};

use tacky::{source_file::SourceSpan, Interner, Symbol};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Root function
// ---------------------------------------------------------------------------

pub fn build<'a>(ast: &'a parse::Ast, ctx: &'a sema::SemaCtx, interner: Interner) -> ssa::Program {
    let mut prog = ssa::Program::new(interner);
    ast.root().iter().for_each(|decl| {
        SSAFuncBuilder::new(ast, ctx, &mut prog, *decl).build();
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
    decl: parse::DeclRef,
    vars: HashMap<parse::DeclRef, ssa::InstRef>,
    labeled_blocks: HashMap<Symbol, ssa::BlockRef>,
    case_blocks: HashMap<parse::StmtRef, ssa::BlockRef>,
    break_blocks: HashMap<parse::StmtRef, ssa::BlockRef>,
    continue_blocks: HashMap<parse::StmtRef, ssa::BlockRef>,
}

impl<'a> SSAFuncBuilder<'a> {
    fn new(
        ast: &'a parse::Ast,
        ctx: &'a sema::SemaCtx,
        prog: &'a mut ssa::Program,
        decl: parse::DeclRef,
    ) -> Self {
        let span = *ast.decl_span(decl);
        match ast.decl(decl) {
            parse::Decl::Var { .. } => todo!("handle variable declarations"),
            parse::Decl::Func { name, .. } => {
                let block = prog.new_block_with_span("entry", span);
                let func = prog.new_func_with_span_interned(*name, span);
                prog.func_mut(func).blocks.push(block);
                Self {
                    ast,
                    ctx,
                    prog,
                    func,
                    decl,
                    block,
                    vars: HashMap::new(),
                    case_blocks: HashMap::new(),
                    break_blocks: HashMap::new(),
                    labeled_blocks: HashMap::new(),
                    continue_blocks: HashMap::new(),
                }
            }
        }
    }

    fn build(mut self) {
        match self.ast.decl(self.decl) {
            parse::Decl::Var { .. } => todo!("handle variable declarations"),
            parse::Decl::Func { body, .. } => {
                let body = body.expect("expected a function body");
                self.ast
                    .block_items(body)
                    .iter()
                    .for_each(|item| match item {
                        parse::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        parse::BlockItem::Decl(decl) => self.visit_decl(*decl),
                    });
                let append_return = match self.ast.block_items(body).last() {
                    Some(parse::BlockItem::Stmt(stmt)) => match self.ast.stmt(*stmt) {
                        parse::Stmt::Return(_) => false,
                        _ => true,
                    },
                    _ => true,
                };
                if append_return {
                    let span = *self.ast.decl_span(self.decl);
                    let val = self.prog.new_inst_with_span(ssa::Inst::const_i32(0), span);
                    let ret = self.prog.new_inst_with_span(ssa::Inst::ret(val), span);
                    self.append_slice_to_block(&[val, ret]);
                }
            }
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

    #[inline]
    fn get_or_make_labeled_block(&mut self, label: Symbol, span: SourceSpan) -> ssa::BlockRef {
        self.labeled_blocks
            .entry(label)
            .or_insert_with(|| {
                let block = self.prog.new_block_with_span_interned(label, span);
                self.prog.func_mut(self.func).blocks.push(block);
                block
            })
            .clone()
    }

    fn visit_decl(&mut self, decl: parse::DeclRef) {
        let span = *self.ast.decl_span(decl);
        match self.ast.decl(decl) {
            parse::Decl::Var { init, .. } => {
                let alloca = self.prog.new_inst_with_span(ssa::Inst::alloca(), span);
                self.append_to_block(alloca);
                self.vars.insert(decl, alloca);
                if let Some(init) = init {
                    let val = self.visit_expr(*init, ExprMode::RightValue);
                    let store = self
                        .prog
                        .new_inst_with_span(ssa::Inst::store(alloca, val), span);
                    self.append_to_block(store);
                }
            }
            parse::Decl::Func { .. } => todo!("handle function declarations"),
        }
    }

    fn visit_stmt(&mut self, stmt: parse::StmtRef) {
        let span = self.ast.stmt_span(stmt).clone();
        match self.ast.stmt(stmt) {
            parse::Stmt::Empty => {}
            parse::Stmt::Expr(expr) => {
                self.visit_expr(*expr, ExprMode::RightValue);
            }
            parse::Stmt::Default(inner) => {
                let block = self
                    .case_blocks
                    .remove(&stmt)
                    .expect("expected a case block");
                self.block = block;
                self.visit_stmt(*inner);
            }
            parse::Stmt::Return(expr) => {
                let val = self.visit_expr(*expr, ExprMode::RightValue);
                let inst = self.prog.new_inst_with_span(ssa::Inst::ret(val), span);
                self.append_to_block(inst);
            }
            parse::Stmt::Goto(label) => {
                let block = self.get_or_make_labeled_block(*label, span);
                let inst = self.prog.new_inst_with_span(ssa::Inst::jump(block), span);
                self.append_to_block(inst);
            }
            parse::Stmt::Break => {
                let block = self
                    .break_blocks
                    .get(self.ctx.breaks.get(&stmt).expect("expected a break block"))
                    .expect("expected a break block");
                let jump = self.prog.new_inst_with_span(ssa::Inst::jump(*block), span);
                self.append_to_block(jump);
            }
            parse::Stmt::Label { label, stmt } => {
                let block = self.get_or_make_labeled_block(*label, span);
                let inst = self.prog.new_inst_with_span(ssa::Inst::jump(block), span);
                self.append_to_block(inst);

                // === Labeled Block ===
                self.block = block;
                self.visit_stmt(*stmt);
            }
            parse::Stmt::Compound(items) => {
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|item| match item {
                        parse::BlockItem::Decl(decl) => self.visit_decl(*decl),
                        parse::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                    });
            }
            parse::Stmt::Case { stmt: inner, .. } => {
                let block = self
                    .case_blocks
                    .remove(&stmt)
                    .expect("expected a case block");

                // === Previous Block ===
                let jump = self.prog.new_inst_with_span(ssa::Inst::jump(block), span);
                self.append_to_block(jump);

                // === Case Block ===
                self.block = block;
                self.visit_stmt(*inner);
            }
            parse::Stmt::Continue => {
                let block = self
                    .continue_blocks
                    .get(
                        self.ctx
                            .continues
                            .get(&stmt)
                            .expect("expected a continue block"),
                    )
                    .expect("expected a continue block");
                let jump = self.prog.new_inst_with_span(ssa::Inst::jump(*block), span);
                self.append_to_block(jump);
            }
            parse::Stmt::If {
                cond,
                then,
                otherwise: None,
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let then_block = self.prog.new_block_with_span("if.then", span);
                let cont_block = self.prog.new_block_with_span("if.cont", span);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[then_block, cont_block]);

                let branch = self
                    .prog
                    .new_inst_with_span(ssa::Inst::branch(cond_val, then_block, cont_block), span);
                self.append_to_block(branch);

                // === Then Block ===
                self.block = then_block;
                self.visit_stmt(*then);
                let jmp_then = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cont_block), span);
                self.append_to_block(jmp_then);

                // === Merge Block ===
                self.block = cont_block;
            }
            parse::Stmt::If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let then_block = self.prog.new_block_with_span("if.then", span);
                let else_block = self.prog.new_block_with_span("if.else", span);
                let cont_block = self.prog.new_block_with_span("if.cont", span);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[then_block, else_block, cont_block]);

                let branch = self
                    .prog
                    .new_inst_with_span(ssa::Inst::branch(cond_val, then_block, else_block), span);
                self.append_to_block(branch);

                // === Then Block ===
                self.block = then_block;
                self.visit_stmt(*then);
                let jmp_then = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cont_block), span);
                self.append_to_block(jmp_then);

                // === Else Block ===
                self.block = else_block;
                self.visit_stmt(*otherwise);
                let jmp_else = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cont_block), span);
                self.append_to_block(jmp_else);

                // === Merge Block ===
                self.block = cont_block;
            }
            parse::Stmt::While { cond, body } => {
                let cond_block = self.prog.new_block_with_span("while.cond", span);
                let body_block = self.prog.new_block_with_span("while.body", span);
                let cont_block = self.prog.new_block_with_span("while.cont", span);
                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, cond_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[cond_block, body_block, cont_block]);

                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cond_block), span);
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self
                    .prog
                    .new_inst_with_span(ssa::Inst::branch(cond_val, body_block, cont_block), span);
                self.append_to_block(branch);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cond_block), span);
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt);
                self.continue_blocks.remove(&stmt);
            }
            parse::Stmt::DoWhile { body, cond } => {
                let body_block = self.prog.new_block_with_span("do.body", span);
                let cond_block = self.prog.new_block_with_span("do.cond", span);
                let cont_block = self.prog.new_block_with_span("do.cont", span);
                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, cond_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[body_block, cond_block, cont_block]);

                // === Jump to Body Block ===
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(body_block), span);
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cond_block), span);
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self
                    .prog
                    .new_inst_with_span(ssa::Inst::branch(cond_val, body_block, cont_block), span);
                self.append_to_block(branch);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt);
                self.continue_blocks.remove(&stmt);
            }
            parse::Stmt::For {
                init,
                cond: None,
                step: None,
                body,
            } => {
                let body_block = self.prog.new_block_with_span("for.body", span);
                let cont_block = self.prog.new_block_with_span("for.cont", span);
                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, body_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        parse::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        parse::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(body_block), span);
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(body_block), span);
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt);
                self.continue_blocks.remove(&stmt);
            }
            parse::Stmt::For {
                init,
                cond: Some(cond),
                step: None,
                body,
            } => {
                let cond_block = self.prog.new_block_with_span("for.cond", span);
                let body_block = self.prog.new_block_with_span("for.body", span);
                let cont_block = self.prog.new_block_with_span("for.cont", span);
                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, cond_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[cond_block, body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        parse::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        parse::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cond_block), span);
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self
                    .prog
                    .new_inst_with_span(ssa::Inst::branch(cond_val, body_block, cont_block), span);
                self.append_to_block(branch);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cond_block), span);
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt);
                self.continue_blocks.remove(&stmt);
            }
            parse::Stmt::For {
                init,
                cond: None,
                step: Some(step),
                body,
            } => {
                let step_block = self.prog.new_block_with_span("for.step", span);
                let body_block = self.prog.new_block_with_span("for.body", span);
                let cont_block = self.prog.new_block_with_span("for.cont", span);
                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, step_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[step_block, body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        parse::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        parse::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(body_block), span);
                self.append_to_block(jump);

                // === Step Block ===
                self.block = step_block;
                self.visit_expr(*step, ExprMode::RightValue);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(body_block), span);
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(step_block), span);
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt);
                self.continue_blocks.remove(&stmt);
            }
            parse::Stmt::For {
                init,
                cond: Some(cond),
                step: Some(step),
                body,
            } => {
                let cond_block = self.prog.new_block_with_span("for.cond", span);
                let step_block = self.prog.new_block_with_span("for.step", span);
                let body_block = self.prog.new_block_with_span("for.body", span);
                let cont_block = self.prog.new_block_with_span("for.cont", span);
                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, step_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[cond_block, step_block, body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        parse::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        parse::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cond_block), span);
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self
                    .prog
                    .new_inst_with_span(ssa::Inst::branch(cond_val, body_block, cont_block), span);
                self.append_to_block(branch);

                // === Step Block ===
                self.block = step_block;
                self.visit_expr(*step, ExprMode::RightValue);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cond_block), span);
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(step_block), span);
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt);
                self.continue_blocks.remove(&stmt);
            }
            parse::Stmt::Switch { cond, body } => {
                let mut cases = Vec::new();
                let mut default_block = None;
                if let Some(switch) = self.ctx.switches.get(&stmt) {
                    cases.reserve(switch.cases.len());
                    switch.cases.iter().for_each(|stmt| {
                        if let parse::Stmt::Case { expr, .. } = self.ast.stmt(*stmt) {
                            let val = match self.ast.expr(*expr) {
                                parse::Expr::Const(c) => *c,
                                _ => panic!("expected a constant expression"),
                            };
                            let case_block = self.prog.new_block_with_span("switch.case", span);
                            self.prog.func_mut(self.func).blocks.push(case_block);
                            self.case_blocks.insert(*stmt, case_block);
                            cases.push((val, case_block));
                        }
                    });
                    if let Some(stmt) = switch.default {
                        let block = self.prog.new_block_with_span("switch.default", span);
                        self.prog.func_mut(self.func).blocks.push(block);
                        self.case_blocks.insert(stmt, block);
                        default_block = Some(block);
                    }
                }

                let cont_block = self.prog.new_block_with_span("switch.cont", span);
                self.prog.func_mut(self.func).blocks.push(cont_block);
                self.break_blocks.insert(stmt, cont_block);

                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let switch = self.prog.new_inst_with_span(
                    ssa::Inst::switch(cond_val, default_block.unwrap_or(cont_block), cases),
                    span,
                );
                self.append_to_block(switch);

                self.visit_stmt(*body);
                let jump = self
                    .prog
                    .new_inst_with_span(ssa::Inst::jump(cont_block), span);
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
            }
        }
    }

    fn visit_expr(&mut self, expr: parse::ExprRef, mode: ExprMode) -> ssa::InstRef {
        let span = self.ast.expr_span(expr).clone();
        match self.ast.expr(expr) {
            parse::Expr::Call { .. } => todo!("handle function calls"),
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
                parse::BinaryOp::Assign => {
                    let lhs = self.visit_expr(*lhs, ExprMode::LeftValue);
                    let rhs = self.visit_expr(*rhs, ExprMode::RightValue);
                    let store = self
                        .prog
                        .new_inst_with_span(ssa::Inst::store(lhs, rhs), span);
                    self.append_to_block(store);
                    match mode {
                        ExprMode::LeftValue => lhs,
                        ExprMode::RightValue => rhs,
                    }
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
