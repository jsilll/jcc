use std::collections::HashMap;

use crate::{
    ast::{self, AstSymbol, ConstValue, StmtRef},
    sema::{Attribute, SemaCtx, SemaSymbol, StaticValue},
};

use jcc_ssa::{
    builder::IRBuilder,
    interner::{Interner, Symbol},
    sourcemap::SourceSpan,
    BinaryOp, BlockRef, Func, FuncRef, Inst, InstRef, Program, StaticVar, StaticVarRef, Type,
    UnaryOp,
};

// ---------------------------------------------------------------------------
// SSABuilder
// ---------------------------------------------------------------------------

pub struct SSABuilder<'a> {
    ast: &'a ast::Ast,
    sema: &'a SemaCtx,
    builder: IRBuilder<'a>,
    symbols: Vec<SymbolEntry>,
    tracked_blocks: HashMap<StmtRef, TrackedBlock>,
}

impl<'a> SSABuilder<'a> {
    pub fn new(ast: &'a ast::Ast, sema: &'a SemaCtx, interner: &'a mut Interner) -> Self {
        Self {
            ast,
            sema,
            tracked_blocks: HashMap::new(),
            builder: IRBuilder::new(interner),
            symbols: vec![Default::default(); ast.symbols_len() + 1],
        }
    }

    pub fn build(mut self) -> Program<'a> {
        self.ast.root().iter().for_each(|decl_ref| {
            let decl = self.ast.decl(*decl_ref);
            match decl.kind {
                ast::DeclKind::Var(_) => {
                    let info = self
                        .sema
                        .symbol(decl.name.sema.get())
                        .expect("expected a sema symbol");
                    if let Attribute::Static { init, is_global } = info.attr {
                        let init = match init {
                            StaticValue::Tentative => Some(0),
                            StaticValue::NoInitializer => None,
                            StaticValue::Initialized(ast::ConstValue::Int(init)) => Some(init),
                            StaticValue::Initialized(ast::ConstValue::Long(_)) => {
                                todo!("handle long static initializers")
                            }
                        };
                        self.get_or_make_static(
                            Type::Int32,
                            &decl.name,
                            is_global,
                            init.map(|i| i as i64),
                            decl.span,
                        );
                    }
                }
                ast::DeclKind::Func { params, body } => {
                    let is_global = self
                        .sema
                        .symbol(decl.name.sema.get())
                        .expect("expected a sema symbol")
                        .is_global();

                    let func = self.get_or_make_function(&decl.name, is_global, decl.span);
                    self.builder.switch_to_func(func);

                    if body.is_some() {
                        let block = self.builder.new_block("entry", decl.span);
                        self.builder.switch_to_block(block);
                    }

                    if let Some(body) = body {
                        self.tracked_blocks.clear();

                        self.ast.decls(params).iter().for_each(|param_ref| {
                            let param = self.ast.decl(*param_ref);
                            let arg = self.builder.insert_inst(Inst::arg(Type::Int32, param.span));
                            self.insert_var(param.name.sema.get(), arg);
                        });

                        self.ast.bitems(body).iter().for_each(|item| match item {
                            ast::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                            ast::BlockItem::Decl(decl) => self.visit_decl(*decl),
                        });

                        let append_return = match self.ast.bitems(body).last() {
                            Some(ast::BlockItem::Stmt(stmt)) => {
                                !matches!(self.ast.stmt(*stmt).kind, ast::StmtKind::Return(_))
                            }
                            _ => true,
                        };

                        if append_return {
                            let val = self.builder.insert_inst(Inst::const_i32(0, decl.span));
                            self.builder.insert_inst(Inst::ret(val, decl.span));
                        }
                    }
                    self.builder.clear_func();
                }
            }
        });
        self.builder.build()
    }

    fn visit_decl(&mut self, decl_ref: ast::DeclRef) {
        let decl = self.ast.decl(decl_ref);
        match decl.kind {
            ast::DeclKind::Func { .. } => {}
            ast::DeclKind::Var(init) => {
                let info = self
                    .sema
                    .symbol(decl.name.sema.get())
                    .expect("expected a sema symbol");
                match info.attr {
                    Attribute::Function { .. } => panic!("unexpected function attribute"),
                    Attribute::Local => {
                        let alloca = self.builder.insert_inst(Inst::alloca(decl.span));
                        self.insert_var(decl.name.sema.get(), alloca);
                        if let Some(init) = init {
                            let val = self.visit_expr(init, ExprMode::RightValue);
                            self.builder
                                .insert_inst(Inst::store(alloca, val, decl.span));
                        }
                    }
                    Attribute::Static { is_global, init } => {
                        let init = match init {
                            StaticValue::Tentative => Some(0),
                            StaticValue::NoInitializer => None,
                            StaticValue::Initialized(ast::ConstValue::Int(init)) => Some(init),
                            StaticValue::Initialized(ast::ConstValue::Long(_)) => {
                                todo!("handle long static initializers")
                            }
                        };
                        self.get_or_make_static(
                            Type::Int32,
                            &decl.name,
                            is_global,
                            init.map(|i| i as i64),
                            decl.span,
                        );
                    }
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt_ref: ast::StmtRef) {
        let stmt = self.ast.stmt(stmt_ref);
        match &stmt.kind {
            ast::StmtKind::Empty => {}
            ast::StmtKind::Expr(expr) => {
                self.visit_expr(*expr, ExprMode::RightValue);
            }
            ast::StmtKind::Return(expr) => {
                let val = self.visit_expr(*expr, ExprMode::RightValue);
                self.builder.insert_inst(Inst::ret(val, stmt.span));
            }
            ast::StmtKind::Break(target) => {
                let block = self.get_break_block(target.get());
                self.builder.insert_inst(Inst::jump(block, stmt.span));
            }
            ast::StmtKind::Continue(target) => {
                let block = self.get_continue_block(target.get());
                self.builder.insert_inst(Inst::jump(block, stmt.span));
            }
            ast::StmtKind::Default(inner) => {
                let block = self.remove_case_block(stmt_ref);

                // === Default Block ===
                self.builder.switch_to_block(block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Compound(items) => {
                self.ast.bitems(*items).iter().for_each(|item| match item {
                    ast::BlockItem::Decl(decl) => self.visit_decl(*decl),
                    ast::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                });
            }
            ast::StmtKind::Goto {
                label,
                stmt: target,
            } => {
                let block = self.get_or_make_labeled(target.get(), *label, stmt.span);
                self.builder.insert_inst(Inst::jump(block, stmt.span));
            }
            ast::StmtKind::Label { label, stmt: inner } => {
                let block = self.get_or_make_labeled(stmt_ref, *label, stmt.span);
                self.builder.insert_inst(Inst::jump(block, stmt.span));

                // === Labeled Block ===
                self.builder.switch_to_block(block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Case { stmt: inner, .. } => {
                let block = self.remove_case_block(stmt_ref);

                // === Previous Block ===
                self.builder.insert_inst(Inst::jump(block, stmt.span));

                // === Case Block ===
                self.builder.switch_to_block(block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::If {
                cond,
                then,
                otherwise: None,
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);

                let then_block = self.builder.new_block("if.then", stmt.span);
                let cont_block = self.builder.new_block("if.cont", stmt.span);

                self.builder
                    .insert_inst(Inst::branch(cond_val, then_block, cont_block, stmt.span));

                // === Then Block ===
                self.builder.switch_to_block(then_block);
                self.visit_stmt(*then);
                self.builder.insert_inst(Inst::jump(cont_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);

                let then_block = self.builder.new_block("if.then", stmt.span);
                let else_block = self.builder.new_block("if.else", stmt.span);
                let cont_block = self.builder.new_block("if.cont", stmt.span);

                self.builder
                    .insert_inst(Inst::branch(cond_val, then_block, else_block, stmt.span));

                // === Then Block ===
                self.builder.switch_to_block(then_block);
                self.visit_stmt(*then);
                self.builder.insert_inst(Inst::jump(cont_block, stmt.span));

                // === Else Block ===
                self.builder.switch_to_block(else_block);
                self.visit_stmt(*otherwise);
                self.builder.insert_inst(Inst::jump(cont_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::While { cond, body } => {
                let cond_block = self.builder.new_block("while.cond", stmt.span);
                let body_block = self.builder.new_block("while.body", stmt.span);
                let cont_block = self.builder.new_block("while.cont", stmt.span);

                self.tracked_blocks.insert(
                    stmt_ref,
                    TrackedBlock::BreakAndContinue(cont_block, cond_block),
                );

                self.builder.insert_inst(Inst::jump(cond_block, stmt.span));

                // === Cond Block ===
                self.builder.switch_to_block(cond_block);
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                self.builder
                    .insert_inst(Inst::branch(cond_val, body_block, cont_block, stmt.span));

                // === Body Block ===
                self.builder.switch_to_block(body_block);
                self.visit_stmt(*body);
                self.builder.insert_inst(Inst::jump(cond_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::DoWhile { body, cond } => {
                let body_block = self.builder.new_block("do.body", stmt.span);
                let cond_block = self.builder.new_block("do.cond", stmt.span);
                let cont_block = self.builder.new_block("do.cont", stmt.span);

                self.tracked_blocks.insert(
                    stmt_ref,
                    TrackedBlock::BreakAndContinue(cont_block, cond_block),
                );

                // === Jump to Body Block ===
                self.builder.insert_inst(Inst::jump(body_block, stmt.span));

                // === Body Block ===
                self.builder.switch_to_block(body_block);
                self.visit_stmt(*body);
                self.builder.insert_inst(Inst::jump(cond_block, stmt.span));

                // === Cond Block ===
                self.builder.switch_to_block(cond_block);
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                self.builder
                    .insert_inst(Inst::branch(cond_val, body_block, cont_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::For {
                init,
                cond: None,
                step: None,
                body,
            } => {
                let body_block = self.builder.new_block("for.body", stmt.span);
                let cont_block = self.builder.new_block("for.cont", stmt.span);

                self.tracked_blocks.insert(
                    stmt_ref,
                    TrackedBlock::BreakAndContinue(cont_block, body_block),
                );

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                self.builder.insert_inst(Inst::jump(body_block, stmt.span));

                // === Body Block ===
                self.builder.switch_to_block(body_block);
                self.visit_stmt(*body);
                self.builder.insert_inst(Inst::jump(body_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::For {
                init,
                cond: Some(cond),
                step: None,
                body,
            } => {
                let cond_block = self.builder.new_block("for.cond", stmt.span);
                let body_block = self.builder.new_block("for.body", stmt.span);
                let cont_block = self.builder.new_block("for.cont", stmt.span);

                self.tracked_blocks.insert(
                    stmt_ref,
                    TrackedBlock::BreakAndContinue(cont_block, cond_block),
                );

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                self.builder.insert_inst(Inst::jump(cond_block, stmt.span));

                // === Cond Block ===
                self.builder.switch_to_block(cond_block);
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                self.builder
                    .insert_inst(Inst::branch(cond_val, body_block, cont_block, stmt.span));

                // === Body Block ===
                self.builder.switch_to_block(body_block);
                self.visit_stmt(*body);
                self.builder.insert_inst(Inst::jump(cond_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::For {
                init,
                cond: None,
                step: Some(step),
                body,
            } => {
                let step_block = self.builder.new_block("for.step", stmt.span);
                let body_block = self.builder.new_block("for.body", stmt.span);
                let cont_block = self.builder.new_block("for.cont", stmt.span);

                self.tracked_blocks.insert(
                    stmt_ref,
                    TrackedBlock::BreakAndContinue(cont_block, step_block),
                );

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                self.builder.insert_inst(Inst::jump(body_block, stmt.span));

                // === Step Block ===
                self.builder.switch_to_block(step_block);
                self.visit_expr(*step, ExprMode::RightValue);
                self.builder.insert_inst(Inst::jump(body_block, stmt.span));

                // === Body Block ===
                self.builder.switch_to_block(body_block);
                self.visit_stmt(*body);
                self.builder.insert_inst(Inst::jump(step_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::For {
                init,
                cond: Some(cond),
                step: Some(step),
                body,
            } => {
                let cond_block = self.builder.new_block("for.cond", stmt.span);
                let step_block = self.builder.new_block("for.step", stmt.span);
                let body_block = self.builder.new_block("for.body", stmt.span);
                let cont_block = self.builder.new_block("for.cont", stmt.span);

                self.tracked_blocks.insert(
                    stmt_ref,
                    TrackedBlock::BreakAndContinue(cont_block, step_block),
                );

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                self.builder.insert_inst(Inst::jump(cond_block, stmt.span));

                // === Cond Block ===
                self.builder.switch_to_block(cond_block);
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                self.builder
                    .insert_inst(Inst::branch(cond_val, body_block, cont_block, stmt.span));

                // === Step Block ===
                self.builder.switch_to_block(step_block);
                self.visit_expr(*step, ExprMode::RightValue);
                self.builder.insert_inst(Inst::jump(cond_block, stmt.span));

                // === Body Block ===
                self.builder.switch_to_block(body_block);
                self.visit_stmt(*body);
                self.builder.insert_inst(Inst::jump(step_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
            ast::StmtKind::Switch { cond, body } => {
                let mut cases = Vec::new();
                let mut default_block = None;
                if let Some(switch) = self.sema.switches.get(&stmt_ref) {
                    cases.reserve(switch.cases.len());
                    switch.cases.iter().for_each(|inner_ref| {
                        let inner = self.ast.stmt(*inner_ref);
                        if let ast::StmtKind::Case { expr, .. } = inner.kind {
                            let val = match self.ast.expr(expr).kind {
                                ast::ExprKind::Const(ConstValue::Int(c)) => c as i64,
                                _ => panic!("expected a constant expression"),
                            };
                            let case_block = self.builder.new_block("switch.case", inner.span);
                            self.tracked_blocks
                                .insert(*inner_ref, TrackedBlock::Case(case_block));
                            cases.push((val, case_block));
                        }
                    });
                    if let Some(inner) = switch.default {
                        let block = self
                            .builder
                            .new_block("switch.default", self.ast.stmt(inner).span);
                        self.tracked_blocks.insert(inner, TrackedBlock::Case(block));
                        default_block = Some(block);
                    }
                }

                let cont_block = self.builder.new_block("switch.cont", stmt.span);
                self.tracked_blocks.insert(
                    stmt_ref,
                    TrackedBlock::BreakAndContinue(cont_block, cont_block),
                );

                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                self.builder.insert_inst(Inst::switch(
                    cond_val,
                    default_block.unwrap_or(cont_block),
                    cases,
                    stmt.span,
                ));

                self.visit_stmt(*body);
                self.builder.insert_inst(Inst::jump(cont_block, stmt.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
            }
        }
    }

    fn visit_expr(&mut self, expr: ast::ExprRef, mode: ExprMode) -> InstRef {
        let expr = self.ast.expr(expr);
        match &expr.kind {
            ast::ExprKind::Grouped(expr) => self.visit_expr(*expr, mode),
            ast::ExprKind::Const(ConstValue::Int(c)) => self
                .builder
                .insert_inst(Inst::const_i32(*c as i64, expr.span)),
            ast::ExprKind::Const(ConstValue::Long(_)) => todo!("handle long constants"),
            ast::ExprKind::Var(name) => {
                let info = self
                    .sema
                    .symbol(name.sema.get())
                    .expect("expected a sema symbol");
                match info.attr {
                    Attribute::Function { .. } => todo!("handle function variables"),
                    Attribute::Static { .. } => {
                        let var = self.get_static(name.sema.get());
                        let ptr = self.builder.insert_inst(Inst::static_addr(var, expr.span));
                        match mode {
                            ExprMode::LeftValue => ptr,
                            ExprMode::RightValue => {
                                self.builder.insert_inst(Inst::load(ptr, expr.span))
                            }
                        }
                    }
                    Attribute::Local => {
                        let ptr = self.get_var(name.sema.get());
                        match mode {
                            ExprMode::LeftValue => ptr,
                            ExprMode::RightValue => {
                                self.builder.insert_inst(Inst::load(ptr, expr.span))
                            }
                        }
                    }
                }
            }
            ast::ExprKind::Call { name, args, .. } => {
                let args = self
                    .ast
                    .exprs(*args)
                    .iter()
                    .map(|arg| self.visit_expr(*arg, ExprMode::RightValue))
                    .collect::<Vec<_>>();
                let is_global = self
                    .sema
                    .symbol(name.sema.get())
                    .expect("expected a sema symbol")
                    .is_global();
                let func = self.get_or_make_function(name, is_global, expr.span);
                self.builder.insert_inst(Inst::call(func, args, expr.span))
            }
            ast::ExprKind::Cast { .. } => todo!("handle cast expressions"),
            ast::ExprKind::Unary { op, expr: inner } => match op {
                ast::UnaryOp::Neg => self.build_unary(UnaryOp::Neg, *inner, expr.span),
                ast::UnaryOp::BitNot => self.build_unary(UnaryOp::Not, *inner, expr.span),
                ast::UnaryOp::PreInc => self.build_prefix_unary(UnaryOp::Inc, *inner, expr.span),
                ast::UnaryOp::PreDec => self.build_prefix_unary(UnaryOp::Dec, *inner, expr.span),
                ast::UnaryOp::PostInc => self.build_postfix_unary(UnaryOp::Inc, *inner, expr.span),
                ast::UnaryOp::PostDec => self.build_postfix_unary(UnaryOp::Dec, *inner, expr.span),
                ast::UnaryOp::LogicalNot => {
                    let val = self.visit_expr(*inner, ExprMode::RightValue);
                    let zero = self.builder.insert_inst(Inst::const_i32(0, expr.span));
                    self.builder.insert_inst(Inst::binary(
                        Type::Int32,
                        BinaryOp::Equal,
                        val,
                        zero,
                        expr.span,
                    ))
                }
            },
            ast::ExprKind::Binary { op, lhs, rhs } => match op {
                ast::BinaryOp::LogicalOr => self.build_sc(LogicalOp::Or, *lhs, *rhs, expr.span),
                ast::BinaryOp::LogicalAnd => self.build_sc(LogicalOp::And, *lhs, *rhs, expr.span),
                ast::BinaryOp::Equal => self.build_bin(BinaryOp::Equal, *lhs, *rhs, expr.span),
                ast::BinaryOp::NotEqual => {
                    self.build_bin(BinaryOp::NotEqual, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::LessThan => {
                    self.build_bin(BinaryOp::LessThan, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::LessEqual => {
                    self.build_bin(BinaryOp::LessEqual, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::GreaterThan => {
                    self.build_bin(BinaryOp::GreaterThan, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::GreaterEqual => {
                    self.build_bin(BinaryOp::GreaterEqual, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::Add => self.build_bin(BinaryOp::Add, *lhs, *rhs, expr.span),
                ast::BinaryOp::Sub => self.build_bin(BinaryOp::Sub, *lhs, *rhs, expr.span),
                ast::BinaryOp::Mul => self.build_bin(BinaryOp::Mul, *lhs, *rhs, expr.span),
                ast::BinaryOp::Div => self.build_bin(BinaryOp::Div, *lhs, *rhs, expr.span),
                ast::BinaryOp::Rem => self.build_bin(BinaryOp::Rem, *lhs, *rhs, expr.span),
                ast::BinaryOp::BitOr => self.build_bin(BinaryOp::Or, *lhs, *rhs, expr.span),
                ast::BinaryOp::BitAnd => self.build_bin(BinaryOp::And, *lhs, *rhs, expr.span),
                ast::BinaryOp::BitXor => self.build_bin(BinaryOp::Xor, *lhs, *rhs, expr.span),
                ast::BinaryOp::BitShl => self.build_bin(BinaryOp::Shl, *lhs, *rhs, expr.span),
                ast::BinaryOp::BitShr => self.build_bin(BinaryOp::Shr, *lhs, *rhs, expr.span),
                ast::BinaryOp::AddAssign => {
                    self.build_bin_asgn(BinaryOp::Add, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::SubAssign => {
                    self.build_bin_asgn(BinaryOp::Sub, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::MulAssign => {
                    self.build_bin_asgn(BinaryOp::Mul, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::DivAssign => {
                    self.build_bin_asgn(BinaryOp::Div, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::RemAssign => {
                    self.build_bin_asgn(BinaryOp::Rem, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitOrAssign => {
                    self.build_bin_asgn(BinaryOp::Or, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitAndAssign => {
                    self.build_bin_asgn(BinaryOp::And, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitXorAssign => {
                    self.build_bin_asgn(BinaryOp::Xor, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitShlAssign => {
                    self.build_bin_asgn(BinaryOp::Shl, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitShrAssign => {
                    self.build_bin_asgn(BinaryOp::Shr, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::Assign => {
                    let lhs = self.visit_expr(*lhs, ExprMode::LeftValue);
                    let rhs = self.visit_expr(*rhs, ExprMode::RightValue);
                    self.builder.insert_inst(Inst::store(lhs, rhs, expr.span));
                    match mode {
                        ExprMode::LeftValue => lhs,
                        ExprMode::RightValue => rhs,
                    }
                }
            },
            ast::ExprKind::Ternary { cond, then, other } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);

                let then_block = self.builder.new_block("tern.then", expr.span);
                let else_block = self.builder.new_block("tern.else", expr.span);
                let cont_block = self.builder.new_block("tern.cont", expr.span);

                let phi = self
                    .builder
                    .insert_inst_skip(Inst::phi(Type::Int32, expr.span));

                self.builder
                    .insert_inst(Inst::branch(cond_val, then_block, else_block, expr.span));

                // === Then Block ===
                self.builder.switch_to_block(then_block);
                let then_val = self.visit_expr(*then, ExprMode::RightValue);
                self.builder
                    .insert_inst(Inst::upsilon(phi, then_val, expr.span));
                self.builder.insert_inst(Inst::jump(cont_block, expr.span));

                // === Else Block ===
                self.builder.switch_to_block(else_block);
                let else_val = self.visit_expr(*other, ExprMode::RightValue);
                self.builder
                    .insert_inst(Inst::upsilon(phi, else_val, expr.span));
                self.builder.insert_inst(Inst::jump(cont_block, expr.span));

                // === Merge Block ===
                self.builder.switch_to_block(cont_block);
                self.builder.insert_skipped(phi);
                phi
            }
        }
    }

    #[inline]
    fn build_unary(&mut self, op: UnaryOp, expr: ast::ExprRef, span: SourceSpan) -> InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        self.builder
            .insert_inst(Inst::unary(Type::Int32, op, val, span))
    }

    #[inline]
    fn build_prefix_unary(&mut self, op: UnaryOp, expr: ast::ExprRef, span: SourceSpan) -> InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .builder
            .insert_inst(Inst::unary(Type::Int32, op, val, span));
        let ptr = self.visit_expr(expr, ExprMode::LeftValue);
        self.builder.insert_inst(Inst::store(ptr, inst, span));
        inst
    }

    #[inline]
    fn build_postfix_unary(
        &mut self,
        op: UnaryOp,
        expr: ast::ExprRef,
        span: SourceSpan,
    ) -> InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .builder
            .insert_inst(Inst::unary(Type::Int32, op, val, span));
        let ptr = self.visit_expr(expr, ExprMode::LeftValue);
        self.builder.insert_inst(Inst::store(ptr, inst, span));
        val
    }

    #[inline]
    fn build_bin(
        &mut self,
        op: BinaryOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> InstRef {
        let lhs = self.visit_expr(lhs, ExprMode::RightValue);
        let rhs = self.visit_expr(rhs, ExprMode::RightValue);
        self.builder
            .insert_inst(Inst::binary(Type::Int32, op, lhs, rhs, span))
    }

    #[inline]
    fn build_bin_asgn(
        &mut self,
        op: BinaryOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> InstRef {
        let l = self.visit_expr(lhs, ExprMode::RightValue);
        let r = self.visit_expr(rhs, ExprMode::RightValue);
        let inst = self
            .builder
            .insert_inst(Inst::binary(Type::Int32, op, l, r, span));

        let ptr = self.visit_expr(lhs, ExprMode::LeftValue);
        self.builder.insert_inst(Inst::store(ptr, inst, span));

        inst
    }

    fn build_sc(
        &mut self,
        op: LogicalOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> InstRef {
        let rhs_block = self.builder.new_block(
            match op {
                LogicalOp::Or => "or",
                LogicalOp::And => "and",
            },
            span,
        );
        let cont_block = self.builder.new_block(
            match op {
                LogicalOp::Or => "or.cont",
                LogicalOp::And => "and.cont",
            },
            span,
        );

        let phi = self.builder.insert_inst_skip(Inst::phi(Type::Int32, span));

        // === LHS Block ===
        let lhs_val = self.visit_expr(lhs, ExprMode::RightValue);
        let short_circuit_val = self.builder.insert_inst(match op {
            LogicalOp::Or => Inst::const_i32(1, span),
            LogicalOp::And => Inst::const_i32(0, span),
        });
        self.builder
            .insert_inst(Inst::upsilon(phi, short_circuit_val, span));
        let (true_target, false_target) = match op {
            LogicalOp::Or => (cont_block, rhs_block),
            LogicalOp::And => (rhs_block, cont_block),
        };
        self.builder
            .insert_inst(Inst::branch(lhs_val, true_target, false_target, span));

        // === RHS Block ===
        self.builder.switch_to_block(rhs_block);
        let rhs_val = self.visit_expr(rhs, ExprMode::RightValue);
        let zero_val = self.builder.insert_inst(Inst::const_i32(0, span));
        let is_nonzero = self.builder.insert_inst(Inst::binary(
            Type::Int32,
            BinaryOp::NotEqual,
            rhs_val,
            zero_val,
            span,
        ));
        self.builder
            .insert_inst(Inst::upsilon(phi, is_nonzero, span));
        self.builder.insert_inst(Inst::jump(cont_block, span));

        // === Merge Block ===
        self.builder.switch_to_block(cont_block);
        self.builder.insert_skipped(phi);
        phi
    }

    // ---------------------------------------------------------------------------
    // Auxiliary Methods
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn get_var(&self, sym: SemaSymbol) -> InstRef {
        match self.symbols[sym.0.get() as usize] {
            SymbolEntry::Inst(inst) => inst,
            _ => panic!("expected an inst ref"),
        }
    }

    #[inline]
    pub fn get_static(&self, sym: SemaSymbol) -> StaticVarRef {
        match self.symbols[sym.0.get() as usize] {
            SymbolEntry::Static(v) => v,
            _ => panic!("expected a static var ref"),
        }
    }

    #[inline]
    pub fn insert_var(&mut self, sym: SemaSymbol, var: InstRef) {
        self.symbols[sym.0.get() as usize] = SymbolEntry::Inst(var);
    }

    #[inline]
    pub fn get_break_block(&self, stmt: StmtRef) -> BlockRef {
        match self.tracked_blocks.get(&stmt) {
            Some(TrackedBlock::BreakAndContinue(b, _)) => *b,
            _ => panic!("expected a break block"),
        }
    }

    #[inline]
    pub fn get_continue_block(&self, stmt: StmtRef) -> BlockRef {
        match self.tracked_blocks.get(&stmt) {
            Some(TrackedBlock::BreakAndContinue(_, c)) => *c,
            _ => panic!("expected a break block"),
        }
    }

    #[inline]
    pub fn remove_case_block(&mut self, stmt: StmtRef) -> BlockRef {
        match self.tracked_blocks.remove(&stmt) {
            Some(TrackedBlock::Case(c)) => c,
            _ => panic!("expected a break block"),
        }
    }

    #[inline]
    pub fn get_or_make_labeled(
        &mut self,
        stmt: StmtRef,
        name: Symbol,
        span: SourceSpan,
    ) -> BlockRef {
        let entry = self
            .tracked_blocks
            .entry(stmt)
            .or_insert_with(|| TrackedBlock::Label(self.builder.new_block_interned(name, span)));
        match entry {
            TrackedBlock::Label(l) => *l,
            _ => panic!("expected a labeled block"),
        }
    }

    #[inline]
    pub fn get_or_make_function(
        &mut self,
        name: &AstSymbol,
        is_global: bool,
        span: SourceSpan,
    ) -> FuncRef {
        let idx = name.sema.get().0.get() as usize;
        match self.symbols[idx] {
            SymbolEntry::Inst(_) | SymbolEntry::Static(_) => panic!("expected a function"),
            SymbolEntry::Function(f) => f,
            SymbolEntry::None => {
                let f = self.builder.prog.new_func(Func {
                    span,
                    is_global,
                    name: name.raw,
                    ..Default::default()
                });
                self.symbols[idx] = SymbolEntry::Function(f);
                f
            }
        }
    }

    #[inline]
    pub fn get_or_make_static(
        &mut self,
        ty: Type,
        name: &AstSymbol,
        is_global: bool,
        init: Option<i64>,
        span: SourceSpan,
    ) -> StaticVarRef {
        let idx = name.sema.get().0.get() as usize;
        match self.symbols[idx] {
            SymbolEntry::Inst(_) | SymbolEntry::Function(_) => panic!("expected a static"),
            SymbolEntry::Static(v) => v,
            SymbolEntry::None => {
                let func = self.builder.func();
                let name = match func {
                    Some(func) if !is_global => {
                        let fname = self.builder.prog.func(func).name;
                        let fname = self.builder.prog.interner.lookup(fname);
                        let name = self.builder.prog.interner.lookup(name.raw);
                        let scoped = format!("{fname}.{name}");
                        self.builder.prog.interner.intern(&scoped)
                    }
                    _ => name.raw,
                };
                let v = self.builder.prog.new_static_var(StaticVar {
                    ty,
                    name,
                    init,
                    span,
                    is_global,
                });
                self.symbols[idx] = SymbolEntry::Static(v);
                v
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary Structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LogicalOp {
    Or,
    And,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprMode {
    LeftValue,
    RightValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrackedBlock {
    Case(BlockRef),
    Label(BlockRef),
    BreakAndContinue(BlockRef, BlockRef),
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum SymbolEntry {
    #[default]
    None,
    Inst(InstRef),
    Function(FuncRef),
    Static(StaticVarRef),
}
