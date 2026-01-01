use crate::{
    ast::{self, Const},
    sema::{self, Attribute, SemaCtx},
};

use jcc_entity::SecondaryMap;
use jcc_ssa::{
    codemap::span::Span,
    ir::{
        builder::Builder,
        inst::{BinaryOp, ICmpOp, Inst, UnaryOp},
        ty::Ty,
        Block, Function, FunctionData, Global, GlobalData, Program, Value,
    },
    Ident, IdentInterner,
};

use std::collections::HashMap;

pub struct SSABuilder<'ctx> {
    // The SSA builder
    builder: Builder<'ctx>,
    /// The AST being compiled
    ast: &'ctx ast::Ast<'ctx>,
    /// The semantic analysis context
    sema: &'ctx SemaCtx<'ctx>,
    /// Blocks tracked for statements
    tracked: HashMap<ast::Stmt, TrackedBlock>,
    /// Mapping from semantic symbols to SSA symbols
    symbols: SecondaryMap<sema::Symbol, Option<SymbolEntry>>,
}

impl<'ctx> SSABuilder<'ctx> {
    pub fn new(
        ast: &'ctx ast::Ast<'ctx>,
        sema: &'ctx SemaCtx<'ctx>,
        interner: &'ctx mut IdentInterner,
    ) -> Self {
        Self {
            ast,
            sema,
            tracked: HashMap::new(),
            builder: Builder::new(interner),
            symbols: SecondaryMap::with_capacity(sema.symbols.len()),
        }
    }

    pub fn build(mut self) -> Program {
        self.ast.root.iter().for_each(|decl| {
            let data = &self.ast.decl[*decl];
            match data.kind {
                ast::DeclKind::Var(_) => {
                    let info = self.sema.symbols
                        [data.name.sema.get().expect("sema symbol not set")]
                    .expect("expected a sema symbol");

                    match info.attr {
                        Attribute::Local => panic!("unexpected local attribute"),
                        Attribute::Function { .. } => panic!("unexpected function attribute"),
                        Attribute::Static { is_global, init } => {
                            self.get_or_make_global(
                                &data.name,
                                GlobalData {
                                    is_global,
                                    span: data.span,
                                    init: init.value(),
                                    name: data.name.name,
                                    ty: data.ty.as_ref().into(),
                                },
                            );
                        }
                    }
                }
                ast::DeclKind::Func { params, body } => {
                    let is_global = self.sema.symbols
                        [data.name.sema.get().expect("sema symbol not set")]
                    .expect("expected a sema symbol")
                    .is_global();

                    let func = self.get_or_make_function(&data.name, is_global, data.span);
                    self.builder.function = Some(func);

                    if body.is_some() {
                        let block = self.builder.build_block("entry", data.span);
                        self.builder.block = Some(block);
                    }

                    if let Some(body) = body {
                        self.tracked.clear();

                        self.ast.decls[params]
                            .iter()
                            .enumerate()
                            .for_each(|(idx, param)| {
                                let param = &self.ast.decl[*param];
                                let ty = match *param.ty {
                                    ast::TyKind::Int => Ty::I32,
                                    ast::TyKind::Long => Ty::I64,
                                    _ => todo!(),
                                };
                                let arg = self
                                    .builder
                                    .build_val(Inst::param(ty, idx as u32), param.span);
                                let sema = param.name.sema.get().expect("sema symbol not set");
                                self.symbols[sema] = Some(SymbolEntry::Value(arg));
                            });

                        self.ast.items[body].iter().for_each(|item| match item {
                            ast::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                            ast::BlockItem::Decl(decl) => self.visit_decl(*decl),
                        });

                        let append_return = match self.ast.items[body].last() {
                            Some(ast::BlockItem::Stmt(stmt)) => {
                                !matches!(self.ast.stmt[*stmt].kind, ast::StmtKind::Return(_))
                            }
                            _ => true,
                        };

                        if append_return {
                            let val = self
                                .builder
                                .build_val(Inst::const_int(Ty::I32, 0), data.span);
                            self.builder.build_val(Inst::ret(Some(val)), data.span);
                        }
                    }
                    self.builder.function = None;
                }
            }
        });
        self.builder.finish()
    }

    fn visit_decl(&mut self, decl: ast::Decl) {
        let data = &self.ast.decl[decl];
        match data.kind {
            ast::DeclKind::Func { .. } => {}
            ast::DeclKind::Var(init) => {
                let sema = data.name.sema.get().expect("sema symbol not set");
                let info = self.sema.symbols[sema].expect("expected a sema symbol");
                match info.attr {
                    Attribute::Function { .. } => panic!("unexpected function attribute"),
                    Attribute::Local => {
                        let alloca = self
                            .builder
                            .build_val(Inst::alloca(data.ty.as_ref().into(), 0), data.span);
                        self.symbols[sema] = Some(SymbolEntry::Value(alloca));
                        if let Some(init) = init {
                            let value = self.visit_expr_rvalue(init);
                            self.builder
                                .build_val(Inst::store(alloca, value, 0), data.span);
                        }
                    }
                    Attribute::Static { is_global, init } => {
                        self.get_or_make_global(
                            &data.name,
                            GlobalData {
                                is_global,
                                span: data.span,
                                init: init.value(),
                                name: data.name.name,
                                ty: data.ty.as_ref().into(),
                            },
                        );
                    }
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: ast::Stmt) {
        let data = &self.ast.stmt[stmt];
        match &data.kind {
            ast::StmtKind::Empty => {}
            ast::StmtKind::Expr(expr) => {
                self.visit_expr_rvalue(*expr);
            }
            ast::StmtKind::Default(inner) => {
                let block = self.remove_case_block(stmt);

                // === Default Block ===
                self.builder.block = Some(block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Return(expr) => {
                let val = self.visit_expr_rvalue(*expr);
                self.builder.build_val(Inst::ret(Some(val)), data.span);
            }
            ast::StmtKind::Break(target) => {
                let block = self.get_break_block(target.get().expect("break block not set"));
                self.builder.build_val(Inst::br(block), data.span);
            }
            ast::StmtKind::Continue(target) => {
                let block = self.get_continue_block(target.get().expect("continue block not set"));
                self.builder.build_val(Inst::br(block), data.span);
            }
            ast::StmtKind::Compound(items) => {
                self.ast.items[*items].iter().for_each(|item| match item {
                    ast::BlockItem::Decl(decl) => self.visit_decl(*decl),
                    ast::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                });
            }
            ast::StmtKind::Goto {
                label,
                stmt: target,
            } => {
                let block = self.get_or_make_labeled(
                    target.get().expect("goto block not set"),
                    *label,
                    data.span,
                );
                self.builder.build_val(Inst::br(block), data.span);
            }
            ast::StmtKind::Label { label, stmt: inner } => {
                let block = self.get_or_make_labeled(stmt, *label, data.span);
                self.builder.build_val(Inst::br(block), data.span);

                // === Labeled Block ===
                self.builder.block = Some(block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Case { stmt: inner, .. } => {
                let block = self.remove_case_block(stmt);

                // === Previous Block ===
                self.builder.build_val(Inst::br(block), data.span);

                // === Case Block ===
                self.builder.block = Some(block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                let then_block = self.builder.build_block("if.then", data.span);
                let cont_block = self.builder.build_block("if.cont", data.span);
                let else_block = otherwise.map(|_| self.builder.build_block("if.else", data.span));
                let false_target = else_block.unwrap_or(cont_block);

                let cond_val = self.visit_expr_rvalue(*cond);
                self.builder
                    .build_val(Inst::cond_br(cond_val, then_block, false_target), data.span);

                // === Then Block ===
                self.builder.block = Some(then_block);
                self.visit_stmt(*then);
                self.builder.build_val(Inst::br(cont_block), data.span);

                if let (Some(otherwise), Some(else_block)) = (otherwise, else_block) {
                    // === Else Block ===
                    self.builder.block = Some(else_block);
                    self.visit_stmt(*otherwise);
                    self.builder.build_val(Inst::br(cont_block), data.span);
                }

                // === Merge Block ===
                self.builder.block = Some(cont_block);
            }
            ast::StmtKind::While { cond, body } => {
                let cond_block = self.builder.build_block("while.cond", data.span);
                let body_block = self.builder.build_block("while.body", data.span);
                let cont_block = self.builder.build_block("while.cont", data.span);

                self.tracked
                    .insert(stmt, TrackedBlock::BreakAndContinue(cont_block, cond_block));

                self.builder.build_val(Inst::br(cond_block), data.span);

                // === Cond Block ===
                self.builder.block = Some(cond_block);
                let cond_val = self.visit_expr_rvalue(*cond);
                self.builder
                    .build_val(Inst::cond_br(cond_val, body_block, cont_block), data.span);

                // === Body Block ===
                self.builder.block = Some(body_block);
                self.visit_stmt(*body);
                self.builder.build_val(Inst::br(cond_block), data.span);

                // === Merge Block ===
                self.builder.block = Some(cont_block);
            }
            ast::StmtKind::DoWhile { body, cond } => {
                let body_block = self.builder.build_block("do.body", data.span);
                let cond_block = self.builder.build_block("do.cond", data.span);
                let cont_block = self.builder.build_block("do.cont", data.span);

                self.tracked
                    .insert(stmt, TrackedBlock::BreakAndContinue(cont_block, cond_block));

                // === Jump to Body Block ===
                self.builder.build_val(Inst::br(body_block), data.span);

                // === Body Block ===
                self.builder.block = Some(body_block);
                self.visit_stmt(*body);
                self.builder.build_val(Inst::br(cond_block), data.span);

                // === Cond Block ===
                self.builder.block = Some(cond_block);
                let cond_val = self.visit_expr_rvalue(*cond);
                self.builder
                    .build_val(Inst::cond_br(cond_val, body_block, cont_block), data.span);

                // === Merge Block ===
                self.builder.block = Some(cont_block);
            }
            ast::StmtKind::For {
                init,
                cond,
                step,
                body,
            } => {
                let body_block = self.builder.build_block("for.body", data.span);
                let exit_block = self.builder.build_block("for.exit", data.span);
                let cond_block = cond.map(|_| self.builder.build_block("for.cond", data.span));
                let step_block = step.map(|_| self.builder.build_block("for.step", data.span));
                let after_body_target = step_block.or(cond_block).unwrap_or(body_block);

                self.tracked.insert(
                    stmt,
                    TrackedBlock::BreakAndContinue(exit_block, after_body_target),
                );

                // === Initializer ===
                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => self.visit_decl(*decl),
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr_rvalue(*expr);
                        }
                    }
                }
                self.builder
                    .build_val(Inst::br(cond_block.unwrap_or(body_block)), data.span);

                // === Condition Block ===
                if let (Some(cond), Some(cond_block)) = (cond, cond_block) {
                    self.builder.block = Some(cond_block);
                    let cond_val = self.visit_expr_rvalue(*cond);
                    self.builder
                        .build_val(Inst::cond_br(cond_val, body_block, exit_block), data.span);
                }

                // === Step Block ===
                if let (Some(step), Some(step_block)) = (step, step_block) {
                    self.builder.block = Some(step_block);
                    self.visit_expr_rvalue(*step);
                    self.builder
                        .build_val(Inst::br(cond_block.unwrap_or(body_block)), data.span);
                }

                // === Body Block ===
                self.builder.block = Some(body_block);
                self.visit_stmt(*body);
                self.builder
                    .build_val(Inst::br(after_body_target), data.span);

                // === Merge Block ===
                self.builder.block = Some(exit_block);
            }
            ast::StmtKind::Switch { cond, body } => {
                let mut cases = Vec::new();
                let mut default_block = None;
                if let Some(switch) = self.sema.switches.get(&stmt) {
                    cases.reserve(switch.cases.len());
                    switch.cases.iter().for_each(|inner_ref| {
                        let inner = &self.ast.stmt[*inner_ref];
                        if let ast::StmtKind::Case { expr, .. } = inner.kind {
                            let val = match self.ast.expr[expr].kind {
                                ast::ExprKind::Const(Const::Long(c)) => c,
                                ast::ExprKind::Const(Const::Int(c)) => c as i64,
                                _ => panic!("expected a constant expression"),
                            };
                            let case_block = self.builder.build_block("switch.case", inner.span);
                            self.tracked
                                .insert(*inner_ref, TrackedBlock::Case(case_block));
                            cases.push((val, case_block));
                        }
                    });
                    if let Some(inner) = switch.default {
                        let block = self
                            .builder
                            .build_block("switch.default", self.ast.stmt[inner].span);
                        self.tracked.insert(inner, TrackedBlock::Case(block));
                        default_block = Some(block);
                    }
                }

                let cont_block = self.builder.build_block("switch.cont", data.span);
                self.tracked
                    .insert(stmt, TrackedBlock::BreakAndContinue(cont_block, cont_block));

                let cond_val = self.visit_expr_rvalue(*cond);
                self.builder.build_val(
                    Inst::switch(cond_val, default_block.unwrap_or(cont_block), cases),
                    data.span,
                );

                self.visit_stmt(*body);
                self.builder.build_val(Inst::br(cont_block), data.span);

                // === Merge Block ===
                self.builder.block = Some(cont_block);
            }
        }
    }

    fn visit_expr_lvalue(&mut self, expr: ast::Expr) -> Value {
        self.visit_expr_inner(expr, ExprMode::LValue)
    }

    fn visit_expr_rvalue(&mut self, expr: ast::Expr) -> Value {
        self.visit_expr_inner(expr, ExprMode::RValue)
    }

    fn visit_expr_inner(&mut self, expr: ast::Expr, mode: ExprMode) -> Value {
        let expr = &self.ast.expr[expr];
        let ty = expr.ty.get().as_ref().into();
        match &expr.kind {
            ast::ExprKind::Grouped(expr) => self.visit_expr_inner(*expr, mode),
            ast::ExprKind::Const(Const::Long(c)) => self
                .builder
                .build_val(Inst::const_int(Ty::I64, *c), expr.span),
            ast::ExprKind::Const(Const::ULong(c)) => self
                .builder
                .build_val(Inst::const_int(Ty::I64, *c as i64), expr.span),
            ast::ExprKind::Const(Const::Int(c)) => self
                .builder
                .build_val(Inst::const_int(Ty::I32, *c as i64), expr.span),
            ast::ExprKind::Const(Const::UInt(c)) => self
                .builder
                .build_val(Inst::const_int(Ty::I32, *c as i64), expr.span),
            ast::ExprKind::Var(name) => {
                let span = expr.span;
                let info = self.sema.symbols[name.sema.get().expect("sema symbol not set")]
                    .expect("expected a sema symbol");
                match info.attr {
                    Attribute::Function { .. } => todo!("handle function variables"),
                    Attribute::Local => {
                        let ptr = self.get_value(name.sema.get().expect("sema symbol not set"));
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => match *info.ty {
                                ast::TyKind::Int => {
                                    self.builder.build_val(Inst::load(Ty::I32, ptr, 0), span)
                                }
                                ast::TyKind::Long => {
                                    self.builder.build_val(Inst::load(Ty::I64, ptr, 0), span)
                                }
                                _ => todo!("handle other types"),
                            },
                        }
                    }
                    Attribute::Static { .. } => {
                        let var = self.get_global(name.sema.get().expect("sema symbol not set"));
                        let ptr = self.builder.build_val(Inst::global_addr(var), expr.span);
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => match *info.ty {
                                ast::TyKind::Int => {
                                    self.builder.build_val(Inst::load(Ty::I32, ptr, 0), span)
                                }
                                ast::TyKind::Long => {
                                    self.builder.build_val(Inst::load(Ty::I64, ptr, 0), span)
                                }
                                _ => todo!("handle other types"),
                            },
                        }
                    }
                }
            }
            ast::ExprKind::Cast { ty, expr: inner } => {
                let inner_ty = self.ast.expr[*inner].ty.get();
                let val = self.visit_expr_rvalue(*inner);
                if *ty == self.sema.ty.int_ty && inner_ty == self.sema.ty.long_ty {
                    self.builder.build_val(Inst::trunc(Ty::I32, val), expr.span)
                } else if *ty == self.sema.ty.long_ty && inner_ty == self.sema.ty.int_ty {
                    self.builder.build_val(Inst::sext(Ty::I64, val), expr.span)
                } else {
                    val
                }
            }
            ast::ExprKind::Call { name, args, .. } => {
                let args = self.ast.exprs[*args]
                    .iter()
                    .map(|arg| self.visit_expr_rvalue(*arg))
                    .collect::<Vec<_>>();
                let symbol = self.sema.symbols[name.sema.get().expect("sema symbol not set")]
                    .expect("expected a sema symbol");
                let func = self.get_or_make_function(name, symbol.is_global(), expr.span);
                let ty = match *symbol.ty {
                    ast::TyKind::Func { ret, .. } => ret.as_ref().into(),
                    _ => panic!("expected a function type"),
                };
                self.builder
                    .build_val(Inst::call(ty, func, args), expr.span)
            }
            ast::ExprKind::Unary { op, expr: inner } => match op {
                ast::UnaryOp::PreInc => self.build_prefix_incdec(*inner, true, expr.span),
                ast::UnaryOp::PreDec => self.build_prefix_incdec(*inner, false, expr.span),
                ast::UnaryOp::PostInc => self.build_postfix_incdec(*inner, true, expr.span),
                ast::UnaryOp::PostDec => self.build_postfix_incdec(*inner, false, expr.span),
                ast::UnaryOp::Neg => self.build_unary(ty, UnaryOp::Neg, *inner, expr.span),
                ast::UnaryOp::BitNot => self.build_unary(ty, UnaryOp::Not, *inner, expr.span),
                ast::UnaryOp::LogNot => {
                    let val = self.visit_expr_rvalue(*inner);
                    let zero = self
                        .builder
                        .build_val(Inst::const_int(Ty::I32, 0), expr.span);
                    let cmp = self
                        .builder
                        .build_val(Inst::icmp(ICmpOp::Eq, val, zero), expr.span);
                    self.builder.build_val(Inst::zext(Ty::I32, cmp), expr.span)
                }
            },
            ast::ExprKind::Binary { op, lhs, rhs } => {
                let rhs_ty = self.ast.expr[*rhs].ty.get();
                match op {
                    ast::BinaryOp::LogOr => self.build_sc(true, *lhs, *rhs, expr.span),
                    ast::BinaryOp::LogAnd => self.build_sc(false, *lhs, *rhs, expr.span),
                    ast::BinaryOp::Eq => {
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.builder
                            .build_val(Inst::icmp(ICmpOp::Eq, lhs, rhs), expr.span)
                    }
                    ast::BinaryOp::Ne => {
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.builder
                            .build_val(Inst::icmp(ICmpOp::Ne, lhs, rhs), expr.span)
                    }
                    ast::BinaryOp::Lt => {
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.builder
                            .build_val(Inst::icmp(ICmpOp::Lt, lhs, rhs), expr.span)
                    }

                    ast::BinaryOp::Le => {
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.builder
                            .build_val(Inst::icmp(ICmpOp::Le, lhs, rhs), expr.span)
                    }

                    ast::BinaryOp::Gt => {
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.builder
                            .build_val(Inst::icmp(ICmpOp::Gt, lhs, rhs), expr.span)
                    }

                    ast::BinaryOp::Ge => {
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.builder
                            .build_val(Inst::icmp(ICmpOp::Ge, lhs, rhs), expr.span)
                    }

                    ast::BinaryOp::Add => self.build_bin(ty, BinaryOp::Add, *lhs, *rhs, expr.span),
                    ast::BinaryOp::Sub => self.build_bin(ty, BinaryOp::Sub, *lhs, *rhs, expr.span),
                    ast::BinaryOp::Mul => self.build_bin(ty, BinaryOp::Mul, *lhs, *rhs, expr.span),
                    ast::BinaryOp::Div => self.build_bin(ty, BinaryOp::Div, *lhs, *rhs, expr.span),
                    ast::BinaryOp::Rem => self.build_bin(ty, BinaryOp::Rem, *lhs, *rhs, expr.span),
                    ast::BinaryOp::BitOr => self.build_bin(ty, BinaryOp::Or, *lhs, *rhs, expr.span),
                    ast::BinaryOp::BitAnd => {
                        self.build_bin(ty, BinaryOp::And, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitXor => {
                        self.build_bin(ty, BinaryOp::Xor, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitShl => {
                        self.build_bin(ty, BinaryOp::Shl, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitShr => {
                        self.build_bin(ty, BinaryOp::Shr, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::AddAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::Add, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::SubAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::Sub, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::MulAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::Mul, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::DivAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::Div, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::RemAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::Rem, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitOrAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::Or, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitAndAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::And, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitXorAssign => {
                        self.build_bin_asgn(mode, rhs_ty, BinaryOp::Xor, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitShlAssign => {
                        let ptr = self.visit_expr_lvalue(*lhs);
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        let inst = self
                            .builder
                            .build_val(Inst::binary(BinaryOp::Shl, ty, lhs, rhs), expr.span);
                        self.builder.build_val(Inst::store(ptr, inst, 0), expr.span);
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => inst,
                        }
                    }
                    ast::BinaryOp::BitShrAssign => {
                        let ptr = self.visit_expr_lvalue(*lhs);
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        let inst = self
                            .builder
                            .build_val(Inst::binary(BinaryOp::Shr, ty, lhs, rhs), expr.span);
                        self.builder.build_val(Inst::store(ptr, inst, 0), expr.span);
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => inst,
                        }
                    }
                    ast::BinaryOp::Assign => {
                        let lhs = self.visit_expr_lvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.builder.build_val(Inst::store(lhs, rhs, 0), expr.span);
                        match mode {
                            ExprMode::LValue => lhs,
                            ExprMode::RValue => rhs,
                        }
                    }
                }
            }
            ast::ExprKind::Ternary { cond, then, other } => {
                let cond_val = self.visit_expr_rvalue(*cond);

                let then_block = self.builder.build_block("tern.then", expr.span);
                let else_block = self.builder.build_block("tern.else", expr.span);
                let cont_block = self.builder.build_block("tern.cont", expr.span);

                let phi = self.builder.build_phi(ty, expr.span);

                self.builder
                    .build_val(Inst::cond_br(cond_val, then_block, else_block), expr.span);

                // === Then Block ===
                self.builder.block = Some(then_block);
                let then_val = self.visit_expr_rvalue(*then);
                self.builder
                    .build_val(Inst::upsilon(phi, then_val), expr.span);
                self.builder.build_val(Inst::br(cont_block), expr.span);

                // === Else Block ===
                self.builder.block = Some(else_block);
                let else_val = self.visit_expr_rvalue(*other);
                self.builder
                    .build_val(Inst::upsilon(phi, else_val), expr.span);
                self.builder.build_val(Inst::br(cont_block), expr.span);

                // === Merge Block ===
                self.builder.block = Some(cont_block);
                self.builder.push(phi);
                phi
            }
        }
    }

    fn build_unary(&mut self, ty: Ty, op: UnaryOp, expr: ast::Expr, span: Span) -> Value {
        let val = self.visit_expr_rvalue(expr);
        self.builder.build_val(Inst::unary(op, ty, val), span)
    }

    fn build_prefix_incdec(&mut self, expr: ast::Expr, is_inc: bool, span: Span) -> Value {
        let obj_ty = self.ast.expr[expr].ty.get();
        let ssa_obj_ty = obj_ty.as_ref().into();

        let ptr = self.visit_expr_lvalue(expr);
        let old = self.builder.build_val(Inst::load(ssa_obj_ty, ptr, 0), span);

        let arith_ty = obj_ty;
        let promoted = old;

        let one = self
            .builder
            .build_val(Inst::const_int(arith_ty.as_ref().into(), 1), span);

        let new = self.builder.build_val(
            Inst::binary(
                if is_inc { BinaryOp::Add } else { BinaryOp::Sub },
                arith_ty.as_ref().into(),
                promoted,
                one,
            ),
            span,
        );

        self.builder.build_val(Inst::store(ptr, new, 0), span);

        new
    }

    fn build_postfix_incdec(&mut self, expr: ast::Expr, is_inc: bool, span: Span) -> Value {
        let obj_ty = self.ast.expr[expr].ty.get();
        let ssa_obj_ty = obj_ty.as_ref().into();

        let ptr = self.visit_expr_lvalue(expr);

        let old = self.builder.build_val(Inst::load(ssa_obj_ty, ptr, 0), span);

        let one = self.builder.build_val(Inst::const_int(ssa_obj_ty, 1), span);

        let new = self.builder.build_val(
            Inst::binary(
                if is_inc { BinaryOp::Add } else { BinaryOp::Sub },
                ssa_obj_ty,
                old,
                one,
            ),
            span,
        );

        self.builder.build_val(Inst::store(ptr, new, 0), span);

        old
    }

    fn build_bin(
        &mut self,
        ty: Ty,
        op: BinaryOp,
        lhs: ast::Expr,
        rhs: ast::Expr,
        span: Span,
    ) -> Value {
        let lhs = self.visit_expr_rvalue(lhs);
        let rhs = self.visit_expr_rvalue(rhs);
        self.builder.build_val(Inst::binary(op, ty, lhs, rhs), span)
    }

    fn build_bin_asgn(
        &mut self,
        mode: ExprMode,
        ty: ast::Ty<'ctx>,
        op: BinaryOp,
        lhs: ast::Expr,
        rhs: ast::Expr,
        span: Span,
    ) -> Value {
        let lty = self.ast.expr[lhs].ty.get();
        let mut l = self.visit_expr_rvalue(lhs);
        if lty == self.sema.ty.int_ty && ty == self.sema.ty.long_ty {
            l = self.builder.build_val(Inst::sext(Ty::I64, l), span);
        }

        let rty = self.ast.expr[rhs].ty.get();
        let r = self.visit_expr_rvalue(rhs);
        let mut value = self
            .builder
            .build_val(Inst::binary(op, rty.as_ref().into(), l, r), span);
        if lty == self.sema.ty.int_ty && rty == self.sema.ty.long_ty {
            value = self.builder.build_val(Inst::trunc(Ty::I32, value), span);
        }

        let ptr = self.visit_expr_lvalue(lhs);
        self.builder.build_val(Inst::store(ptr, value, 0), span);

        match mode {
            ExprMode::LValue => ptr,
            ExprMode::RValue => value,
        }
    }

    fn build_sc(&mut self, is_or: bool, lhs: ast::Expr, rhs: ast::Expr, span: Span) -> Value {
        let rhs_block = self
            .builder
            .build_block(if is_or { "or" } else { "and" }, span);
        let cont_block = self
            .builder
            .build_block(if is_or { "or.cont" } else { "and.cont" }, span);

        let phi = self.builder.build_phi(Ty::I32, span);

        // === LHS Block ===
        let lhs_val = self.visit_expr_rvalue(lhs);
        let short_circuit_val = self.builder.build_val(
            if is_or {
                Inst::const_int(Ty::I32, 1)
            } else {
                Inst::const_int(Ty::I32, 0)
            },
            span,
        );
        self.builder
            .build_val(Inst::upsilon(phi, short_circuit_val), span);
        let (true_target, false_target) = if is_or {
            (cont_block, rhs_block)
        } else {
            (rhs_block, cont_block)
        };
        self.builder
            .build_val(Inst::cond_br(lhs_val, true_target, false_target), span);

        // === RHS Block ===
        self.builder.block = Some(rhs_block);
        let rhs_val = self.visit_expr_rvalue(rhs);
        let zero_val = self.builder.build_val(Inst::const_int(Ty::I32, 0), span);
        let is_nonzero = self
            .builder
            .build_val(Inst::icmp(ICmpOp::Ne, rhs_val, zero_val), span);
        self.builder.build_val(Inst::upsilon(phi, is_nonzero), span);
        self.builder.build_val(Inst::br(cont_block), span);

        // === Merge Block ===
        self.builder.block = Some(cont_block);
        self.builder.push(phi);
        phi
    }

    // ---------------------------------------------------------------------------
    // Auxiliary Methods
    // ---------------------------------------------------------------------------

    fn get_value(&self, sym: sema::Symbol) -> Value {
        match self.symbols[sym] {
            Some(SymbolEntry::Value(value)) => value,
            _ => panic!("expected an inst ref"),
        }
    }

    fn get_global(&self, sym: sema::Symbol) -> Global {
        match self.symbols[sym] {
            Some(SymbolEntry::Global(v)) => v,
            _ => panic!("expected a static var ref"),
        }
    }

    fn get_break_block(&self, stmt: ast::Stmt) -> Block {
        match self.tracked.get(&stmt) {
            Some(TrackedBlock::BreakAndContinue(b, _)) => *b,
            _ => panic!("expected a break block"),
        }
    }

    fn get_continue_block(&self, stmt: ast::Stmt) -> Block {
        match self.tracked.get(&stmt) {
            Some(TrackedBlock::BreakAndContinue(_, c)) => *c,
            _ => panic!("expected a break block"),
        }
    }

    fn remove_case_block(&mut self, stmt: ast::Stmt) -> Block {
        match self.tracked.remove(&stmt) {
            Some(TrackedBlock::Case(c)) => c,
            _ => panic!("expected a break block"),
        }
    }

    fn get_or_make_labeled(&mut self, stmt: ast::Stmt, name: Ident, span: Span) -> Block {
        let entry = self
            .tracked
            .entry(stmt)
            .or_insert_with(|| TrackedBlock::Label(self.builder.build_block_sym(name, span)));
        match entry {
            TrackedBlock::Label(l) => *l,
            _ => panic!("expected a labeled block"),
        }
    }

    fn get_or_make_function(
        &mut self,
        name: &ast::Symbol,
        is_global: bool,
        span: Span,
    ) -> Function {
        let sym = name.sema.get().expect("sema symbol not set");
        match self.symbols[sym] {
            Some(SymbolEntry::Function(f)) => f,
            None => {
                let f = self.builder.program.functions.push(FunctionData {
                    span,
                    is_global,
                    name: name.name,
                    blocks: Vec::new(),
                });
                self.symbols[sym] = Some(SymbolEntry::Function(f));
                f
            }
            _ => panic!("expected a function"),
        }
    }

    fn get_or_make_global(&mut self, name: &ast::Symbol, mut global: GlobalData) -> Global {
        let sym = name.sema.get().expect("sema symbol not set");
        match self.symbols[sym] {
            Some(SymbolEntry::Global(v)) => v,
            None => {
                let name = match self.builder.function {
                    Some(func) if !global.is_global => {
                        let fname = self.builder.program.functions[func].name;
                        let fname = self.builder.interner.lookup(fname);
                        let name = self.builder.interner.lookup(name.name);
                        let scoped = format!("{fname}.{name}");
                        self.builder.interner.intern(&scoped)
                    }
                    _ => name.name,
                };
                global.name = name;
                let v = self.builder.program.globals.push(global);
                self.symbols[sym] = Some(SymbolEntry::Global(v));
                v
            }
            _ => panic!("expected a static"),
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary Structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprMode {
    LValue,
    RValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrackedBlock {
    Case(Block),
    Label(Block),
    BreakAndContinue(Block, Block),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolEntry {
    Value(Value),
    Global(Global),
    Function(Function),
}
