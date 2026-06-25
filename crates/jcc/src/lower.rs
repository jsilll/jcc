use crate::{
    ast::{self, Expr},
    sema::{self, Attribute, SemaCtx},
};

use jcc_backend::{
    codemap::span::Span,
    ir::{
        builder::ProgramBuilder,
        inst::{BinaryOp, FCmpOp, ICmpOp, Inst, UnaryOp},
        term::Terminator,
        ty::Ty,
        Block, Function, FunctionData, Global, GlobalData, Program, Value,
    },
    Ident, IdentInterner,
};
use jcc_entity::{EntityMap, SecondaryMap};

pub struct LoweringPass<'ctx> {
    /// The SSA builder
    bld: ProgramBuilder<'ctx>,
    /// The AST being compiled
    ast: &'ctx ast::Ast<'ctx>,
    /// The semantic analysis context
    sema: &'ctx SemaCtx<'ctx>,
    /// Blocks tracked for statements
    tracked: EntityMap<ast::Stmt, TrackedBlock>,
    /// Mapping from semantic symbols to SSA symbols
    symbols: SecondaryMap<sema::Symbol, Option<SymbolEntry>>,
}

impl<'ctx> LoweringPass<'ctx> {
    pub fn new(
        ast: &'ctx ast::Ast<'ctx>,
        sema: &'ctx SemaCtx<'ctx>,
        interner: &'ctx mut IdentInterner,
    ) -> Self {
        Self {
            ast,
            sema,
            tracked: EntityMap::default(),
            bld: ProgramBuilder::new(interner),
            symbols: SecondaryMap::with_capacity(sema.symbols.len()),
        }
    }

    pub fn build(mut self) -> Program {
        self.ast.root.iter().for_each(|decl| {
            let data = &self.ast.decl[*decl];
            let sym = data.name.resolved();
            match data.kind {
                ast::DeclKind::Var(_) => {
                    let info = self.sema.symbols[sym].expect("expected a sema symbol");

                    match info.attr {
                        Attribute::Local => {
                            unreachable!("top-level variable cannot have local attribute")
                        }
                        Attribute::Function { .. } => {
                            unreachable!("top-level variable cannot have function attribute")
                        }
                        Attribute::Static { is_global, init } => {
                            self.get_or_make_global(
                                &data.name,
                                GlobalData {
                                    is_global,
                                    span: data.span,
                                    init: init.value(),
                                    name: data.name.name,
                                    ty: data.ty.lower().0,
                                },
                            );
                        }
                    }
                }
                ast::DeclKind::Func { params, body } => {
                    let is_global = self.sema.symbols[sym]
                        .expect("expected a sema symbol")
                        .is_global();

                    let func = self.get_or_make_function(
                        data.name.resolved(),
                        FunctionData::new(data.name.name, is_global, data.span),
                    );

                    if let Some(body) = body {
                        self.tracked.clear();
                        self.bld.exit_function();
                        self.bld.enter_function(func);

                        self.ast.decls[params]
                            .iter()
                            .enumerate()
                            .for_each(|(idx, param)| {
                                let idx = idx as u32;
                                let param = &self.ast.decl[*param];
                                let arg = self
                                    .bld
                                    .build_val(Inst::param(param.ty.lower().0, idx), param.span);
                                self.symbols[param.name.resolved()] = Some(SymbolEntry::Value(arg));
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
                            let val = self.bld.build_val(Inst::constant(Ty::I32, 0), data.span);
                            self.bld.seal_with_dead(
                                Terminator::ret(Some(val)),
                                "return.dead",
                                data.span,
                            );
                        }
                    }
                }
            }
        });

        self.bld.finish()
    }

    fn visit_decl(&mut self, decl: ast::Decl) {
        let data = &self.ast.decl[decl];
        match data.kind {
            ast::DeclKind::Func { .. } => {}
            ast::DeclKind::Var(init) => {
                let sema = data.name.resolved();
                let info = self.sema.symbols[sema].expect("expected a sema symbol");
                match info.attr {
                    Attribute::Function { .. } => {
                        unreachable!("variable declaration cannot have function attribute")
                    }
                    Attribute::Local => {
                        let alloca = self.bld.build_alloca(data.ty.lower().0, 0, data.span);
                        self.symbols[sema] = Some(SymbolEntry::Value(alloca));
                        if let Some(init) = init {
                            let value = self.visit_expr_rvalue(init);
                            self.bld.build_val(Inst::store(alloca, value, 0), data.span);
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
                                ty: data.ty.lower().0,
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
                self.bld.seal_and_move_to(Terminator::br(block), block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Return(expr) => {
                let term = Terminator::ret(Some(self.visit_expr_rvalue(*expr)));
                self.bld.seal_with_dead(term, "return.dead", data.span);
            }
            ast::StmtKind::Break(target) => {
                debug_assert!(target.get().is_some());
                let term = Terminator::br(self.bld.top_targets().unwrap().break_target);
                self.bld.seal_with_dead(term, "break.dead", data.span);
            }
            ast::StmtKind::Continue(target) => {
                debug_assert!(target.get().is_some());
                let term = Terminator::br(self.bld.top_targets().unwrap().continue_target);
                self.bld.seal_with_dead(term, "continue.dead", data.span);
            }
            ast::StmtKind::Compound(items) => {
                self.ast.items[*items].iter().for_each(|item| match item {
                    ast::BlockItem::Decl(decl) => self.visit_decl(*decl),
                    ast::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                });
            }
            ast::StmtKind::Case { stmt: inner, .. } => {
                let block = self.remove_case_block(stmt);
                self.bld.seal_and_move_to(Terminator::br(block), block);
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Label { label, stmt: inner } => {
                let block = self.get_or_make_labeled(stmt, *label, data.span);
                self.bld.seal_and_move_to(Terminator::br(block), block);
                self.visit_stmt(*inner);
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
                self.bld
                    .seal_with_dead(Terminator::br(block), "goto.dead", data.span);
            }
            ast::StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                let then_block = self.bld.build_block("if.then", data.span);
                let cont_block = self.bld.build_block("if.cont", data.span);
                let else_block = otherwise.map(|_| self.bld.build_block("if.else", data.span));
                let false_target = else_block.unwrap_or(cont_block);

                let cond = self.build_truthy(*cond, data.span);
                self.bld.seal_and_move_to(
                    Terminator::cond_br(cond, then_block, false_target),
                    then_block,
                );

                self.visit_stmt(*then);
                self.bld
                    .seal_and_move_to(Terminator::br(cont_block), else_block.unwrap_or(cont_block));

                if let Some(otherwise) = otherwise {
                    self.visit_stmt(*otherwise);
                    self.bld
                        .seal_and_move_to(Terminator::br(cont_block), cont_block);
                }
            }
            ast::StmtKind::While { cond, body } => {
                let cond_block = self.bld.build_block("while.cond", data.span);
                let body_block = self.bld.build_block("while.body", data.span);
                let cont_block = self.bld.build_block("while.cont", data.span);

                self.bld.push_loop(cont_block, cond_block);
                self.bld
                    .seal_and_move_to(Terminator::br(cond_block), cond_block);

                let cond = self.build_truthy(*cond, data.span);
                self.bld.seal_and_move_to(
                    Terminator::cond_br(cond, body_block, cont_block),
                    body_block,
                );

                self.visit_stmt(*body);
                self.bld.pop_targets();

                self.bld
                    .seal_and_move_to(Terminator::br(cond_block), cont_block);
            }
            ast::StmtKind::DoWhile { body, cond } => {
                let body_block = self.bld.build_block("do.body", data.span);
                let cond_block = self.bld.build_block("do.cond", data.span);
                let cont_block = self.bld.build_block("do.cont", data.span);

                self.bld.push_loop(cont_block, cond_block);

                self.bld
                    .seal_and_move_to(Terminator::br(body_block), body_block);

                self.visit_stmt(*body);
                self.bld.pop_targets();

                self.bld
                    .seal_and_move_to(Terminator::br(cond_block), cond_block);

                let cond = self.build_truthy(*cond, data.span);
                self.bld.seal_and_move_to(
                    Terminator::cond_br(cond, body_block, cont_block),
                    cont_block,
                );
            }
            ast::StmtKind::For {
                init,
                cond,
                step,
                body,
            } => {
                let body_block = self.bld.build_block("for.body", data.span);
                let exit_block = self.bld.build_block("for.exit", data.span);
                let cond_block = cond.map(|_| self.bld.build_block("for.cond", data.span));
                let step_block = step.map(|_| self.bld.build_block("for.step", data.span));
                let after_body_target = step_block.or(cond_block).unwrap_or(body_block);

                self.bld.push_loop(exit_block, after_body_target);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => self.visit_decl(*decl),
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr_rvalue(*expr);
                        }
                    }
                }
                self.bld.seal_and_move_to(
                    Terminator::br(cond_block.unwrap_or(body_block)),
                    cond_block.unwrap_or(body_block),
                );

                if let Some(cond) = cond {
                    let cond = self.build_truthy(*cond, data.span);
                    self.bld.seal_and_move_to(
                        Terminator::cond_br(cond, body_block, exit_block),
                        step_block.unwrap_or(body_block),
                    );
                }

                if let Some(step) = step {
                    self.visit_expr_rvalue(*step);
                    self.bld.seal_and_move_to(
                        Terminator::br(cond_block.unwrap_or(body_block)),
                        body_block,
                    );
                }

                self.visit_stmt(*body);
                self.bld.pop_targets();

                self.bld
                    .seal_and_move_to(Terminator::br(after_body_target), exit_block);
            }
            ast::StmtKind::Switch { cond, body } => {
                let mut cases = Vec::new();
                let mut default_block = None;
                if let Some(switch) = self.sema.switches.get(&stmt) {
                    cases.reserve(switch.cases.len());
                    switch.cases.iter().for_each(|case| {
                        let data = &self.ast.stmt[*case];
                        if let ast::StmtKind::Case { expr, .. } = data.kind {
                            if let ast::ExprKind::Const(c) = self.ast.expr[expr].kind {
                                let block = self.bld.build_block("switch.case", data.span);
                                self.tracked.insert(*case, TrackedBlock::Case(block));
                                cases.push((c.lower().0, block));
                            }
                        }
                    });
                    if let Some(inner) = switch.default {
                        let block = self
                            .bld
                            .build_block("switch.default", self.ast.stmt[inner].span);
                        self.tracked.insert(inner, TrackedBlock::Case(block));
                        default_block = Some(block);
                    }
                }

                let cont_block = self.bld.build_block("switch.cont", data.span);
                self.bld.push_switch(cont_block);
                let cond_val = self.visit_expr_rvalue(*cond);
                self.bld.seal_with_dead(
                    Terminator::switch(cond_val, cases, default_block.unwrap_or(cont_block)),
                    "switch.dead",
                    data.span,
                );
                self.visit_stmt(*body);
                self.bld.pop_targets();

                self.bld
                    .seal_and_move_to(Terminator::br(cont_block), cont_block);
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
        let ty = expr.ty.get().lower().0;
        match &expr.kind {
            ast::ExprKind::Grouped(expr) => self.visit_expr_inner(*expr, mode),
            ast::ExprKind::Const(c) => {
                let (c, ty) = c.lower();
                self.bld.build_val(Inst::constant(ty, c), expr.span)
            }
            ast::ExprKind::Cast { expr: inner, .. } => {
                let from_ty = self.ast.expr[*inner].ty.get();
                let inner = self.visit_expr_rvalue(*inner);
                self.build_cast(inner, from_ty, expr.ty.get(), expr.span)
            }
            ast::ExprKind::Var(name) => {
                let span = expr.span;
                let sym = name.resolved();
                let info = self.sema.symbols[sym].expect("expected a sema symbol");
                match info.attr {
                    Attribute::Function { .. } => {
                        unreachable!("function-as-variable should be caught by the type checker")
                    }
                    Attribute::Local => {
                        let ptr = self.get_value(sym);
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => self.bld.build_val(Inst::load(ty, ptr, 0), span),
                        }
                    }
                    Attribute::Static { .. } => {
                        let var = self.get_global(sym);
                        let ptr = self.bld.build_val(Inst::global_addr(var), expr.span);
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => self.bld.build_val(Inst::load(ty, ptr, 0), span),
                        }
                    }
                }
            }
            ast::ExprKind::Call { name, args, .. } => {
                let args = self.ast.exprs[*args]
                    .iter()
                    .map(|arg| self.visit_expr_rvalue(*arg))
                    .collect::<Vec<_>>();
                let symbol = self.sema.symbols[name.resolved()].expect("expected a sema symbol");
                let data = FunctionData::new(name.name, symbol.is_global(), expr.span);
                let func = self.get_or_make_function(name.resolved(), data);
                let ty = symbol.ty.ret().unwrap().lower().0;
                self.bld.build_val(Inst::call(ty, func, args), expr.span)
            }
            ast::ExprKind::Unary { op, expr: inner } => {
                let inner_ty = self.ast.expr[*inner].ty.get().lower().0;
                let one = match inner_ty {
                    Ty::F32 => Inst::const_f32(1.0),
                    Ty::F64 => Inst::const_f64(1.0),
                    _ => Inst::constant(inner_ty, 1),
                };
                let add_or_sub = match op {
                    ast::UnaryOp::PreInc | ast::UnaryOp::PostInc => match inner_ty {
                        Ty::F32 | Ty::F64 => BinaryOp::FAdd,
                        _ => BinaryOp::Add,
                    },
                    _ => match inner_ty {
                        Ty::F32 | Ty::F64 => BinaryOp::FSub,
                        _ => BinaryOp::Sub,
                    },
                };
                match op {
                    ast::UnaryOp::PreInc | ast::UnaryOp::PreDec => {
                        let one = self.bld.build_val(one, expr.span);
                        self.build_inc_dec(*inner, add_or_sub, one, expr.span).1
                    }
                    ast::UnaryOp::PostInc | ast::UnaryOp::PostDec => {
                        let one = self.bld.build_val(one, expr.span);
                        self.build_inc_dec(*inner, add_or_sub, one, expr.span).0
                    }
                    ast::UnaryOp::Neg => {
                        let neg = if matches!(inner_ty, Ty::F32 | Ty::F64) {
                            UnaryOp::FNeg
                        } else {
                            UnaryOp::Neg
                        };
                        self.build_unary(ty, neg, *inner, expr.span)
                    }
                    ast::UnaryOp::BitNot => self.build_unary(ty, UnaryOp::Not, *inner, expr.span),
                    ast::UnaryOp::LogNot => {
                        let inner = self.build_truthy(*inner, expr.span);
                        let zero = self.bld.build_val(Inst::constant(Ty::I1, 0), expr.span);
                        let cmp = self
                            .bld
                            .build_val(Inst::icmp(ICmpOp::Eq, inner, zero), expr.span);
                        self.bld.build_val(Inst::zext(Ty::I32, cmp), expr.span)
                    }
                }
            }
            ast::ExprKind::Binary { op, lhs, rhs } => {
                let rhs_ty = self.ast.expr[*rhs].ty.get();
                match op {
                    ast::BinaryOp::LogOr => self.build_short_circuit(true, *lhs, *rhs, expr.span),
                    ast::BinaryOp::LogAnd => self.build_short_circuit(false, *lhs, *rhs, expr.span),
                    ast::BinaryOp::BitOr => self.build_bin(ty, BinaryOp::Or, *lhs, *rhs, expr.span),
                    ast::BinaryOp::BitAnd => {
                        self.build_bin(ty, BinaryOp::And, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitXor => {
                        self.build_bin(ty, BinaryOp::Xor, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::Eq => {
                        if rhs_ty.is_floating_point() {
                            self.build_fcmp(FCmpOp::Oeq, *lhs, *rhs, expr.span)
                        } else {
                            self.build_icmp(ICmpOp::Eq, *lhs, *rhs, expr.span)
                        }
                    }
                    ast::BinaryOp::Ne => {
                        if rhs_ty.is_floating_point() {
                            self.build_fcmp(FCmpOp::Une, *lhs, *rhs, expr.span)
                        } else {
                            self.build_icmp(ICmpOp::Ne, *lhs, *rhs, expr.span)
                        }
                    }
                    ast::BinaryOp::Lt => {
                        if rhs_ty.is_floating_point() {
                            self.build_fcmp(FCmpOp::Olt, *lhs, *rhs, expr.span)
                        } else {
                            let op = if rhs_ty.is_signed_integer() {
                                ICmpOp::Lt
                            } else {
                                ICmpOp::Ult
                            };
                            self.build_icmp(op, *lhs, *rhs, expr.span)
                        }
                    }
                    ast::BinaryOp::Le => {
                        if rhs_ty.is_floating_point() {
                            self.build_fcmp(FCmpOp::Ole, *lhs, *rhs, expr.span)
                        } else {
                            let op = if rhs_ty.is_signed_integer() {
                                ICmpOp::Le
                            } else {
                                ICmpOp::Ule
                            };
                            self.build_icmp(op, *lhs, *rhs, expr.span)
                        }
                    }
                    ast::BinaryOp::Gt => {
                        if rhs_ty.is_floating_point() {
                            self.build_fcmp(FCmpOp::Ogt, *lhs, *rhs, expr.span)
                        } else {
                            let op = if rhs_ty.is_signed_integer() {
                                ICmpOp::Gt
                            } else {
                                ICmpOp::Ugt
                            };
                            self.build_icmp(op, *lhs, *rhs, expr.span)
                        }
                    }
                    ast::BinaryOp::Ge => {
                        if rhs_ty.is_floating_point() {
                            self.build_fcmp(FCmpOp::Oge, *lhs, *rhs, expr.span)
                        } else {
                            let op = if rhs_ty.is_signed_integer() {
                                ICmpOp::Ge
                            } else {
                                ICmpOp::Uge
                            };
                            self.build_icmp(op, *lhs, *rhs, expr.span)
                        }
                    }
                    ast::BinaryOp::Add => {
                        let op = if rhs_ty.is_floating_point() {
                            BinaryOp::FAdd
                        } else {
                            BinaryOp::Add
                        };
                        self.build_bin(ty, op, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::Sub => {
                        let op = if rhs_ty.is_floating_point() {
                            BinaryOp::FSub
                        } else {
                            BinaryOp::Sub
                        };
                        self.build_bin(ty, op, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::Mul => {
                        let op = if rhs_ty.is_floating_point() {
                            BinaryOp::FMul
                        } else {
                            BinaryOp::Mul
                        };
                        self.build_bin(ty, op, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::Div => {
                        let op = if rhs_ty.is_floating_point() {
                            BinaryOp::FDiv
                        } else if rhs_ty.is_signed_integer() {
                            BinaryOp::SDiv
                        } else {
                            BinaryOp::UDiv
                        };
                        self.build_bin(ty, op, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::Rem => {
                        let op = if rhs_ty.is_signed_integer() {
                            BinaryOp::SRem
                        } else {
                            BinaryOp::URem
                        };
                        self.build_bin(ty, op, *lhs, *rhs, expr.span)
                    }
                    ast::BinaryOp::BitShl | ast::BinaryOp::BitShr => {
                        let lhs_ty = self.ast.expr[*lhs].ty.get();

                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);

                        let op = if matches!(op, ast::BinaryOp::BitShl) {
                            BinaryOp::Shl
                        } else if lhs_ty.is_signed_integer() {
                            BinaryOp::AShr
                        } else {
                            BinaryOp::Shr
                        };
                        self.bld
                            .build_val(Inst::binary(op, ty, lhs, rhs), expr.span)
                    }
                    ast::BinaryOp::BitShlAssign | ast::BinaryOp::BitShrAssign => {
                        let lhs_ty = self.ast.expr[*lhs].ty.get();

                        let ptr = self.visit_expr_lvalue(*lhs);
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);

                        let op = if matches!(op, ast::BinaryOp::BitShlAssign) {
                            BinaryOp::Shl
                        } else if lhs_ty.is_signed_integer() {
                            BinaryOp::AShr
                        } else {
                            BinaryOp::Shr
                        };
                        let inst = self
                            .bld
                            .build_val(Inst::binary(op, ty, lhs, rhs), expr.span);
                        self.bld.build_val(Inst::store(ptr, inst, 0), expr.span);
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => inst,
                        }
                    }
                    ast::BinaryOp::Assign => {
                        let lhs = self.visit_expr_lvalue(*lhs);
                        let rhs = self.visit_expr_rvalue(*rhs);
                        self.bld.build_val(Inst::store(lhs, rhs, 0), expr.span);
                        match mode {
                            ExprMode::LValue => lhs,
                            ExprMode::RValue => rhs,
                        }
                    }
                    ast::BinaryOp::AddAssign
                    | ast::BinaryOp::SubAssign
                    | ast::BinaryOp::MulAssign
                    | ast::BinaryOp::DivAssign
                    | ast::BinaryOp::RemAssign
                    | ast::BinaryOp::BitOrAssign
                    | ast::BinaryOp::BitAndAssign
                    | ast::BinaryOp::BitXorAssign => {
                        let lhs_ty = self.ast.expr[*lhs].ty.get();
                        let rhs_ty = self.ast.expr[*rhs].ty.get();
                        let common = ast::TyKind::common(lhs_ty, rhs_ty)
                            .expect("no common type for binary assignment");

                        let op = if common.is_floating_point() {
                            match op {
                                ast::BinaryOp::AddAssign => BinaryOp::FAdd,
                                ast::BinaryOp::SubAssign => BinaryOp::FSub,
                                ast::BinaryOp::MulAssign => BinaryOp::FMul,
                                ast::BinaryOp::DivAssign => BinaryOp::FDiv,
                                ast::BinaryOp::RemAssign => BinaryOp::FRem,
                                ast::BinaryOp::BitOrAssign => BinaryOp::Or,
                                ast::BinaryOp::BitAndAssign => BinaryOp::And,
                                ast::BinaryOp::BitXorAssign => BinaryOp::Xor,
                                _ => unreachable!(),
                            }
                        } else {
                            match op {
                                ast::BinaryOp::AddAssign => BinaryOp::Add,
                                ast::BinaryOp::SubAssign => BinaryOp::Sub,
                                ast::BinaryOp::MulAssign => BinaryOp::Mul,
                                ast::BinaryOp::BitOrAssign => BinaryOp::Or,
                                ast::BinaryOp::BitAndAssign => BinaryOp::And,
                                ast::BinaryOp::BitXorAssign => BinaryOp::Xor,
                                ast::BinaryOp::DivAssign => {
                                    if common.is_signed_integer() {
                                        BinaryOp::SDiv
                                    } else {
                                        BinaryOp::UDiv
                                    }
                                }
                                ast::BinaryOp::RemAssign => {
                                    if common.is_signed_integer() {
                                        BinaryOp::SRem
                                    } else {
                                        BinaryOp::URem
                                    }
                                }
                                _ => unreachable!(),
                            }
                        };

                        let ptr = self.visit_expr_lvalue(*lhs);
                        let lhs = self.visit_expr_rvalue(*lhs);
                        let lhs = self.build_cast(lhs, lhs_ty, common, expr.span);

                        let rhs = self.visit_expr_rvalue(*rhs);
                        let rhs = self.build_cast(rhs, rhs_ty, common, expr.span);

                        let value = self
                            .bld
                            .build_val(Inst::binary(op, common.lower().0, lhs, rhs), expr.span);
                        let value = self.build_cast(value, common, lhs_ty, expr.span);

                        self.bld.build_val(Inst::store(ptr, value, 0), expr.span);
                        match mode {
                            ExprMode::LValue => ptr,
                            ExprMode::RValue => value,
                        }
                    }
                }
            }
            ast::ExprKind::Ternary { cond, then, other } => {
                let then_block = self.bld.build_block("tern.then", expr.span);
                let else_block = self.bld.build_block("tern.else", expr.span);
                let cont_block = self.bld.build_block("tern.cont", expr.span);

                let phi = self.bld.build_phi(ty, expr.span);

                let cond = self.build_truthy(*cond, expr.span);
                self.bld.seal_and_move_to(
                    Terminator::cond_br(cond, then_block, else_block),
                    else_block,
                );

                let then_val = self.visit_expr_rvalue(*then);
                self.bld.build_val(Inst::upsilon(phi, then_val), expr.span);
                self.bld
                    .seal_and_move_to(Terminator::br(cont_block), else_block);

                let else_val = self.visit_expr_rvalue(*other);
                self.bld.build_val(Inst::upsilon(phi, else_val), expr.span);
                self.bld
                    .seal_and_move_to(Terminator::br(cont_block), cont_block);

                self.bld.push_val(phi);
                phi
            }
        }
    }

    fn build_unary(&mut self, ty: Ty, op: UnaryOp, expr: ast::Expr, span: Span) -> Value {
        let val = self.visit_expr_rvalue(expr);
        self.bld.build_val(Inst::unary(op, ty, val), span)
    }

    fn build_icmp(&mut self, op: ICmpOp, lhs: ast::Expr, rhs: ast::Expr, span: Span) -> Value {
        let lhs = self.visit_expr_rvalue(lhs);
        let rhs = self.visit_expr_rvalue(rhs);
        self.bld.build_val(Inst::icmp(op, lhs, rhs), span)
    }

    fn build_fcmp(&mut self, op: FCmpOp, lhs: ast::Expr, rhs: ast::Expr, span: Span) -> Value {
        let lhs = self.visit_expr_rvalue(lhs);
        let rhs = self.visit_expr_rvalue(rhs);
        self.bld.build_val(Inst::fcmp(op, lhs, rhs), span)
    }

    fn build_truthy(&mut self, cond: Expr, span: Span) -> Value {
        let val = self.visit_expr_rvalue(cond);
        let ty = self.ast.expr[cond].ty.get().lower().0;
        match ty {
            Ty::I1 => val,
            Ty::F32 | Ty::F64 => {
                let zero = self.bld.build_val(Inst::constant(ty, 0), span);
                self.bld.build_val(Inst::fcmp(FCmpOp::Une, val, zero), span)
            }
            _ => {
                let zero = self.bld.build_val(Inst::constant(ty, 0), span);
                self.bld.build_val(Inst::icmp(ICmpOp::Ne, val, zero), span)
            }
        }
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
        self.bld.build_val(Inst::binary(op, ty, lhs, rhs), span)
    }

    fn build_inc_dec(
        &mut self,
        expr: ast::Expr,
        op: BinaryOp,
        one: Value,
        span: Span,
    ) -> (Value, Value) {
        let ty = self.ast.expr[expr].ty.get().lower().0;
        let ptr = self.visit_expr_lvalue(expr);
        let old = self.bld.build_val(Inst::load(ty, ptr, 0), span);
        let new = self.bld.build_val(Inst::binary(op, ty, old, one), span);
        self.bld.build_val(Inst::store(ptr, new, 0), span);
        (old, new)
    }

    fn build_cast(&mut self, expr: Value, from: ast::Ty, to: ast::Ty, span: Span) -> Value {
        let (to, to_signed) = to.lower();
        let (from, from_signed) = from.lower();
        match (from, to) {
            (Ty::F32, Ty::F64) => self.bld.build_val(Inst::fext(to, expr), span),
            (Ty::F64, Ty::F32) => self.bld.build_val(Inst::ftrunc(to, expr), span),
            (Ty::F32 | Ty::F64, _) => {
                if to_signed {
                    self.bld.build_val(Inst::fp_to_si(to, expr), span)
                } else {
                    self.bld.build_val(Inst::fp_to_ui(to, expr), span)
                }
            }
            (_, Ty::F32 | Ty::F64) => {
                if from_signed {
                    self.bld.build_val(Inst::si_to_fp(to, expr), span)
                } else {
                    self.bld.build_val(Inst::ui_to_fp(to, expr), span)
                }
            }
            _ => match from.size_bytes().cmp(&to.size_bytes()) {
                std::cmp::Ordering::Equal => expr,
                std::cmp::Ordering::Greater => self.bld.build_val(Inst::trunc(to, expr), span),
                std::cmp::Ordering::Less => {
                    if from_signed {
                        self.bld.build_val(Inst::sext(to, expr), span)
                    } else {
                        self.bld.build_val(Inst::zext(to, expr), span)
                    }
                }
            },
        }
    }

    fn build_short_circuit(
        &mut self,
        is_or: bool,
        lhs: ast::Expr,
        rhs: ast::Expr,
        span: Span,
    ) -> Value {
        let rhs_block = self.bld.build_block(if is_or { "or" } else { "and" }, span);
        let cont_block = self
            .bld
            .build_block(if is_or { "or.cont" } else { "and.cont" }, span);

        let phi = self.bld.build_phi(Ty::I32, span);

        let lhs = self.build_truthy(lhs, span);
        let short_circuit_val = self
            .bld
            .build_val(Inst::constant(Ty::I32, if is_or { 1 } else { 0 }), span);
        self.bld
            .build_val(Inst::upsilon(phi, short_circuit_val), span);
        let (then_block, else_block) = if is_or {
            (cont_block, rhs_block)
        } else {
            (rhs_block, cont_block)
        };
        self.bld
            .seal_and_move_to(Terminator::cond_br(lhs, then_block, else_block), rhs_block);

        let rhs = self.build_truthy(rhs, span);
        let extended = self.bld.build_val(Inst::zext(Ty::I32, rhs), span);
        self.bld.build_val(Inst::upsilon(phi, extended), span);
        self.bld
            .seal_and_move_to(Terminator::br(cont_block), cont_block);

        self.bld.push_val(phi);
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
            Some(SymbolEntry::Global(global)) => global,
            _ => panic!("expected a static var ref"),
        }
    }

    fn remove_case_block(&mut self, stmt: ast::Stmt) -> Block {
        match self.tracked.remove(&stmt) {
            Some(TrackedBlock::Case(c)) => c,
            _ => panic!("expected a case block"),
        }
    }

    fn get_or_make_labeled(&mut self, stmt: ast::Stmt, name: Ident, span: Span) -> Block {
        if let Some(TrackedBlock::Label(l)) = self.tracked.get(&stmt) {
            return *l;
        }
        let block = self.bld.build_block_ident(name, span);
        self.tracked.insert(stmt, TrackedBlock::Label(block));
        block
    }

    fn get_or_make_global(&mut self, name: &ast::Symbol, global: GlobalData) -> Global {
        let sym = name.resolved();
        match self.symbols[sym] {
            Some(SymbolEntry::Global(v)) => v,
            None => {
                let v = self.bld.build_global(sym.as_u32(), global);
                self.symbols[sym] = Some(SymbolEntry::Global(v));
                v
            }
            _ => unreachable!("symbol was previously registered as a non-global"),
        }
    }

    fn get_or_make_function(&mut self, sym: sema::Symbol, data: FunctionData) -> Function {
        match self.symbols[sym] {
            Some(SymbolEntry::Function(f)) => f,
            None => {
                let f = self.bld.build_function(data);
                self.symbols[sym] = Some(SymbolEntry::Function(f));
                f
            }
            _ => unreachable!("symbol was previously registered as a non-function"),
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolEntry {
    Value(Value),
    Global(Global),
    Function(Function),
}
