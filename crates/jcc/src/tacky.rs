use crate::{ast, sema};

use tacky::{
    source_file::SourceSpan, BinaryOp, Block, BlockRef, FnDef, Inst, Interner, Program, Symbol,
    UnaryOp, Value,
};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// TackyBuilder
// ---------------------------------------------------------------------------

pub struct TackyBuilder<'a> {
    ctx: &'a sema::SemaCtx,
    interner: &'a mut Interner,
}

impl<'a> TackyBuilder<'a> {
    pub fn new(ctx: &'a sema::SemaCtx, interner: &'a mut Interner) -> Self {
        Self { ctx, interner }
    }

    pub fn build(self, ast: &ast::Ast) -> Program {
        let decl = ast
            .root()
            .iter()
            .next()
            .expect("expected at least one item in the AST");
        let fn_def = TackyFnDefBuilder::new(ast, self.ctx, self.interner).build(*decl);
        Program(fn_def)
    }
}

// ---------------------------------------------------------------------------
// TackyFnDefBuilder
// ---------------------------------------------------------------------------

struct TackyFnDefBuilder<'a> {
    ast: &'a ast::Ast,
    ctx: &'a sema::SemaCtx,
    interner: &'a mut Interner,
    fn_def: FnDef,
    tmp_count: u32,
    block: BlockRef,
    variables: HashMap<ast::DeclRef, Value>,
    labeled_blocks: HashMap<Symbol, BlockRef>,
    case_blocks: HashMap<ast::StmtRef, BlockRef>,
    break_blocks: HashMap<ast::StmtRef, BlockRef>,
    continue_blocks: HashMap<ast::StmtRef, BlockRef>,
}

impl<'a> TackyFnDefBuilder<'a> {
    fn new(ast: &'a ast::Ast, ctx: &'a sema::SemaCtx, interner: &'a mut Interner) -> Self {
        let mut fn_def = FnDef::default();
        let block = fn_def.push_block(Block::default());
        Self {
            ast,
            ctx,
            block,
            fn_def,
            interner,
            tmp_count: 0,
            variables: HashMap::new(),
            case_blocks: HashMap::new(),
            break_blocks: HashMap::new(),
            labeled_blocks: HashMap::new(),
            continue_blocks: HashMap::new(),
        }
    }

    fn build(mut self, decl: ast::DeclRef) -> FnDef {
        self.fn_def.span = *self.ast.decl_span(decl);
        match self.ast.decl(decl) {
            ast::Decl::Var { .. } => todo!("handle variable declarations"),
            ast::Decl::Func { body, .. } => {
                let body = body.expect("expected a function body");
                self.ast
                    .block_items(body)
                    .iter()
                    .for_each(|item| match item {
                        ast::BlockItem::Decl(decl) => self.build_from_decl(*decl),
                        ast::BlockItem::Stmt(stmt) => self.build_from_stmt(*stmt),
                    });
                match self.ast.block_items(body).last() {
                    Some(ast::BlockItem::Stmt(stmt)) => match self.ast.stmt(*stmt) {
                        ast::Stmt::Return(_) => {}
                        _ => self.append_to_block(Inst::Return(Value::Const(0)), self.fn_def.span),
                    },
                    _ => self.append_to_block(Inst::Return(Value::Const(0)), self.fn_def.span),
                }
                self.fn_def
            }
        }
    }

    #[inline]
    fn make_tmp(&mut self) -> Value {
        let tmp = Value::Variable(self.tmp_count);
        self.tmp_count += 1;
        tmp
    }

    #[inline]
    fn append_to_block(&mut self, instr: Inst, span: SourceSpan) {
        let root = self.fn_def.get_block_mut(self.block);
        root.instrs.push(instr);
        root.spans.push(span);
    }

    #[inline]
    fn get_or_make_block(&mut self, label: Symbol) -> BlockRef {
        self.labeled_blocks
            .entry(label)
            .or_insert_with(|| self.fn_def.push_block(Block::with_label(label)))
            .clone()
    }

    fn get_or_make_var(&mut self, decl: ast::DeclRef) -> Value {
        self.variables
            .entry(decl)
            .or_insert_with(|| {
                let var = Value::Variable(self.tmp_count);
                self.tmp_count += 1;
                var
            })
            .clone()
    }

    fn get_or_make_some_var(&mut self, decl: Option<ast::DeclRef>) -> Value {
        self.variables
            .entry(decl.expect("expected a resolved variable"))
            .or_insert_with(|| {
                let var = Value::Variable(self.tmp_count);
                self.tmp_count += 1;
                var
            })
            .clone()
    }

    fn build_from_decl(&mut self, decl: ast::DeclRef) {
        let span = *self.ast.decl_span(decl);
        match self.ast.decl(decl) {
            ast::Decl::Func { .. } => todo!("handle function declarations"),
            ast::Decl::Var { init, .. } => {
                if let Some(init) = init {
                    let dst = self.get_or_make_var(decl);
                    let src = self.build_from_expr(*init);
                    self.append_to_block(Inst::Copy { src, dst }, span);
                }
            }
        }
    }

    fn build_from_stmt(&mut self, stmt: ast::StmtRef) {
        match self.ast.stmt(stmt) {
            ast::Stmt::Empty => {}
            ast::Stmt::Break => {
                let block = self
                    .break_blocks
                    .get(self.ctx.breaks.get(&stmt).expect("expected a break block"))
                    .expect("expected a break block");
                self.append_to_block(Inst::Jump(*block), *self.ast.stmt_span(stmt));
            }
            ast::Stmt::Expr(expr) => {
                self.build_from_expr(*expr);
            }
            ast::Stmt::Return(expr) => {
                let value = self.build_from_expr(*expr);
                self.append_to_block(Inst::Return(value), *self.ast.stmt_span(stmt));
            }
            ast::Stmt::Goto(label) => {
                let block = self.get_or_make_block(*label);
                self.append_to_block(Inst::Jump(block), *self.ast.stmt_span(stmt));
            }
            ast::Stmt::Label { label, stmt: inner } => {
                let block = self.get_or_make_block(*label);
                self.append_to_block(Inst::Jump(block), *self.ast.stmt_span(stmt));
                self.block = block;
                self.build_from_stmt(*inner);
            }
            ast::Stmt::Default(inner) => {
                let block = self
                    .case_blocks
                    .get(&stmt)
                    .expect("expected a case block")
                    .clone();
                self.append_to_block(Inst::Jump(block), *self.ast.stmt_span(stmt));
                self.block = block;
                self.build_from_stmt(*inner);
            }
            ast::Stmt::Case { stmt: inner, .. } => {
                let block = self
                    .case_blocks
                    .get(&stmt)
                    .expect("expected a case block")
                    .clone();
                self.append_to_block(Inst::Jump(block), *self.ast.stmt_span(stmt));
                self.block = block;
                self.build_from_stmt(*inner);
            }
            ast::Stmt::Compound(items) => {
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|item| match item {
                        ast::BlockItem::Decl(decl) => self.build_from_decl(*decl),
                        ast::BlockItem::Stmt(stmt) => self.build_from_stmt(*stmt),
                    });
            }
            ast::Stmt::Continue => {
                let block = self
                    .continue_blocks
                    .get(
                        self.ctx
                            .continues
                            .get(&stmt)
                            .expect("expected a continue block"),
                    )
                    .expect("expected a continue block");
                self.append_to_block(Inst::Jump(*block), *self.ast.stmt_span(stmt));
            }
            ast::Stmt::If {
                cond,
                then,
                otherwise: None,
            } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));

                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Inst::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.stmt_span(stmt),
                );
                self.build_from_stmt(*then);
                self.append_to_block(Inst::Jump(cont_block), *self.ast.stmt_span(stmt));
                self.block = cont_block;
            }
            ast::Stmt::If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let else_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("else"),
                ));
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Inst::JumpIfZero {
                        cond,
                        target: else_block,
                    },
                    *self.ast.stmt_span(stmt),
                );
                self.build_from_stmt(*then);
                self.append_to_block(Inst::Jump(cont_block), *self.ast.stmt_span(stmt));
                self.block = else_block;
                self.build_from_stmt(*otherwise);
                self.append_to_block(Inst::Jump(cont_block), *self.ast.stmt_span(stmt));
                self.block = cont_block;
            }
            ast::Stmt::While { cond, body } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let loop_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("loop"),
                ));
                let cond_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cond"),
                ));

                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, cond_block);

                self.append_to_block(Inst::Jump(cond_block), *self.ast.stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Inst::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.stmt_span(stmt),
                );
                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Inst::Jump(cond_block), *self.ast.stmt_span(stmt));

                self.block = cont_block;
            }
            ast::Stmt::DoWhile { body, cond } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let loop_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("loop"),
                ));
                let cond_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cond"),
                ));

                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, cond_block);

                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Inst::Jump(cond_block), *self.ast.stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Inst::JumpIfNotZero {
                        cond,
                        target: loop_block,
                    },
                    *self.ast.stmt_span(stmt),
                );
                self.append_to_block(Inst::Jump(cont_block), *self.ast.stmt_span(stmt));

                self.block = cont_block;
            }
            ast::Stmt::For {
                init,
                cond: None,
                step: None,
                body,
            } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let loop_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("loop"),
                ));

                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, loop_block);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                        ast::ForInit::VarDecl(decl) => self.build_from_decl(*decl),
                    }
                }
                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = cont_block;
            }
            ast::Stmt::For {
                init,
                cond: Some(cond),
                step: None,
                body,
            } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let loop_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("loop"),
                ));
                let cond_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cond"),
                ));

                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, cond_block);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                        ast::ForInit::VarDecl(decl) => self.build_from_decl(*decl),
                    }
                }
                self.append_to_block(Inst::Jump(cond_block), *self.ast.stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Inst::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.stmt_span(stmt),
                );
                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Inst::Jump(cond_block), *self.ast.stmt_span(stmt));

                self.block = cont_block;
            }
            ast::Stmt::For {
                init,
                cond: None,
                step: Some(step),
                body,
            } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let loop_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("loop"),
                ));
                let step_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("step"),
                ));

                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, step_block);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                        ast::ForInit::VarDecl(decl) => self.build_from_decl(*decl),
                    }
                }
                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Inst::Jump(step_block), *self.ast.stmt_span(stmt));

                self.block = step_block;
                self.build_from_expr(*step);
                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = cont_block;
            }
            ast::Stmt::For {
                init,
                cond: Some(cond),
                step: Some(step),
                body,
            } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let loop_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("loop"),
                ));
                let cond_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cond"),
                ));
                let step_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("step"),
                ));

                self.break_blocks.insert(stmt, cont_block);
                self.continue_blocks.insert(stmt, step_block);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                        ast::ForInit::VarDecl(decl) => self.build_from_decl(*decl),
                    }
                }
                self.append_to_block(Inst::Jump(cond_block), *self.ast.stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Inst::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.stmt_span(stmt),
                );
                self.append_to_block(Inst::Jump(loop_block), *self.ast.stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Inst::Jump(step_block), *self.ast.stmt_span(stmt));

                self.block = step_block;
                self.build_from_expr(*step);
                self.append_to_block(Inst::Jump(cond_block), *self.ast.stmt_span(stmt));

                self.block = cont_block;
            }
            ast::Stmt::Switch { cond, body } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));

                self.break_blocks.insert(stmt, cont_block);

                let lhs = self.build_from_expr(*cond);
                if let Some(switch) = self.ctx.switches.get(&stmt) {
                    switch.cases.iter().for_each(|stmt| {
                        if let ast::Stmt::Case { expr, .. } = self.ast.stmt(*stmt) {
                            let target = self.fn_def.push_block(Block::with_label(
                                self.interner.get_or_intern_static("case"),
                            ));
                            self.case_blocks.insert(*stmt, target);
                            let dst = self.make_tmp();
                            let rhs = self.build_from_expr(*expr);
                            self.append_to_block(
                                Inst::Binary {
                                    op: BinaryOp::Equal,
                                    lhs,
                                    rhs,
                                    dst,
                                },
                                *self.ast.stmt_span(*stmt),
                            );
                            self.append_to_block(
                                Inst::JumpIfNotZero { cond: dst, target },
                                *self.ast.stmt_span(*stmt),
                            );
                        }
                    });
                    match switch.default {
                        None => {
                            self.append_to_block(Inst::Jump(cont_block), *self.ast.stmt_span(stmt));
                        }
                        Some(stmt) => {
                            let target = self.fn_def.push_block(Block::with_label(
                                self.interner.get_or_intern_static("default"),
                            ));
                            self.case_blocks.insert(stmt, target);
                            self.append_to_block(Inst::Jump(target), *self.ast.stmt_span(stmt));
                        }
                    }
                    self.build_from_stmt(*body);
                }

                self.append_to_block(Inst::Jump(cont_block), *self.ast.stmt_span(stmt));

                self.block = cont_block;
            }
        }
    }

    fn build_from_expr(&mut self, expr: ast::ExprRef) -> Value {
        let span = *self.ast.expr_span(expr);
        match self.ast.expr(expr) {
            ast::Expr::Call { .. } => todo!("handle function calls"),
            ast::Expr::Const(c) => Value::Const(*c),
            ast::Expr::Grouped(expr) => self.build_from_expr(*expr),
            ast::Expr::Var(_) => self.get_or_make_some_var(self.ctx.vars.get(&expr).cloned()),
            ast::Expr::Unary { op, expr } => match op {
                ast::UnaryOp::Neg => self.build_unary_op(UnaryOp::Neg, *expr, span),
                ast::UnaryOp::BitNot => self.build_unary_op(UnaryOp::BitNot, *expr, span),
                ast::UnaryOp::LogicalNot => self.build_unary_op(UnaryOp::Not, *expr, span),
                ast::UnaryOp::PreInc => self.build_prefix_unary_op(UnaryOp::Inc, *expr, span),
                ast::UnaryOp::PreDec => self.build_prefix_unary_op(UnaryOp::Dec, *expr, span),
                ast::UnaryOp::PostInc => self.build_postfix_unary_op(UnaryOp::Inc, *expr, span),
                ast::UnaryOp::PostDec => self.build_postfix_unary_op(UnaryOp::Dec, *expr, span),
            },
            ast::Expr::Binary { op, lhs, rhs } => match op {
                ast::BinaryOp::LogicalOr => self.build_short_circuit(true, *lhs, *rhs, span),
                ast::BinaryOp::LogicalAnd => self.build_short_circuit(false, *lhs, *rhs, span),
                ast::BinaryOp::Equal => self.build_binary_op(BinaryOp::Equal, *lhs, *rhs, span),
                ast::BinaryOp::NotEqual => {
                    self.build_binary_op(BinaryOp::NotEqual, *lhs, *rhs, span)
                }
                ast::BinaryOp::LessThan => {
                    self.build_binary_op(BinaryOp::LessThan, *lhs, *rhs, span)
                }
                ast::BinaryOp::LessEqual => {
                    self.build_binary_op(BinaryOp::LessEqual, *lhs, *rhs, span)
                }
                ast::BinaryOp::GreaterThan => {
                    self.build_binary_op(BinaryOp::GreaterThan, *lhs, *rhs, span)
                }
                ast::BinaryOp::GreaterEqual => {
                    self.build_binary_op(BinaryOp::GreaterEqual, *lhs, *rhs, span)
                }
                ast::BinaryOp::Add => self.build_binary_op(BinaryOp::Add, *lhs, *rhs, span),
                ast::BinaryOp::Sub => self.build_binary_op(BinaryOp::Sub, *lhs, *rhs, span),
                ast::BinaryOp::Mul => self.build_binary_op(BinaryOp::Mul, *lhs, *rhs, span),
                ast::BinaryOp::Div => self.build_binary_op(BinaryOp::Div, *lhs, *rhs, span),
                ast::BinaryOp::Rem => self.build_binary_op(BinaryOp::Rem, *lhs, *rhs, span),
                ast::BinaryOp::BitOr => self.build_binary_op(BinaryOp::BitOr, *lhs, *rhs, span),
                ast::BinaryOp::BitAnd => self.build_binary_op(BinaryOp::BitAnd, *lhs, *rhs, span),
                ast::BinaryOp::BitXor => self.build_binary_op(BinaryOp::BitXor, *lhs, *rhs, span),
                ast::BinaryOp::BitShl => self.build_binary_op(BinaryOp::BitShl, *lhs, *rhs, span),
                ast::BinaryOp::BitShr => self.build_binary_op(BinaryOp::BitShr, *lhs, *rhs, span),
                ast::BinaryOp::Assign => {
                    let lhs = self.build_from_expr(*lhs);
                    let rhs = self.build_from_expr(*rhs);
                    self.append_to_block(Inst::Copy { src: rhs, dst: lhs }, span);
                    lhs
                }
                ast::BinaryOp::AddAssign => {
                    self.build_binary_assign_op(BinaryOp::Add, *lhs, *rhs, span)
                }
                ast::BinaryOp::SubAssign => {
                    self.build_binary_assign_op(BinaryOp::Sub, *lhs, *rhs, span)
                }
                ast::BinaryOp::MulAssign => {
                    self.build_binary_assign_op(BinaryOp::Mul, *lhs, *rhs, span)
                }
                ast::BinaryOp::DivAssign => {
                    self.build_binary_assign_op(BinaryOp::Div, *lhs, *rhs, span)
                }
                ast::BinaryOp::RemAssign => {
                    self.build_binary_assign_op(BinaryOp::Rem, *lhs, *rhs, span)
                }
                ast::BinaryOp::BitOrAssign => {
                    self.build_binary_assign_op(BinaryOp::BitOr, *lhs, *rhs, span)
                }
                ast::BinaryOp::BitAndAssign => {
                    self.build_binary_assign_op(BinaryOp::BitAnd, *lhs, *rhs, span)
                }
                ast::BinaryOp::BitXorAssign => {
                    self.build_binary_assign_op(BinaryOp::BitXor, *lhs, *rhs, span)
                }
                ast::BinaryOp::BitShlAssign => {
                    self.build_binary_assign_op(BinaryOp::BitShl, *lhs, *rhs, span)
                }
                ast::BinaryOp::BitShrAssign => {
                    self.build_binary_assign_op(BinaryOp::BitShr, *lhs, *rhs, span)
                }
            },
            ast::Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                let dst = self.make_tmp();
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));
                let otherwise_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("otherwise"),
                ));

                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Inst::JumpIfZero {
                        cond,
                        target: otherwise_block,
                    },
                    span,
                );
                let then = self.build_from_expr(*then);
                self.append_to_block(Inst::Copy { src: then, dst }, span);
                self.append_to_block(Inst::Jump(cont_block), span);

                self.block = otherwise_block;
                let otherwise = self.build_from_expr(*otherwise);
                self.append_to_block(
                    Inst::Copy {
                        src: otherwise,
                        dst,
                    },
                    span,
                );
                self.append_to_block(Inst::Jump(cont_block), span);

                self.block = cont_block;
                dst
            }
        }
    }

    fn build_unary_op(&mut self, op: UnaryOp, expr: ast::ExprRef, span: SourceSpan) -> Value {
        let dst = self.make_tmp();
        let src = self.build_from_expr(expr);
        self.append_to_block(Inst::Unary { op, src, dst }, span);
        dst
    }

    fn build_prefix_unary_op(
        &mut self,
        op: UnaryOp,
        expr: ast::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let src = self.build_from_expr(expr);
        self.append_to_block(Inst::Unary { op, src, dst: src }, span);
        src
    }

    fn build_postfix_unary_op(
        &mut self,
        op: UnaryOp,
        expr: ast::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let dst = self.make_tmp();
        let src = self.build_from_expr(expr);
        self.append_to_block(Inst::Copy { src, dst }, span);
        self.append_to_block(Inst::Unary { op, src, dst: src }, span);
        dst
    }

    fn build_binary_op(
        &mut self,
        op: BinaryOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let dst = self.make_tmp();
        let lhs = self.build_from_expr(lhs);
        let rhs = self.build_from_expr(rhs);
        self.append_to_block(Inst::Binary { op, lhs, rhs, dst }, span);
        dst
    }

    fn build_binary_assign_op(
        &mut self,
        op: BinaryOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let lhs = self.build_from_expr(lhs);
        let rhs = self.build_from_expr(rhs);
        self.append_to_block(
            Inst::Binary {
                op,
                lhs,
                rhs,
                dst: lhs,
            },
            span,
        );
        lhs
    }

    fn build_short_circuit(
        &mut self,
        skips_on: bool,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let dst = self.make_tmp();
        let (skip_value, other_value) = if skips_on { (1, 0) } else { (0, 1) };

        let skip_block = self.fn_def.push_block(Block::with_label(
            self.interner.get_or_intern_static("skip"),
        ));
        let cont_block = self.fn_def.push_block(Block::with_label(
            self.interner.get_or_intern_static("cont"),
        ));

        let lhs = self.build_from_expr(lhs);
        self.append_to_block(
            if skips_on {
                Inst::JumpIfNotZero {
                    cond: lhs,
                    target: skip_block,
                }
            } else {
                Inst::JumpIfZero {
                    cond: lhs,
                    target: skip_block,
                }
            },
            span,
        );

        let rhs = self.build_from_expr(rhs);
        self.append_to_block(
            if skips_on {
                Inst::JumpIfNotZero {
                    cond: rhs,
                    target: skip_block,
                }
            } else {
                Inst::JumpIfZero {
                    cond: rhs,
                    target: skip_block,
                }
            },
            span,
        );

        self.append_to_block(
            Inst::Copy {
                src: Value::Const(other_value),
                dst,
            },
            span,
        );
        self.append_to_block(Inst::Jump(cont_block), span);

        self.block = skip_block;
        self.append_to_block(
            Inst::Copy {
                src: Value::Const(skip_value),
                dst,
            },
            span,
        );
        self.append_to_block(Inst::Jump(cont_block), span);

        self.block = cont_block;
        dst
    }
}
