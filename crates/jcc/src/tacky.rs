use crate::{
    parse::{self, Ast},
    sema::SemaCtx,
};

use tacky::{
    source_file::SourceSpan,
    string_interner::{DefaultStringInterner, DefaultSymbol},
    BinaryOp, Block, BlockRef, FnDef, Instr, Program, UnaryOp, Value,
};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// TackyBuilder
// ---------------------------------------------------------------------------

pub struct TackyBuilder<'a> {
    ctx: &'a SemaCtx,
    interner: &'a mut DefaultStringInterner,
}

impl<'a> TackyBuilder<'a> {
    pub fn new(ctx: &'a SemaCtx, interner: &'a mut DefaultStringInterner) -> Self {
        Self { ctx, interner }
    }

    pub fn build(self, ast: &Ast) -> Program {
        let (item_ref, item) = ast
            .items_iter_both()
            .next()
            .expect("expected at least one item in the AST");
        let fn_def = TackyFnDefBuilder::new(ast, self.ctx, self.interner).build(item_ref, item);
        Program(fn_def)
    }
}

// ---------------------------------------------------------------------------
// TackyFnDefBuilder
// ---------------------------------------------------------------------------

struct TackyFnDefBuilder<'a> {
    ast: &'a Ast,
    ctx: &'a SemaCtx,
    interner: &'a mut DefaultStringInterner,
    fn_def: FnDef,
    tmp_count: u32,
    block: BlockRef,
    variables: HashMap<parse::DeclRef, Value>,
    case_blocks: HashMap<parse::StmtRef, BlockRef>,
    break_blocks: HashMap<parse::StmtRef, BlockRef>,
    labeled_blocks: HashMap<DefaultSymbol, BlockRef>,
    continue_blocks: HashMap<parse::StmtRef, BlockRef>,
}

impl<'a> TackyFnDefBuilder<'a> {
    fn new(ast: &'a parse::Ast, ctx: &'a SemaCtx, interner: &'a mut DefaultStringInterner) -> Self {
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

    fn build(mut self, item_ref: parse::ItemRef, item: &parse::Item) -> FnDef {
        self.fn_def.span = *self.ast.get_item_span(item_ref);
        self.ast
            .get_block_items(item.body)
            .iter()
            .for_each(|item| match item {
                parse::BlockItem::Decl(decl) => self.build_from_decl(*decl),
                parse::BlockItem::Stmt(stmt) => self.build_from_stmt(*stmt),
            });
        match self.ast.get_block_items(item.body).last() {
            Some(parse::BlockItem::Stmt(stmt)) => match self.ast.get_stmt(*stmt) {
                parse::Stmt::Return(_) => {}
                _ => self.append_to_block(Instr::Return(Value::Constant(0)), self.fn_def.span),
            },
            _ => self.append_to_block(Instr::Return(Value::Constant(0)), self.fn_def.span),
        }
        self.fn_def
    }

    fn make_tmp(&mut self) -> Value {
        let tmp = Value::Variable(self.tmp_count);
        self.tmp_count += 1;
        tmp
    }

    fn append_to_block(&mut self, instr: Instr, span: SourceSpan) {
        let root = self.fn_def.get_block_mut(self.block);
        root.instrs.push(instr);
        root.spans.push(span);
    }

    fn get_or_make_block(&mut self, label: DefaultSymbol) -> BlockRef {
        self.labeled_blocks
            .entry(label)
            .or_insert_with(|| self.fn_def.push_block(Block::with_label(label)))
            .clone()
    }

    fn get_or_make_var(&mut self, decl: parse::DeclRef) -> Value {
        self.variables
            .entry(decl)
            .or_insert_with(|| {
                let var = Value::Variable(self.tmp_count);
                self.tmp_count += 1;
                var
            })
            .clone()
    }

    fn get_or_make_some_var(&mut self, decl: Option<parse::DeclRef>) -> Value {
        self.variables
            .entry(decl.expect("expected a resolved variable"))
            .or_insert_with(|| {
                let var = Value::Variable(self.tmp_count);
                self.tmp_count += 1;
                var
            })
            .clone()
    }

    fn build_from_decl(&mut self, decl: parse::DeclRef) {
        let span = *self.ast.get_decl_span(decl);
        match self.ast.get_decl(decl) {
            parse::Decl::Var { init, .. } => {
                if let Some(init) = init {
                    let dst = self.get_or_make_var(decl);
                    let src = self.build_from_expr(*init);
                    self.append_to_block(Instr::Copy { src, dst }, span);
                }
            }
        }
    }

    fn build_from_stmt(&mut self, stmt: parse::StmtRef) {
        match self.ast.get_stmt(stmt) {
            parse::Stmt::Empty => {}
            parse::Stmt::Break(inner) => {
                let block = self
                    .break_blocks
                    .get(&inner.expect("expected a break block"))
                    .expect("expected a break block");
                self.append_to_block(Instr::Jump(*block), *self.ast.get_stmt_span(stmt));
            }
            parse::Stmt::Continue(inner) => {
                let block = self
                    .continue_blocks
                    .get(&inner.expect("expected a continue block"))
                    .expect("expected a continue block");
                self.append_to_block(Instr::Jump(*block), *self.ast.get_stmt_span(stmt));
            }
            parse::Stmt::Expr(expr) => {
                self.build_from_expr(*expr);
            }
            parse::Stmt::Return(expr) => {
                let value = self.build_from_expr(*expr);
                self.append_to_block(Instr::Return(value), *self.ast.get_stmt_span(stmt));
            }
            parse::Stmt::Goto(label) => {
                let block = self.get_or_make_block(*label);
                self.append_to_block(Instr::Jump(block), *self.ast.get_stmt_span(stmt));
            }
            parse::Stmt::Label { label, stmt: inner } => {
                let block = self.get_or_make_block(*label);
                self.append_to_block(Instr::Jump(block), *self.ast.get_stmt_span(stmt));
                self.block = block;
                self.build_from_stmt(*inner);
            }
            parse::Stmt::Default(inner) => {
                let block = self
                    .case_blocks
                    .get(&stmt)
                    .expect("expected a case block")
                    .clone();
                self.append_to_block(Instr::Jump(block), *self.ast.get_stmt_span(stmt));
                self.block = block;
                self.build_from_stmt(*inner);
            }
            parse::Stmt::Case { stmt: inner, .. } => {
                let block = self
                    .case_blocks
                    .get(&stmt)
                    .expect("expected a case block")
                    .clone();
                self.append_to_block(Instr::Jump(block), *self.ast.get_stmt_span(stmt));
                self.block = block;
                self.build_from_stmt(*inner);
            }
            parse::Stmt::Compound(items) => {
                self.ast
                    .get_block_items(*items)
                    .iter()
                    .for_each(|item| match item {
                        parse::BlockItem::Decl(decl) => self.build_from_decl(*decl),
                        parse::BlockItem::Stmt(stmt) => self.build_from_stmt(*stmt),
                    });
            }
            parse::Stmt::If {
                cond,
                then,
                otherwise: None,
            } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));

                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Instr::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.get_stmt_span(stmt),
                );
                self.build_from_stmt(*then);
                self.append_to_block(Instr::Jump(cont_block), *self.ast.get_stmt_span(stmt));
                self.block = cont_block;
            }
            parse::Stmt::If {
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
                    Instr::JumpIfZero {
                        cond,
                        target: else_block,
                    },
                    *self.ast.get_stmt_span(stmt),
                );
                self.build_from_stmt(*then);
                self.append_to_block(Instr::Jump(cont_block), *self.ast.get_stmt_span(stmt));
                self.block = else_block;
                self.build_from_stmt(*otherwise);
                self.append_to_block(Instr::Jump(cont_block), *self.ast.get_stmt_span(stmt));
                self.block = cont_block;
            }
            parse::Stmt::While { cond, body } => {
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

                self.append_to_block(Instr::Jump(cond_block), *self.ast.get_stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Instr::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.get_stmt_span(stmt),
                );
                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Instr::Jump(cond_block), *self.ast.get_stmt_span(stmt));

                self.block = cont_block;
            }
            parse::Stmt::DoWhile { body, cond } => {
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

                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Instr::Jump(cond_block), *self.ast.get_stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Instr::JumpIfNotZero {
                        cond,
                        target: loop_block,
                    },
                    *self.ast.get_stmt_span(stmt),
                );
                self.append_to_block(Instr::Jump(cont_block), *self.ast.get_stmt_span(stmt));

                self.block = cont_block;
            }
            parse::Stmt::For {
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
                        parse::ForInit::Decl(decl) => self.build_from_decl(*decl),
                        parse::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                    }
                }
                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = cont_block;
            }
            parse::Stmt::For {
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
                        parse::ForInit::Decl(decl) => self.build_from_decl(*decl),
                        parse::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                    }
                }
                self.append_to_block(Instr::Jump(cond_block), *self.ast.get_stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Instr::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.get_stmt_span(stmt),
                );
                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Instr::Jump(cond_block), *self.ast.get_stmt_span(stmt));

                self.block = cont_block;
            }
            parse::Stmt::For {
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
                        parse::ForInit::Decl(decl) => self.build_from_decl(*decl),
                        parse::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                    }
                }
                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Instr::Jump(step_block), *self.ast.get_stmt_span(stmt));

                self.block = step_block;
                self.build_from_expr(*step);
                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = cont_block;
            }
            parse::Stmt::For {
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
                        parse::ForInit::Decl(decl) => self.build_from_decl(*decl),
                        parse::ForInit::Expr(expr) => {
                            self.build_from_expr(*expr);
                        }
                    }
                }
                self.append_to_block(Instr::Jump(cond_block), *self.ast.get_stmt_span(stmt));

                self.block = cond_block;
                let cond = self.build_from_expr(*cond);
                self.append_to_block(
                    Instr::JumpIfZero {
                        cond,
                        target: cont_block,
                    },
                    *self.ast.get_stmt_span(stmt),
                );
                self.append_to_block(Instr::Jump(loop_block), *self.ast.get_stmt_span(stmt));

                self.block = loop_block;
                self.build_from_stmt(*body);
                self.append_to_block(Instr::Jump(step_block), *self.ast.get_stmt_span(stmt));

                self.block = step_block;
                self.build_from_expr(*step);
                self.append_to_block(Instr::Jump(cond_block), *self.ast.get_stmt_span(stmt));

                self.block = cont_block;
            }
            parse::Stmt::Switch { cond, body } => {
                let cont_block = self.fn_def.push_block(Block::with_label(
                    self.interner.get_or_intern_static("cont"),
                ));

                self.break_blocks.insert(stmt, cont_block);

                let lhs = self.build_from_expr(*cond);
                if let Some(switch) = self.ctx.switches.get(&stmt) {
                    switch.cases.iter().for_each(|stmt| {
                        if let parse::Stmt::Case { expr, .. } = self.ast.get_stmt(*stmt) {
                            let target = self.fn_def.push_block(Block::with_label(
                                self.interner.get_or_intern_static("case"),
                            ));
                            self.case_blocks.insert(*stmt, target);
                            let dst = self.make_tmp();
                            let rhs = self.build_from_expr(*expr);
                            self.append_to_block(
                                Instr::Binary {
                                    op: BinaryOp::Equal,
                                    lhs,
                                    rhs,
                                    dst,
                                },
                                *self.ast.get_stmt_span(*stmt),
                            );
                            self.append_to_block(
                                Instr::JumpIfNotZero { cond: dst, target },
                                *self.ast.get_stmt_span(*stmt),
                            );
                        }
                    });
                    match switch.default {
                        None => {
                            self.append_to_block(
                                Instr::Jump(cont_block),
                                *self.ast.get_stmt_span(stmt),
                            );
                        }
                        Some(stmt) => {
                            let target = self.fn_def.push_block(Block::with_label(
                                self.interner.get_or_intern_static("default"),
                            ));
                            self.case_blocks.insert(stmt, target);
                            self.append_to_block(
                                Instr::Jump(target),
                                *self.ast.get_stmt_span(stmt),
                            );
                        }
                    }
                    self.build_from_stmt(*body);
                }

                self.append_to_block(Instr::Jump(cont_block), *self.ast.get_stmt_span(stmt));

                self.block = cont_block;
            }
        }
    }

    fn build_from_expr(&mut self, expr: parse::ExprRef) -> Value {
        let span = *self.ast.get_expr_span(expr);
        match self.ast.get_expr(expr) {
            parse::Expr::Constant(value) => Value::Constant(*value),
            parse::Expr::Var { decl, .. } => self.get_or_make_some_var(*decl),
            parse::Expr::Grouped(mut expr) => {
                while let parse::Expr::Grouped(inner) = self.ast.get_expr(expr) {
                    expr = *inner
                }
                self.build_from_expr(expr)
            }
            parse::Expr::Unary { op, expr } => match op {
                parse::UnaryOp::Neg => self.build_unary_op(UnaryOp::Neg, *expr, span),
                parse::UnaryOp::BitNot => self.build_unary_op(UnaryOp::BitNot, *expr, span),
                parse::UnaryOp::LogicalNot => self.build_unary_op(UnaryOp::Not, *expr, span),
                parse::UnaryOp::PreInc => self.build_prefix_unary_op(UnaryOp::Inc, *expr, span),
                parse::UnaryOp::PreDec => self.build_prefix_unary_op(UnaryOp::Dec, *expr, span),
                parse::UnaryOp::PostInc => self.build_postfix_unary_op(UnaryOp::Inc, *expr, span),
                parse::UnaryOp::PostDec => self.build_postfix_unary_op(UnaryOp::Dec, *expr, span),
            },
            parse::Expr::Binary { op, lhs, rhs } => match op {
                parse::BinaryOp::LogicalOr => self.build_short_circuit(true, *lhs, *rhs, span),
                parse::BinaryOp::LogicalAnd => self.build_short_circuit(false, *lhs, *rhs, span),
                parse::BinaryOp::Equal => self.build_binary_op(BinaryOp::Equal, *lhs, *rhs, span),
                parse::BinaryOp::NotEqual => {
                    self.build_binary_op(BinaryOp::NotEqual, *lhs, *rhs, span)
                }
                parse::BinaryOp::LessThan => {
                    self.build_binary_op(BinaryOp::LessThan, *lhs, *rhs, span)
                }
                parse::BinaryOp::LessEqual => {
                    self.build_binary_op(BinaryOp::LessEqual, *lhs, *rhs, span)
                }
                parse::BinaryOp::GreaterThan => {
                    self.build_binary_op(BinaryOp::GreaterThan, *lhs, *rhs, span)
                }
                parse::BinaryOp::GreaterEqual => {
                    self.build_binary_op(BinaryOp::GreaterEqual, *lhs, *rhs, span)
                }
                parse::BinaryOp::Add => self.build_binary_op(BinaryOp::Add, *lhs, *rhs, span),
                parse::BinaryOp::Sub => self.build_binary_op(BinaryOp::Sub, *lhs, *rhs, span),
                parse::BinaryOp::Mul => self.build_binary_op(BinaryOp::Mul, *lhs, *rhs, span),
                parse::BinaryOp::Div => self.build_binary_op(BinaryOp::Div, *lhs, *rhs, span),
                parse::BinaryOp::Rem => self.build_binary_op(BinaryOp::Rem, *lhs, *rhs, span),
                parse::BinaryOp::BitOr => self.build_binary_op(BinaryOp::BitOr, *lhs, *rhs, span),
                parse::BinaryOp::BitAnd => self.build_binary_op(BinaryOp::BitAnd, *lhs, *rhs, span),
                parse::BinaryOp::BitXor => self.build_binary_op(BinaryOp::BitXor, *lhs, *rhs, span),
                parse::BinaryOp::BitLsh => self.build_binary_op(BinaryOp::BitShl, *lhs, *rhs, span),
                parse::BinaryOp::BitRsh => self.build_binary_op(BinaryOp::BitShr, *lhs, *rhs, span),
                parse::BinaryOp::Assign => {
                    let lhs = self.build_from_expr(*lhs);
                    let rhs = self.build_from_expr(*rhs);
                    self.append_to_block(Instr::Copy { src: rhs, dst: lhs }, span);
                    lhs
                }
                parse::BinaryOp::AddAssign => {
                    self.build_binary_assign_op(BinaryOp::Add, *lhs, *rhs, span)
                }
                parse::BinaryOp::SubAssign => {
                    self.build_binary_assign_op(BinaryOp::Sub, *lhs, *rhs, span)
                }
                parse::BinaryOp::MulAssign => {
                    self.build_binary_assign_op(BinaryOp::Mul, *lhs, *rhs, span)
                }
                parse::BinaryOp::DivAssign => {
                    self.build_binary_assign_op(BinaryOp::Div, *lhs, *rhs, span)
                }
                parse::BinaryOp::RemAssign => {
                    self.build_binary_assign_op(BinaryOp::Rem, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitOrAssign => {
                    self.build_binary_assign_op(BinaryOp::BitOr, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitAndAssign => {
                    self.build_binary_assign_op(BinaryOp::BitAnd, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitXorAssign => {
                    self.build_binary_assign_op(BinaryOp::BitXor, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitLshAssign => {
                    self.build_binary_assign_op(BinaryOp::BitShl, *lhs, *rhs, span)
                }
                parse::BinaryOp::BitRshAssign => {
                    self.build_binary_assign_op(BinaryOp::BitShr, *lhs, *rhs, span)
                }
            },
            parse::Expr::Ternary {
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
                    Instr::JumpIfZero {
                        cond,
                        target: otherwise_block,
                    },
                    span,
                );
                let then = self.build_from_expr(*then);
                self.append_to_block(Instr::Copy { src: then, dst }, span);
                self.append_to_block(Instr::Jump(cont_block), span);

                self.block = otherwise_block;
                let otherwise = self.build_from_expr(*otherwise);
                self.append_to_block(
                    Instr::Copy {
                        src: otherwise,
                        dst,
                    },
                    span,
                );
                self.append_to_block(Instr::Jump(cont_block), span);

                self.block = cont_block;
                dst
            }
        }
    }

    fn build_unary_op(&mut self, op: UnaryOp, expr: parse::ExprRef, span: SourceSpan) -> Value {
        let dst = self.make_tmp();
        let src = self.build_from_expr(expr);
        self.append_to_block(Instr::Unary { op, src, dst }, span);
        dst
    }

    fn build_prefix_unary_op(
        &mut self,
        op: UnaryOp,
        expr: parse::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let src = self.build_from_expr(expr);
        self.append_to_block(Instr::Unary { op, src, dst: src }, span);
        src
    }

    fn build_postfix_unary_op(
        &mut self,
        op: UnaryOp,
        expr: parse::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let dst = self.make_tmp();
        let src = self.build_from_expr(expr);
        self.append_to_block(Instr::Copy { src, dst }, span);
        self.append_to_block(Instr::Unary { op, src, dst: src }, span);
        dst
    }

    fn build_binary_op(
        &mut self,
        op: BinaryOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let dst = self.make_tmp();
        let lhs = self.build_from_expr(lhs);
        let rhs = self.build_from_expr(rhs);
        self.append_to_block(Instr::Binary { op, lhs, rhs, dst }, span);
        dst
    }

    fn build_binary_assign_op(
        &mut self,
        op: BinaryOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let lhs = self.build_from_expr(lhs);
        let rhs = self.build_from_expr(rhs);
        self.append_to_block(
            Instr::Binary {
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
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
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
                Instr::JumpIfNotZero {
                    cond: lhs,
                    target: skip_block,
                }
            } else {
                Instr::JumpIfZero {
                    cond: lhs,
                    target: skip_block,
                }
            },
            span,
        );

        let rhs = self.build_from_expr(rhs);
        self.append_to_block(
            if skips_on {
                Instr::JumpIfNotZero {
                    cond: rhs,
                    target: skip_block,
                }
            } else {
                Instr::JumpIfZero {
                    cond: rhs,
                    target: skip_block,
                }
            },
            span,
        );

        self.append_to_block(
            Instr::Copy {
                src: Value::Constant(other_value),
                dst,
            },
            span,
        );
        self.append_to_block(Instr::Jump(cont_block), span);

        self.block = skip_block;
        self.append_to_block(
            Instr::Copy {
                src: Value::Constant(skip_value),
                dst,
            },
            span,
        );
        self.append_to_block(Instr::Jump(cont_block), span);

        self.block = cont_block;
        dst
    }
}
