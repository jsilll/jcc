use crate::parse;

use tacky::{
    source_file::SourceSpan, string_interner::DefaultStringInterner, BinaryOp, Block, BlockRef,
    FnDef, Instr, Program, UnaryOp, Value,
};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// TackyBuilder
// ---------------------------------------------------------------------------

pub struct TackyBuilder<'a> {
    ast: &'a parse::Ast,
    interner: &'a mut DefaultStringInterner,
}

impl<'a> TackyBuilder<'a> {
    pub fn new(ast: &'a parse::Ast, interner: &'a mut DefaultStringInterner) -> Self {
        Self { ast, interner }
    }

    pub fn build(self) -> Program {
        let (item_ref, item) = self
            .ast
            .items_iter_both()
            .next()
            .expect("expected at least one item in the AST");
        let fn_def = TackyFnDefBuilder::new(self.ast, self.interner).build(item_ref, item);
        Program(fn_def)
    }
}

// ---------------------------------------------------------------------------
// TackyFnDefBuilder
// ---------------------------------------------------------------------------

struct TackyFnDefBuilder<'a> {
    fn_def: FnDef,
    tmp_count: u32,
    block: BlockRef,
    ast: &'a parse::Ast,
    interner: &'a mut DefaultStringInterner,
    variables: HashMap<parse::DeclRef, Value>,
}

impl<'a> TackyFnDefBuilder<'a> {
    fn new(ast: &'a parse::Ast, interner: &'a mut DefaultStringInterner) -> Self {
        let mut fn_def = FnDef::default();
        let block = fn_def.push_block(Block::default());
        Self {
            ast,
            block,
            fn_def,
            interner,
            tmp_count: 0,
            variables: HashMap::new(),
        }
    }

    fn build(mut self, item_ref: parse::ItemRef, item: &parse::Item) -> FnDef {
        self.fn_def.span = *self.ast.get_item_span(item_ref);
        item.body.iter().for_each(|item| match item {
            parse::BlockItem::Decl(decl) => self.build_from_decl(*decl),
            parse::BlockItem::Stmt(stmt) => self.build_from_stmt(*stmt),
        });
        match item.body.last() {
            Some(parse::BlockItem::Stmt(stmt)) => {
                if !matches!(self.ast.get_stmt(*stmt), parse::Stmt::Return(_)) {
                    self.append_to_block(Instr::Return(Value::Constant(0)), self.fn_def.span);
                }
            }
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
                    let var = self.get_or_make_var(decl);
                    let init = self.build_from_expr_rvalue(*init);
                    self.append_to_block(
                        Instr::Copy {
                            src: init,
                            dst: var,
                        },
                        span,
                    );
                }
            }
        }
    }

    fn build_from_stmt(&mut self, stmt: parse::StmtRef) {
        match self.ast.get_stmt(stmt) {
            parse::Stmt::Empty => {}
            parse::Stmt::Expr(expr) => {
                self.build_from_expr_rvalue(*expr);
            }
            parse::Stmt::Return(inner) => {
                let value = self.build_from_expr_rvalue(*inner);
                self.append_to_block(Instr::Return(value), *self.ast.get_stmt_span(stmt));
            }
        }
    }

    fn build_from_expr_lvalue(&mut self, expr: parse::ExprRef) -> Value {
        match self.ast.get_expr(expr) {
            parse::Expr::Var { decl, .. } => self.get_or_make_some_var(*decl),
            _ => panic!("expected an lvalue expression"),
        }
    }

    fn build_from_expr_rvalue(&mut self, expr: parse::ExprRef) -> Value {
        let span = *self.ast.get_expr_span(expr);
        match self.ast.get_expr(expr) {
            parse::Expr::Constant(value) => Value::Constant(*value),
            parse::Expr::Var { decl, .. } => self.get_or_make_some_var(*decl),
            parse::Expr::Grouped(mut inner) => {
                while let parse::Expr::Grouped(expr) = self.ast.get_expr(inner) {
                    inner = *expr
                }
                self.build_from_expr_rvalue(inner)
            }
            parse::Expr::Unary { op, expr: inner } => {
                let op = UnaryOp::from(*op);
                let src = self.build_from_expr_rvalue(*inner);
                let dst = self.make_tmp();
                self.append_to_block(Instr::Unary { op, src, dst }, span);
                dst
            }
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
                    let lhs = self.build_from_expr_lvalue(*lhs);
                    let rhs = self.build_from_expr_rvalue(*rhs);
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
        }
    }

    fn build_binary_op(
        &mut self,
        op: BinaryOp,
        lhs: parse::ExprRef,
        rhs: parse::ExprRef,
        span: SourceSpan,
    ) -> Value {
        let dst = self.make_tmp();
        let lhs = self.build_from_expr_rvalue(lhs);
        let rhs = self.build_from_expr_rvalue(rhs);
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
        let lhs = self.build_from_expr_lvalue(lhs);
        let rhs = self.build_from_expr_rvalue(rhs);
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

        let cont_block = self.fn_def.push_block(Block::with_label(
            self.interner.get_or_intern_static("cont"),
        ));
        let skip_block = self.fn_def.push_block(
            Block::with_label(self.interner.get_or_intern_static("skip")).with_instrs(
                &[
                    Instr::Copy {
                        src: Value::Constant(skip_value),
                        dst,
                    },
                    Instr::Jump(cont_block),
                ],
                span,
            ),
        );

        let lhs = self.build_from_expr_rvalue(lhs);
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

        let rhs = self.build_from_expr_rvalue(rhs);
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

        self.block = cont_block;
        dst
    }
}
