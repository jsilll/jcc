use crate::parse;

use tacky::{
    source_file::SourceSpan, string_interner::DefaultStringInterner, BinaryOp, Block, BlockRef,
    FnDef, Instr, Program, UnaryOp, Value,
};

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
        }
    }

    fn build(mut self, item_ref: parse::ItemRef, item: &parse::Item) -> FnDef {
        self.fn_def.span = *self.ast.get_item_span(item_ref);
        item.body.iter().for_each(|item| match item {
            parse::BlockItem::Decl(_) => todo!("handle declarations"),
            parse::BlockItem::Stmt(stmt) => self.build_from_stmt(*stmt),
        });
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

    fn build_from_stmt(&mut self, stmt: parse::StmtRef) {
        match self.ast.get_stmt(stmt) {
            parse::Stmt::Empty => todo!("handle empty statements"),
            parse::Stmt::Expr(_) => todo!("handle expressions"),
            parse::Stmt::Return(inner) => {
                let value = self.build_from_expr(*inner);
                self.append_to_block(Instr::Return(value), *self.ast.get_stmt_span(stmt));
            }
        }
    }

    fn build_from_expr(&mut self, expr: parse::ExprRef) -> Value {
        let span = *self.ast.get_expr_span(expr);
        match self.ast.get_expr(expr) {
            parse::Expr::Constant(value) => Value::Constant(*value),
            parse::Expr::Var { .. } => todo!("handle named variables"),
            parse::Expr::Grouped(mut inner) => {
                while let parse::Expr::Grouped(expr) = self.ast.get_expr(inner) {
                    inner = *expr
                }
                self.build_from_expr(inner)
            }
            parse::Expr::Unary { op, expr: inner } => {
                let op = UnaryOp::from(*op);
                let src = self.build_from_expr(*inner);
                let dst = self.make_tmp();
                self.append_to_block(Instr::Unary { op, src, dst }, span);
                dst
            }
            parse::Expr::Binary { op, lhs, rhs } => match op {
                parse::BinaryOp::LogicalOr => self.build_short_circuit(true, *lhs, *rhs, span),
                parse::BinaryOp::LogicalAnd => self.build_short_circuit(false, *lhs, *rhs, span),
                _ => {
                    let op = BinaryOp::try_from(*op).expect("unexpected binary operator");
                    let lhs = self.build_from_expr(*lhs);
                    let rhs = self.build_from_expr(*rhs);
                    let dst = self.make_tmp();
                    self.append_to_block(
                        Instr::Binary { op, lhs, rhs, dst },
                        *self.ast.get_expr_span(expr),
                    );
                    dst
                }
            },
        }
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

        self.block = cont_block;
        dst
    }
}
