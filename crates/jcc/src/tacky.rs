use crate::parser;

use tacky::{
    source_file::SourceSpan, string_interner::DefaultStringInterner, BinaryOp, Block, BlockRef,
    FnDef, Instr, Program, UnaryOp, Value,
};

// ---------------------------------------------------------------------------
// TackyBuilder
// ---------------------------------------------------------------------------

pub struct TackyBuilder<'a> {
    ast: &'a parser::Ast,
    interner: &'a mut DefaultStringInterner,
}

impl<'a> TackyBuilder<'a> {
    pub fn new(ast: &'a parser::Ast, interner: &'a mut DefaultStringInterner) -> Self {
        Self { ast, interner }
    }

    pub fn build(mut self) -> Program {
        Program(self.build_from_item(&self.ast.items()[0]))
    }

    fn build_from_item(&mut self, item: &parser::Item) -> FnDef {
        TackyFnDefBuilder::new(self.ast, self.interner).build(item)
    }
}

// ---------------------------------------------------------------------------
// TackyFnDefBuilder
// ---------------------------------------------------------------------------

struct TackyFnDefBuilder<'a> {
    fn_def: FnDef,
    tmp_count: u32,
    block: BlockRef,
    ast: &'a parser::Ast,
    interner: &'a mut DefaultStringInterner,
}

impl<'a> TackyFnDefBuilder<'a> {
    fn new(ast: &'a parser::Ast, interner: &'a mut DefaultStringInterner) -> Self {
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

    fn build(mut self, item: &parser::Item) -> FnDef {
        self.fn_def.span = item.span;
        self.build_from_stmt(item.body);
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

    fn build_from_stmt(&mut self, stmt: parser::StmtRef) {
        match self.ast.get_stmt(stmt) {
            parser::Stmt::Return(inner) => {
                let value = self.build_from_expr(*inner);
                self.append_to_block(Instr::Return(value), *self.ast.get_stmt_span(stmt));
            }
        }
    }

    fn build_from_expr(&mut self, expr: parser::ExprRef) -> Value {
        match self.ast.get_expr(expr) {
            parser::Expr::Constant(value) => Value::Constant(*value),
            parser::Expr::Variable(_) => todo!("handle named variables"),
            parser::Expr::Grouped(mut inner) => {
                while let parser::Expr::Grouped(expr) = self.ast.get_expr(inner) {
                    inner = *expr
                }
                self.build_from_expr(inner)
            }
            parser::Expr::Unary { op, expr: inner } => {
                let op = UnaryOp::from(*op);
                let src = self.build_from_expr(*inner);
                let dst = self.make_tmp();
                self.append_to_block(Instr::Unary { op, src, dst }, *self.ast.get_expr_span(expr));
                dst
            }
            parser::Expr::Binary { op, lhs, rhs } => match op {
                parser::BinaryOp::LogicalOr | parser::BinaryOp::LogicalAnd => {
                    let (skip_value, other_value) = match op {
                        parser::BinaryOp::LogicalOr => (Value::Constant(1), Value::Constant(0)),
                        parser::BinaryOp::LogicalAnd => (Value::Constant(0), Value::Constant(1)),
                        _ => unreachable!(),
                    };

                    let dst = self.make_tmp();
                    let skip_block = self.fn_def.push_block(Block::with_label(
                        self.interner.get_or_intern_static("skip"),
                    ));
                    let cont_block = self.fn_def.push_block(Block::with_label(
                        self.interner.get_or_intern_static("cont"),
                    ));
                    self.fn_def.get_block_mut(skip_block).instrs.extend([
                        Instr::Copy {
                            src: skip_value,
                            dst,
                        },
                        Instr::Jump(cont_block),
                    ]);
                    self.fn_def
                        .get_block_mut(skip_block)
                        .spans
                        .extend([*self.ast.get_expr_span(expr), *self.ast.get_expr_span(expr)]);

                    let lhs = self.build_from_expr(*lhs);
                    self.append_to_block(
                        match op {
                            parser::BinaryOp::LogicalOr => Instr::JumpIfNotZero {
                                cond: lhs,
                                target: skip_block,
                            },
                            parser::BinaryOp::LogicalAnd => Instr::JumpIfZero {
                                cond: lhs,
                                target: skip_block,
                            },
                            _ => unreachable!(),
                        },
                        *self.ast.get_expr_span(expr),
                    );

                    let rhs = self.build_from_expr(*rhs);
                    self.append_to_block(
                        match op {
                            parser::BinaryOp::LogicalOr => Instr::JumpIfNotZero {
                                cond: rhs,
                                target: skip_block,
                            },
                            parser::BinaryOp::LogicalAnd => Instr::JumpIfZero {
                                cond: rhs,
                                target: skip_block,
                            },
                            _ => unreachable!(),
                        },
                        *self.ast.get_expr_span(expr),
                    );

                    self.append_to_block(
                        Instr::Copy {
                            src: other_value,
                            dst,
                        },
                        *self.ast.get_expr_span(expr),
                    );
                    self.append_to_block(Instr::Jump(cont_block), *self.ast.get_expr_span(expr));

                    self.block = cont_block;
                    dst
                }
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
}
