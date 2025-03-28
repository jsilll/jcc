use crate::parse;

use ssa::{string_interner::DefaultStringInterner, Program};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// SSABuilder
// ---------------------------------------------------------------------------

pub struct SSABuilder<'a> {
    prog: Program<'a>,
    ast: &'a parse::Ast,
    // ctx: &'a sema::SemaCtx,
    func: Option<ssa::FuncRef>,
    block: Option<ssa::BlockRef>,
    variables: HashMap<parse::DeclRef, ssa::InstRef>,
}

impl<'a> SSABuilder<'a> {
    pub fn new(
        ast: &'a parse::Ast,
        // ctx: &'a sema::SemaCtx,
        interner: &'a mut DefaultStringInterner,
    ) -> Self {
        Self {
            ast,
            // ctx,
            func: None,
            block: None,
            variables: HashMap::new(),
            prog: Program::new(interner),
        }
    }

    pub fn build(mut self) -> Program<'a> {
        self.ast.items_iter().for_each(|item| {
            self.visit_item(item);
        });
        self.prog
    }

    fn visit_item(&mut self, item: parse::ItemRef) {
        let span = *self.ast.item_span(item);

        let func = self
            .prog
            .new_func_with_span_interned(self.ast.item(item).name, span);
        let block = self.prog.new_block_with_span("entry", span);
        self.prog.func_mut(func).blocks.push(block);

        self.func = Some(func);
        self.block = Some(block);

        self.ast
            .block_items(self.ast.item(item).body)
            .iter()
            .for_each(|item| match item {
                parse::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                parse::BlockItem::Decl(decl) => self.visit_decl(*decl),
            });

        let append_return = match self.ast.block_items(self.ast.item(item).body).last() {
            Some(parse::BlockItem::Stmt(stmt)) => match self.ast.stmt(*stmt) {
                parse::Stmt::Return(_) => false,
                _ => true,
            },
            _ => true,
        };

        if append_return {
            let val = self.prog.new_inst_with_span(
                ssa::Inst::new(ssa::Type::Int32, ssa::InstKind::Const(0)),
                span,
            );
            let ret = self.prog.new_inst_with_span(
                ssa::Inst::new(ssa::Type::Void, ssa::InstKind::Ret(val)),
                span,
            );
            self.append_slice_to_block(&[val, ret]);
        }
    }

    fn visit_decl(&mut self, decl: parse::DeclRef) {
        match self.ast.decl(decl) {
            parse::Decl::Var { init, .. } => {
                let span = *self.ast.decl_span(decl);

                let ptr = self.prog.new_inst_with_span(
                    ssa::Inst::new(ssa::Type::Int32, ssa::InstKind::Alloca),
                    span,
                );

                self.append_to_block(ptr);
                self.variables.insert(decl, ptr);

                if let Some(init) = init {
                    let val = self.visit_expr(*init, ExprMode::RightValue);
                    let inst = self.prog.new_inst_with_span(
                        ssa::Inst::new(ssa::Type::Void, ssa::InstKind::Store { val, ptr }),
                        span,
                    );

                    self.append_to_block(inst);
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: parse::StmtRef) {
        match self.ast.stmt(stmt) {
            parse::Stmt::Empty => {}
            parse::Stmt::Expr(expr) => {
                self.visit_expr(*expr, ExprMode::RightValue);
            }
            parse::Stmt::Return(expr) => {
                let val = self.visit_expr(*expr, ExprMode::RightValue);
                let inst = self
                    .prog
                    .new_inst(ssa::Inst::new(ssa::Type::Void, ssa::InstKind::Ret(val)));
                self.append_to_block(inst);
            }
            parse::Stmt::Default(_) => todo!(),
            parse::Stmt::Goto(_) => todo!(),
            parse::Stmt::Break(_) => todo!(),
            parse::Stmt::Continue(_) => todo!(),
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
        match self.ast.expr(expr) {
            parse::Expr::Grouped(mut expr) => {
                while let parse::Expr::Grouped(inner) = self.ast.expr(expr) {
                    expr = *inner;
                }
                self.visit_expr(expr, mode)
            }
            parse::Expr::Const(c) => {
                let inst = self
                    .prog
                    .new_inst(ssa::Inst::new(ssa::Type::Int32, ssa::InstKind::Const(*c)));
                self.append_to_block(inst);
                inst
            }
            parse::Expr::Var { decl, .. } => {
                let ptr = self
                    .variables
                    .get(&decl.expect("expected a resolved variable"))
                    .copied()
                    .expect("expected a pointer to a variable");
                match mode {
                    ExprMode::LeftValue => ptr,
                    ExprMode::RightValue => {
                        let inst = self
                            .prog
                            .new_inst(ssa::Inst::new(ssa::Type::Int32, ssa::InstKind::Load(ptr)));
                        self.append_to_block(inst);
                        inst
                    }
                }
            }
            parse::Expr::Unary { .. } => todo!(),
            parse::Expr::Binary { op, lhs, rhs } => match op {
                parse::BinaryOp::LogicalOr => todo!(),
                parse::BinaryOp::LogicalAnd => todo!(),
                parse::BinaryOp::Equal => todo!(),
                parse::BinaryOp::NotEqual => todo!(),
                parse::BinaryOp::LessThan => todo!(),
                parse::BinaryOp::LessEqual => todo!(),
                parse::BinaryOp::GreaterThan => todo!(),
                parse::BinaryOp::GreaterEqual => todo!(),
                parse::BinaryOp::Add => todo!(),
                parse::BinaryOp::Sub => todo!(),
                parse::BinaryOp::Mul => todo!(),
                parse::BinaryOp::Div => todo!(),
                parse::BinaryOp::Rem => todo!(),
                parse::BinaryOp::BitOr => todo!(),
                parse::BinaryOp::BitAnd => todo!(),
                parse::BinaryOp::BitXor => todo!(),
                parse::BinaryOp::BitLsh => todo!(),
                parse::BinaryOp::BitRsh => todo!(),
                parse::BinaryOp::Assign => {
                    let lhs = self.visit_expr(*lhs, ExprMode::LeftValue);
                    let rhs = self.visit_expr(*rhs, ExprMode::RightValue);
                    let store = self.prog.new_inst(ssa::Inst::new(
                        ssa::Type::Void,
                        ssa::InstKind::Store { ptr: lhs, val: rhs },
                    ));
                    self.append_to_block(store);
                    lhs
                }
                parse::BinaryOp::AddAssign => todo!(),
                parse::BinaryOp::SubAssign => todo!(),
                parse::BinaryOp::MulAssign => todo!(),
                parse::BinaryOp::DivAssign => todo!(),
                parse::BinaryOp::RemAssign => todo!(),
                parse::BinaryOp::BitOrAssign => todo!(),
                parse::BinaryOp::BitAndAssign => todo!(),
                parse::BinaryOp::BitXorAssign => todo!(),
                parse::BinaryOp::BitLshAssign => todo!(),
                parse::BinaryOp::BitRshAssign => todo!(),
            },
            parse::Expr::Ternary { .. } => todo!(),
        }
    }

    fn append_to_block(&mut self, instr: ssa::InstRef) {
        self.prog
            .block_mut(self.block.expect("block not set"))
            .insts
            .push(instr);
    }

    fn append_slice_to_block(&mut self, instrs: &[ssa::InstRef]) {
        self.prog
            .block_mut(self.block.expect("block not set"))
            .insts
            .extend_from_slice(instrs);
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
