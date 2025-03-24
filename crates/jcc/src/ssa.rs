use crate::{parse, sema};

use ssa::{string_interner::DefaultStringInterner, Program};

// ---------------------------------------------------------------------------
// SSABuilder
// ---------------------------------------------------------------------------

pub struct SSABuilder<'a> {
    prog: Program<'a>,
    ast: &'a parse::Ast,
    ctx: &'a sema::SemaCtx,
    func: Option<ssa::FuncRef>,
    block: Option<ssa::BlockRef>,
}

impl<'a> SSABuilder<'a> {
    pub fn new(
        ast: &'a parse::Ast,
        ctx: &'a sema::SemaCtx,
        interner: &'a mut DefaultStringInterner,
    ) -> Self {
        Self {
            ast,
            ctx,
            func: None,
            block: None,
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
        let span = self.ast.item_span(item);
        let func = self
            .prog
            .new_func_with_span_interned(self.ast.item(item).name, *span);
        let block = self.prog.new_block_with_span("entry", *span);
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
        // TODO: Add return instruction if not present
        // match self.ast.block_items(item.body).last() {
        //     Some(parse::BlockItem::Stmt(stmt)) => match self.ast.stmt(*stmt) {
        //         parse::Stmt::Return(_) => {}
        //         _ => self.append_to_block(Instr::Return(Value::Constant(0)), self.fn_def.span),
        //     },
        //     _ => self.append_to_block(Instr::Return(Value::Constant(0)), self.fn_def.span),
        // }
    }

    fn visit_decl(&mut self, decl: parse::DeclRef) {
        match self.ast.decl(decl) {
            parse::Decl::Var { init, .. } => {
                let ptr = self
                    .prog
                    .new_inst(ssa::Inst::new(ssa::Type::Int32, ssa::InstKind::Alloca));
                self.prog.block_mut(self.block.unwrap()).insts.push(ptr);
                if let Some(init) = init {
                    let val = self.visit_expr(*init);
                    let inst = self.prog.new_inst(ssa::Inst::new(
                        ssa::Type::Int32,
                        ssa::InstKind::Store { val, ptr },
                    ));
                    self.prog.block_mut(self.block.unwrap()).insts.push(inst);
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: parse::StmtRef) {
        match self.ast.stmt(stmt) {
            parse::Stmt::Empty => {}
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: parse::ExprRef) -> ssa::InstRef {
        match self.ast.expr(expr) {
            parse::Expr::Const(c) => {
                let inst = self
                    .prog
                    .new_inst(ssa::Inst::new(ssa::Type::Int32, ssa::InstKind::Const(*c)));
                self.prog.block_mut(self.block.unwrap()).insts.push(inst);
                inst
            }
            _ => todo!(),
        }
    }
}
