use crate::{parse::{Ast, ItemRef}, sema::SemaCtx};

use ssa::Program;
use tacky::string_interner::DefaultStringInterner;

// ---------------------------------------------------------------------------
// SSABuilder
// ---------------------------------------------------------------------------

pub struct SSABuilder<'a> {
    p: Program<'a>,
    ctx: &'a SemaCtx,
}

impl<'a> SSABuilder<'a> {
    pub fn new(ctx: &'a SemaCtx, interner: &'a mut DefaultStringInterner) -> Self {
        Self { ctx, p: Program::new(interner) }
    }

    pub fn build(mut self, ast: &Ast) -> Program<'a> {
        ast.item_iter().for_each(|i| self.build_item(i));
        self.p
    }

    fn build_item(&mut self, item: ItemRef) {}
}
