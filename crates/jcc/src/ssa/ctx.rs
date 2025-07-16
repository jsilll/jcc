use crate::{ast, sema::SemaSymbol};

use jcc_ssa::{
    builder::IRBuilder,
    interner::{Interner, Symbol},
    sourcemap::SourceSpan,
    BlockRef, FuncRef, InstRef,
};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// BuilderCtx
// ---------------------------------------------------------------------------

pub struct BuilderCtx<'a> {
    pub builder: IRBuilder<'a>,
    vars: Vec<Option<InstRef>>,
    funcs: HashMap<Symbol, FuncRef>,
    labeled_blocks: HashMap<Symbol, BlockRef>,
    pub case_blocks: HashMap<ast::StmtRef, BlockRef>,
    pub break_blocks: HashMap<ast::StmtRef, BlockRef>,
    pub continue_blocks: HashMap<ast::StmtRef, BlockRef>,
}

impl<'a> BuilderCtx<'a> {
    pub fn new(ast: &ast::Ast, interner: &'a mut Interner) -> Self {
        Self {
            funcs: HashMap::new(),
            case_blocks: HashMap::new(),
            break_blocks: HashMap::new(),
            labeled_blocks: HashMap::new(),
            continue_blocks: HashMap::new(),
            builder: IRBuilder::new(interner),
            vars: vec![Default::default(); ast.symbols_len() + 1],
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.case_blocks.clear();
        self.break_blocks.clear();
        self.labeled_blocks.clear();
        self.continue_blocks.clear();
    }

    #[inline]
    pub fn get_var_ptr(&self, sym: SemaSymbol) -> InstRef {
        self.vars[sym.0.get() as usize].unwrap()
    }

    #[inline]
    pub fn insert_var(&mut self, sym: SemaSymbol, var: InstRef) {
        let _ = self.vars[sym.0.get() as usize].insert(var);
    }

    #[inline]
    pub fn get_or_make_block(&mut self, name: Symbol, span: SourceSpan) -> BlockRef {
        *self
            .labeled_blocks
            .entry(name)
            .or_insert_with(|| self.builder.new_block_interned(name, span))
    }

    #[inline]
    pub fn get_or_make_function(
        &mut self,
        name: Symbol,
        is_global: bool,
        span: SourceSpan,
    ) -> FuncRef {
        *self
            .funcs
            .entry(name)
            .or_insert(self.builder.new_func(name, is_global, span))
    }
}
