use crate::{
    ast::{Ast, AstSymbol, StmtRef},
    sema::SemaSymbol,
};

use jcc_ssa::{
    builder::IRBuilder,
    interner::{Interner, Symbol},
    sourcemap::SourceSpan,
    BlockRef, Func, FuncRef, InstRef, StaticVar, StaticVarRef, Type,
};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// BuilderCtx
// ---------------------------------------------------------------------------

pub struct BuilderCtx<'a> {
    pub builder: IRBuilder<'a>,
    symbols: Vec<SymbolEntry>,
    pub tracked_blocks: HashMap<StmtRef, TrackedBlock>,
}

impl<'a> BuilderCtx<'a> {
    pub fn new(ast: &Ast, interner: &'a mut Interner) -> Self {
        Self {
            tracked_blocks: HashMap::new(),
            builder: IRBuilder::new(interner),
            symbols: vec![Default::default(); ast.symbols_len() + 1],
        }
    }

    #[inline]
    pub fn get_var_ptr(&self, sym: SemaSymbol) -> InstRef {
        match self.symbols[sym.0.get() as usize] {
            SymbolEntry::Inst(inst) => inst,
            _ => panic!("expected an inst ref"),
        }
    }

    #[inline]
    pub fn get_static_var_ref(&self, sym: SemaSymbol) -> StaticVarRef {
        match self.symbols[sym.0.get() as usize] {
            SymbolEntry::Static(v) => v,
            _ => panic!("expected a static var ref"),
        }
    }

    #[inline]
    pub fn get_break_block(&self, stmt: StmtRef) -> BlockRef {
        match self.tracked_blocks.get(&stmt) {
            Some(TrackedBlock::BreakAndContinue(b, _)) => *b,
            _ => panic!("expected a break block"),
        }
    }

    #[inline]
    pub fn get_continue_block(&self, stmt: StmtRef) -> BlockRef {
        match self.tracked_blocks.get(&stmt) {
            Some(TrackedBlock::BreakAndContinue(_, c)) => *c,
            _ => panic!("expected a break block"),
        }
    }

    #[inline]
    pub fn clear_block_cache(&mut self) {
        self.tracked_blocks.clear();
    }

    #[inline]
    pub fn insert_var(&mut self, sym: SemaSymbol, var: InstRef) {
        self.symbols[sym.0.get() as usize] = SymbolEntry::Inst(var);
    }

    #[inline]
    pub fn remove_case_block(&mut self, stmt: StmtRef) -> BlockRef {
        match self.tracked_blocks.remove(&stmt) {
            Some(TrackedBlock::Case(c)) => c,
            _ => panic!("expected a break block"),
        }
    }

    #[inline]
    pub fn get_or_make_labeled_block(
        &mut self,
        stmt: StmtRef,
        name: Symbol,
        span: SourceSpan,
    ) -> BlockRef {
        let entry = self
            .tracked_blocks
            .entry(stmt)
            .or_insert_with(|| TrackedBlock::Label(self.builder.new_block_interned(name, span)));
        match entry {
            TrackedBlock::Label(l) => *l,
            _ => panic!("expected a labeled block"),
        }
    }

    #[inline]
    pub fn get_or_make_function(
        &mut self,
        name: &AstSymbol,
        is_global: bool,
        span: SourceSpan,
    ) -> FuncRef {
        let idx = name.sema.get().0.get() as usize;
        match self.symbols[idx] {
            SymbolEntry::Inst(_) | SymbolEntry::Static(_) => panic!("expected a static var ref"),
            SymbolEntry::Function(f) => f,
            SymbolEntry::None => {
                let f = self.builder.prog.new_func(Func {
                    span,
                    is_global,
                    name: name.raw,
                    ..Default::default()
                });
                self.symbols[idx] = SymbolEntry::Function(f);
                f
            }
        }
    }

    #[inline]
    pub fn get_or_make_static_var(
        &mut self,
        ty: Type,
        name: &AstSymbol,
        is_global: bool,
        init: Option<i64>,
        span: SourceSpan,
    ) -> StaticVarRef {
        let idx = name.sema.get().0.get() as usize;
        match self.symbols[idx] {
            SymbolEntry::Inst(_) | SymbolEntry::Function(_) => panic!("expected a static var ref"),
            SymbolEntry::Static(v) => v,
            SymbolEntry::None => {
                let func = self.builder.func();
                let name = match func {
                    Some(func) if !is_global => {
                        let fname = self.builder.prog.func(func).name;
                        let fname = self.builder.prog.interner.lookup(fname);
                        let name = self.builder.prog.interner.lookup(name.raw);
                        let scoped = format!("{fname}.{name}");
                        self.builder.prog.interner.intern(&scoped)
                    }
                    _ => name.raw,
                };
                let v = self.builder.prog.new_static_var(StaticVar {
                    ty,
                    name,
                    init,
                    span,
                    is_global,
                });
                self.symbols[idx] = SymbolEntry::Static(v);
                v
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary Structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrackedBlock {
    Case(BlockRef),
    Label(BlockRef),
    BreakAndContinue(BlockRef, BlockRef),
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum SymbolEntry {
    #[default]
    None,
    Inst(InstRef),
    Function(FuncRef),
    Static(StaticVarRef),
}
