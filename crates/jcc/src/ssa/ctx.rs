use crate::{
    ast::{self, AstSymbol},
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
    vars: Vec<VarEntry>,
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
        match self.vars[sym.0.get() as usize] {
            VarEntry::Inst(inst) => inst,
            VarEntry::None | VarEntry::Static(_) => panic!("expected an inst ref"),
        }
    }

    #[inline]
    pub fn insert_var(&mut self, sym: SemaSymbol, var: InstRef) {
        self.vars[sym.0.get() as usize] = VarEntry::Inst(var);
    }

    #[inline]
    pub fn get_static_var_ref(&self, sym: SemaSymbol) -> StaticVarRef {
        match self.vars[sym.0.get() as usize] {
            VarEntry::Static(v) => v,
            VarEntry::None | VarEntry::Inst(_) => panic!("expected a static var ref"),
        }
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
        *self.funcs.entry(name).or_insert_with(|| {
            self.builder.prog.new_func(Func {
                name,
                span,
                is_global,
                ..Default::default()
            })
        })
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
        match self.vars[idx] {
            VarEntry::Inst(_) => panic!("unexpected inst ref"),
            VarEntry::Static(v) => v,
            VarEntry::None => {
                let name = match is_global {
                    true => name.raw,
                    false => {
                        let func = self.builder.func();
                        let fname = self.builder.prog.func(func).name;
                        let fname = self.builder.prog.interner.lookup(fname);
                        let name = self.builder.prog.interner.lookup(name.raw);
                        let scoped = format!("{fname}.{name}");
                        self.builder.prog.interner.intern(&scoped)
                    }
                };
                let v = self.builder.prog.new_static_var(StaticVar {
                    ty,
                    name,
                    init,
                    span,
                    is_global,
                });
                self.vars[idx] = VarEntry::Static(v);
                v
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary Structures
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum VarEntry {
    #[default]
    None,
    Inst(InstRef),
    Static(StaticVarRef),
}
