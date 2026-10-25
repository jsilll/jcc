use crate::ir::{
    analysis::dom::Dominance,
    inst::Inst,
    passes::mem2reg::{collect::Collect, insert::Insert},
    term::Terminator,
    ty::Ty,
    Block, Program, Value, ValueData,
};

use jcc_entity::{PackedOption, SecondaryMap, SparseMap, SparseSet};

enum Entry {
    /// Enter a block
    Block(Block),
    /// Undo the definitions
    Undo(usize),
}

pub struct Context<'a> {
    pub dom: &'a Dominance,
    pub prog: &'a mut Program,
    pub promo: &'a SparseMap<Value, Ty>,
    pub phi_blocks: &'a SparseSet<Block>,
    pub phi2alloca: &'a SecondaryMap<Value, PackedOption<Value>>,
}

impl<'a> Context<'a> {
    pub fn new(
        dom: &'a Dominance,
        insert: &'a Insert,
        collect: &'a Collect,
        prog: &'a mut Program,
    ) -> Self {
        Self {
            dom,
            prog,
            promo: collect.promo(),
            phi_blocks: insert.phi_blocks(),
            phi2alloca: insert.phi2alloca(),
        }
    }
}

#[derive(Default)]
pub struct Rename {
    stack: Vec<Entry>,
    successors: SparseSet<Block>,
    phi_alloca_pairs: Vec<(Value, Value)>,
    log: Vec<(Value, PackedOption<Value>)>,
    definitions: SecondaryMap<Value, PackedOption<Value>>,
    replacements: SecondaryMap<Value, PackedOption<Value>>,
}

impl Rename {
    /// Renames variables in the dominance tree starting from the entry block.
    pub fn run(&mut self, entry: Block, ctx: &mut Context) {
        self.log.clear();
        self.stack.clear();
        self.definitions.clear();
        self.replacements.clear();

        self.stack.push(Entry::Block(entry));
        while let Some(entry) = self.stack.pop() {
            match entry {
                Entry::Block(block) => {
                    let len = self.log.len();
                    self.rename(block, ctx);
                    self.stack.push(Entry::Undo(len));
                    for &child in ctx.dom.children(block).iter().rev() {
                        self.stack.push(Entry::Block(child));
                    }
                }
                Entry::Undo(len) => {
                    while self.log.len() > len {
                        if let Some((alloca, prev)) = self.log.pop() {
                            self.definitions[alloca] = prev;
                        }
                    }
                }
            }
        }
    }

    #[inline(always)]
    fn rename_operands<'a>(&self, operands: impl IntoIterator<Item = &'a mut Value>) {
        for op in operands {
            if let Some(rename) = self.replacements[*op].expand() {
                *op = rename;
            }
        }
    }

    fn rename(&mut self, block: Block, ctx: &mut Context) {
        let mut write_idx = 0;
        let count = ctx.prog.blocks[block].insts.len();
        for read_idx in 0..count {
            let mut keep = true;
            let v = ctx.prog.blocks[block].insts[read_idx];
            match ctx.prog.values[v].inst {
                Inst::Alloca { .. } => {
                    if ctx.promo.contains_key(v) {
                        keep = false;
                    }
                }
                Inst::Phi(_) => {
                    if let Some(alloca) = ctx.phi2alloca[v].expand() {
                        self.log.push((
                            alloca,
                            std::mem::replace(&mut self.definitions[alloca], v.into()),
                        ));
                    }
                }
                Inst::Load { ptr, ty, .. } => {
                    if !ctx.promo.contains_key(ptr) {
                        self.rename_operands(ctx.prog.values[v].inst.operands_mut());
                    } else {
                        if let Some(def) = self.definitions[ptr].expand() {
                            self.replacements[v] = def.into();
                            keep = false;
                        } else {
                            ctx.prog.values[v].inst = Inst::Undef(ty);
                        }
                    }
                }
                Inst::Store { ptr, value, .. } => {
                    if !ctx.promo.contains_key(ptr) {
                        self.rename_operands(ctx.prog.values[v].inst.operands_mut());
                    } else {
                        keep = false;
                        self.log.push((
                            ptr,
                            std::mem::replace(
                                &mut self.definitions[ptr],
                                self.replacements[value].expand().unwrap_or(value).into(),
                            ),
                        ));
                    }
                }
                _ => {
                    self.rename_operands(ctx.prog.values[v].inst.operands_mut());
                }
            }
            if keep {
                ctx.prog.blocks[block].insts[write_idx] = v;
                ctx.prog.values[v].idx = write_idx as u32;
                write_idx += 1;
            }
        }
        ctx.prog.blocks[block].insts.truncate(write_idx);
        self.rename_operands(ctx.prog.blocks[block].term.operands_mut());
        if !ctx.phi_blocks.is_empty() {
            self.insert_upsilons(block, ctx);
        }
    }

    fn insert_upsilons(&mut self, block: Block, ctx: &mut Context) {
        match &ctx.prog.blocks[block].term {
            Terminator::Ret(_) | Terminator::Unreachable => {}
            Terminator::Br(succ) => {
                if ctx.phi_blocks.contains(*succ) {
                    self.insert_successor_upsilons(block, *succ, ctx);
                }
            }
            Terminator::CondBr {
                then_block,
                else_block,
                ..
            } => {
                let then_b = *then_block;
                let else_b = *else_block;
                if ctx.phi_blocks.contains(then_b) {
                    self.insert_successor_upsilons(block, then_b, ctx);
                }
                if else_b != then_b && ctx.phi_blocks.contains(else_b) {
                    self.insert_successor_upsilons(block, else_b, ctx);
                }
            }
            Terminator::Switch { default, cases, .. } => {
                self.successors.clear();
                if ctx.phi_blocks.contains(*default) {
                    self.successors.insert(*default);
                }
                for &(_, block) in cases {
                    if ctx.phi_blocks.contains(block) {
                        self.successors.insert(block);
                    }
                }
                let len = self.successors.len();
                for idx in 0..len {
                    let succ = self.successors.as_slice()[idx];
                    self.insert_successor_upsilons(block, succ, ctx);
                }
            }
        }
    }

    fn insert_successor_upsilons(&mut self, block: Block, succ: Block, ctx: &mut Context) {
        self.phi_alloca_pairs.clear();
        for &value in &ctx.prog.blocks[succ].insts {
            if !matches!(ctx.prog.values[value].inst, Inst::Phi(_)) {
                break;
            }
            if let Some(alloca) = ctx.phi2alloca[value].expand() {
                self.phi_alloca_pairs.push((value, alloca));
            }
        }

        for &(phi, alloca) in &self.phi_alloca_pairs {
            let Some(ty) = ctx.promo.get(alloca) else {
                continue;
            };

            // Current value of the alloca, or undef
            let value = self.definitions[alloca].expand().unwrap_or_else(|| {
                let idx = ctx.prog.blocks[block].insts.len() as u32;
                let undef = ctx.prog.values.push(ValueData {
                    idx,
                    block,
                    inst: Inst::Undef(*ty),
                    span: ctx.prog.values[alloca].span,
                });
                ctx.prog.blocks[block].insts.push(undef);
                undef
            });

            // Insert the upsilon instruction in the current block
            let idx = ctx.prog.blocks[block].insts.len() as u32;
            let upsilon = ctx.prog.values.push(ValueData {
                idx,
                block,
                span: ctx.prog.values[phi].span,
                inst: Inst::Upsilon { phi, value },
            });
            ctx.prog.blocks[block].insts.push(upsilon);
        }
    }
}
