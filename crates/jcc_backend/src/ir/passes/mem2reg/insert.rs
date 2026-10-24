use crate::ir::{
    analysis::dom::Dominance, inst::Inst, passes::mem2reg::collect::Collect, Block, Program, Value,
    ValueData,
};

use jcc_entity::{EntitySet, PackedOption, SecondaryMap, SparseSet};

#[derive(Default)]
pub struct Insert {
    stack: Vec<Block>,
    seen: EntitySet<Block>,
    has_phi: EntitySet<Block>,
    phi_blocks: SparseSet<Block>,
    phi_to_alloca: SecondaryMap<Value, PackedOption<Value>>,
}

impl Insert {
    /// Returns a set of blocks that contain at least one newly inserted phi.
    pub fn phi_blocks(&self) -> &SparseSet<Block> {
        &self.phi_blocks
    }

    /// Returns a mapping of newly inserted phis to the allocas they correspond to.
    pub fn phi_to_alloca(&self) -> &SecondaryMap<Value, PackedOption<Value>> {
        &self.phi_to_alloca
    }

    /// Runs the phi insertion pass on the given program.
    pub fn run(&mut self, program: &mut Program, dominance: &Dominance, collect: &Collect) {
        let promo = collect.promo();
        let loaded = collect.loaded();
        let stores = collect.stores();

        self.phi_blocks.clear();
        self.phi_to_alloca.clear();
        for (alloca, ty) in promo.entries() {
            if !loaded.contains(*alloca) {
                continue;
            }

            // Fast path: 
            // If the alloca was never stored to, or has only a single store block
            // whose dominance frontier is empty (e.g. entry block), no phi can ever be needed.
            let mut iter = stores.iter(*alloca);
            let Some(&first) = iter.next() else {
                continue;
            };
            if iter.next().is_none() && dominance.frontier(first).is_empty() {
                continue;
            }

            self.seen.clear();
            self.stack.clear();
            self.has_phi.clear();

            // 1. Seed worklist with blocks containing stores
            for &block in stores.iter(*alloca) {
                self.stack.push(block);
                self.seen.insert(block);
            }

            // 2. Standard Iterated Dominance Frontier Algorithm
            while let Some(parent) = self.stack.pop() {
                for &child in dominance.frontier(parent) {
                    if self.has_phi.insert(child) {
                        let phi = program.values.push(ValueData {
                            idx: 0,
                            block: child,
                            inst: Inst::Phi(*ty),
                            span: program.values[*alloca].span,
                        });
                        self.phi_to_alloca[phi] = (*alloca).into();
                        program.blocks[child].insts.push(phi);
                        self.phi_blocks.insert(child);
                        if self.seen.insert(child) {
                            self.stack.push(child);
                        }
                    }
                }
            }
        }

        for block in self.phi_blocks.iter() {
            let insts = &mut program.blocks[block].insts;
            let phi_count = insts
                .iter()
                .rev()
                .take_while(|&&v| matches!(program.values[v].inst, Inst::Phi(_)))
                .count();
            if phi_count > 0 {
                insts.rotate_right(phi_count);
            }
        }
    }
}
