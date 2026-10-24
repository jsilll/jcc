use crate::ir::{analysis::order::Order, inst::Inst, ty::Ty, Block, Program, Value};

use jcc_entity::{list::LinkedList, EntitySet, SparseMap};

#[derive(Default)]
pub struct Collect {
    loaded: EntitySet<Value>,
    promo: SparseMap<Value, Ty>,
    stores: LinkedList<Value, Block>,
}

impl Collect {
    /// Returns the subset of promotable allocas that are actually loaded.
    pub fn loaded(&self) -> &EntitySet<Value> {
        &self.loaded
    }

    /// Returns allocas that are safe to promote, mapped to their respective types.
    pub fn promo(&self) -> &SparseMap<Value, Ty> {
        &self.promo
    }

    /// Returns a mapping of allocas to linked lists of blocks where they are modified.
    pub fn stores(&self) -> &LinkedList<Value, Block> {
        &self.stores
    }

    /// Runs the collection pass on the given program.
    ///
    /// Returns `true` if any promotable allocas were found, `false` otherwise.
    pub fn run(&mut self, entry: Block, program: &Program, order: &Order) -> bool {
        self.promo.clear();
        self.loaded.clear();
        self.stores.clear();

        // Phase 1: Collect promotable allocas and their types.
        for &value in &program.blocks[entry].insts {
            if let Inst::Alloca { ty, .. } = program.values[value].inst {
                self.promo.insert(value, ty);
            }
        }
        if self.promo.is_empty() {
            return false;
        }

        // Phase 2: Track stores and loads of allocas to determine which can be promoted.
        for &block in order.rpo(entry) {
            if self.promo.is_empty() {
                break;
            }
            let data = &program.blocks[block];

            // 1. Escapes via terminator
            for op in data.term.operands() {
                self.promo.remove(op);
            }

            // 2. Track usages and escapes in instructions
            for &value in &data.insts {
                match &program.values[value].inst {
                    Inst::Alloca { .. } => {}
                    Inst::Load { ptr, .. } => {
                        if self.promo.contains_key(*ptr) {
                            self.loaded.insert(*ptr);
                        }
                    }
                    Inst::Store { ptr, value, .. } => {
                        self.promo.remove(*value); // escaped
                        if self.promo.contains_key(*ptr)
                            && !self.stores.head(*ptr).is_some_and(|head| *head == block)
                        {
                            self.stores.push_front(*ptr, block);
                        }
                    }
                    inst => {
                        for op in inst.operands() {
                            self.promo.remove(op); // escaped
                        }
                    }
                }
            }
        }

        !self.promo.is_empty()
    }
}
