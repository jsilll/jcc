use crate::{BlockRef, InstKind, InstRef, Program};

#[derive(Debug, Default)]
pub struct InsertionSet {
    insertions: Vec<(u32, InstRef)>,
}

impl InsertionSet {
    pub fn new() -> Self {
        Self {
            insertions: Vec::new(),
        }
    }

    pub fn push(&mut self, idx: usize, inst: InstRef) {
        self.insertions.push((idx as u32, inst));
    }

    pub fn execute(&mut self, program: &mut Program, block: BlockRef) {
        if self.insertions.is_empty() {
            return;
        }

        self.insertions.sort_by_key(|(idx, _)| *idx);

        let b = program.get_block(block);
        let mut insts = Vec::with_capacity(b.insts.len() + self.insertions.len());

        let mut curr = 0;
        for (idx, inst) in b.insts.iter().enumerate() {
            while curr < self.insertions.len() && self.insertions[curr].0 <= idx as u32 {
                insts.push(self.insertions[curr].1);
                curr += 1;
            }
            if !matches!(program.get_inst(*inst).kind, InstKind::Nop) {
                insts.push(*inst);
            }
        }

        while curr < self.insertions.len() {
            insts.push(self.insertions[curr].1);
            curr += 1;
        }

        self.insertions.clear();

        program.get_block_mut(block).insts = insts;
    }
}
