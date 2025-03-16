use crate::{BlockRef, InstRef, Program};

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

    pub fn execute(&mut self, p: &mut Program, b: BlockRef) {
        if self.insertions.is_empty() {
            return;
        }

        if !self.insertions.is_sorted_by_key(|(idx, _)| *idx) {
            self.insertions.sort_by_key(|(idx, _)| *idx);
        }

        let block = p.get_block(b);
        let mut insts = Vec::with_capacity(block.insts.len() + self.insertions.len());

        let mut curr = 0;
        for (idx, inst) in block.insts.iter().enumerate() {
            self.insertions[curr..]
                .iter()
                .take_while(|(i, _)| *i <= idx as u32)
                .for_each(|(_, inst)| {
                    insts.push(*inst);
                    curr += 1;
                });
            insts.push(*inst);
        }

        self.insertions[curr..].iter().for_each(|(_, inst)| {
            insts.push(*inst);
        });

        self.insertions.clear();

        p.get_block_mut(b).insts = insts;
    }
}
