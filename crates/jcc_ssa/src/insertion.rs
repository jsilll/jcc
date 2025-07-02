use crate::{BlockRef, Inst, InstIdx, InstKind, Program, Type};

use jcc_sourcemap::SourceSpan;

#[derive(Debug)]
struct Insertion {
    pub ty: Type,
    pub idx: InstIdx,
    pub kind: InstKind,
    pub span: SourceSpan,
}

#[derive(Debug, Default)]
pub struct InsertionSet {
    insertions: Vec<Insertion>,
}

impl InsertionSet {
    pub fn new() -> Self {
        Self {
            insertions: Vec::new(),
        }
    }

    #[inline]
    pub fn insert_before(&mut self, inst: &Inst, ty: Type, kind: InstKind) {
        self.insertions.push(Insertion {
            ty,
            kind,
            idx: inst.idx,
            span: inst.span,
        })
    }

    #[inline]
    pub fn insert_before_with_span(
        &mut self,
        inst: &Inst,
        ty: Type,
        kind: InstKind,
        span: SourceSpan,
    ) {
        self.insertions.push(Insertion {
            ty,
            kind,
            span,
            idx: inst.idx,
        })
    }

    pub fn execute(&mut self, p: &mut Program, b: BlockRef) {
        if self.insertions.is_empty() {
            return;
        }

        if !self.insertions.is_sorted_by_key(|i| i.idx) {
            self.insertions.sort_by_key(|i| i.idx);
        }

        let mut insts = Vec::with_capacity(p.block(b).insts.len() + self.insertions.len());

        let mut curr = 0;
        for idx in 0..p.block(b).insts.len() {
            self.insertions[curr..]
                .iter()
                .take_while(|i| i.idx.0 <= idx as u32)
                .for_each(|i| {
                    insts.push(p.new_inst(Inst::new(i.ty, i.kind.clone(), i.span)));
                    curr += 1;
                });
            insts.push(p.block(b).insts[idx]);
        }

        self.insertions[curr..].iter().for_each(|i| {
            insts.push(p.new_inst(Inst::new(i.ty, i.kind.clone(), i.span)));
        });

        p.block_mut(b).insts = insts;

        self.insertions.clear();
    }
}
