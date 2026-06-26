use jcc_codemap::span::Span;

use crate::{
    ir::{
        analysis::{cfg::ControlFlowGraph, dom::Dominance, order::Order},
        inst::Inst,
        term::Terminator,
        ty::Ty,
        Block, BlockData, Function, FunctionData, Global, GlobalData, Program, Value, ValueData,
    },
    Ident, IdentInterner,
};

pub struct ProgramBuilder<'a> {
    /// The program being built.
    prog: Program,
    /// The interner for identifier interning.
    itrn: &'a mut IdentInterner,
    /// A stack of block targets.
    stack: Vec<Targets>,
    /// The state for the function being built.
    state: Option<FunctionState>,
}

impl<'a> ProgramBuilder<'a> {
    pub fn new(itrn: &'a mut IdentInterner) -> Self {
        Self {
            itrn,
            state: None,
            stack: Vec::new(),
            prog: Program::default(),
        }
    }

    pub fn finish(self) -> Program {
        let mut ord = Order::default();
        let mut dom = Dominance::default();
        let mut cfg = ControlFlowGraph::default();
        ord.compute(&self.prog);
        cfg.compute(&self.prog, &ord);
        dom.compute(&self.prog, &ord, &cfg);
        self.prog
    }

    // -------------------------------------------------------------------
    // Function Scope
    // -------------------------------------------------------------------

    #[inline]
    pub fn exit_function(&mut self) {
        self.state = None
    }

    pub fn enter_function(&mut self, function: Function) {
        assert!(
            self.state.is_none(),
            "enter_function needs to be called on a cleared state"
        );
        let entry = self.build_block("entry", self.prog.functions[function].span);
        self.prog.functions[function].entry = Some(entry);
        self.state = Some(FunctionState {
            entry,
            function,
            block: entry,
        })
    }

    // -------------------------------------------------------------------
    // Targets Scope
    // -------------------------------------------------------------------

    pub fn pop_targets(&mut self) {
        self.stack.pop();
    }

    pub fn top_targets(&mut self) -> Option<Targets> {
        self.stack.last().copied()
    }

    pub fn push_switch(&mut self, break_target: Block) {
        let continue_target = self
            .stack
            .last()
            .map_or(break_target, |t| t.continue_target);
        self.push_loop(break_target, continue_target);
    }

    pub fn push_loop(&mut self, break_target: Block, continue_target: Block) {
        self.stack.push(Targets {
            break_target,
            continue_target,
        });
    }

    // -------------------------------------------------------------------
    // Blocks
    // -------------------------------------------------------------------

    pub fn seal_and_move_to(&mut self, term: Terminator, next: Block) {
        let st = self.state.as_mut().expect("state should be valid");

        self.prog.blocks[st.block].term = term;
        st.block = next;
    }

    pub fn seal_with_dead(&mut self, term: Terminator, name: &str, span: Span) {
        let next = self.build_block(name, span);
        self.seal_and_move_to(term, next);
    }

    // -------------------------------------------------------------------
    // Builders
    // -------------------------------------------------------------------

    #[inline]
    pub fn build_block(&mut self, name: &str, span: Span) -> Block {
        let name = self.itrn.intern(name);
        self.build_block_ident(name, span)
    }

    #[inline]
    pub fn build_function(&mut self, data: FunctionData) -> Function {
        self.prog.functions.push(data)
    }

    #[inline]
    pub fn build_block_ident(&mut self, name: Ident, span: Span) -> Block {
        self.prog.blocks.push(BlockData::new(name, span))
    }

    pub fn push_val(&mut self, value: Value) {
        let st = self.state.as_ref().expect("state should be valid");

        let data = &mut self.prog.blocks[st.block];
        let idx = data.insts.len() as u32;
        data.insts.push(value);

        let value = &mut self.prog.values[value];
        value.block = st.block;
        value.idx = idx;
    }

    pub fn build_phi(&mut self, ty: Ty, span: Span) -> Value {
        let st = self.state.as_ref().unwrap();

        self.prog.values.push(ValueData {
            span,
            idx: 0,
            block: st.block,
            inst: Inst::phi(ty),
        })
    }

    pub fn build_val(&mut self, inst: Inst, span: Span) -> Value {
        let st = self.state.as_ref().expect("state should be valid");

        let idx = self.prog.blocks[st.block].insts.len() as u32;
        let value = self.prog.values.push(ValueData {
            idx,
            inst,
            span,
            block: st.block,
        });

        self.prog.blocks[st.block].insts.push(value);
        value
    }

    pub fn build_alloca(&mut self, ty: Ty, align: u32, span: Span) -> Value {
        let st = self.state.as_ref().expect("state should be valid");

        let idx = self.prog.blocks[st.entry].insts.len() as u32;
        let value = self.prog.values.push(ValueData {
            idx,
            span,
            block: st.entry,
            inst: Inst::alloca(ty, align),
        });

        self.prog.blocks[st.entry].insts.push(value);
        value
    }

    pub fn build_global(&mut self, idx: u32, mut data: GlobalData) -> Global {
        if !data.is_global {
            if let Some(st) = &self.state {
                let name = self.itrn.lookup(data.name);
                let fname = self.itrn.lookup(self.prog.functions[st.function].name);
                data.name = self.itrn.intern(&format!("{fname}.{name}.{idx}"));
            }
        }

        self.prog.globals.push(data)
    }
}

// -------------------------------------------------------------------
// Auxiliary Structures
// -------------------------------------------------------------------

struct FunctionState {
    block: Block,
    entry: Block,
    function: Function,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Targets {
    pub break_target: Block,
    pub continue_target: Block,
}
