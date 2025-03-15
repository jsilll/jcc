pub mod effects;

pub mod insertion;

pub use source_file;

use effects::{AbstractHeap, FastEffects};

use source_file::SourceSpan;

use std::{collections::HashMap, fmt, num::NonZeroU32};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    #[default]
    Void,
    Int32,
    Int64,
    IntPtr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum InstKind {
    #[default]
    Nop,
    Phi,
    Jump,
    Branch,
    GetArg,
    Const(i64),
    Ret(InstRef),
    Identity(InstRef),
    Upsilon {
        phi: InstRef,
        value: InstRef,
    },
    Generic {
        opcode: OpCode,
        args: Vec<InstRef>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstRef(NonZeroU32);

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Inst {
    // TODO: index in block
    // TODO: index of block
    // TODO: resulting variable name
    pub ty: Type,
    pub kind: InstKind,
}

impl Inst {
    pub fn ret(v: InstRef) -> Self {
        Self {
            ty: Type::Void,
            kind: InstKind::Ret(v),
        }
    }

    pub fn phi(ty: Type) -> Self {
        Self {
            ty,
            kind: InstKind::Phi,
        }
    }

    pub fn upsilon(phi: InstRef, value: InstRef) -> Self {
        Self {
            ty: Type::Void,
            kind: InstKind::Upsilon { phi, value },
        }
    }

    pub fn const_i64(value: i64) -> Self {
        Self {
            ty: Type::Int64,
            kind: InstKind::Const(value),
        }
    }

    pub fn const_i32(value: i64) -> Self {
        Self {
            ty: Type::Int32,
            kind: InstKind::Const(value),
        }
    }

    pub fn add_i32(lhs: InstRef, rhs: InstRef) -> Self {
        Self {
            ty: Type::Int32,
            kind: InstKind::Generic {
                opcode: OpCode::Add,
                args: vec![lhs, rhs],
            },
        }
    }

    pub fn is_const(&self, value: i64) -> bool {
        match self.kind {
            InstKind::Const(v) => v == value,
            _ => false,
        }
    }

    pub fn get_effects(&self, program: &Program) -> FastEffects {
        let mut effects = FastEffects::new();
        match self.kind {
            // Phi reads from the SSA state
            InstKind::Phi => effects.reads.add(program.heaps.ssa_state),

            // Upsilon writes to the SSA state
            InstKind::Upsilon { .. } => effects.writes.add(program.heaps.ssa_state),

            // Control flow instruction write to the control heap
            InstKind::Jump | InstKind::Ret(_) | InstKind::Branch => {
                effects.writes.add(program.heaps.control)
            }

            _ => {}
        }
        effects
    }

    pub fn into_identity(&mut self, value: InstRef) {
        self.kind = InstKind::Identity(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockRef(NonZeroU32);

#[derive(Debug, Default, Clone)]
pub struct Block {
    // TODO: name of block
    // TODO: index in function
    pub insts: Vec<InstRef>,
    pub succ: Vec<BlockRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncRef(NonZeroU32);

#[derive(Debug, Default, Clone)]
pub struct Func {
    // TODO: name of function
    pub blocks: Vec<BlockRef>,
}

#[derive(Debug, Clone)]
pub struct BaseHeaps {
    pub world: AbstractHeap,
    pub memory: AbstractHeap,
    pub control: AbstractHeap,
    pub ssa_state: AbstractHeap,
}

impl BaseHeaps {
    pub fn new() -> Self {
        Self {
            world: AbstractHeap::new(1, 8),
            memory: AbstractHeap::new(2, 3),
            control: AbstractHeap::new(4, 5),
            ssa_state: AbstractHeap::new(6, 7),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    heaps: BaseHeaps,
    funcs: Vec<Func>,
    insts: Vec<Inst>,
    blocks: Vec<Block>,
    funcs_name: Vec<String>,
    blocks_name: Vec<String>,
    insts_span: Vec<SourceSpan>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            heaps: BaseHeaps::new(),
            funcs: vec![Default::default()],
            insts: vec![Default::default()],
            blocks: vec![Default::default()],
            insts_span: vec![Default::default()],
            funcs_name: vec![Default::default()],
            blocks_name: vec![Default::default()],
        }
    }

    pub fn get_blocks_iter(&self) -> impl Iterator<Item = BlockRef> {
        (1..self.blocks.len())
            .map(|i| BlockRef(NonZeroU32::new(i as u32).expect("expected a positive index")))
    }

    pub fn get_funcs_iter(&self) -> impl Iterator<Item = FuncRef> {
        (1..self.funcs.len())
            .map(|i| FuncRef(NonZeroU32::new(i as u32).expect("expected a positive index")))
    }

    pub fn get_inst(&self, inst: InstRef) -> &Inst {
        &self.insts[inst.0.get() as usize]
    }

    pub fn get_block(&self, block: BlockRef) -> &Block {
        &self.blocks[block.0.get() as usize]
    }

    pub fn get_func(&self, func: FuncRef) -> &Func {
        &self.funcs[func.0.get() as usize]
    }

    pub fn get_inst_mut(&mut self, inst: InstRef) -> &mut Inst {
        &mut self.insts[inst.0.get() as usize]
    }

    pub fn get_block_mut(&mut self, block: BlockRef) -> &mut Block {
        &mut self.blocks[block.0.get() as usize]
    }

    pub fn get_func_mut(&mut self, func: FuncRef) -> &mut Func {
        &mut self.funcs[func.0.get() as usize]
    }

    pub fn get_block_name(&self, block: BlockRef) -> &str {
        &self.blocks_name[block.0.get() as usize]
    }

    pub fn get_func_name(&self, func: FuncRef) -> &str {
        &self.funcs_name[func.0.get() as usize]
    }

    pub fn add_inst(&mut self, inst: Inst) -> InstRef {
        let idx =
            InstRef(NonZeroU32::new(self.insts.len() as u32).expect("expected a positive index"));
        self.insts.push(inst);
        self.insts_span.push(Default::default());
        idx
    }

    pub fn add_block(&mut self, name: &str) -> BlockRef {
        let idx =
            BlockRef(NonZeroU32::new(self.blocks.len() as u32).expect("expected a positive index"));
        self.blocks.push(Default::default());
        self.blocks_name.push(name.to_owned());
        idx
    }

    pub fn add_func(&mut self, name: &str) -> FuncRef {
        let idx =
            FuncRef(NonZeroU32::new(self.funcs.len() as u32).expect("expected a positive index"));
        self.funcs.push(Default::default());
        self.funcs_name.push(name.to_owned());
        idx
    }

    pub fn get_phi_args(&self, phi: InstRef) -> Vec<InstRef> {
        // TODO: build a function that builds a map for all phis??
        let mut res = Vec::new();
        for (idx, inst) in self.insts.iter().enumerate().skip(1) {
            if let InstKind::Upsilon { phi: p, .. } = inst.kind {
                if p == phi {
                    res.push(InstRef(
                        NonZeroU32::new(idx as u32).expect("expeted a positive index"),
                    ))
                }
            }
        }
        res
    }

    pub fn get_predecessors(&self) -> HashMap<BlockRef, Vec<BlockRef>> {
        let mut res = HashMap::new();

        for (idx, block) in self.blocks.iter().enumerate() {
            let idx = BlockRef(NonZeroU32::new(idx as u32).expect("expected a positive index"));
            for succ in &block.succ {
                res.entry(*succ).or_insert(Vec::new()).push(idx);
            }
        }

        res
    }

    pub fn validate(&self) -> bool {
        // TODO: Check SSA dominance rule: each use must be dominated by its definition
        // This would require implementing dominance calculation first
        true
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, func) in self.funcs_name.iter().zip(self.funcs.iter()).skip(1) {
            writeln!(f, "define {}", name)?;
            for block in &func.blocks {
                writeln!(f, "{}:", self.get_block_name(*block))?;
                for (idx, inst) in self.get_block(*block).insts.iter().enumerate() {
                    let inst = self.get_inst(*inst);
                    writeln!(f, "  {:?} %{} = {:?}", inst.ty, idx, inst.kind)?;
                }
            }
        }
        Ok(())
        // for &block_idx in &program.functions[func_idx].blocks {
        //     let block = &program.blocks[block_idx.0];
        //     println!("{}:", block.name);

        //     for &inst_idx in &block.insts {
        //         let inst = &program.insts[inst_idx.0];
        //         let base = inst.get_base();

        //         print!("  {:?} {} = {:?}", base.inst_type, base.name, base.opcode);

        //         if !base.args.is_empty() {
        //             print!("(");
        //             for (i, &arg) in base.args.iter().enumerate() {
        //                 if i > 0 {
        //                     print!(", ");
        //                 }
        //                 print!("{}", program.insts[arg.0].get_base().name);
        //             }
        //             print!(")");
        //         }

        //         // Special handling for Upsilon to show phi target
        //         if let InstData::Upsilon(upsilon) = inst {
        //             print!(" → ^{}", program.insts[upsilon.phi.0].get_base().name);
        //         }

        //         // Special handling for Const to show value
        //         if let InstData::Const(const_inst) = inst {
        //             print!(" [{}]", const_inst.value);
        //         }

        //         println!();
        //     }

        //     if !block.successors.is_empty() {
        //         print!("  successors: ");
        //         for (i, &succ) in block.successors.iter().enumerate() {
        //             if i > 0 {
        //                 print!(", ");
        //             }
        //             print!("{}", program.blocks[succ.0].name);
        //         }
        //         println!();
        //     }
        //     println!();
        // }
    }
}
