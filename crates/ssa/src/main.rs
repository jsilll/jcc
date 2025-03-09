use std::collections::{HashMap, HashSet};

// Type definitions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int32,
    Int64,
    IntPtr,
    // Add more types as needed
}

// Opcodes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    Add,
    Sub,
    Mul,
    Div,
    Const,
    GetArgument,
    Branch,
    Jump,
    Return,
    Phi,
    Upsilon,
    Nop,
    Identity,
    // Add more opcodes as needed
}

// Source location information
#[derive(Debug, Clone)]
pub struct Origin {
    file: String,
    line: usize,
}

// Abstract heap for effects
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AbstractHeap {
    pre_order: usize,
    post_order: usize,
}

impl AbstractHeap {
    pub fn new(pre_order: usize, post_order: usize) -> Self {
        Self { pre_order, post_order }
    }

    pub fn interferes_with(&self, other: &AbstractHeap) -> bool {
        // Two abstract heaps interfere if their ranges overlap
        (self.pre_order <= other.pre_order && other.pre_order <= self.post_order) ||
        (other.pre_order <= self.pre_order && self.pre_order <= other.post_order)
    }
}

// Effects system
#[derive(Debug, Clone, Default)]
pub struct Effects {
    reads: Vec<AbstractHeap>,
    writes: Vec<AbstractHeap>,
}

impl Effects {
    pub fn new() -> Self {
        Self {
            reads: Vec::new(),
            writes: Vec::new(),
        }
    }

    pub fn add_read(&mut self, heap: AbstractHeap) {
        self.reads.push(heap);
    }

    pub fn add_write(&mut self, heap: AbstractHeap) {
        self.writes.push(heap);
    }

    pub fn add(&mut self, other: &Effects) {
        self.reads.extend_from_slice(&other.reads);
        self.writes.extend_from_slice(&other.writes);
    }

    pub fn interferes_with(&self, other: &Effects) -> bool {
        // Read-write interference
        for read in &self.reads {
            for write in &other.writes {
                if read.interferes_with(write) {
                    return true;
                }
            }
        }

        // Write-read interference
        for write in &self.writes {
            for read in &other.reads {
                if write.interferes_with(read) {
                    return true;
                }
            }
        }

        // Write-write interference
        for write1 in &self.writes {
            for write2 in &other.writes {
                if write1.interferes_with(write2) {
                    return true;
                }
            }
        }

        false
    }
}

// Common abstract heaps
#[derive(Debug, Clone)]
pub struct CommonHeaps {
    pub world: AbstractHeap,
    pub memory: AbstractHeap,
    pub ssa_state: AbstractHeap,
    pub control: AbstractHeap,
}

impl CommonHeaps {
    pub fn new() -> Self {
        // Simple hierarchy with pre/post order numbers
        // World (1, 8) contains everything
        //   - Memory (2, 3)
        //   - SSAState (4, 5)
        //   - Control (6, 7)
        Self {
            world: AbstractHeap::new(1, 8),
            memory: AbstractHeap::new(2, 3),
            ssa_state: AbstractHeap::new(4, 5),
            control: AbstractHeap::new(6, 7),
        }
    }
}

// The base instruction type
#[derive(Debug, Clone)]
pub struct Inst {
    pub name: String,
    pub inst_type: Type,
    pub opcode: Opcode,
    pub args: Vec<InstIndex>,
    pub origin: Option<Origin>,
    pub index: usize,
    pub block_index: usize,
}

// Special instructions that need extra data
#[derive(Debug, Clone)]
pub struct ConstInst {
    pub base: Inst,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct UpsilonInst {
    pub base: Inst,
    pub phi: InstIndex,
}

// Instruction indices use newtype pattern for type safety
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstIndex(pub usize);

// Basic block indices
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockIndex(pub usize);

// Basic block
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub name: String,
    pub insts: Vec<InstIndex>,
    pub successors: Vec<BlockIndex>,
    pub index: usize,
}

// Function
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: Vec<BlockIndex>,
}

// Entire program
#[derive(Debug, Clone)]
pub struct Program {
    pub insts: Vec<InstData>,
    pub blocks: Vec<BasicBlock>,
    pub functions: Vec<Function>,
    pub common_heaps: CommonHeaps,
}

// The actual instruction data, using an enum to handle different instruction types
#[derive(Debug, Clone)]
pub enum InstData {
    Basic(Inst),
    Const(ConstInst),
    Upsilon(UpsilonInst),
}

impl InstData {
    pub fn get_base(&self) -> &Inst {
        match self {
            InstData::Basic(inst) => inst,
            InstData::Const(inst) => &inst.base,
            InstData::Upsilon(inst) => &inst.base,
        }
    }

    pub fn get_base_mut(&mut self) -> &mut Inst {
        match self {
            InstData::Basic(inst) => inst,
            InstData::Const(inst) => &mut inst.base,
            InstData::Upsilon(inst) => &mut inst.base,
        }
    }

    pub fn get_effects(&self, program: &Program) -> Effects {
        let mut effects = Effects::new();
        
        match self.get_base().opcode {
            Opcode::Phi => {
                // Phi reads from the SSA state
                effects.add_read(program.common_heaps.ssa_state);
            },
            Opcode::Upsilon => {
                // Upsilon writes to the SSA state
                effects.add_write(program.common_heaps.ssa_state);
            },
            Opcode::Branch | Opcode::Jump | Opcode::Return => {
                // Control flow instructions write to the Control abstract heap
                effects.add_write(program.common_heaps.control);
            },
            // Handle other opcodes as needed
            _ => {}
        }
        
        effects
    }

    pub fn is_const(&self, value: i64) -> bool {
        match self {
            InstData::Const(const_inst) => const_inst.value == value,
            _ => false,
        }
    }
}

// InsertionSet for efficient transformations
#[derive(Debug, Default)]
pub struct InsertionSet {
    insertions: Vec<(usize, InstIndex)>,
}

impl InsertionSet {
    pub fn new() -> Self {
        Self {
            insertions: Vec::new(),
        }
    }

    pub fn insert_at(&mut self, index: usize, inst_index: InstIndex) {
        self.insertions.push((index, inst_index));
    }

    pub fn execute(&mut self, program: &mut Program, block_index: BlockIndex) {
        if self.insertions.is_empty() {
            return;
        }

        // Sort insertions by index
        self.insertions.sort_by_key(|(index, _)| *index);

        let block = &mut program.blocks[block_index.0];
        let mut new_insts = Vec::with_capacity(block.insts.len() + self.insertions.len());
        let mut insertion_idx = 0;

        for (pos, inst_index) in block.insts.iter().enumerate() {
            // Add any instructions that should be inserted before the current position
            while insertion_idx < self.insertions.len() && self.insertions[insertion_idx].0 <= pos {
                new_insts.push(self.insertions[insertion_idx].1);
                insertion_idx += 1;
            }
            
            // Add the current instruction
            new_insts.push(*inst_index);
        }

        // Add any remaining insertions
        while insertion_idx < self.insertions.len() {
            new_insts.push(self.insertions[insertion_idx].1);
            insertion_idx += 1;
        }

        // Update the block's instructions
        block.insts = new_insts;

        // Update instruction indices
        for (i, inst_index) in block.insts.iter().enumerate() {
            program.insts[inst_index.0].get_base_mut().index = i;
        }

        // Clear the insertion set
        self.insertions.clear();
    }
}

impl Program {
    pub fn new() -> Self {
        Self {
            insts: Vec::new(),
            blocks: Vec::new(),
            functions: Vec::new(),
            common_heaps: CommonHeaps::new(),
        }
    }

    // Add an instruction to the program
    pub fn add_inst(&mut self, inst: InstData) -> InstIndex {
        let index = InstIndex(self.insts.len());
        self.insts.push(inst);
        index
    }

    // Create a new basic block
    pub fn add_block(&mut self, name: &str) -> BlockIndex {
        let index = BlockIndex(self.blocks.len());
        let block = BasicBlock {
            name: name.to_string(),
            insts: Vec::new(),
            successors: Vec::new(),
            index: index.0,
        };
        self.blocks.push(block);
        index
    }

    // Add an instruction to a block
    pub fn add_inst_to_block(&mut self, block_index: BlockIndex, inst_index: InstIndex) {
        let block = &mut self.blocks[block_index.0];
        let inst_pos = block.insts.len();
        
        // Update the instruction's block index and position
        self.insts[inst_index.0].get_base_mut().block_index = block_index.0;
        self.insts[inst_index.0].get_base_mut().index = inst_pos;
        
        block.insts.push(inst_index);
    }

    // Create a new function
    pub fn add_function(&mut self, name: &str) -> usize {
        let function = Function {
            name: name.to_string(),
            blocks: Vec::new(),
        };
        let index = self.functions.len();
        self.functions.push(function);
        index
    }

    // Add a block to a function
    pub fn add_block_to_function(&mut self, function_index: usize, block_index: BlockIndex) {
        self.functions[function_index].blocks.push(block_index);
    }

    // Helper to create a const instruction
    pub fn create_const(&mut self, value: i64, inst_type: Type, block_index: BlockIndex) -> InstIndex {
        let base = Inst {
            name: format!("const_{}", value),
            inst_type,
            opcode: Opcode::Const,
            args: Vec::new(),
            origin: None,
            index: 0, // Will be set when added to block
            block_index: block_index.0,
        };
        
        let const_inst = ConstInst {
            base,
            value,
        };
        
        let inst_index = self.add_inst(InstData::Const(const_inst));
        self.add_inst_to_block(block_index, inst_index);
        inst_index
    }

    // Helper to create a phi instruction
    pub fn create_phi(&mut self, inst_type: Type, name: &str, block_index: BlockIndex) -> InstIndex {
        let inst = Inst {
            name: name.to_string(),
            inst_type,
            opcode: Opcode::Phi,
            args: Vec::new(),
            origin: None,
            index: 0, // Will be set when added to block
            block_index: block_index.0,
        };
        
        let inst_index = self.add_inst(InstData::Basic(inst));
        self.add_inst_to_block(block_index, inst_index);
        inst_index
    }

    // Helper to create an upsilon instruction
    pub fn create_upsilon(&mut self, value: InstIndex, phi: InstIndex, block_index: BlockIndex) -> InstIndex {
        let base = Inst {
            name: format!("upsilon_{}", self.insts.len()),
            inst_type: Type::Void,
            opcode: Opcode::Upsilon,
            args: vec![value],
            origin: None,
            index: 0, // Will be set when added to block
            block_index: block_index.0,
        };
        
        let upsilon_inst = UpsilonInst {
            base,
            phi,
        };
        
        let inst_index = self.add_inst(InstData::Upsilon(upsilon_inst));
        self.add_inst_to_block(block_index, inst_index);
        inst_index
    }

    // Helper to create a binary operation instruction
    pub fn create_binary_op(&mut self, opcode: Opcode, inst_type: Type, 
                           arg1: InstIndex, arg2: InstIndex, 
                           name: &str, block_index: BlockIndex) -> InstIndex {
        let inst = Inst {
            name: name.to_string(),
            inst_type,
            opcode,
            args: vec![arg1, arg2],
            origin: None,
            index: 0, // Will be set when added to block
            block_index: block_index.0,
        };
        
        let inst_index = self.add_inst(InstData::Basic(inst));
        self.add_inst_to_block(block_index, inst_index);
        inst_index
    }

    // Helper to replace an instruction with an identity instruction
    pub fn replace_with_identity(&mut self, inst_index: InstIndex, value: InstIndex) {
        let inst = &mut self.insts[inst_index.0];
        let base = inst.get_base_mut();
        base.opcode = Opcode::Identity;
        base.args = vec![value];
    }

    // Get the Phi arguments (all Upsilons that target this Phi)
    pub fn get_phi_arguments(&self, phi_index: InstIndex) -> Vec<InstIndex> {
        let mut result = Vec::new();
        
        // Make sure it's actually a Phi
        if self.insts[phi_index.0].get_base().opcode != Opcode::Phi {
            return result;
        }
        
        // Find all Upsilons targeting this Phi
        for (i, inst) in self.insts.iter().enumerate() {
            if let InstData::Upsilon(upsilon) = inst {
                if upsilon.phi == phi_index {
                    result.push(InstIndex(i));
                }
            }
        }
        
        result
    }

    // Get basic block predecessors
    pub fn get_predecessors(&self) -> HashMap<BlockIndex, HashSet<BlockIndex>> {
        let mut result = HashMap::new();
        
        // Initialize empty sets for all blocks
        for (i, _) in self.blocks.iter().enumerate() {
            result.insert(BlockIndex(i), HashSet::new());
        }
        
        // Add predecessors based on successors
        for (i, block) in self.blocks.iter().enumerate() {
            let block_idx = BlockIndex(i);
            for succ in &block.successors {
                result.get_mut(succ).unwrap().insert(block_idx);
            }
        }
        
        result
    }

    // Validate SSA form
    pub fn validate(&self) -> Result<(), String> {
        // Check that all instruction arguments exist
        for inst in &self.insts {
            for &arg in &inst.get_base().args {
                if arg.0 >= self.insts.len() {
                    return Err(format!("Instruction {:?} uses non-existent argument {}", 
                                     inst.get_base().name, arg.0));
                }
            }
        }
        
        // Check that all block successors exist
        for block in &self.blocks {
            for &succ in &block.successors {
                if succ.0 >= self.blocks.len() {
                    return Err(format!("Block {} has non-existent successor {}", 
                                     block.name, succ.0));
                }
            }
        }
        
        // Check SSA dominance rule: each use must be dominated by its definition
        // This would require implementing dominance calculation first
        
        Ok(())
    }

    // Example optimizer: constant folding for Add instructions
    pub fn optimize_constant_add(&mut self, insertion_set: &mut InsertionSet) {
        for block_idx in 0..self.blocks.len() {
            let block_index = BlockIndex(block_idx);
            
            for inst_index in &self.blocks[block_idx].insts.clone() {
                let base = self.insts[inst_index.0].get_base().clone();
                
                if base.opcode == Opcode::Add && base.args.len() == 2 {
                    // let arg0 = &self.insts[base.args[0].0];
                    let arg1 = &self.insts[base.args[1].0];
                    
                    // Check if adding with constant 0
                    if arg1.is_const(0) {
                        // Replace Add with Identity
                        self.replace_with_identity(*inst_index, base.args[0]);
                    }
                    // Check if both arguments are constants
                    else if let (InstData::Const(const0), InstData::Const(const1)) = 
                             (&self.insts[base.args[0].0], &self.insts[base.args[1].0]) {
                        // Compute the folded constant
                        let result = const0.value + const1.value;
                        
                        // Create a new constant instruction
                        let new_const = Inst {
                            name: format!("const_{}", result),
                            inst_type: base.inst_type.clone(),
                            opcode: Opcode::Const,
                            args: Vec::new(),
                            origin: base.origin.clone(),
                            index: 0,
                            block_index: block_index.0,
                        };
                        
                        let const_inst = ConstInst {
                            base: new_const,
                            value: result,
                        };
                        
                        let const_index = self.add_inst(InstData::Const(const_inst));
                        insertion_set.insert_at(base.index, const_index);
                        
                        // Replace the add with identity to the new constant
                        self.replace_with_identity(*inst_index, const_index);
                    }
                }
            }
            
            // Execute all insertions for this block
            insertion_set.execute(self, block_index);
        }
    }

    // Example program: create a simple function with if-else structure
    pub fn create_example_function(&mut self) -> usize {
        let func_idx = self.add_function("example");
        
        // Create blocks
        let root = self.add_block("root");
        let then_block = self.add_block("then");
        let else_block = self.add_block("else");
        let return_block = self.add_block("return");
        
        // Add blocks to function
        self.add_block_to_function(func_idx, root);
        self.add_block_to_function(func_idx, then_block);
        self.add_block_to_function(func_idx, else_block);
        self.add_block_to_function(func_idx, return_block);
        
        // Set up successors
        self.blocks[root.0].successors = vec![then_block, else_block];
        self.blocks[then_block.0].successors = vec![return_block];
        self.blocks[else_block.0].successors = vec![return_block];
        
        // Create root block instructions
        let arg0 = Inst {
            name: "a".to_string(),
            inst_type: Type::Int32,
            opcode: Opcode::GetArgument,
            args: Vec::new(),
            origin: None,
            index: 0,
            block_index: root.0,
        };
        let arg0_idx = self.add_inst(InstData::Basic(arg0));
        self.add_inst_to_block(root, arg0_idx);
        
        let arg1 = Inst {
            name: "b".to_string(),
            inst_type: Type::Int32,
            opcode: Opcode::GetArgument,
            args: Vec::new(),
            origin: None,
            index: 1,
            block_index: root.0,
        };
        let arg1_idx = self.add_inst(InstData::Basic(arg1));
        self.add_inst_to_block(root, arg1_idx);
        
        let branch = Inst {
            name: "branch".to_string(),
            inst_type: Type::Void,
            opcode: Opcode::Branch,
            args: vec![arg0_idx],
            origin: None,
            index: 2,
            block_index: root.0,
        };
        let branch_idx = self.add_inst(InstData::Basic(branch));
        self.add_inst_to_block(root, branch_idx);
        
        // Create then block instructions
        let one_idx = self.create_const(1, Type::Int32, then_block);
        let x1_idx = self.create_binary_op(Opcode::Add, Type::Int32, arg1_idx, one_idx, "x1", then_block);
        
        // Create else block instructions
        let two_idx = self.create_const(2, Type::Int32, else_block);
        let x2_idx = self.create_binary_op(Opcode::Mul, Type::Int32, arg1_idx, two_idx, "x2", else_block);
        
        // Create phi in return block
        let phi_idx = self.create_phi(Type::Int32, "x", return_block);
        
        // Create upsilons in predecessor blocks
        self.create_upsilon(x1_idx, phi_idx, then_block);
        self.create_upsilon(x2_idx, phi_idx, else_block);
        
        // Add final computation and return in return block
        let forty_two_idx = self.create_const(42, Type::Int32, return_block);
        let result_idx = self.create_binary_op(Opcode::Add, Type::Int32, phi_idx, forty_two_idx, "result", return_block);
        
        let ret = Inst {
            name: "return".to_string(),
            inst_type: Type::Void,
            opcode: Opcode::Return,
            args: vec![result_idx],
            origin: None,
            index: 3,
            block_index: return_block.0,
        };
        let ret_idx = self.add_inst(InstData::Basic(ret));
        self.add_inst_to_block(return_block, ret_idx);
        
        func_idx
    }
}

// Example usage
fn main() {
    let mut program = Program::new();
    let func_idx = program.create_example_function();
    
    // Run constant folding optimization
    let mut insertion_set = InsertionSet::new();
    program.optimize_constant_add(&mut insertion_set);
    
    // Output the optimized function
    println!("Function: {}", program.functions[func_idx].name);
    for &block_idx in &program.functions[func_idx].blocks {
        let block = &program.blocks[block_idx.0];
        println!("{}:", block.name);
        
        for &inst_idx in &block.insts {
            let inst = &program.insts[inst_idx.0];
            let base = inst.get_base();
            
            print!("  {:?} {} = {:?}", base.inst_type, base.name, base.opcode);
            
            if !base.args.is_empty() {
                print!("(");
                for (i, &arg) in base.args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{}", program.insts[arg.0].get_base().name);
                }
                print!(")");
            }
            
            // Special handling for Upsilon to show phi target
            if let InstData::Upsilon(upsilon) = inst {
                print!(" → ^{}", program.insts[upsilon.phi.0].get_base().name);
            }
            
            // Special handling for Const to show value
            if let InstData::Const(const_inst) = inst {
                print!(" [{}]", const_inst.value);
            }
            
            println!();
        }
        
        if !block.successors.is_empty() {
            print!("  successors: ");
            for (i, &succ) in block.successors.iter().enumerate() {
                if i > 0 {
                    print!(", ");
                }
                print!("{}", program.blocks[succ.0].name);
            }
            println!();
        }
        println!();
    }
}