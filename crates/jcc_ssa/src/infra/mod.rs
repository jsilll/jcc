pub mod inset;

/// Trait for intermediate representation (IR) data structures used in compilers.
///
/// This trait provides an abstraction over different IR implementations, allowing
/// the `InsertionSet` to work with various compiler backends while maintaining
/// type safety and efficient operations.
pub trait IR {
    /// The type of instruction used in the IR.
    /// This is typically a struct that represents an instruction
    /// in the IR, containing fields like opcode, operands, etc.
    type Inst: Clone;

    /// A reference to an instruction that can be copied and hashed.
    /// This is typically an index or handle into an instruction storage.
    type InstRef: Copy + std::hash::Hash;

    /// A reference to a basic block that can be copied and hashed.
    /// This is typically an index or handle into a block storage.
    type BlockRef: Copy + std::hash::Hash;

    /// Returns true if the instruction is a no-op (no operation).
    ///
    /// No-op instructions are typically removed during optimization passes
    /// as they don't affect program semantics.
    fn is_nop(&self, inst: Self::InstRef) -> bool;

    /// Creates a new instruction in the IR.
    /// This method adds an instruction to the IR and returns a reference to it.
    fn new_inst(&mut self, inst: Self::Inst) -> Self::InstRef;

    /// Returns the instructions in a block as a slice.
    ///
    /// The returned slice represents the current sequence of instructions
    /// in the specified block, in execution order.
    fn block_insts(&self, block: Self::BlockRef) -> &[Self::InstRef];

    /// Swaps the instructions in a block with a new set of instructions.
    /// Returns the previous instructions.
    ///
    /// This operation replaces the entire instruction sequence of a block,
    /// which is useful for applying batched modifications efficiently.
    fn swap_block_insts(
        &mut self,
        block: Self::BlockRef,
        insts: Vec<Self::InstRef>,
    ) -> Vec<Self::InstRef>;
}
