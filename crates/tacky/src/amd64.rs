use source_file::SourceSpan;

// TODO: Build an AMD64 checker:
// - Certain instructions do not allow to use the stack for both operands

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64Builder<'a> {
    program: &'a crate::Program,
}

impl<'a> AMD64Builder<'a> {
    pub fn new(program: &'a crate::Program) -> Self {
        Self { program }
    }

    pub fn build(self) -> Program {
        Program(self.build_from_fn_def(&self.program.0))
    }

    fn build_from_fn_def(&self, fn_def: &crate::FnDef) -> FnDef {
        FnDefBuilder::new().build(fn_def)
    }
}

// ---------------------------------------------------------------------------
// FnDefBuilder
// ---------------------------------------------------------------------------

#[derive(Default)]
struct FnDefBuilder {
    res: FnDef,
}

impl FnDefBuilder {
    fn new() -> Self {
        Self::default()
    }

    fn build(mut self, fn_def: &crate::FnDef) -> FnDef {
        self.res.id = fn_def.id;
        self.res.span = fn_def.span;
        self.res
    }
}

// ---------------------------------------------------------------------------
// AMD64 IR
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Program(pub FnDef);

#[derive(Default, Clone, PartialEq, Eq)]
pub struct FnDef {
    pub id: u32,
    pub span: SourceSpan,
    pub instrs: Vec<Instr>,
    pub instrs_span: Vec<SourceSpan>,
}

impl std::fmt::Debug for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FnDef")
            .field("id", &self.id)
            .field("span", &self.span)
            .field("instrs", &self.instrs)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    /// A `ret` instruction.
    Ret,
    /// Stack allocation instruction.
    Alloca(u32),
    /// A `mov` instruction.
    Mov { src: Operand, dst: Operand },
    /// A unary operation instruction.
    Unary { op: UnaryOp, src: Operand },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    /// An immediate value.
    Imm(u32),
    /// A register.
    Reg(Reg),
    /// A stack operand.
    Stack(u32),
    /// Pseudo register.
    Pseudo(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    /// The ax register.
    Ax,
    /// The R10 register.
    Rg10,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// The unary logical not operator.
    Not,
    /// The unary arithmetic negation operator.
    Neg,
}
