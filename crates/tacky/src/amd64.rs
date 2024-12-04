use source_file::SourceSpan;

use std::collections::{HashMap, LinkedList};

// TODO: Build an AMD64 checker:
// - Certain instructions do not allow to use the stack for both operands

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64Builder<'a> {
    tacky: &'a crate::Program,
    program: Program,
}

impl<'a> AMD64Builder<'a> {
    pub fn new(tacky: &'a crate::Program) -> Self {
        Self {
            tacky,
            program: Program::default(),
        }
    }

    pub fn build(mut self) -> Program {
        self.program.fn_def = self.build_from_fn_def(&self.tacky.0);
        self.program
    }

    fn build_from_fn_def(&mut self, fn_def: &crate::FnDef) -> FnDef {
        FnDefBuilder::new(&mut self.program).build(fn_def)
    }
}

// ---------------------------------------------------------------------------
// FnDefBuilder
// ---------------------------------------------------------------------------

struct FnDefBuilder<'a> {
    fn_def: FnDef,
    program: &'a mut Program,
}

impl<'a> FnDefBuilder<'a> {
    fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            fn_def: Default::default(),
        }
    }

    fn build(mut self, fn_def: &crate::FnDef) -> FnDef {
        self.fn_def.id = fn_def.id;
        self.fn_def.span = fn_def.span;
        self.fn_def.instrs.push_back(Instr::Alloca(0));
        self.program
            .insert_instr_span(self.fn_def.instrs.back(), fn_def.span);
        for (instr, span) in fn_def.instrs.iter().zip(fn_def.instrs_span.iter()) {
            self.build_from_instr(instr, span);
        }
        self.fn_def
    }

    fn build_from_instr(&mut self, instr: &crate::Instr, span: &SourceSpan) {
        match instr {
            crate::Instr::Return(value) => {
                let src = Self::build_from_value(*value);
                let dst = Operand::Reg(Reg::Rax);
                self.fn_def.instrs.push_back(Instr::Mov { src, dst });
                self.program
                    .insert_instr_span(self.fn_def.instrs.back(), *span);
                self.fn_def.instrs.push_back(Instr::Ret);
                self.program
                    .insert_instr_span(self.fn_def.instrs.back(), *span);
            }
            crate::Instr::Unary { op, src, dst } => {
                let op = UnaryOp::from(*op);
                let src = Self::build_from_value(*src);
                let dst = Self::build_from_value(*dst);
                self.fn_def.instrs.push_back(Instr::Mov { src, dst });
                self.program
                    .insert_instr_span(self.fn_def.instrs.back(), *span);
                self.fn_def.instrs.push_back(Instr::Unary { op, src: dst });
                self.program
                    .insert_instr_span(self.fn_def.instrs.back(), *span);
            }
        }
    }

    fn build_from_value(value: crate::Value) -> Operand {
        match value {
            crate::Value::Constant(value) => Operand::Imm(value),
            crate::Value::Variable(id) => Operand::Pseudo(id),
        }
    }
}

// ---------------------------------------------------------------------------
// AMD64PseudoReplacer
// ---------------------------------------------------------------------------

pub struct AMD64PseudoReplacer {
    offset: u32,
    offsets: HashMap<u32, u32>,
}

impl AMD64PseudoReplacer {
    pub fn new() -> Self {
        Self {
            offset: 0,
            offsets: HashMap::new(),
        }
    }

    pub fn replace(mut self, program: &mut Program) {
        for instr in program.fn_def.instrs.iter_mut() {
            self.replace_inside_instr(instr);
        }
        if let Some(Instr::Alloca(size)) = program.fn_def.instrs.front_mut() {
            *size = self.offset;
        }
    }

    fn replace_inside_instr(&mut self, instr: &mut Instr) {
        match instr {
            Instr::Ret | Instr::Alloca(_) => {}
            Instr::Mov { src, dst } => {
                self.replace_operand(src);
                self.replace_operand(dst);
            }
            Instr::Unary { src, .. } => {
                self.replace_operand(src);
            }
        }
    }

    fn replace_operand(&mut self, oper: &mut Operand) {
        if let Operand::Pseudo(id) = oper {
            *oper = Operand::Stack(*self.offsets.entry(*id).or_insert_with(|| {
                self.offset += 4;
                self.offset
            }));
        }
    }
}

// ---------------------------------------------------------------------------
// AMD64 IR
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Program {
    pub fn_def: FnDef,
    pub instrs_span: HashMap<InstrId, SourceSpan>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InstrId(*const Instr);

impl TryFrom<Option<&Instr>> for InstrId {
    type Error = ();

    fn try_from(instr: Option<&Instr>) -> Result<Self, Self::Error> {
        match instr {
            Some(instr) => Ok(Self(instr as *const Instr)),
            None => Err(()),
        }
    }
}

impl Program {
    pub fn insert_instr_span(&mut self, instr: impl TryInto<InstrId>, span: SourceSpan) {
        if let Ok(instr) = instr.try_into() {
            self.instrs_span.insert(instr, span);
        }
    }
}

impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FnDef")
            .field("fn_def", &self.fn_def)
            .finish()
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct FnDef {
    pub id: u32,
    pub span: SourceSpan,
    pub instrs: LinkedList<Instr>,
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
    /// The RAX register.
    Rax,
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

impl From<crate::UnaryOp> for UnaryOp {
    fn from(op: crate::UnaryOp) -> UnaryOp {
        match op {
            crate::UnaryOp::Neg => UnaryOp::Neg,
            crate::UnaryOp::Not => UnaryOp::Not,
        }
    }
}
