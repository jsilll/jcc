use source_file::SourceSpan;

use std::collections::HashMap;

// TODO: Build an AMD64 checker:
// - Certain instructions do not allow to use the stack for both operands

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64Builder<'a> {
    program: Program,
    tacky: &'a crate::Program,
}

impl<'a> AMD64Builder<'a> {
    pub fn new(tacky: &'a crate::Program) -> Self {
        Self {
            tacky,
            program: Program::default(),
        }
    }

    pub fn build(mut self) -> Program {
        self.program.0 = FnDefBuilder::new().build(&self.tacky.0);
        self.program
    }
}

// ---------------------------------------------------------------------------
// FnDefBuilder
// ---------------------------------------------------------------------------

struct FnDefBuilder {
    res: FnDef,
}

impl FnDefBuilder {
    fn new() -> Self {
        Self {
            res: Default::default(),
        }
    }

    fn build(mut self, fn_def: &crate::FnDef) -> FnDef {
        self.res.id = fn_def.id;
        self.res.span = fn_def.span;
        self.res.instrs.push(Instr::Alloca(0));
        self.res.instrs_span.push(fn_def.span);
        for (instr, span) in fn_def.instrs.iter().zip(fn_def.instrs_span.iter()) {
            self.build_from_instr(instr, span);
        }
        self.res
    }

    fn build_from_instr(&mut self, instr: &crate::Instr, span: &SourceSpan) {
        match instr {
            crate::Instr::Return(value) => {
                let src = Self::build_from_value(*value);
                let dst = Operand::Reg(Reg::Rax);
                self.res.instrs.push(Instr::Mov { src, dst });
                self.res.instrs_span.push(*span);
                self.res.instrs.push(Instr::Ret);
                self.res.instrs_span.push(*span);
            }
            crate::Instr::Unary { op, src, dst } => {
                let op = UnaryOp::from(*op);
                let src = Self::build_from_value(*src);
                let dst = Self::build_from_value(*dst);
                self.res.instrs.push(Instr::Mov { src, dst });
                self.res.instrs_span.push(*span);
                self.res.instrs.push(Instr::Unary { op, src: dst });
                self.res.instrs_span.push(*span);
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
        for idx in (0..).take_while(|idx| *idx < program.0.instrs.len()) {
            self.replace_instr(program, idx);
        }
        if let Some(Instr::Alloca(size)) = program.0.instrs.first_mut() {
            *size = self.offset;
        }
    }

    fn replace_instr(&mut self, program: &mut Program, idx: usize) {
        let instr = &mut program.0.instrs[idx];
        match instr {
            Instr::Ret | Instr::Alloca(_) => {}
            Instr::Mov { src, dst } => {
                self.replace_operand(src);
                self.replace_operand(dst);
                if matches!(src, Operand::Stack(_)) && matches!(src, Operand::Stack(_)) {
                    todo!()
                }
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
