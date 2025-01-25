use source_file::SourceSpan;

use std::collections::HashMap;

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
    /// A `cdq` instruction.
    Cdq,
    /// Stack allocation instruction.
    Alloca(u32),
    /// An `idiv` instruction.
    Idiv(Operand),
    /// A `mov` instruction.
    Mov { src: Operand, dst: Operand },
    /// A unary operation instruction.
    Unary { op: UnaryOp, src: Operand },
    /// A binary operation instruction.
    Binary {
        op: BinaryOp,
        src: Operand,
        dst: Operand,
    },
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
    /// The RCX register.
    Rcx,
    /// The RDX register.
    Rdx,
    /// The R10 register.
    Rg10,
    /// The R10 register.
    Rg11,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// The not operator.
    Not,
    /// The arithmetic negation operator.
    Neg,
}

impl From<crate::UnaryOp> for UnaryOp {
    fn from(op: crate::UnaryOp) -> Self {
        match op {
            crate::UnaryOp::Neg => Self::Neg,
            crate::UnaryOp::Not => Self::Not,
            crate::UnaryOp::BitNot => Self::Not,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// The add operator.
    Add,
    /// The sub operator.
    Sub,
    /// The sub operator.
    Mul,
    /// The or operator.
    Or,
    /// The and operator.
    And,
    /// The xor operator.
    Xor,
    /// The shift left operator.
    Shl,
    /// The shift right operator.
    Shr,
}

impl TryFrom<crate::BinaryOp> for BinaryOp {
    type Error = ();

    fn try_from(op: crate::BinaryOp) -> Result<Self, Self::Error> {
        match op {
            crate::BinaryOp::Add => Ok(Self::Add),
            crate::BinaryOp::Sub => Ok(Self::Sub),
            crate::BinaryOp::Mul => Ok(Self::Mul),
            crate::BinaryOp::BitOr => Ok(Self::Or),
            crate::BinaryOp::BitAnd => Ok(Self::And),
            crate::BinaryOp::BitXor => Ok(Self::Xor),
            crate::BinaryOp::BitShl => Ok(Self::Shl),
            crate::BinaryOp::BitShr => Ok(Self::Shr),
            crate::BinaryOp::Div | crate::BinaryOp::Rem => Err(()),
            _ => todo!(),
        }
    }
}

// ---------------------------------------------------------------------------
// AMD64Builder
// ---------------------------------------------------------------------------

pub struct AMD64Builder<'a> {
    tacky: &'a crate::Program,
}

impl<'a> AMD64Builder<'a> {
    pub fn new(tacky: &'a crate::Program) -> Self {
        Self { tacky }
    }

    pub fn build(self) -> Program {
        Program(FnDefBuilder::new().build(&self.tacky.0))
    }
}

// ---------------------------------------------------------------------------
// FnDefBuilder
// ---------------------------------------------------------------------------

struct FnDefBuilder {
    fn_def: FnDef,
}

impl FnDefBuilder {
    fn new() -> Self {
        Self {
            fn_def: Default::default(),
        }
    }

    fn build(mut self, fn_def: &crate::FnDef) -> FnDef {
        self.fn_def.id = fn_def.id;
        self.fn_def.span = fn_def.span;
        self.fn_def.instrs.push(Instr::Alloca(0));
        self.fn_def.instrs_span.push(fn_def.span);
        for (instr, span) in fn_def.blocks[0]
            .instrs
            .iter()
            .zip(fn_def.blocks[0].spans.iter())
        {
            self.build_from_instr(instr, span);
        }
        self.fn_def
    }

    fn build_from_instr(&mut self, instr: &crate::Instr, span: &SourceSpan) {
        match instr {
            crate::Instr::Return(value) => {
                let src = Self::build_from_value(*value);
                let dst = Operand::Reg(Reg::Rax);
                self.fn_def.instrs.push(Instr::Mov { src, dst });
                self.fn_def.instrs_span.push(*span);
                self.fn_def.instrs.push(Instr::Ret);
                self.fn_def.instrs_span.push(*span);
            }
            crate::Instr::Unary { op, src, dst } => {
                let op = UnaryOp::from(*op);
                let src = Self::build_from_value(*src);
                let dst = Self::build_from_value(*dst);
                self.fn_def.instrs.push(Instr::Mov { src, dst });
                self.fn_def.instrs_span.push(*span);
                self.fn_def.instrs.push(Instr::Unary { op, src: dst });
                self.fn_def.instrs_span.push(*span);
            }
            crate::Instr::Binary { op, lhs, rhs, dst } => {
                let lhs = Self::build_from_value(*lhs);
                let rhs = Self::build_from_value(*rhs);
                let dst = Self::build_from_value(*dst);
                match op {
                    crate::BinaryOp::Div | crate::BinaryOp::Rem => {
                        self.fn_def.instrs.push(Instr::Mov {
                            src: lhs,
                            dst: Operand::Reg(Reg::Rax),
                        });
                        self.fn_def.instrs_span.push(*span);
                        self.fn_def.instrs.push(Instr::Cdq);
                        self.fn_def.instrs_span.push(*span);
                        self.fn_def.instrs.push(Instr::Idiv(rhs));
                        self.fn_def.instrs_span.push(*span);
                        self.fn_def.instrs.push(Instr::Mov {
                            src: match op {
                                crate::BinaryOp::Div => Operand::Reg(Reg::Rax),
                                crate::BinaryOp::Rem => Operand::Reg(Reg::Rdx),
                                _ => unreachable!(),
                            },
                            dst,
                        });
                        self.fn_def.instrs_span.push(*span);
                    }
                    _ => {
                        if let Ok(op) = BinaryOp::try_from(*op) {
                            self.fn_def.instrs.push(Instr::Mov { src: lhs, dst });
                            self.fn_def.instrs_span.push(*span);
                            self.fn_def.instrs.push(Instr::Binary { op, src: rhs, dst });
                            self.fn_def.instrs_span.push(*span);
                        }
                    }
                }
            }
            crate::Instr::Jump { .. } => todo!(),
            crate::Instr::JumpIfZero { .. } => todo!(),
            crate::Instr::JumpIfNotZero { .. } => todo!(),
            crate::Instr::Copy { .. } => todo!(),
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
// AMD64Fixer
// ---------------------------------------------------------------------------

pub struct AMD64Fixer {
    offset: u32,
    offsets: HashMap<u32, u32>,
}

impl AMD64Fixer {
    pub fn new() -> Self {
        Self {
            offset: 0,
            offsets: HashMap::new(),
        }
    }

    pub fn fix(mut self, program: &mut Program) {
        let mut idx = 0;
        while idx < program.0.instrs.len() {
            self.fix_instr(program, &mut idx);
            idx += 1;
        }
        if let Some(Instr::Alloca(size)) = program.0.instrs.first_mut() {
            *size = self.offset;
        }
    }

    fn fix_instr(&mut self, program: &mut Program, idx: &mut usize) {
        let instr = &mut program.0.instrs[*idx];
        match instr {
            Instr::Ret | Instr::Cdq | Instr::Alloca(_) => {}
            Instr::Idiv(oper) => {
                self.fix_operand(oper);
                if let Operand::Imm(_) = oper {
                    // NOTE
                    //
                    // The `idiv` doesn't support
                    // immediate values, so we need to move it
                    // to a register.
                    //
                    // idivl $3
                    //
                    // movl $3, %r10d
                    // idivl %r10d
                    let tmp = *oper;
                    *oper = Operand::Reg(Reg::Rg10);
                    program.0.instrs.insert(
                        *idx,
                        Instr::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg10),
                        },
                    );
                    *idx += 1;
                }
            }
            Instr::Mov { src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                if matches!(src, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) {
                    // NOTE
                    //
                    // The `mov` instruction
                    // doesn't support moving from memory
                    // to memory, so we need to use a register
                    // as a temporary.
                    //
                    // movl -4(%rbp), -8(%rbp)
                    //
                    // movl -4(%rbp), %r10d
                    // movl %r10d, -8(%rbp)
                    let tmp = *dst;
                    *dst = Operand::Reg(Reg::Rg10);
                    program.0.instrs.insert(
                        *idx + 1,
                        Instr::Mov {
                            src: Operand::Reg(Reg::Rg10),
                            dst: tmp,
                        },
                    );
                    *idx += 1;
                }
            }
            Instr::Unary { src, .. } => self.fix_operand(src),
            Instr::Binary { op, src, dst } => {
                self.fix_operand(src);
                self.fix_operand(dst);
                match *op {
                    BinaryOp::Mul => {
                        if let Operand::Stack(_) = dst {
                            // NOTE
                            //
                            // The `imul` can't use
                            // a memory address as the destination
                            // operand, so we need to use a register
                            // as a temporary.
                            //
                            // imull $3, -4(%rbp)
                            //
                            // movl -4(%rbp), %r11d
                            // imull $3, %r11d
                            // movl %r11d, -4(%rbp)
                            let tmp = *dst;
                            *dst = Operand::Reg(Reg::Rg11);
                            program.0.instrs.insert(
                                *idx,
                                Instr::Mov {
                                    src: tmp,
                                    dst: Operand::Reg(Reg::Rg11),
                                },
                            );
                            *idx += 2;
                            program.0.instrs.insert(
                                *idx,
                                Instr::Mov {
                                    src: Operand::Reg(Reg::Rg11),
                                    dst: tmp,
                                },
                            );
                        }
                    }
                    BinaryOp::Shl | BinaryOp::Shr => {
                        if !matches!(src, Operand::Imm(_)) && !matches!(src, Operand::Reg(Reg::Rcx))
                        {
                            // NOTE
                            //
                            // The shift instructions only supports
                            // dynamic shift counts in the CL register, so
                            // we need to move the shift count to CL.
                            //
                            // shl $1, %rax
                            //
                            // movl $1, %cl
                            // shll %cl, %rax
                            let tmp = *src;
                            *src = Operand::Reg(Reg::Rcx);
                            program.0.instrs.insert(
                                *idx,
                                Instr::Mov {
                                    src: tmp,
                                    dst: Operand::Reg(Reg::Rcx),
                                },
                            );
                            *idx += 1;
                        }
                    }
                    _ => {
                        match (&src, &dst) {
                            (Operand::Stack(_), Operand::Stack(_)) => {
                                // NOTE
                                //
                                // Binary instructions don't support
                                // moving from memory to memory, so we need to
                                // use a register as a temporary.
                                //
                                // addl -4(%rbp), -8(%rbp)
                                //
                                // movl -4(%rbp), %r10d
                                // addl %r10d, -8(%rbp)
                                let tmp = *src;
                                *src = Operand::Reg(Reg::Rg10);
                                program.0.instrs.insert(
                                    *idx,
                                    Instr::Mov {
                                        src: tmp,
                                        dst: Operand::Reg(Reg::Rg10),
                                    },
                                );
                                *idx += 1;
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    fn fix_operand(&mut self, oper: &mut Operand) {
        if let Operand::Pseudo(id) = oper {
            *oper = Operand::Stack(*self.offsets.entry(*id).or_insert_with(|| {
                self.offset += 4;
                self.offset
            }));
        }
    }
}

// ---------------------------------------------------------------------------
// AMD64Emitter
// ---------------------------------------------------------------------------

pub struct AMD64Emitter<'a> {
    output: String,
    indent_level: usize,
    program: &'a Program,
}

impl<'a> AMD64Emitter<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            indent_level: 0,
            output: String::with_capacity(1024),
        }
    }

    pub fn emit(mut self) -> String {
        self.emit_fn_def(&self.program.0);
        self.with_indent(|emitter| emitter.writeln(".section .note.GNU-stack,\"\",@progbits"));
        self.output
    }

    fn emit_fn_def(&mut self, fn_def: &FnDef) {
        // TODO: use interner data for function names
        // TODO: If in macOS, use .globl _name instead of .globl name
        let name = "main";
        self.with_indent(|emitter| emitter.writeln(&format!(".globl {name}")));
        self.writeln(&format!("{name}:"));
        self.with_indent(|emitter| {
            emitter.writeln("pushq %rbp");
            emitter.writeln("movq %rsp, %rbp");
        });
        self.with_indent(|emitter| {
            fn_def
                .instrs
                .iter()
                .for_each(|instr| emitter.emit_instr(instr));
        });
    }

    fn emit_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::Ret => {
                self.writeln("movq %rbp, %rsp");
                self.writeln("popq %rbp");
                self.writeln("ret");
            }
            Instr::Cdq => self.writeln("cdq"),
            Instr::Alloca(size) => {
                if *size > 0 {
                    self.writeln(&format!("subq ${size}, %rsp"));
                }
            }
            Instr::Idiv(oper) => {
                let oper = self.emit_operand(oper);
                self.writeln(&format!("idivl {oper}"));
            }
            Instr::Mov { src, dst } => {
                let src = self.emit_operand(src);
                let dst = self.emit_operand(dst);
                self.writeln(&format!("movl {src}, {dst}"));
            }
            Instr::Unary { op, src } => {
                let src = self.emit_operand(src);
                match op {
                    UnaryOp::Not => self.writeln(&format!("notl {src}")),
                    UnaryOp::Neg => self.writeln(&format!("negl {src}")),
                };
            }
            Instr::Binary { op, src, dst } => {
                let src = self.emit_operand(src);
                let dst = self.emit_operand(dst);
                match op {
                    BinaryOp::Add => self.writeln(&format!("addl {src}, {dst}")),
                    BinaryOp::Sub => self.writeln(&format!("subl {src}, {dst}")),
                    BinaryOp::Mul => self.writeln(&format!("imull {src}, {dst}")),
                    BinaryOp::Or => self.writeln(&format!("orl {src}, {dst}")),
                    BinaryOp::And => self.writeln(&format!("andl {src}, {dst}")),
                    BinaryOp::Xor => self.writeln(&format!("xorl {src}, {dst}")),
                    // NOTE: Since we assume all values are `int`, we use arithmetic shift
                    BinaryOp::Shl => self.writeln(&format!("sall {src}, {dst}")),
                    BinaryOp::Shr => self.writeln(&format!("sarl {src}, {dst}")),
                };
            }
        }
    }

    fn emit_operand(&mut self, oper: &Operand) -> String {
        match oper {
            Operand::Imm(value) => format!("${}", value),
            Operand::Reg(reg) => match reg {
                Reg::Rax => "%eax".to_string(),
                Reg::Rcx => "%ecx".to_string(),
                Reg::Rdx => "%edx".to_string(),
                Reg::Rg10 => "%r10d".to_string(),
                Reg::Rg11 => "%r11d".to_string(),
            },
            Operand::Stack(offset) => format!("-{}(%rbp)", offset),
            Operand::Pseudo(id) => format!("pseudo({})", id),
        }
    }

    fn with_indent<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.indent_level += 1;
        f(self);
        self.indent_level -= 1;
    }

    fn writeln(&mut self, s: &str) {
        if !s.is_empty() {
            self.output.push_str(&"    ".repeat(self.indent_level));
            self.output.push_str(s);
        }
        self.output.push('\n');
    }
}
