use source_file::SourceSpan;

use std::collections::HashMap;

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
            crate::Instr::Binary { .. } => todo!(),
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
        let mut idx = 0;
        while idx < program.0.instrs.len() {
            self.replace_instr(program, idx);
            idx += 1;
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
                    let tmp = *dst;
                    *dst = Operand::Reg(Reg::Rg10);
                    program.0.instrs.insert(
                        idx + 1,
                        Instr::Mov {
                            src: Operand::Reg(Reg::Rg10),
                            dst: tmp,
                        },
                    );
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
            for instr in &fn_def.instrs {
                emitter.emit_instr(instr);
            }
        });
    }

    fn emit_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::Ret => {
                self.writeln("movq %rbp, %rsp");
                self.writeln("popq %rbp");
                self.writeln("ret");
            }
            Instr::Alloca(size) => {
                if *size > 0 {
                    self.writeln(&format!("subq ${size}, %rsp"));
                }
            }
            Instr::Mov { src, dst } => {
                let src = Self::emit_operand(src);
                let dst = Self::emit_operand(dst);
                self.writeln(&format!("movl {src}, {dst}"));
            }
            Instr::Unary { op, src } => {
                let op = Self::emit_unary_operator(op);
                let src = Self::emit_operand(src);
                self.writeln(&format!("{op} {src}"));
            }
        }
    }

    fn emit_operand(oper: &Operand) -> String {
        match oper {
            Operand::Imm(value) => format!("${value}"),
            Operand::Reg(Reg::Rax) => "%eax".to_owned(),
            Operand::Reg(Reg::Rg10) => "%r10d".to_owned(),
            Operand::Stack(offset) => format!("-{offset}(%rbp)"),
            Operand::Pseudo(_) => panic!("invalid operand"),
        }
    }

    fn emit_unary_operator(op: &UnaryOp) -> &'static str {
        match op {
            UnaryOp::Neg => "negl",
            UnaryOp::Not => "notl",
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
