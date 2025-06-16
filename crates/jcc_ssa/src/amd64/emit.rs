use crate::Interner;

use super::{BinaryOp, CondCode, FnDef, Inst, Operand, Program, Reg, UnaryOp};

// ---------------------------------------------------------------------------
// AMD64Emitter
// ---------------------------------------------------------------------------

pub struct AMD64Emitter<'a> {
    output: String,
    indent_level: usize,
    fn_def: &'a FnDef,
    program: &'a Program,
    interner: &'a Interner,
}

impl<'a> AMD64Emitter<'a> {
    pub fn new(program: &'a Program, interner: &'a Interner) -> Self {
        Self {
            program,
            interner,
            indent_level: 0,
            output: String::with_capacity(1024),
            fn_def: program
                .funcs
                .first()
                .expect("No function definitions found"),
        }
    }

    pub fn emit(mut self) -> String {
        self.program.funcs.iter().for_each(|fn_def| {
            self.fn_def = fn_def;
            self.emit_fn_def(fn_def);
        });
        self.with_indent(|emitter| emitter.writeln(".section .note.GNU-stack,\"\",@progbits"));
        self.output
    }

    #[inline]
    fn with_indent<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.indent_level += 1;
        f(self);
        self.indent_level -= 1;
    }

    #[inline]
    fn writeln(&mut self, s: &str) {
        if !s.is_empty() {
            self.output.push_str(&"    ".repeat(self.indent_level));
            self.output.push_str(s);
        }
        self.output.push('\n');
    }

    fn emit_fn_def(&mut self, fn_def: &FnDef) {
        // TODO: Use interner data for function names
        // If in macOS, use .globl _name instead of .globl name
        // All user-defined labels are prefixed with `_` on macOS
        let name = self.interner.lookup(fn_def.name);
        if name == "main" {
            // If the function is `main`, we don't need to prefix it with `_`
            // on macOS, but we do on Linux.
            self.with_indent(|emitter| emitter.writeln(".globl main"));
        }
        self.with_indent(|emitter| emitter.writeln(&format!(".globl {name}")));
        self.writeln(&format!("{name}:"));
        self.with_indent(|emitter| {
            emitter.writeln("pushq %rbp");
            emitter.writeln("movq %rsp, %rbp");
        });
        fn_def.blocks.iter().enumerate().for_each(|(idx, block)| {
            if let Some(label) = block.label {
                // TODO: The local label prefix on Linux is `.L` and on macOS is `L`
                let label = self.interner.lookup(label);
                self.writeln(&format!(".L{label}{idx}{name}:"));
            }
            self.with_indent(|emitter| {
                block
                    .instrs
                    .iter()
                    .for_each(|instr| emitter.emit_instr(instr));
            });
        });
    }

    fn emit_instr(&mut self, instr: &Inst) {
        match instr {
            Inst::Ret => {
                self.writeln("movq %rbp, %rsp");
                self.writeln("popq %rbp");
                self.writeln("ret");
            }
            Inst::Cdq => self.writeln("cdq"),
            Inst::Alloca(size) => {
                if *size > 0 {
                    self.writeln(&format!("subq ${size}, %rsp"));
                }
            }
            Inst::Dealloca(size) => {
                if *size > 0 {
                    self.writeln(&format!("addq ${size}, %rsp"));
                }
            }
            Inst::Idiv(oper) => {
                let oper = self.emit_operand_32(oper);
                self.writeln(&format!("idivl {oper}"));
            }
            Inst::Push(oper) => {
                let oper = self.emit_operand_64(oper);
                self.writeln(&format!("pushq {oper}"));
            }
            Inst::Call(name) => {
                let name = self.interner.lookup(*name);
                // TODO: on macOS, we need to prefix the function name with `_`
                // TODO: on Linux, if the function is not defined in the same file,
                // we need to use `@plt` to call it. Ideally we should check if the
                // function is defined in the same file or not and use `@plt` only if it's not.
                // For now, we assume all functions are defined not in the same file.
                self.writeln(&format!("call {name}@plt"));
            }
            Inst::Jmp(target) => {
                let block = self.fn_def.get_block(*target);
                match block.label {
                    Some(label) => {
                        let label = self.interner.lookup(label);
                        let name = self.interner.lookup(self.fn_def.name);
                        self.writeln(&format!("jmp .L{label}{target}{name}"));
                    }
                    None => {
                        self.writeln(&format!("jmp .L{target}"));
                    }
                }
            }
            Inst::Mov { src, dst } => {
                let src = self.emit_operand_32(src);
                let dst = self.emit_operand_32(dst);
                self.writeln(&format!("movl {src}, {dst}"));
            }
            Inst::Cmp { lhs, rhs } => {
                let lhs = self.emit_operand_32(lhs);
                let rhs = self.emit_operand_32(rhs);
                self.writeln(&format!("cmpl {lhs}, {rhs}"));
            }
            Inst::SetCC { cond_code, dst } => {
                let dst = self.emit_operand_8(dst);
                let cond_code = self.emit_cond_code(*cond_code);
                self.writeln(&format!("set{cond_code} {dst}"));
            }
            Inst::Test { lhs, rhs } => {
                let src = self.emit_operand_32(lhs);
                let dst = self.emit_operand_32(rhs);
                self.writeln(&format!("testl {src}, {dst}"));
            }
            Inst::JmpCC { cond_code, target } => {
                let block = self.fn_def.get_block(*target);
                match block.label {
                    Some(label) => {
                        let label = self.interner.lookup(label);
                        let cond_code = self.emit_cond_code(*cond_code);
                        let name = self.interner.lookup(self.fn_def.name);
                        self.writeln(&format!("j{cond_code} .L{label}{target}{name}"));
                    }
                    None => {
                        let cond_code = self.emit_cond_code(*cond_code);
                        self.writeln(&format!("j{cond_code} .L{target}"));
                    }
                }
            }
            Inst::Unary { op, dst } => {
                let dst = self.emit_operand_32(dst);
                match op {
                    UnaryOp::Not => self.writeln(&format!("notl {dst}")),
                    UnaryOp::Neg => self.writeln(&format!("negl {dst}")),
                    UnaryOp::Inc => self.writeln(&format!("incl {dst}")),
                    UnaryOp::Dec => self.writeln(&format!("decl {dst}")),
                };
            }
            Inst::Binary { op, src, dst } => {
                let src = self.emit_operand_32(src);
                let dst = self.emit_operand_32(dst);
                match op {
                    BinaryOp::Add => self.writeln(&format!("addl {src}, {dst}")),
                    BinaryOp::Sub => self.writeln(&format!("subl {src}, {dst}")),
                    BinaryOp::Mul => self.writeln(&format!("imull {src}, {dst}")),
                    BinaryOp::Or => self.writeln(&format!("orl {src}, {dst}")),
                    BinaryOp::And => self.writeln(&format!("andl {src}, {dst}")),
                    BinaryOp::Xor => self.writeln(&format!("xorl {src}, {dst}")),
                    // WARN: Since we assume all values are `int`, we use arithmetic shift
                    BinaryOp::Shl => self.writeln(&format!("sall {src}, {dst}")),
                    BinaryOp::Shr => self.writeln(&format!("sarl {src}, {dst}")),
                };
            }
        }
    }

    fn emit_operand_8(&mut self, oper: &Operand) -> String {
        match oper {
            Operand::Imm(value) => format!("${}", value),
            Operand::Reg(reg) => match reg {
                Reg::Rax => "%al".to_string(),
                Reg::Rbx => "%bl".to_string(),
                Reg::Rcx => "%cl".to_string(),
                Reg::Rdx => "%dl".to_string(),
                Reg::Rdi => "%dil".to_string(),
                Reg::Rsi => "%sil".to_string(),
                Reg::R8 => "%r8b".to_string(),
                Reg::R9 => "%r9b".to_string(),
                Reg::Rg10 => "%r10b".to_string(),
                Reg::Rg11 => "%r11b".to_string(),
            },
            Operand::Stack(offset) => format!("{}(%rbp)", offset),
            Operand::Pseudo(id) => format!("pseudo({})", id),
        }
    }

    fn emit_operand_32(&mut self, oper: &Operand) -> String {
        match oper {
            Operand::Imm(value) => format!("${}", value),
            Operand::Reg(reg) => match reg {
                Reg::Rax => "%eax".to_string(),
                Reg::Rbx => "%ebx".to_string(),
                Reg::Rcx => "%ecx".to_string(),
                Reg::Rdi => "%edi".to_string(),
                Reg::Rdx => "%edx".to_string(),
                Reg::Rsi => "%esi".to_string(),
                Reg::R8 => "%r8d".to_string(),
                Reg::R9 => "%r9d".to_string(),
                Reg::Rg10 => "%r10d".to_string(),
                Reg::Rg11 => "%r11d".to_string(),
            },
            Operand::Stack(offset) => format!("{}(%rbp)", offset),
            Operand::Pseudo(id) => format!("pseudo({})", id),
        }
    }

    fn emit_operand_64(&mut self, oper: &Operand) -> String {
        match oper {
            Operand::Imm(value) => format!("${}", value),
            Operand::Reg(reg) => match reg {
                Reg::Rax => "%rax".to_string(),
                Reg::Rbx => "%rbx".to_string(),
                Reg::Rcx => "%rcx".to_string(),
                Reg::Rdi => "%rdi".to_string(),
                Reg::Rdx => "%rdx".to_string(),
                Reg::Rsi => "%rsi".to_string(),
                Reg::R8 => "%r8".to_string(),
                Reg::R9 => "%r9".to_string(),
                Reg::Rg10 => "%r10".to_string(),
                Reg::Rg11 => "%r11".to_string(),
            },
            Operand::Stack(offset) => format!("{}(%rbp)", offset),
            Operand::Pseudo(id) => format!("pseudo({})", id),
        }
    }

    fn emit_cond_code(&mut self, cond_code: CondCode) -> String {
        match cond_code {
            CondCode::Equal => "e".to_string(),
            CondCode::NotEqual => "ne".to_string(),
            CondCode::LessThan => "l".to_string(),
            CondCode::LessEqual => "le".to_string(),
            CondCode::GreaterThan => "g".to_string(),
            CondCode::GreaterEqual => "ge".to_string(),
        }
    }
}
