use crate::{
    amd64::{InstKind, InstRef},
    Interner,
};

use super::{BinaryOp, CondCode, Func, Operand, Program, Reg, UnaryOp};

// ---------------------------------------------------------------------------
// AMD64Emitter
// ---------------------------------------------------------------------------

pub struct AMD64Emitter<'a> {
    output: String,
    indent_level: usize,
    func: &'a Func,
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
            func: program
                .funcs
                .first()
                .expect("No function definitions found"),
        }
    }

    pub fn emit(mut self) -> String {
        self.program
            .iter_static_vars_with_ref()
            .for_each(|(v, var)| {
                let name = self.interner.lookup(var.name);
                if let Some(init) = var.init {
                    self.with_indent(|e| {
                        if var.is_global {
                            e.writeln(&format!(".globl {name}"))
                        }
                        if init == 0 {
                            e.writeln(".bss");
                        } else {
                            e.writeln(".data");
                        }
                        e.writeln(".align 4");
                    });
                    if var.is_global {
                        self.writeln(&format!("{name}:"));
                    } else {
                        self.writeln(&format!("{name}{}:", v.0));
                    }
                    self.with_indent(|e| {
                        if init == 0 {
                            e.writeln(".zero 4");
                        } else {
                            e.writeln(&format!(".long {init}"));
                        }
                    });
                }
            });
        self.program.funcs.iter().for_each(|func| {
            self.func = func;
            self.emit_func(func);
        });
        self.with_indent(|e| e.writeln(".section .note.GNU-stack,\"\",@progbits"));
        self.output
    }

    fn emit_func(&mut self, func: &Func) {
        // TODO: Use interner data for function names
        // If in macOS, use .globl _name instead of .globl name
        // All user-defined labels are prefixed with `_` on macOS
        let name = self.interner.lookup(func.name);
        if func.is_global {
            // If the function is `main`, we don't need to prefix it with `_`
            // on macOS, but we do on Linux.
            self.with_indent(|e| e.writeln(&format!(".globl {name}")));
        }
        self.with_indent(|e| {
            e.writeln(".text");
        });
        self.writeln(&format!("{name}:"));
        self.with_indent(|e| {
            e.writeln("pushq %rbp");
            e.writeln("movq %rsp, %rbp");
        });
        func.blocks[1..]
            .iter()
            .enumerate()
            .for_each(|(idx, block)| {
                if let Some(label) = block.label {
                    // TODO: The local label prefix on Linux is `.L` and on macOS is `L`
                    let label = self.interner.lookup(label);
                    self.writeln(&format!(".L{label}{}{name}:", idx + 1));
                }
                self.with_indent(|e| {
                    block.insts.iter().for_each(|inst| e.emit_inst(*inst));
                });
            });
    }

    fn emit_inst(&mut self, inst: InstRef) {
        let inst = self.func.inst(inst);
        match inst.kind {
            InstKind::Nop => {}
            InstKind::Ret => {
                self.writeln("movq %rbp, %rsp");
                self.writeln("popq %rbp");
                self.writeln("ret");
            }
            InstKind::Cdq => self.writeln("cdq"),
            InstKind::Alloca(size) => {
                if size > 0 {
                    self.writeln(&format!("subq ${size}, %rsp"));
                }
            }
            InstKind::Dealloca(size) => {
                if size > 0 {
                    self.writeln(&format!("addq ${size}, %rsp"));
                }
            }
            InstKind::Idiv(oper) => {
                let oper = self.emit_operand_32(&oper);
                self.writeln(&format!("idivl {oper}"));
            }
            InstKind::Push(oper) => {
                let oper = self.emit_operand_64(&oper);
                self.writeln(&format!("pushq {oper}"));
            }
            InstKind::Call(name) => {
                let name = self.interner.lookup(name);
                // TODO: on macOS, we need to prefix the function name with `_`
                // TODO: on Linux, if the function is not defined in the same file,
                // we need to use `@plt` to call it. Ideally we should check if the
                // function is defined in the same file or not and use `@plt` only if it's not.
                // For now, we assume all functions are defined not in the same file.
                self.writeln(&format!("call {name}@plt"));
            }
            InstKind::Jmp(target) => {
                let block = self.func.block(target);
                match block.label {
                    Some(label) => {
                        let label = self.interner.lookup(label);
                        let name = self.interner.lookup(self.func.name);
                        self.writeln(&format!("jmp .L{label}{target}{name}"));
                    }
                    None => {
                        self.writeln(&format!("jmp .L{target}"));
                    }
                }
            }
            InstKind::Mov { src, dst } => {
                let src = self.emit_operand_32(&src);
                let dst = self.emit_operand_32(&dst);
                self.writeln(&format!("movl {src}, {dst}"));
            }
            InstKind::Cmp { lhs, rhs } => {
                let lhs = self.emit_operand_32(&lhs);
                let rhs = self.emit_operand_32(&rhs);
                self.writeln(&format!("cmpl {lhs}, {rhs}"));
            }
            InstKind::SetCC { code, dst } => {
                let dst = self.emit_operand_8(&dst);
                let cond_code = self.emit_cond_code(code);
                self.writeln(&format!("set{cond_code} {dst}"));
            }
            InstKind::Test { lhs, rhs } => {
                let src = self.emit_operand_32(&lhs);
                let dst = self.emit_operand_32(&rhs);
                self.writeln(&format!("testl {src}, {dst}"));
            }
            InstKind::JmpCC { code, target } => {
                let block = self.func.block(target);
                match block.label {
                    Some(label) => {
                        let label = self.interner.lookup(label);
                        let cond_code = self.emit_cond_code(code);
                        let name = self.interner.lookup(self.func.name);
                        self.writeln(&format!("j{cond_code} .L{label}{target}{name}"));
                    }
                    None => {
                        let cond_code = self.emit_cond_code(code);
                        self.writeln(&format!("j{cond_code} .L{target}"));
                    }
                }
            }
            InstKind::Unary { op, dst } => {
                let dst = self.emit_operand_32(&dst);
                match op {
                    UnaryOp::Not => self.writeln(&format!("notl {dst}")),
                    UnaryOp::Neg => self.writeln(&format!("negl {dst}")),
                    UnaryOp::Inc => self.writeln(&format!("incl {dst}")),
                    UnaryOp::Dec => self.writeln(&format!("decl {dst}")),
                };
            }
            InstKind::Binary { op, src, dst } => {
                let src = self.emit_operand_32(&src);
                let dst = self.emit_operand_32(&dst);
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
            Operand::Data(_) => todo!(),
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
            Operand::Pseudo(id) => format!("pseudo({})", id),
            Operand::Stack(offset) => format!("{}(%rbp)", offset),
            Operand::Data(v) => {
                let var = self.program.static_var(*v);
                let name = self.interner.lookup(var.name);
                if var.is_global {
                    format!("{}(%rip)", name)
                } else {
                    format!("{}{}(%rip)", name, v.0)
                }
            }
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
        }
    }

    fn emit_operand_64(&mut self, oper: &Operand) -> String {
        match oper {
            Operand::Data(_) => todo!(),
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

    // ---------------------------------------------------------------------------
    // Auxiliary methods
    // ---------------------------------------------------------------------------

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
}
