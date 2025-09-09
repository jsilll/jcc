use crate::{
    amd64::{InstKind, InstRef, Type},
    ConstValue, Interner,
};

use super::{BinaryOp, Func, Operand, Program, UnaryOp};

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
                        if init.is_zero() {
                            e.writeln(".bss");
                        } else {
                            e.writeln(".data");
                        }
                        e.writeln(&format!(".align {}", var.align))
                    });
                    if var.is_global {
                        self.writeln(&format!("{name}:"));
                    } else {
                        self.writeln(&format!("{name}{}:", v.0));
                    }
                    self.with_indent(|e| match init {
                        ConstValue::Int32(0) => e.writeln(".zero 4"),
                        ConstValue::Int64(0) => e.writeln(".zero 8"),
                        ConstValue::Int32(v) => e.writeln(&format!(".long {v}")),
                        ConstValue::Int64(v) => e.writeln(&format!(".quad {v}")),
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
            InstKind::Cdq(Type::Byte) => self.writeln("cbtw"),
            InstKind::Cdq(Type::Long) => self.writeln("cdq"),
            InstKind::Cdq(Type::Quad) => self.writeln("cqo"),
            InstKind::Push(oper) => {
                let oper = self.emit_operand(&oper, Type::Quad);
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
            InstKind::Idiv { ty, dst } => {
                let oper = self.emit_operand(&dst, ty);
                self.writeln(&format!("idiv{ty} {oper}"));
            }
            InstKind::SetCC { code, dst } => {
                let dst = self.emit_operand(&dst, Type::Byte);
                self.writeln(&format!("set{code} {dst}"));
            }
            InstKind::Test { lhs, rhs } => {
                let src = self.emit_operand(&lhs, Type::Long);
                let dst = self.emit_operand(&rhs, Type::Long);
                self.writeln(&format!("testl {src}, {dst}"));
            }
            InstKind::Movsx { src, dst } => {
                let src = self.emit_operand(&src, Type::Byte);
                let dst = self.emit_operand(&dst, Type::Long);
                self.writeln(&format!("movslq {src}, {dst}"));
            }
            InstKind::Mov { ty, src, dst } => {
                let src = self.emit_operand(&src, Type::Long);
                let dst = self.emit_operand(&dst, Type::Long);
                self.writeln(&format!("mov{ty} {src}, {dst}"));
            }
            InstKind::Cmp { ty, lhs, rhs } => {
                let lhs = self.emit_operand(&lhs, Type::Long);
                let rhs = self.emit_operand(&rhs, Type::Long);
                self.writeln(&format!("cmp{ty} {lhs}, {rhs}"));
            }
            InstKind::JmpCC { code, target } => {
                let block = self.func.block(target);
                match block.label {
                    Some(label) => {
                        let label = self.interner.lookup(label);
                        let name = self.interner.lookup(self.func.name);
                        self.writeln(&format!("j{code} .L{label}{target}{name}"));
                    }
                    None => {
                        self.writeln(&format!("j{code} .L{target}"));
                    }
                }
            }
            InstKind::Unary { ty, op, dst } => {
                let dst = self.emit_operand(&dst, ty);
                match op {
                    UnaryOp::Not => self.writeln(&format!("not{ty} {dst}")),
                    UnaryOp::Neg => self.writeln(&format!("neg{ty} {dst}")),
                    UnaryOp::Inc => self.writeln(&format!("inc{ty} {dst}")),
                    UnaryOp::Dec => self.writeln(&format!("dec{ty} {dst}")),
                };
            }
            InstKind::Binary { ty, op, src, dst } => {
                // WARN: Since we assume all values are `int`, we use arithmetic shift
                let src = self.emit_operand(&src, Type::Long);
                let dst = self.emit_operand(&dst, Type::Long);
                match op {
                    BinaryOp::Or => self.writeln(&format!("or{ty} {src}, {dst}")),
                    BinaryOp::Add => self.writeln(&format!("add{ty} {src}, {dst}")),
                    BinaryOp::Sub => self.writeln(&format!("sub{ty} {src}, {dst}")),
                    BinaryOp::And => self.writeln(&format!("and{ty} {src}, {dst}")),
                    BinaryOp::Xor => self.writeln(&format!("xor{ty} {src}, {dst}")),
                    BinaryOp::Shl => self.writeln(&format!("sal{ty} {src}, {dst}")),
                    BinaryOp::Shr => self.writeln(&format!("sar{ty} {src}, {dst}")),
                    BinaryOp::Mul => self.writeln(&format!("imul{ty} {src}, {dst}")),
                };
            }
        }
    }

    fn emit_operand(&mut self, oper: &Operand, ty: Type) -> String {
        match oper {
            Operand::Imm(v) => format!("${}", v),
            Operand::Pseudo(id) => format!("pseudo({})", id),
            Operand::Stack(offset) => format!("{}(%rbp)", offset),
            Operand::Reg(reg) => match ty {
                Type::Byte => format!("%{}", reg.as_str_8()),
                Type::Long => format!("%{}", reg.as_str_32()),
                Type::Quad => format!("%{}", reg.as_str_64()),
            },
            Operand::Data(v) => {
                let var = self.program.static_var(*v);
                let name = self.interner.lookup(var.name);
                if var.is_global {
                    format!("{}(%rip)", name)
                } else {
                    format!("{}{}(%rip)", name, v.0)
                }
            }
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
