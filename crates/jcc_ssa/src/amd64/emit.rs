use crate::{
    amd64::{InstKind, InstRef, Type},
    infra::emitter::IndentedEmitter,
    ConstValue, Interner,
};

use super::{BinaryOp, Func, Operand, Program, UnaryOp};

use std::fmt::Write;

// ---------------------------------------------------------------------------
// AMD64Emitter
// ---------------------------------------------------------------------------

pub struct AMD64Emitter<'a> {
    func: &'a Func,
    program: &'a Program,
    interner: &'a Interner,
    e: IndentedEmitter<String>,
}

impl<'a> AMD64Emitter<'a> {
    pub fn new(program: &'a Program, interner: &'a Interner) -> Self {
        let func = program
            .funcs
            .first()
            .expect("No function definitions found");
        Self {
            func,
            program,
            interner,
            e: IndentedEmitter::new(String::with_capacity(1024)),
        }
    }

    pub fn emit(mut self) -> Result<String, std::fmt::Error> {
        for (v, var) in self.program.iter_static_vars_with_ref() {
            let name = self.interner.lookup(var.name);
            if let Some(init) = var.init {
                self.indented(|s| {
                    if var.is_global {
                        writeln!(s.e, ".globl {name}")?;
                    }
                    if init.is_zero() {
                        writeln!(s.e, ".bss")?;
                    } else {
                        writeln!(s.e, ".data")?;
                    }
                    // TODO: On macOS, the alignment directive is `.balign` instead of `.align`
                    writeln!(s.e, ".align {}", var.align)?;
                    Ok(())
                })?;
                if var.is_global {
                    writeln!(self.e, "{name}:")?;
                } else {
                    writeln!(self.e, "{name}{}:", v.0)?;
                }
                self.indented(|s| match init {
                    ConstValue::Int32(0) => writeln!(s.e, ".zero 4"),
                    ConstValue::Int64(0) => writeln!(s.e, ".zero 8"),
                    ConstValue::Int32(v) => writeln!(s.e, ".long {v}"),
                    ConstValue::Int64(v) => writeln!(s.e, ".quad {v}"),
                })?;
            }
        }
        for func in self.program.funcs.iter() {
            self.func = func;
            self.emit_func(func)?;
        }
        self.indented(|s| writeln!(s.e, ".section .note.GNU-stack,\"\",@progbits"))?;
        Ok(self.e.into_inner())
    }

    fn emit_func(&mut self, func: &Func) -> std::fmt::Result {
        // TODO: Use interner data for function names
        // If in macOS, use .globl _name instead of .globl name
        // All user-defined labels are prefixed with `_` on macOS
        let name = self.interner.lookup(func.name);
        if func.is_global {
            // If the function is `main`, we don't need to prefix it with `_`
            // on macOS, but we do on Linux.
            self.indented(|s| writeln!(s.e, ".globl {name}"))?;
        }
        self.indented(|s| writeln!(s.e, ".text"))?;
        writeln!(self.e, "{name}:")?;
        self.indented(|s| {
            writeln!(s.e, "pushq %rbp")?;
            writeln!(s.e, "movq %rsp, %rbp")
        })?;
        for (idx, block) in func.blocks[1..].iter().enumerate() {
            if let Some(label) = block.label {
                // TODO: The local label prefix on Linux is `.L` and on macOS is `L`
                let label = self.interner.lookup(label);
                writeln!(self.e, ".L{label}{}{name}:", idx + 1)?;
            }
            self.indented(|s| {
                for inst in block.insts.iter() {
                    s.emit_inst(*inst)?;
                }
                Ok(())
            })?;
        }
        Ok(())
    }

    fn emit_inst(&mut self, inst: InstRef) -> std::fmt::Result {
        let inst = self.func.inst(inst);
        match inst.kind {
            InstKind::Nop => Ok(()),
            InstKind::Ret => {
                writeln!(self.e, "movq %rbp, %rsp")?;
                writeln!(self.e, "popq %rbp")?;
                writeln!(self.e, "ret")
            }
            InstKind::Cdq => match inst.ty {
                Type::Byte => writeln!(self.e, "cbtw"),
                Type::Long => writeln!(self.e, "cdq"),
                Type::Quad => writeln!(self.e, "cqo"),
            },
            InstKind::Push(oper) => {
                let oper = self.emit_operand(&oper, inst.ty);
                writeln!(self.e, "pushq {oper}")
            }
            InstKind::Idiv(oper) => {
                let oper = self.emit_operand(&oper, inst.ty);
                writeln!(self.e, "idiv{} {oper}", inst.ty)
            }
            InstKind::Call(name) => {
                // TODO: on macOS, we need to prefix the function name with `_`
                // TODO: on Linux, if the function is not defined in the same file,
                // we need to use `@plt` to call it. Ideally we should check if the
                // function is defined in the same file or not and use `@plt` only if it's not.
                // For now, we assume all functions are defined not in the same file.
                let name = self.interner.lookup(name);
                writeln!(self.e, "call {name}@plt")
            }
            InstKind::Jmp(target) => {
                let block = self.func.block(target);
                match block.label {
                    None => writeln!(self.e, "jmp .L{target}"),
                    Some(label) => {
                        let label = self.interner.lookup(label);
                        let name = self.interner.lookup(self.func.name);
                        writeln!(self.e, "jmp .L{label}{target}{name}")
                    }
                }
            }
            InstKind::SetCC { code, dst } => {
                let dst = self.emit_operand(&dst, Type::Byte);
                writeln!(self.e, "set{code} {dst}")
            }
            InstKind::Test { lhs, rhs } => {
                let src = self.emit_operand(&lhs, inst.ty);
                let dst = self.emit_operand(&rhs, inst.ty);
                writeln!(self.e, "test{} {src}, {dst}", inst.ty)
            }
            InstKind::Movsx { src, dst } => {
                let src = self.emit_operand(&src, Type::Long);
                let dst = self.emit_operand(&dst, Type::Quad);
                writeln!(self.e, "movslq {src}, {dst}")
            }
            InstKind::Mov { src, dst } => {
                let src = self.emit_operand(&src, inst.ty);
                let dst = self.emit_operand(&dst, inst.ty);
                writeln!(self.e, "mov{} {src}, {dst}", inst.ty)
            }
            InstKind::Cmp { lhs, rhs } => {
                let lhs = self.emit_operand(&lhs, inst.ty);
                let rhs = self.emit_operand(&rhs, inst.ty);
                writeln!(self.e, "cmp{} {lhs}, {rhs}", inst.ty)
            }
            InstKind::JmpCC { code, target } => {
                let block = self.func.block(target);
                match block.label {
                    None => writeln!(self.e, "j{code} .L{target}"),
                    Some(label) => {
                        let label = self.interner.lookup(label);
                        let name = self.interner.lookup(self.func.name);
                        writeln!(self.e, "j{code} .L{label}{target}{name}")
                    }
                }
            }
            InstKind::Unary { op, dst } => {
                let dst = self.emit_operand(&dst, inst.ty);
                match op {
                    UnaryOp::Not => writeln!(self.e, "not{} {dst}", inst.ty),
                    UnaryOp::Neg => writeln!(self.e, "neg{} {dst}", inst.ty),
                    UnaryOp::Inc => writeln!(self.e, "inc{} {dst}", inst.ty),
                    UnaryOp::Dec => writeln!(self.e, "dec{} {dst}", inst.ty),
                }
            }
            InstKind::Binary { op, src, dst } => {
                // WARN: Since we assume all values are `int`, we use arithmetic shift
                let src = self.emit_operand(&src, inst.ty);
                let dst = self.emit_operand(&dst, inst.ty);
                match op {
                    BinaryOp::Or => writeln!(self.e, "or{} {src}, {dst}", inst.ty),
                    BinaryOp::Add => writeln!(self.e, "add{} {src}, {dst}", inst.ty),
                    BinaryOp::Sub => writeln!(self.e, "sub{} {src}, {dst}", inst.ty),
                    BinaryOp::And => writeln!(self.e, "and{} {src}, {dst}", inst.ty),
                    BinaryOp::Xor => writeln!(self.e, "xor{} {src}, {dst}", inst.ty),
                    BinaryOp::Shl => writeln!(self.e, "sal{} {src}, {dst}", inst.ty),
                    BinaryOp::Shr => writeln!(self.e, "sar{} {src}, {dst}", inst.ty),
                    BinaryOp::Mul => writeln!(self.e, "imul{} {src}, {dst}", inst.ty),
                }
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
    pub fn indented<F>(&mut self, f: F) -> std::fmt::Result
    where
        F: FnOnce(&mut Self) -> std::fmt::Result,
    {
        self.e.indent();
        let res = f(self);
        self.e.unindent();
        res
    }
}
