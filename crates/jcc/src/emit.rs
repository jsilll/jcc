use crate::ir;

use string_interner::{DefaultStringInterner, DefaultSymbol};
use thiserror::Error;

// ---------------------------------------------------------------------------
// X64Emitter
// ---------------------------------------------------------------------------

pub struct X64Emitter<'a> {
    output: String,
    indent_level: usize,
    program: &'a ir::Program,
    interner: &'a DefaultStringInterner,
}

impl<'a> X64Emitter<'a> {
    pub fn new(program: &'a ir::Program, interner: &'a DefaultStringInterner) -> Self {
        Self {
            program,
            interner,
            indent_level: 0,
            output: String::with_capacity(1024),
        }
    }

    pub fn emit(mut self) -> Result<String, X64EmitterError> {
        self.emit_fn_def(&self.program.0)?;
        self.writeln(".section .note.GNU-stack,\"\",@progbits");
        Ok(self.output)
    }

    fn emit_fn_def(&mut self, fn_def: &ir::FnDef) -> Result<(), X64EmitterError> {
        // TODO: If in macOS, use .globl _name instead of .globl name
        let name = self
            .interner
            .resolve(fn_def.name)
            .ok_or(X64EmitterError::UnresolvedSymbol(fn_def.name))?;
        self.writeln(&format!(".globl {name}"));
        self.writeln(&format!("{name}:"));
        self.with_indent(|emitter| {
            for instr in &fn_def.body {
                emitter.emit_instr(instr);
            }
        });
        Ok(())
    }

    fn emit_instr(&mut self, instr: &ir::Instr) {
        match instr {
            ir::Instr::Ret => self.writeln("ret"),
            ir::Instr::Mov { src, dst } => {
                let src = self.emit_oper(src);
                let dst = self.emit_oper(dst);
                self.writeln(&format!("movl {src}, {dst}"));
            }
        }
    }

    fn emit_oper(&self, oper: &ir::Oper) -> String {
        match oper {
            ir::Oper::Reg => "%eax".to_string(),
            ir::Oper::Imm(value) => format!("${value}"),
        }
    }

    fn writeln(&mut self, s: &str) {
        if !s.is_empty() {
            self.output.push_str(&"    ".repeat(self.indent_level));
            self.output.push_str(s);
        }
        self.output.push('\n');
    }

    fn with_indent<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.indent_level += 1;
        f(self);
        self.indent_level -= 1;
    }
}

// ---------------------------------------------------------------------------
// X64EmitterError
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum X64EmitterError {
    #[error("unresolved symbol {0:?}")]
    UnresolvedSymbol(DefaultSymbol),
}
