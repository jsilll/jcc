use crate::ir;

use string_interner::DefaultStringInterner;

// ---------------------------------------------------------------------------
// X86Emitter
// ---------------------------------------------------------------------------

pub struct X86Emitter<'a> {
    ident: usize,
    output: String,
    program: &'a ir::Program,
    interner: &'a DefaultStringInterner,
}

impl<'a> X86Emitter<'a> {
    pub fn new(program: &'a ir::Program, interner: &'a DefaultStringInterner) -> Self {
        Self {
            program,
            interner,
            ident: 0,
            output: String::new(),
        }
    }

    pub fn emit(mut self) -> String {
        self.emit_fn_def(&self.program.0);
        self.ident += 1;
        self.write(".section .note.GNU-stack,\"\",@progbits");
        self.ident -= 1;
        self.output
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(&"    ".repeat(self.ident));
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn emit_fn_def(&mut self, fn_def: &ir::FnDef) {
        // TODO: Improve handle interner errors
        // TODO: If in macOS, use .globl _name instead of .globl name
        let name = self
            .interner
            .resolve(fn_def.name)
            .expect("unresolved symbol");
        self.ident += 1;
        self.write(&format!(".globl {}", name));
        self.ident -= 1;
        self.write(&format!("{}:", name));
        self.ident += 1;
        for instr in &fn_def.body {
            self.emit_instr(instr);
        }
        self.ident -= 1;
    }

    fn emit_instr(&mut self, instr: &ir::Instr) {
        match instr {
            ir::Instr::Ret => {
                self.write("ret");
            }
            ir::Instr::Mov { src, dst } => {
                let src = self.resolve_oper(src);
                let dst = self.resolve_oper(dst);
                self.write(&format!("movl {}, {}", src, dst));
            }
        }
    }

    fn resolve_oper(&self, oper: &ir::Oper) -> String {
        match oper {
            ir::Oper::Reg => "%eax".to_string(),
            ir::Oper::Imm(value) => format!("${}", value),
        }
    }
}
