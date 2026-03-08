use crate::{
    ir::{
        inst::Inst, term::Terminator, ty::Ty, Block, Function, FunctionData, Global, Program, Value,
    },
    IdentInterner,
};

const INVALID_IDENT: &str = "<invalid-ident>";

// ---------------------------------------------------------------------------
// ProgramPretty
// ---------------------------------------------------------------------------

pub struct ProgramPretty<'a> {
    program: &'a Program,
    interner: &'a IdentInterner,
}

impl<'a> ProgramPretty<'a> {
    pub fn new(program: &'a Program, interner: &'a IdentInterner) -> Self {
        Self { program, interner }
    }
}

impl std::fmt::Display for ProgramPretty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ProgramPrettyEmitter::with_resolved(self.program, self.interner, f).emit()
    }
}

// ---------------------------------------------------------------------------
// ProgramPrettyEmitter
// ---------------------------------------------------------------------------

pub struct ProgramPrettyEmitter<'a, W: std::fmt::Write> {
    out: W,
    mode: NameMode<'a>,
    program: &'a Program,
}

impl<'a, W: std::fmt::Write> ProgramPrettyEmitter<'a, W> {
    pub fn with_handles(program: &'a Program, out: W) -> Self {
        Self {
            out,
            program,
            mode: NameMode::Handles,
        }
    }

    pub fn with_resolved(program: &'a Program, interner: &'a IdentInterner, out: W) -> Self {
        Self {
            out,
            program,
            mode: NameMode::Resolved(interner),
        }
    }

    pub fn emit(mut self) -> std::fmt::Result {
        for (handle, data) in self.program.globals.iter() {
            self.write_global_ref(handle)?;
            write!(&mut self.out, " = ")?;
            if data.is_global {
                write!(&mut self.out, "global ")?;
            } else {
                write!(&mut self.out, "constant ")?;
            }
            write!(&mut self.out, "{} ", data.ty)?;

            match data.init {
                None => writeln!(&mut self.out, "zeroinitializer")?,
                Some(val) => match data.ty {
                    Ty::F64 => writeln!(&mut self.out, "{}", f64::from_bits(val))?,
                    Ty::F32 => writeln!(&mut self.out, "{}", f32::from_bits(val as u32))?,
                    _ => writeln!(&mut self.out, "{}", val)?,
                },
            }
        }

        if !self.program.globals.is_empty() {
            writeln!(&mut self.out)?;
        }

        for (handle, data) in self.program.functions.iter() {
            write!(&mut self.out, "define ")?;
            self.write_function_ref(handle)?;
            writeln!(&mut self.out, " {{")?;
            self.emit_function_body(data)?;
            writeln!(&mut self.out, "}}")?;
        }

        Ok(())
    }

    fn emit_function_body(&mut self, function: &FunctionData) -> std::fmt::Result {
        for (idx, block) in function.blocks(self.program).enumerate() {
            if idx > 0 {
                writeln!(&mut self.out)?;
            }
            self.write_block_ref(block)?;
            writeln!(&mut self.out, ":")?;

            for value in &self.program.blocks[block].insts {
                let data = &self.program.values[*value];
                write!(&mut self.out, "  ")?;
                if data.inst.ty() != Ty::Void {
                    write!(&mut self.out, "{} = ", value)?;
                }
                self.write_inst(&data.inst)?;
                writeln!(&mut self.out)?;
            }

            write!(&mut self.out, "  ")?;
            self.write_term(&self.program.blocks[block].term)?;
            writeln!(&mut self.out)?;
        }
        Ok(())
    }

    fn write_inst(&mut self, inst: &Inst) -> std::fmt::Result {
        match inst {
            Inst::GlobalAddr(global) => {
                write!(&mut self.out, "global.addr ")?;
                self.write_global_ref(*global)
            }
            Inst::Call { ty, func, args } => {
                write!(&mut self.out, "call {} ", ty)?;
                self.write_function_ref(*func)?;
                write!(&mut self.out, "(")?;
                self.write_values(args)?;
                write!(&mut self.out, ")")
            }
            _ => write!(&mut self.out, "{}", inst),
        }
    }

    fn write_term(&mut self, term: &Terminator) -> std::fmt::Result {
        match term {
            Terminator::Unreachable => write!(&mut self.out, "unreachable"),
            Terminator::Br(dest) => {
                write!(&mut self.out, "br ")?;
                self.write_block_ref(*dest)
            }
            Terminator::Ret(val) => match val {
                Some(v) => write!(&mut self.out, "ret {}", v),
                None => write!(&mut self.out, "ret void"),
            },
            Terminator::CondBr {
                cond,
                then_block,
                else_block,
            } => {
                write!(&mut self.out, "br i1 {}, ", cond)?;
                self.write_block_ref(*then_block)?;
                write!(&mut self.out, ", ")?;
                self.write_block_ref(*else_block)
            }
            Terminator::Switch {
                value,
                cases,
                default,
            } => {
                write!(&mut self.out, "switch {} [ default: ", value)?;
                self.write_block_ref(*default)?;
                for (val, blk) in cases {
                    write!(&mut self.out, ", {}: ", val)?;
                    self.write_block_ref(*blk)?;
                }
                write!(&mut self.out, " ]")
            }
        }
    }

    fn write_values(&mut self, values: &[Value]) -> std::fmt::Result {
        let mut it = values.iter();
        if let Some(first) = it.next() {
            write!(&mut self.out, "{}", first)?;
            for value in it {
                write!(&mut self.out, ", {}", value)?;
            }
        }
        Ok(())
    }

    fn write_block_ref(&mut self, handle: Block) -> std::fmt::Result {
        match self.mode {
            NameMode::Handles => write!(&mut self.out, "{}", handle),
            NameMode::Resolved(interner) => {
                let data = &self.program.blocks[handle];
                let name = interner.get(data.name).unwrap_or(INVALID_IDENT);
                write!(&mut self.out, "{}.{}", name, handle.as_u32())
            }
        }
    }

    fn write_global_ref(&mut self, handle: Global) -> std::fmt::Result {
        match self.mode {
            NameMode::Handles => write!(&mut self.out, "{}", handle),
            NameMode::Resolved(interner) => {
                let data = &self.program.globals[handle];
                let name = interner.get(data.name).unwrap_or(INVALID_IDENT);
                write!(&mut self.out, "@{}", name)
            }
        }
    }

    fn write_function_ref(&mut self, handle: Function) -> std::fmt::Result {
        match self.mode {
            NameMode::Handles => write!(&mut self.out, "{}", handle),
            NameMode::Resolved(interner) => {
                let data = &self.program.functions[handle];
                let name = interner.get(data.name).unwrap_or(INVALID_IDENT);
                write!(&mut self.out, "@{}", name)
            }
        }
    }
}

#[derive(Clone, Copy)]
enum NameMode<'a> {
    Handles,
    Resolved(&'a IdentInterner),
}
