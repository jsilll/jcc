use source_file::SourceSpan;
use string_interner::DefaultSymbol;

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
    blocks: Vec<Block>,
}

impl FnDef {
    pub fn new(id: u32, span: SourceSpan) -> Self {
        Self {
            id,
            span,
            blocks: Vec::new(),
        }
    }

    pub fn get_block(&self, block_ref: BlockRef) -> &Block {
        &self.blocks[block_ref.0 as usize]
    }

    pub fn get_block_mut(&mut self, block_ref: BlockRef) -> &mut Block {
        &mut self.blocks[block_ref.0 as usize]
    }

    pub fn push_default_block(&mut self) -> BlockRef {
        self.push_block(Block::default())
    }

    pub fn push_block(&mut self, block: Block) -> BlockRef {
        self.blocks.push(block);
        BlockRef((self.blocks.len() - 1) as u32)
    }
}

impl std::fmt::Debug for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FnDef")
            .field("id", &self.id)
            .field("blocks", &self.blocks)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockRef(u32);

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub spans: Vec<SourceSpan>,
    pub label: Option<DefaultSymbol>,
}

impl std::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("label", &self.label)
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
    /// A `jmp` instruction.
    Jmp(BlockRef),
    /// A `mov` instruction.
    Mov { src: Operand, dst: Operand },
    /// A `cmp` instruction.
    Cmp { src: Operand, dst: Operand },
    /// A conditional set instruction.
    SetCC { cond_cond: CondCode, dst: Operand },
    /// A conditional jump instruction.
    JmpCC {
        cond_code: CondCode,
        target: BlockRef,
    },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondCode {
    /// The equal condition.
    Equal,
    /// The not equal condition.
    NotEqual,
    /// The less than condition.
    LessThan,
    /// The less than or equal condition.
    LessThanOrEqual,
    /// The greater than condition.
    GreaterThan,
    /// The greater than or equal condition.
    GreaterThanOrEqual,
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
    block: BlockRef,
    block_map: HashMap<crate::BlockRef, BlockRef>,
}

impl FnDefBuilder {
    fn new() -> Self {
        let mut fn_def = FnDef::default();
        let block = fn_def.push_default_block();
        Self {
            block,
            fn_def,
            block_map: HashMap::new(),
        }
    }

    fn append_to_block(&mut self, instr: Instr, span: SourceSpan) {
        let root = self.fn_def.get_block_mut(self.block);
        root.instrs.push(instr);
        root.spans.push(span);
    }

    fn get_or_create_block(&mut self, block: crate::BlockRef) -> BlockRef {
        *self.block_map.entry(block).or_insert_with(|| {
            self.fn_def.push_default_block()
        })
    }

    fn build(mut self, fn_def: &crate::FnDef) -> FnDef {
        self.fn_def.id = fn_def.id;
        self.fn_def.span = fn_def.span;
        self.append_to_block(Instr::Alloca(0), fn_def.span);
        fn_def.blocks.iter().for_each(|block| {
            // TODO: change the block we are writing to
            block.instrs.iter().zip(block.spans.iter()).for_each(|(instr, span)| {
                self.build_from_instr(instr, span);
            });
        });
        self.fn_def
    }

    fn build_from_instr(&mut self, instr: &crate::Instr, span: &SourceSpan) {
        match instr {
            crate::Instr::Return(value) => {
                let src = Self::build_from_value(*value);
                let dst = Operand::Reg(Reg::Rax);
                self.append_to_block(Instr::Mov { src, dst }, *span);
                self.append_to_block(Instr::Ret, *span);
            }
            crate::Instr::Jump(target) => {
                let target = self.get_or_create_block(*target);
                self.append_to_block(Instr::Jmp(target), *span);
            }
            crate::Instr::Copy { src, dst } => {
                let src = Self::build_from_value(*src);
                let dst = Self::build_from_value(*dst);
                self.append_to_block(Instr::Mov { src, dst }, *span);
            }
            crate::Instr::JumpIfZero { cond, target } => {
                let cond = Self::build_from_value(*cond);
                let target = self.get_or_create_block(*target);
                self.append_to_block(Instr::Cmp { src: Operand::Imm(0), dst: cond }, *span);
                self.append_to_block(Instr::JmpCC { cond_code: CondCode::Equal, target }, *span);
            }
            crate::Instr::JumpIfNotZero { cond, target } => {
                let cond = Self::build_from_value(*cond);
                let target = self.get_or_create_block(*target);
                self.append_to_block(Instr::Cmp { src: Operand::Imm(0), dst: cond }, *span);
                self.append_to_block(Instr::JmpCC { cond_code: CondCode::NotEqual, target }, *span);
            }
            crate::Instr::Unary { op, src, dst } => {
                let op = UnaryOp::from(*op);
                let src = Self::build_from_value(*src);
                let dst = Self::build_from_value(*dst);
                self.append_to_block(Instr::Mov { src, dst }, *span);
                self.append_to_block(Instr::Unary { op, src: dst }, *span);
            }
            crate::Instr::Binary { op, lhs, rhs, dst } => {
                let lhs = Self::build_from_value(*lhs);
                let rhs = Self::build_from_value(*rhs);
                let dst = Self::build_from_value(*dst);
                match op {
                    crate::BinaryOp::Div | crate::BinaryOp::Rem => {
                        self.append_to_block(
                            Instr::Mov {
                                src: lhs,
                                dst: Operand::Reg(Reg::Rax),
                            },
                            *span,
                        );
                        self.append_to_block(Instr::Cdq, *span);
                        self.append_to_block(Instr::Idiv(rhs), *span);
                        self.append_to_block(
                            Instr::Mov {
                                src: match op {
                                    crate::BinaryOp::Div => Operand::Reg(Reg::Rax),
                                    crate::BinaryOp::Rem => Operand::Reg(Reg::Rdx),
                                    _ => unreachable!(),
                                },
                                dst,
                            },
                            *span,
                        );
                    }
                    _ => {
                        if let Ok(op) = BinaryOp::try_from(*op) {
                            self.append_to_block(Instr::Mov { src: lhs, dst }, *span);
                            self.append_to_block(Instr::Binary { op, src: rhs, dst }, *span);
                        }
                    }
                }
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
        program.0.blocks.iter_mut().for_each(|block| {
            while idx < block.instrs.len() {
                self.fix_instr(block, &mut idx);
                idx += 1;
            }
        });
        if let Some(Instr::Alloca(size)) = program.0.blocks[0].instrs.first_mut() {
            *size = self.offset;
        }
    }

    fn fix_instr(&mut self, block: &mut Block, idx: &mut usize) {
        let instr = &mut block.instrs[*idx];
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
                    block.instrs.insert(
                        *idx,
                        Instr::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg10),
                        },
                    );
                    *idx += 1;
                }
            }
            Instr::Jmp(_) => todo!(),
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
                    block.instrs.insert(
                        *idx + 1,
                        Instr::Mov {
                            src: Operand::Reg(Reg::Rg10),
                            dst: tmp,
                        },
                    );
                    *idx += 1;
                }
            }
            Instr::Cmp { .. } => todo!(),
            Instr::SetCC { .. } => todo!(),
            Instr::JmpCC { .. } => todo!(),
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
                            block.instrs.insert(
                                *idx,
                                Instr::Mov {
                                    src: tmp,
                                    dst: Operand::Reg(Reg::Rg11),
                                },
                            );
                            *idx += 2;
                            block.instrs.insert(
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
                            block.instrs.insert(
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
                                block.instrs.insert(
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
            fn_def.blocks.iter().for_each(|block| {
                block
                    .instrs
                    .iter()
                    .for_each(|instr| emitter.emit_instr(instr));
            });
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
            Instr::Jmp(_) => todo!(),
            Instr::Mov { src, dst } => {
                let src = self.emit_operand(src);
                let dst = self.emit_operand(dst);
                self.writeln(&format!("movl {src}, {dst}"));
            }
            Instr::Cmp { .. } => todo!(),
            Instr::SetCC { .. } => todo!(),
            Instr::JmpCC { .. } => todo!(),
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
