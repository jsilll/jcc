use source_file::SourceSpan;
use string_interner::{DefaultStringInterner, DefaultSymbol};

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

impl std::fmt::Display for BlockRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub spans: Vec<SourceSpan>,
    pub label: Option<DefaultSymbol>,
}

impl Block {
    pub fn with_label(label: DefaultSymbol) -> Self {
        Block {
            label: Some(label),
            ..Default::default()
        }
    }
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
    Cmp { lhs: Operand, rhs: Operand },
    /// A conditional set instruction.
    SetCC { cond_code: CondCode, dst: Operand },
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

impl TryFrom<crate::UnaryOp> for UnaryOp {
    type Error = ();

    fn try_from(op: crate::UnaryOp) -> Result<Self, Self::Error> {
        match op {
            crate::UnaryOp::Neg => Ok(Self::Neg),
            crate::UnaryOp::BitNot => Ok(Self::Not),
            _ => Err(()),
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
            _ => Err(()),
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
    LessEqual,
    /// The greater than condition.
    GreaterThan,
    /// The greater than or equal condition.
    GreaterEqual,
}

impl TryFrom<crate::BinaryOp> for CondCode {
    type Error = ();

    fn try_from(op: crate::BinaryOp) -> Result<Self, Self::Error> {
        match op {
            crate::BinaryOp::Equal => Ok(Self::Equal),
            crate::BinaryOp::NotEqual => Ok(Self::NotEqual),
            crate::BinaryOp::LessThan => Ok(Self::LessThan),
            crate::BinaryOp::LessEqual => Ok(Self::LessEqual),
            crate::BinaryOp::GreaterThan => Ok(Self::GreaterThan),
            crate::BinaryOp::GreaterEqual => Ok(Self::GreaterEqual),
            _ => Err(()),
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
        Program(AMD64FnDefBuilder::new().build(&self.tacky.0))
    }
}

// ---------------------------------------------------------------------------
// AMD64FnDefBuilder
// ---------------------------------------------------------------------------

struct AMD64FnDefBuilder {
    fn_def: FnDef,
    block: BlockRef,
    block_map: HashMap<crate::BlockRef, BlockRef>,
}

impl AMD64FnDefBuilder {
    fn new() -> Self {
        let mut fn_def = FnDef::default();
        let block = fn_def.push_block(Block::default());
        Self {
            block,
            fn_def,
            block_map: HashMap::new(),
        }
    }

    fn build(mut self, fn_def: &crate::FnDef) -> FnDef {
        self.fn_def.id = fn_def.id;
        self.fn_def.span = fn_def.span;
        self.append_to_block(Instr::Alloca(0), fn_def.span);
        if let Some(block) = fn_def.blocks_iter().next() {
            self.block_map.insert(block, self.block);
            fn_def
                .get_block(block)
                .instrs
                .iter()
                .zip(fn_def.get_block(block).spans.iter())
                .for_each(|(instr, span)| {
                    self.build_from_instr(instr, span);
                });
        }
        fn_def.blocks_iter().skip(1).for_each(|block| {
            self.block = self.get_or_create_block(block);
            fn_def
                .get_block(block)
                .instrs
                .iter()
                .zip(fn_def.get_block(block).spans.iter())
                .for_each(|(instr, span)| {
                    self.build_from_instr(instr, span);
                });
        });
        self.block_map
            .into_iter()
            .for_each(|(tacky_block, block_ref)| {
                let block = self.fn_def.get_block_mut(block_ref);
                if block.label.is_none() {
                    block.label = fn_def.get_block(tacky_block).label;
                }
            });
        self.fn_def
    }

    fn append_to_block(&mut self, instr: Instr, span: SourceSpan) {
        let block = self.fn_def.get_block_mut(self.block);
        block.instrs.push(instr);
        block.spans.push(span);
    }

    fn get_or_create_block(&mut self, block: crate::BlockRef) -> BlockRef {
        *self
            .block_map
            .entry(block)
            .or_insert_with(|| self.fn_def.push_block(Block::default()))
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
                self.append_to_block(
                    Instr::Cmp {
                        lhs: Operand::Imm(0),
                        rhs: cond,
                    },
                    *span,
                );
                self.append_to_block(
                    Instr::JmpCC {
                        cond_code: CondCode::Equal,
                        target,
                    },
                    *span,
                );
            }
            crate::Instr::JumpIfNotZero { cond, target } => {
                let cond = Self::build_from_value(*cond);
                let target = self.get_or_create_block(*target);
                self.append_to_block(
                    Instr::Cmp {
                        lhs: Operand::Imm(0),
                        rhs: cond,
                    },
                    *span,
                );
                self.append_to_block(
                    Instr::JmpCC {
                        cond_code: CondCode::NotEqual,
                        target,
                    },
                    *span,
                );
            }
            crate::Instr::Unary { op, src, dst } => match op {
                crate::UnaryOp::Not => {
                    let src = Self::build_from_value(*src);
                    let dst = Self::build_from_value(*dst);
                    self.append_to_block(
                        Instr::Cmp {
                            lhs: Operand::Imm(0),
                            rhs: src,
                        },
                        *span,
                    );
                    self.append_to_block(
                        Instr::Mov {
                            src: Operand::Imm(0),
                            dst,
                        },
                        *span,
                    );
                    self.append_to_block(
                        Instr::SetCC {
                            cond_code: CondCode::Equal,
                            dst,
                        },
                        *span,
                    );
                }
                _ => {
                    let op = UnaryOp::try_from(*op).expect("unexpected unary operator");
                    let src = Self::build_from_value(*src);
                    let dst = Self::build_from_value(*dst);
                    self.append_to_block(Instr::Mov { src, dst }, *span);
                    self.append_to_block(Instr::Unary { op, src: dst }, *span);
                }
            },
            crate::Instr::Binary { op, lhs, rhs, dst } => {
                let lhs = Self::build_from_value(*lhs);
                let rhs = Self::build_from_value(*rhs);
                let dst = Self::build_from_value(*dst);
                match op {
                    crate::BinaryOp::Equal
                    | crate::BinaryOp::NotEqual
                    | crate::BinaryOp::LessThan
                    | crate::BinaryOp::LessEqual
                    | crate::BinaryOp::GreaterThan
                    | crate::BinaryOp::GreaterEqual => {
                        self.append_to_block(Instr::Cmp { lhs: rhs, rhs: lhs }, *span);
                        self.append_to_block(
                            Instr::Mov {
                                src: Operand::Imm(0),
                                dst,
                            },
                            *span,
                        );
                        self.append_to_block(
                            Instr::SetCC {
                                cond_code: CondCode::try_from(*op)
                                    .expect("unexpected binary operator"),
                                dst,
                            },
                            *span,
                        );
                    }
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
                        self.append_to_block(Instr::Mov { src: lhs, dst }, *span);
                        self.append_to_block(
                            Instr::Binary {
                                op: BinaryOp::try_from(*op).expect("unexpected binary operator"),
                                src: rhs,
                                dst,
                            },
                            *span,
                        );
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
            idx = 0;
        });
        if let Some(Instr::Alloca(size)) = program.0.blocks[0].instrs.first_mut() {
            *size = self.offset;
        }
    }

    fn fix_instr(&mut self, block: &mut Block, idx: &mut usize) {
        let instr = &mut block.instrs[*idx];
        match instr {
            Instr::Ret | Instr::Cdq | Instr::Jmp(_) | Instr::Alloca(_) | Instr::JmpCC { .. } => {}
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
            Instr::Cmp { lhs, rhs } => {
                self.fix_operand(lhs);
                self.fix_operand(rhs);
                if matches!(rhs, Operand::Imm(_)) {
                    // NOTE
                    //
                    // The `cmp` instruction doesn't support
                    // an immediate value as the second operand,
                    // so we need to move the immediate value
                    // to a register.
                    //
                    // cmpl -4(%rbp), $3
                    //
                    // movl $3, %r11d
                    // cmpl -4(%rbp), %r11d
                    let tmp = *rhs;
                    *rhs = Operand::Reg(Reg::Rg11);
                    block.instrs.insert(
                        *idx,
                        Instr::Mov {
                            src: tmp,
                            dst: Operand::Reg(Reg::Rg11),
                        },
                    );
                    *idx += 1;
                } else if matches!(lhs, Operand::Stack(_)) && matches!(rhs, Operand::Stack(_)) {
                    // NOTE
                    //
                    // The `cmp` instruction doesn't support
                    // comparing memory to memory, so we need to
                    // use a register as a temporary.
                    //
                    // cmpl -4(%rbp), -8(%rbp)
                    //
                    // movl -4(%rbp), %r10d
                    // cmpl %r10d, -8(%rbp)
                    let tmp = *lhs;
                    *lhs = Operand::Reg(Reg::Rg10);
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
            Instr::SetCC { dst, .. } => {
                self.fix_operand(dst);
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
                        if matches!(src, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) {
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
    interner: &'a DefaultStringInterner,
}

impl<'a> AMD64Emitter<'a> {
    pub fn new(program: &'a Program, interner: &'a DefaultStringInterner) -> Self {
        Self {
            program,
            interner,
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
        // All user-defined labels are prefixed with `_` on macOS
        let name = "main";
        self.with_indent(|emitter| emitter.writeln(&format!(".globl {name}")));
        self.writeln(&format!("{name}:"));
        self.with_indent(|emitter| {
            emitter.writeln("pushq %rbp");
            emitter.writeln("movq %rsp, %rbp");
        });
        fn_def.blocks.iter().enumerate().for_each(|(idx, block)| {
            if let Some(label) = block.label {
                // NOTE: The local label prefix on Linux is `.L` and on macOS is `L`
                let label = self.interner.resolve(label).expect("invalid label");
                self.writeln(&format!(".L{label}{idx}:"));
            }
            self.with_indent(|emitter| {
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
                let oper = self.emit_operand_32(oper);
                self.writeln(&format!("idivl {oper}"));
            }
            Instr::Jmp(target) => {
                let block = self.program.0.get_block(*target);
                match block.label {
                    Some(label) => {
                        let label = self.interner.resolve(label).expect("invalid label");
                        self.writeln(&format!("jmp .L{label}{target}"));
                    }
                    None => todo!("cannot emit jmp to block without label"),
                }
            }
            Instr::Mov { src, dst } => {
                let src = self.emit_operand_32(src);
                let dst = self.emit_operand_32(dst);
                self.writeln(&format!("movl {src}, {dst}"));
            }
            Instr::Cmp { lhs, rhs } => {
                let lhs = self.emit_operand_32(lhs);
                let rhs = self.emit_operand_32(rhs);
                self.writeln(&format!("cmpl {lhs}, {rhs}"));
            }
            Instr::SetCC { cond_code, dst } => {
                let dst = self.emit_operand_8(dst);
                let cond_code = self.emit_cond_code(*cond_code);
                self.writeln(&format!("set{cond_code} {dst}"));
            }
            Instr::JmpCC { cond_code, target } => {
                let block = self.program.0.get_block(*target);
                match block.label {
                    Some(label) => {
                        let label = self.interner.resolve(label).expect("invalid label");
                        let cond_code = self.emit_cond_code(*cond_code);
                        self.writeln(&format!("j{cond_code} .L{label}{target}"));
                    }
                    None => todo!("emit jmpcc to block without label"),
                }
            }
            Instr::Unary { op, src } => {
                let src = self.emit_operand_32(src);
                match op {
                    UnaryOp::Not => self.writeln(&format!("notl {src}")),
                    UnaryOp::Neg => self.writeln(&format!("negl {src}")),
                };
            }
            Instr::Binary { op, src, dst } => {
                let src = self.emit_operand_32(src);
                let dst = self.emit_operand_32(dst);
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

    fn emit_operand_8(&mut self, oper: &Operand) -> String {
        match oper {
            Operand::Imm(value) => format!("${}", value),
            Operand::Reg(reg) => match reg {
                Reg::Rax => "%al".to_string(),
                Reg::Rcx => "%cl".to_string(),
                Reg::Rdx => "%dl".to_string(),
                Reg::Rg10 => "%r10b".to_string(),
                Reg::Rg11 => "%r11b".to_string(),
            },
            Operand::Stack(offset) => format!("-{}(%rbp)", offset),
            Operand::Pseudo(id) => format!("pseudo({})", id),
        }
    }

    fn emit_operand_32(&mut self, oper: &Operand) -> String {
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
