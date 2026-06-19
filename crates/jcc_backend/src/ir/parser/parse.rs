use crate::{
    ir::{
        inst::{BinaryOp, Inst, UnaryOp},
        parser::{
            lex::{Lexer, LexerIssue},
            token::{Token, TokenKind},
        },
        term::Terminator,
        ty::Ty,
        Block, BlockData, Function, FunctionData, Global, GlobalData, Program, Value, ValueData,
    },
    Ident, IdentInterner,
};

use jcc_codemap::{file::SourceFile, span::Span, Diagnostic, IntoDiagnostic, Issue, Label};
use jcc_entity::EntityMap;

use std::{borrow::Cow, iter::Peekable, str::FromStr};

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'ctx> {
    /// The source file being parsed.
    file: &'ctx SourceFile,
    /// The interner for identifier interning.
    interner: &'ctx mut IdentInterner,
    /// The result of the parsing process, containing the program and issues.
    result: ParserResult,
    /// The lexer used to tokenize the input source file.
    lexer: Peekable<Lexer<'ctx>>,
    /// A stack to hold argument values during parsing of function calls.
    args_stack: Vec<Value>,
    /// A stack to hold index values during parsing of gep instructions.
    indices_stack: Vec<Value>,
    /// A stack to hold cases their during parsing of switch statements.
    cases_stack: Vec<(u64, Block)>,
    /// A mapping from identifier names to their corresponding blocks in the program.
    blocks: EntityMap<Ident, Block>,
    /// A mapping from identifier names to their corresponding values in the program.
    values: EntityMap<Ident, Value>,
    /// A mapping from identifier names to their corresponding globals in the program.
    globals: EntityMap<Ident, Global>,
    /// A mapping from identifier names to their corresponding functions in the program.
    functions: EntityMap<Ident, Function>,
}

impl<'ctx> Parser<'ctx> {
    pub fn new(file: &'ctx SourceFile, interner: &'ctx mut IdentInterner) -> Self {
        Self {
            file,
            interner,
            args_stack: Vec::new(),
            cases_stack: Vec::new(),
            indices_stack: Vec::new(),
            values: EntityMap::default(),
            blocks: EntityMap::default(),
            globals: EntityMap::default(),
            functions: EntityMap::default(),
            result: ParserResult::default(),
            lexer: Lexer::new(file).peekable(),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        while self.peek().is_some_and(|t| t.kind == TokenKind::AtIdent) {
            let Some(data) = self.parse_global() else {
                continue;
            };
            let name = data.name;
            let global = self.result.program.globals.push(data);
            self.globals.insert(name, global);
        }
        while self.peek().is_some_and(|t| t.kind == TokenKind::Define) {
            self.parse_function();
        }
        if self.peek().is_some() {
            self.error_expected(TokenKind::Define);
        }
        self.result
    }

    // ---------------------------------------------------------------------------
    // Globals
    // ---------------------------------------------------------------------------

    fn parse_global(&mut self) -> Option<GlobalData> {
        let (name, span) = self.parse_at_ident()?;
        self.expect(TokenKind::Assign);
        self.expect(TokenKind::Global)?;
        let ty = self.parse_ty()?;
        let init = self.parse_global_init(ty);
        Some(GlobalData {
            ty,
            span,
            name,
            init,
            is_global: true,
        })
    }

    fn parse_global_init(&mut self, ty: Ty) -> Option<u64> {
        let neg = self.eat(TokenKind::Minus);
        match self.peek().map(|t| t.kind) {
            Some(TokenKind::ZeroInitializer) if !neg => {
                self.advance();
                None
            }
            Some(TokenKind::Float) => {
                let span = self.advance()?.span;
                let f: f64 = self.parse_lit(span)?;
                let f = if neg { -f } else { f };
                Some(match ty {
                    Ty::F32 => (f as f32).to_bits() as u64,
                    _ => f.to_bits(),
                })
            }
            Some(TokenKind::Integer) => {
                let span = self.advance()?.span;
                let i: u64 = self.parse_lit(span)?;
                let i = if neg {
                    (i as i64).wrapping_neg() as u64
                } else {
                    i
                };
                Some(match ty {
                    Ty::F64 => (i as f64).to_bits(),
                    Ty::F32 => (i as f32).to_bits() as u64,
                    _ => i,
                })
            }
            _ => {
                self.error_expected(TokenKind::Integer);
                None
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Functions
    // ---------------------------------------------------------------------------

    fn parse_function(&mut self) {
        self.advance();
        self.blocks.clear();
        self.values.clear();

        let Some((name, span)) = self.parse_at_ident() else {
            return;
        };

        let f = self.get_or_create_function(name, span);
        self.result.program.functions[f].span = span;
        self.expect(TokenKind::LBrace);

        while let Some(block) = self
            .peek()
            .filter(|t| t.kind != TokenKind::RBrace)
            .and_then(|_| self.parse_block())
        {
            self.result.program.functions[f].entry.get_or_insert(block);
        }

        self.expect(TokenKind::RBrace);
    }

    // ---------------------------------------------------------------------------
    // Blocks
    // ---------------------------------------------------------------------------

    fn parse_block(&mut self) -> Option<Block> {
        let span = self.expect(TokenKind::BlockRef)?.span;
        let ident = self.intern_span(span);
        let block = self.get_or_create_block(ident, span);
        self.expect(TokenKind::Colon);

        while let Some(kind) = self.peek().map(|t| t.kind) {
            match kind {
                TokenKind::BlockRef | TokenKind::RBrace => break,
                TokenKind::Br | TokenKind::Ret | TokenKind::Unreachable | TokenKind::Switch => {
                    self.result.program.blocks[block].term = self.parse_term();
                    break;
                }
                _ => {
                    self.parse_inst(block);
                }
            }
        }

        Some(block)
    }

    fn parse_term(&mut self) -> Terminator {
        match self.peek().map(|t| t.kind) {
            Some(TokenKind::Unreachable) => {
                self.advance();
                Terminator::Unreachable
            }
            Some(TokenKind::Ret) => {
                self.advance();
                match self.peek().map(|t| t.kind) {
                    Some(TokenKind::ValueRef) => {
                        let v = self.parse_value_ref();
                        Terminator::Ret(Some(v))
                    }
                    Some(TokenKind::Void) => {
                        self.advance();
                        Terminator::Ret(None)
                    }
                    _ => Terminator::Ret(None),
                }
            }
            Some(TokenKind::Br) => {
                self.advance();
                match self.peek().map(|t| t.kind) {
                    Some(TokenKind::BlockRef) => {
                        let v = self.parse_block_ref();
                        Terminator::Br(v)
                    }
                    Some(TokenKind::I1) => {
                        self.advance();
                        let cond = self.parse_value_ref();
                        self.expect(TokenKind::Comma);
                        let then_block = self.parse_block_ref();
                        self.expect(TokenKind::Comma);
                        let else_block = self.parse_block_ref();
                        Terminator::CondBr {
                            cond,
                            then_block,
                            else_block,
                        }
                    }
                    _ => {
                        self.error_expected(TokenKind::BlockRef);
                        Terminator::Unreachable
                    }
                }
            }
            Some(TokenKind::Switch) => {
                self.advance();
                let value = self.parse_value_ref();
                self.expect(TokenKind::LBrack);
                self.expect(TokenKind::Default);
                self.expect(TokenKind::Colon);
                let default = self.parse_block_ref();
                self.cases_stack.clear();

                while self.eat(TokenKind::Comma) {
                    match self
                        .expect(TokenKind::Integer)
                        .and_then(|t| self.parse_lit::<u64>(t.span))
                    {
                        None => break,
                        Some(value) => {
                            self.expect(TokenKind::Colon);
                            let block = self.parse_block_ref();
                            self.cases_stack.push((value, block));
                        }
                    }
                }

                self.expect(TokenKind::RBrack);
                let cases = self.cases_stack.drain(..).collect();

                Terminator::Switch {
                    value,
                    cases,
                    default,
                }
            }
            _ => {
                self.error_expected(TokenKind::Unreachable);
                Terminator::Unreachable
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Instructions
    // ---------------------------------------------------------------------------

    fn parse_inst(&mut self, block: Block) {
        let value = self.parse_value_def();
        let span = self.peek_span();
        if let (Some(value), Some(inst)) = (value, self.parse_inst_body()) {
            let data = &mut self.result.program.values[value];
            let idx = self.result.program.blocks[block].insts.len();
            data.inst = inst;
            data.span = span;
            data.block = block;
            data.idx = idx as u32;
            self.result.program.blocks[block].insts.push(value);
        }
    }

    fn parse_inst_body(&mut self) -> Option<Inst> {
        let kind = self.peek()?.kind;
        match BinaryOp::try_from(kind) {
            Ok(op) => {
                self.advance();
                let ty = self.parse_ty()?;
                let (lhs, rhs) = self.parse_value_pair();
                Some(Inst::Binary { op, ty, lhs, rhs })
            }
            Err(_) => match UnaryOp::try_from(kind) {
                Ok(op) => {
                    self.advance();
                    let ty = self.parse_ty()?;
                    let operand = self.parse_value_ref();
                    Some(Inst::Unary { op, ty, operand })
                }
                Err(_) => match kind {
                    TokenKind::Noop => {
                        self.advance();
                        Some(Inst::Noop)
                    }
                    TokenKind::Null => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        Some(Inst::ConstNull(ty))
                    }
                    TokenKind::Phi => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        Some(Inst::Phi(ty))
                    }
                    TokenKind::Alloca => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        let align = self.parse_align()?;
                        Some(Inst::Alloca { ty, align })
                    }
                    TokenKind::GlobalAddr => {
                        self.advance();
                        let (ident, span) = self.parse_at_ident()?;
                        let g = self.get_or_create_global(ident, span);
                        Some(Inst::GlobalAddr(g))
                    }
                    TokenKind::Icmp => {
                        self.advance();
                        let pred = self.parse_predicate(TokenKind::Eq)?;
                        let (lhs, rhs) = self.parse_value_pair();
                        Some(Inst::ICmp { lhs, rhs, pred })
                    }
                    TokenKind::Fcmp => {
                        self.advance();
                        let pred = self.parse_predicate(TokenKind::False)?;
                        let (lhs, rhs) = self.parse_value_pair();
                        Some(Inst::FCmp { lhs, rhs, pred })
                    }
                    TokenKind::Const => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        let span = self.expect(TokenKind::Integer)?.span;
                        let value = self.parse_lit(span)?;
                        Some(Inst::Const { ty, value })
                    }
                    TokenKind::Upsilon => {
                        self.advance();
                        let value = self.parse_value_ref();
                        self.expect(TokenKind::Arrow);
                        let phi = self.parse_value_ref();
                        Some(Inst::Upsilon { phi, value })
                    }
                    TokenKind::Store => {
                        self.advance();
                        let ptr = self.parse_value_ref();
                        let value = self.parse_value_ref();
                        let align = self.parse_align()?;
                        Some(Inst::Store { ptr, align, value })
                    }
                    TokenKind::Load => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        self.expect(TokenKind::Comma);
                        self.expect(TokenKind::Ptr);
                        let ptr = self.parse_value_ref();
                        let align = self.parse_align()?;
                        Some(Inst::Load { ty, ptr, align })
                    }
                    TokenKind::ICall => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        let ptr = self.parse_value_ref();
                        let args = self.parse_call_args();
                        Some(Inst::IndirectCall { ty, ptr, args })
                    }
                    TokenKind::Call => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        let (ident, span) = self.parse_at_ident()?;
                        let func = self.get_or_create_function(ident, span);
                        let args = self.parse_call_args();
                        Some(Inst::Call { ty, func, args })
                    }
                    TokenKind::Param => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        let span = self.expect(TokenKind::ParamIndex)?.span;
                        let text = self.file.slice(span).unwrap_or("");
                        let index = text
                            .strip_prefix("#")
                            .and_then(|s| s.parse().ok())
                            .unwrap_or_else(|| {
                                self.error_at(ParserIssue::InvalidLiteral, span);
                                0
                            });
                        Some(Inst::Param { ty, index })
                    }
                    TokenKind::Gep => {
                        self.advance();
                        let ty = self.parse_ty()?;
                        self.expect(TokenKind::Comma);
                        self.expect(TokenKind::Ptr);
                        let ptr = self.parse_value_ref();

                        self.indices_stack.clear();
                        while self.eat(TokenKind::Comma) {
                            let val = self.parse_value_ref();
                            self.indices_stack.push(val);
                        }

                        let indices = self.indices_stack.drain(..).collect();
                        Some(Inst::GetElementPtr { ty, ptr, indices })
                    }
                    TokenKind::Select => {
                        self.advance();
                        self.expect(TokenKind::I1);
                        let cond = self.parse_value_ref();
                        self.expect(TokenKind::Comma);
                        let ty = self.parse_ty()?;
                        let then_val = self.parse_value_ref();
                        self.expect(TokenKind::Comma);
                        self.parse_ty();
                        let else_val = self.parse_value_ref();
                        Some(Inst::Select {
                            ty,
                            cond,
                            then_val,
                            else_val,
                        })
                    }
                    TokenKind::ZExt
                    | TokenKind::SExt
                    | TokenKind::FExt
                    | TokenKind::Trunc
                    | TokenKind::FTrunc
                    | TokenKind::FpToSi
                    | TokenKind::FpToUi
                    | TokenKind::SiToFp
                    | TokenKind::UiToFp
                    | TokenKind::Bitcast
                    | TokenKind::IntToPtr
                    | TokenKind::PtrToInt => {
                        let kind = self.advance()?.kind;
                        let value = self.parse_value_ref();
                        self.expect(TokenKind::To);
                        let to = self.parse_ty()?;
                        Some(match kind {
                            TokenKind::ZExt => Inst::ZExt { to, value },
                            TokenKind::SExt => Inst::SExt { to, value },
                            TokenKind::FExt => Inst::FExt { to, value },
                            TokenKind::Trunc => Inst::Trunc { to, value },
                            TokenKind::FTrunc => Inst::FTrunc { to, value },
                            TokenKind::FpToSi => Inst::FpToSi { to, value },
                            TokenKind::FpToUi => Inst::FpToUi { to, value },
                            TokenKind::SiToFp => Inst::SiToFp { to, value },
                            TokenKind::UiToFp => Inst::UiToFp { to, value },
                            TokenKind::Bitcast => Inst::Bitcast { to, value },
                            TokenKind::IntToPtr => Inst::IntToPtr { to, value },
                            TokenKind::PtrToInt => Inst::PtrToInt { to, value },
                            _ => unreachable!("parse_conversion called for non-conversion token"),
                        })
                    }
                    _ => {
                        self.error_expected(TokenKind::Noop);
                        None
                    }
                },
            },
        }
    }

    // ---------------------------------------------------------------------------
    // Parsing Helpers
    // ---------------------------------------------------------------------------

    #[inline]
    fn parse_ty(&mut self) -> Option<Ty> {
        self.parse_predicate(TokenKind::Void)
    }

    fn parse_u32(&mut self) -> Option<u32> {
        let t = self.expect(TokenKind::Integer)?;
        let n: u64 = self.parse_lit(t.span)?;
        if n > u32::MAX as u64 {
            self.error_expected(TokenKind::Integer);
            None
        } else {
            Some(n as u32)
        }
    }

    #[inline]
    fn parse_align(&mut self) -> Option<u32> {
        self.expect(TokenKind::Comma);
        self.expect(TokenKind::Align);
        self.parse_u32()
    }

    #[inline]
    fn parse_value_pair(&mut self) -> (Value, Value) {
        let lhs = self.parse_value_ref();
        self.expect(TokenKind::Comma);
        let rhs = self.parse_value_ref();
        (lhs, rhs)
    }

    #[inline]
    fn parse_at_ident(&mut self) -> Option<(Ident, Span)> {
        let span = self.expect(TokenKind::AtIdent)?.span;
        let ident = self.intern_span(span.shrink_left(1).unwrap());
        Some((ident, span))
    }

    #[inline]
    fn parse_value_ref(&mut self) -> Value {
        self.expect(TokenKind::ValueRef)
            .map(|t| {
                let id = self.intern_span(t.span);
                self.get_or_create_value(id, t.span)
            })
            .unwrap_or(Value::from_u32(0))
    }

    #[inline]
    fn parse_value_def(&mut self) -> Option<Value> {
        let t = self.peek().filter(|t| t.kind == TokenKind::ValueRef)?;
        self.advance();
        self.expect(TokenKind::Assign);
        let name = self.intern_span(t.span);
        Some(self.get_or_create_value(name, t.span))
    }

    #[inline]
    fn parse_block_ref(&mut self) -> Block {
        self.expect(TokenKind::BlockRef)
            .map(|t| {
                let id = self.intern_span(t.span);
                self.get_or_create_block(id, t.span)
            })
            .unwrap_or(Block::from_u32(0))
    }

    fn parse_call_args(&mut self) -> Vec<Value> {
        self.expect(TokenKind::LParen);
        self.args_stack.clear();
        while self.peek().is_some_and(|t| t.kind != TokenKind::RParen) {
            let v = self.parse_value_ref();
            self.args_stack.push(v);
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::RParen);
        self.args_stack.drain(..).collect()
    }

    #[inline]
    fn parse_lit<T: FromStr>(&mut self, span: Span) -> Option<T> {
        match self.file.slice(span).unwrap_or("").parse() {
            Ok(n) => Some(n),
            Err(_) => {
                self.error_at(ParserIssue::InvalidLiteral, span);
                None
            }
        }
    }

    #[inline]
    fn parse_predicate<P>(&mut self, expected: TokenKind) -> Option<P>
    where
        P: TryFrom<TokenKind>,
    {
        match self.peek().and_then(|t| P::try_from(t.kind).ok()) {
            Some(pred) => {
                self.advance();
                Some(pred)
            }
            None => {
                self.error_expected(expected);
                None
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Helpers
    // ---------------------------------------------------------------------------

    #[inline]
    fn peek_span(&mut self) -> Span {
        self.peek()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::single(self.file.last()))
    }

    #[inline]
    fn intern_span(&mut self, span: Span) -> Ident {
        self.interner
            .intern(self.file.slice(span).expect("expected span to be valid"))
    }

    #[inline]
    fn error_expected(&mut self, expected: TokenKind) {
        let span = self.peek_span();
        self.error_at(ParserIssue::UnexpectedToken(expected), span);
    }

    #[inline]
    fn error_at(&mut self, kind: ParserIssue, span: Span) {
        self.result.parser_issues.push(Issue::new(kind, span));
    }

    fn peek(&mut self) -> Option<Token> {
        loop {
            match self.lexer.peek()? {
                Ok(t) => return Some(*t),
                Err(issue) => {
                    self.result.lexer_issues.push(issue.clone());
                    self.lexer.next();
                }
            }
        }
    }

    fn advance(&mut self) -> Option<Token> {
        loop {
            match self.lexer.next()? {
                Ok(t) => return Some(t),
                Err(issue) => {
                    self.result.lexer_issues.push(issue.clone());
                }
            }
        }
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.peek().is_some_and(|t| t.kind == kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Option<Token> {
        match self.peek() {
            Some(token) if token.kind == expected => self.advance(),
            Some(_) => {
                self.error_expected(expected);
                None
            }
            None => {
                let span = self.peek_span();
                self.error_at(ParserIssue::UnexpectedEof, span);
                None
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Symbol Tables
    // ---------------------------------------------------------------------------

    fn get_or_create_block(&mut self, name: Ident, span: Span) -> Block {
        *self.blocks.entry(name).or_insert_with(|| {
            let data = BlockData::new(name, span);
            self.result.program.blocks.push(data)
        })
    }

    fn get_or_create_value(&mut self, name: Ident, span: Span) -> Value {
        *self.values.entry(name).or_insert_with(|| {
            self.result.program.values.push(ValueData {
                span,
                idx: 0,
                inst: Inst::noop(),
                block: Block::from_u32(0),
            })
        })
    }

    fn get_or_create_global(&mut self, name: Ident, span: Span) -> Global {
        *self.globals.entry(name).or_insert_with(|| {
            self.result.program.globals.push(GlobalData {
                span,
                name,
                init: None,
                ty: Ty::Void,
                is_global: true,
            })
        })
    }

    fn get_or_create_function(&mut self, name: Ident, span: Span) -> Function {
        *self.functions.entry(name).or_insert_with(|| {
            self.result
                .program
                .functions
                .push(FunctionData::new(name, true, span))
        })
    }
}

// ---------------------------------------------------------------------------
// ParserResult
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct ParserResult {
    pub program: Program,
    pub lexer_issues: Vec<Issue<LexerIssue>>,
    pub parser_issues: Vec<Issue<ParserIssue>>,
}

// ---------------------------------------------------------------------------
// ParserIssue
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum ParserIssue {
    UnexpectedEof,
    InvalidLiteral,
    UnexpectedToken(TokenKind),
}

impl IntoDiagnostic for ParserIssue {
    fn into_diagnostic(self, span: Span) -> Diagnostic {
        let (msg, note): (Cow<str>, Cow<str>) = match self {
            ParserIssue::UnexpectedEof => (
                "unexpected end of file".into(),
                "the IR text ended before it was complete".into(),
            ),
            ParserIssue::InvalidLiteral => (
                "invalid literal".into(),
                "this token could not be parsed as a numeric literal".into(),
            ),
            ParserIssue::UnexpectedToken(expected) => (
                "unexpected token".into(),
                format!("expected {expected}").into(),
            ),
        };
        Diagnostic::error()
            .with_label(Label::primary(span).with_message(msg))
            .with_note(note)
    }
}
