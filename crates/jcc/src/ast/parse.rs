use crate::{
    ast::{
        ty::{Ty, TyCtx},
        Ast, BinaryOp, Block, BlockItem, Constant, Decl, DeclData, DeclKind, DeclList, Expr,
        ExprData, ExprKind, ExprList, ForInit, Stmt, StmtData, StmtKind, StorageClass, Symbol,
        UnaryOp,
    },
    token::{
        lex::{Lexer, LexerDiagnostic},
        Token, TokenKind,
    },
};

use jcc_ssa::{
    codemap::{
        file::{FileId, SourceFile},
        span::Span,
        Diagnostic, Label,
    },
    Ident, IdentInterner,
};

use std::iter::Peekable;

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a, 'ctx> {
    /// The source file being parsed.
    file: &'a SourceFile,
    /// The type context used for creating and interning types.
    tys: &'ctx TyCtx<'ctx>,
    /// The lexer that provides a stream of tokens.
    lexer: Peekable<Lexer<'a>>,
    /// The interner for identifier interning.
    interner: &'a mut IdentInterner,
    /// A temporary buffer for collecting type specifiers.
    specifiers: Vec<TokenKind>,
    /// The result of the parsing process, containing the AST and diagnostics.
    result: ParserResult<'ctx>,
    /// A stack used for building expression lists, like function arguments.
    expr_stack: Vec<Expr>,
    /// A stack used for building declaration lists, like function parameters.
    decl_stack: Vec<Decl>,
    /// A stack used for building block item lists, like function bodies.
    items_stack: Vec<BlockItem>,
}

impl<'a, 'ctx> Parser<'a, 'ctx> {
    pub fn new(
        lexer: Lexer<'a>,
        file: &'a SourceFile,
        tys: &'ctx TyCtx<'ctx>,
        interner: &'a mut IdentInterner,
    ) -> Self {
        Self {
            tys,
            file,
            interner,
            lexer: lexer.peekable(),
            specifiers: Vec::with_capacity(16),
            expr_stack: Vec::with_capacity(16),
            decl_stack: Vec::with_capacity(16),
            items_stack: Vec::with_capacity(16),
            result: ParserResult::new(file.id()),
        }
    }

    pub fn parse(mut self) -> ParserResult<'ctx> {
        while self.peek().is_some() {
            match self.parse_decl() {
                Some(decl) => self.result.ast.root.push(decl),
                None => self.sync(TokenKind::Semi, TokenKind::KwInt),
            }
        }
        self.result
    }

    fn collect_types(&mut self) -> Span {
        let start_span = self.peek_span();
        let mut end_span = None;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::KwVoid
                | TokenKind::KwInt
                | TokenKind::KwLong
                | TokenKind::KwDouble
                | TokenKind::KwSigned
                | TokenKind::KwUnsigned => {
                    self.specifiers.push(token.kind);
                    end_span = Some(token.span);
                }
                _ => break,
            }
            self.next();
        }
        end_span
            .map(|s| start_span.merge(s))
            .unwrap_or(Span::empty(start_span.start()))
    }

    fn parse_type(&mut self, span: Span) -> Ty<'ctx> {
        match self.specifiers.as_slice() {
            [] => {
                self.result.parser_diagnostics.push(ParserDiagnostic {
                    span,
                    file: self.file.id(),
                    kind: ParserDiagnosticKind::MissingTypeSpecifier,
                });
                self.tys.void_ty
            }
            [TokenKind::KwVoid] => {
                self.specifiers.clear();
                self.tys.void_ty
            }
            [TokenKind::KwDouble] => {
                self.specifiers.clear();
                self.tys.double_ty
            }
            specs if specs.contains(&TokenKind::KwDouble) || specs.contains(&TokenKind::KwVoid) => {
                self.result.parser_diagnostics.push(ParserDiagnostic {
                    span,
                    file: self.file.id(),
                    kind: ParserDiagnosticKind::InvalidTypeSpecifier,
                });
                self.specifiers.clear();
                self.tys.void_ty
            }
            _ => {
                let mut has_int = false;
                let mut has_long = false;
                let mut has_signed = false;
                let mut has_unsigned = false;
                for token in &self.specifiers {
                    match token {
                        TokenKind::KwInt if !has_int => has_int = true,
                        TokenKind::KwLong if !has_long => has_long = true,
                        TokenKind::KwSigned if !has_signed => has_signed = true,
                        TokenKind::KwUnsigned if !has_unsigned => has_unsigned = true,
                        TokenKind::KwInt
                        | TokenKind::KwLong
                        | TokenKind::KwSigned
                        | TokenKind::KwUnsigned => {
                            self.result.parser_diagnostics.push(ParserDiagnostic {
                                span,
                                file: self.file.id(),
                                kind: ParserDiagnosticKind::DuplicateTypeSpecifier,
                            });
                        }
                        _ => {
                            self.result.parser_diagnostics.push(ParserDiagnostic {
                                span,
                                file: self.file.id(),
                                kind: ParserDiagnosticKind::InvalidTypeSpecifier,
                            });
                        }
                    }
                }
                if has_signed && has_unsigned {
                    self.result.parser_diagnostics.push(ParserDiagnostic {
                        span,
                        file: self.file.id(),
                        kind: ParserDiagnosticKind::ConflictingTypeSpecifiers,
                    });
                }
                self.specifiers.clear();
                match (has_unsigned, has_long) {
                    (true, true) => self.tys.ulong_ty,
                    (true, false) => self.tys.uint_ty,
                    (false, true) => self.tys.long_ty,
                    (false, false) => self.tys.int_ty,
                }
            }
        }
    }

    fn parse_specifiers(&mut self) -> (Ty<'ctx>, Option<StorageClass>) {
        let mut count = 0;
        let mut storage = None;
        let start_span = self.peek_span();

        let mut end_span = start_span;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::KwStatic => {
                    storage = Some(StorageClass::Static);
                    count += 1;
                }
                TokenKind::KwExtern => {
                    storage = Some(StorageClass::Extern);
                    count += 1;
                }
                TokenKind::KwVoid
                | TokenKind::KwInt
                | TokenKind::KwLong
                | TokenKind::KwDouble
                | TokenKind::KwSigned
                | TokenKind::KwUnsigned => {
                    self.specifiers.push(token.kind);
                }
                _ => break,
            }
            end_span = token.span;
            self.next();
        }

        let span = start_span.merge(end_span);
        if count > 1 {
            self.result.parser_diagnostics.push(ParserDiagnostic {
                span,
                file: self.file.id(),
                kind: ParserDiagnosticKind::MultipleStorageClasses,
            });
        }

        (self.parse_type(span), storage)
    }

    fn parse_var_decl(&mut self) -> Option<Decl> {
        let (ty, storage) = self.parse_specifiers();
        let (span, name) = self.eat_identifier()?;
        let init = if let Some(Token {
            kind: TokenKind::Eq,
            ..
        }) = self.peek()
        {
            self.next();
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        self.eat(TokenKind::Semi)?;
        Some(self.result.ast.decl.push(DeclData {
            ty,
            span,
            storage,
            kind: DeclKind::Var(init),
            name: Symbol {
                name,
                sema: Default::default(),
            },
        }))
    }

    fn parse_decl(&mut self) -> Option<Decl> {
        let (ty, storage) = self.parse_specifiers();
        let (span, name) = self.eat_identifier()?;
        let token = self.eat_some()?;
        match token.kind {
            TokenKind::Semi => Some(self.result.ast.decl.push(DeclData {
                ty,
                span,
                storage,
                kind: DeclKind::Var(None),
                name: Symbol {
                    name,
                    sema: Default::default(),
                },
            })),
            TokenKind::Eq => {
                let init = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.decl.push(DeclData {
                    ty,
                    span,
                    storage,
                    kind: DeclKind::Var(Some(init)),
                    name: Symbol {
                        name,
                        sema: Default::default(),
                    },
                }))
            }
            TokenKind::LParen => {
                let params = self.parse_params();
                self.eat(TokenKind::RParen)?;
                let body = if let Some(Token {
                    kind: TokenKind::Semi,
                    ..
                }) = self.peek()
                {
                    self.next();
                    None
                } else {
                    self.eat(TokenKind::LBrace)?;
                    let body = self.parse_body();
                    self.eat(TokenKind::RBrace)?;
                    Some(body)
                };
                let ty = self.build_func_type(params, ty);
                Some(self.result.ast.decl.push(DeclData {
                    ty,
                    span,
                    storage,
                    kind: DeclKind::Func { params, body },
                    name: Symbol {
                        name,
                        sema: Default::default(),
                    },
                }))
            }
            _ => {
                self.result.parser_diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    file: self.file.id(),
                    kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Semi),
                });
                None
            }
        }
    }

    fn parse_param(&mut self, ty: Ty<'ctx>) -> Option<Decl> {
        let (span, name) = self.eat_identifier()?;
        Some(self.result.ast.decl.push(DeclData {
            ty,
            span,
            storage: None,
            kind: DeclKind::Var(None),
            name: Symbol {
                name,
                sema: Default::default(),
            },
        }))
    }

    fn parse_params(&mut self) -> DeclList {
        let Some(token) = self.peek_some() else {
            return DeclList::empty();
        };
        match token.kind {
            TokenKind::KwVoid => {
                self.next();
                DeclList::empty()
            }
            TokenKind::RParen => DeclList::default(),
            _ => {
                let ty_span = self.collect_types();
                let ty = self.parse_type(ty_span);
                let base = self.decl_stack.len();
                if let Some(decl) = self.parse_param(ty) {
                    self.decl_stack.push(decl);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.peek()
                {
                    self.next();
                    let ty_span = self.collect_types();
                    let ty = self.parse_type(ty_span);
                    if let Some(decl) = self.parse_param(ty) {
                        self.decl_stack.push(decl);
                    }
                }
                self.result.ast.decls.extend(self.decl_stack.drain(base..))
            }
        }
    }

    fn parse_body(&mut self) -> Block {
        let base = self.items_stack.len();
        while let Some(Token { kind, .. }) = self.peek() {
            match kind {
                TokenKind::RBrace => break,
                kind if kind.is_decl_start() => match self.parse_decl() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(decl) => self.items_stack.push(BlockItem::Decl(decl)),
                },
                _ => match self.parse_stmt() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(expr) => self.items_stack.push(BlockItem::Stmt(expr)),
                },
            }
        }
        self.result.ast.items.extend(self.items_stack.drain(base..))
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        let Token { kind, span } = self.peek_some()?;
        match kind {
            TokenKind::Semi => {
                self.next();
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Empty,
                }))
            }
            TokenKind::KwBreak => {
                self.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Break(Default::default()),
                }))
            }
            TokenKind::KwContinue => {
                self.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Continue(Default::default()),
                }))
            }
            TokenKind::KwGoto => {
                self.next();
                let (_, label) = self.eat_identifier()?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Goto {
                        label,
                        stmt: Default::default(),
                    },
                }))
            }
            TokenKind::KwReturn => {
                self.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Return(expr),
                }))
            }
            TokenKind::KwDefault => {
                self.next();
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Default(stmt),
                }))
            }
            TokenKind::LBrace => {
                self.next();
                let body = self.parse_body();
                self.eat(TokenKind::RBrace)?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Compound(body),
                }))
            }
            TokenKind::KwCase => {
                self.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Case { expr, stmt },
                }))
            }
            TokenKind::KwWhile => {
                self.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::While { cond, body },
                }))
            }
            TokenKind::KwSwitch => {
                self.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Switch { cond, body },
                }))
            }
            TokenKind::KwDo => {
                self.next();
                let body = self.parse_stmt()?;
                self.eat(TokenKind::KwWhile)?;
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::DoWhile { body, cond },
                }))
            }
            TokenKind::KwIf => {
                self.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let then = self.parse_stmt()?;
                let otherwise = match self.peek() {
                    Some(Token {
                        kind: TokenKind::KwElse,
                        ..
                    }) => {
                        self.next();
                        Some(self.parse_stmt()?)
                    }
                    _ => None,
                };
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::If {
                        cond,
                        then,
                        otherwise,
                    },
                }))
            }
            TokenKind::KwFor => {
                self.next();
                self.eat(TokenKind::LParen)?;
                let init = match self.peek() {
                    None => None,
                    Some(Token { kind, .. }) => {
                        if kind.is_decl_start() {
                            Some(ForInit::VarDecl(self.parse_var_decl()?))
                        } else {
                            self.parse_optional_expr(TokenKind::Semi).map(ForInit::Expr)
                        }
                    }
                };
                let cond = self.parse_optional_expr(TokenKind::Semi);
                let step = self.parse_optional_expr(TokenKind::RParen);
                let body = self.parse_stmt()?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::For {
                        init,
                        cond,
                        step,
                        body,
                    },
                }))
            }
            TokenKind::Identifier => {
                let is_label = {
                    let mut ahead = self.lexer.clone();
                    ahead.next();
                    matches!(
                        ahead.next(),
                        Some(Ok(Token {
                            kind: TokenKind::Colon,
                            ..
                        }))
                    )
                };
                if is_label {
                    let (span, label) = self.eat_identifier()?;
                    self.eat(TokenKind::Colon)?;
                    let stmt = self.parse_stmt()?;
                    Some(self.result.ast.stmt.push(StmtData {
                        span,
                        kind: StmtKind::Label { label, stmt },
                    }))
                } else {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    Some(self.result.ast.stmt.push(StmtData {
                        span,
                        kind: StmtKind::Expr(expr),
                    }))
                }
            }
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmt.push(StmtData {
                    span,
                    kind: StmtKind::Expr(expr),
                }))
            }
        }
    }

    fn parse_optional_expr(&mut self, delim: TokenKind) -> Option<Expr> {
        match self.peek() {
            Some(Token { kind, .. }) if kind == delim => {
                self.next();
                None
            }
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(delim)?;
                Some(expr)
            }
        }
    }

    fn parse_expr(&mut self, min_prec: u8) -> Option<Expr> {
        let mut lhs = self.parse_expr_prefix()?;
        while let Some(Token { kind, span }) = self.peek() {
            match Option::<Precedence>::from(kind) {
                None => break,
                Some(Precedence { token, prec, assoc }) => {
                    if prec < min_prec {
                        break;
                    }
                    self.next();
                    let prec = match assoc {
                        Associativity::Right => prec,
                        Associativity::Left => prec + 1,
                    };
                    match token {
                        InfixToken::Binary(op) => {
                            let rhs = self.parse_expr(prec)?;
                            lhs = self.result.ast.expr.push(ExprData {
                                span,
                                ty: self.tys.void_ty.into(),
                                kind: ExprKind::Binary { op, lhs, rhs },
                            });
                        }
                        InfixToken::Ternary => {
                            let then = self.parse_expr(0)?;
                            self.eat(TokenKind::Colon)?;
                            let other = self.parse_expr(prec)?;
                            lhs = self.result.ast.expr.push(ExprData {
                                span,
                                ty: self.tys.void_ty.into(),
                                kind: ExprKind::Ternary {
                                    cond: lhs,
                                    then,
                                    other,
                                },
                            });
                        }
                    }
                }
            }
        }
        Some(lhs)
    }

    fn parse_expr_prefix(&mut self) -> Option<Expr> {
        let Token { kind, span } = self.eat_some()?;
        if let Some(op) = match kind {
            TokenKind::Minus => Some(UnaryOp::Neg),
            TokenKind::Bang => Some(UnaryOp::LogNot),
            TokenKind::Tilde => Some(UnaryOp::BitNot),
            TokenKind::PlusPlus => Some(UnaryOp::PreInc),
            TokenKind::MinusMinus => Some(UnaryOp::PreDec),
            _ => None,
        } {
            let expr = self.parse_expr_prefix()?;
            return Some(self.result.ast.expr.push(ExprData {
                span,
                ty: self.tys.void_ty.into(),
                kind: ExprKind::Unary { op, expr },
            }));
        }
        match kind {
            TokenKind::NumLong => {
                let n = self.file.slice(span).expect("expected span to be valid");
                let n = &n[0..n.len() - 1]; // remove 'L' suffix
                let n = n.parse::<i64>().expect("expected number to be valid");
                Some(self.result.ast.expr.push(ExprData {
                    span,
                    ty: self.tys.void_ty.into(),
                    kind: ExprKind::Const(Constant::Long(n)),
                }))
            }
            TokenKind::NumULong => {
                let n = self.file.slice(span).expect("expected span to be valid");
                let n = &n[0..n.len() - 2]; // remove 'UL' suffix
                let n = n.parse::<u64>().expect("expected number to be valid");
                Some(self.result.ast.expr.push(ExprData {
                    span,
                    ty: self.tys.void_ty.into(),
                    kind: ExprKind::Const(Constant::ULong(n)),
                }))
            }
            TokenKind::NumInt => {
                let n = self.file.slice(span).expect("expected span to be valid");
                match n.parse::<i32>() {
                    Ok(n) => Some(self.result.ast.expr.push(ExprData {
                        span,
                        ty: self.tys.void_ty.into(),
                        kind: ExprKind::Const(Constant::Int(n)),
                    })),
                    Err(_) => {
                        let n = n.parse::<i64>().expect("expected number to be valid");
                        Some(self.result.ast.expr.push(ExprData {
                            span,
                            ty: self.tys.void_ty.into(),
                            kind: ExprKind::Const(Constant::Long(n)),
                        }))
                    }
                }
            }
            TokenKind::NumUInt => {
                let n = self.file.slice(span).expect("expected span to be valid");
                let n = &n[0..n.len() - 1]; // remove 'U' suffix
                match n.parse::<u32>() {
                    Ok(n) => Some(self.result.ast.expr.push(ExprData {
                        span,
                        ty: self.tys.void_ty.into(),
                        kind: ExprKind::Const(Constant::UInt(n)),
                    })),
                    Err(_) => {
                        let n = n.parse::<u64>().expect("expected number to be valid");
                        Some(self.result.ast.expr.push(ExprData {
                            span,
                            ty: self.tys.void_ty.into(),
                            kind: ExprKind::Const(Constant::ULong(n)),
                        }))
                    }
                }
            }
            TokenKind::NumFloat => {
                let n = self.file.slice(span).expect("expected span to be valid");
                let n = n
                    .parse::<f64>()
                    .expect("expected floating-point number to be valid");
                Some(self.result.ast.expr.push(ExprData {
                    span,
                    ty: self.tys.void_ty.into(),
                    kind: ExprKind::Const(Constant::Double(n.to_bits())),
                }))
            }
            TokenKind::Identifier => {
                let name = self.intern_span(span);
                let expr = self.result.ast.expr.push(ExprData {
                    span,
                    ty: self.tys.void_ty.into(),
                    kind: ExprKind::Var(Symbol {
                        name,
                        sema: Default::default(),
                    }),
                });
                match self.peek() {
                    Some(Token {
                        kind: TokenKind::LParen,
                        ..
                    }) => {
                        self.next();
                        let args = self.parse_args();
                        self.eat(TokenKind::RParen)?;
                        Some(self.result.ast.expr.push(ExprData {
                            span,
                            ty: self.tys.void_ty.into(),
                            kind: ExprKind::Call {
                                args,
                                name: Symbol {
                                    name,
                                    sema: Default::default(),
                                },
                            },
                        }))
                    }
                    _ => Some(self.parse_expr_postfix(expr)),
                }
            }
            TokenKind::LParen => {
                let ty_span = self.collect_types();
                if ty_span.is_empty() {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::RParen)?;
                    let expr = self.result.ast.expr.push(ExprData {
                        span,
                        ty: self.tys.void_ty.into(),
                        kind: ExprKind::Grouped(expr),
                    });
                    Some(self.parse_expr_postfix(expr))
                } else {
                    let ty = self.parse_type(ty_span);
                    self.eat(TokenKind::RParen)?;
                    let expr = self.parse_expr_prefix()?;
                    let expr = self.result.ast.expr.push(ExprData {
                        span,
                        ty: self.tys.void_ty.into(),
                        kind: ExprKind::Cast { ty, expr },
                    });
                    Some(self.parse_expr_postfix(expr))
                }
            }
            _ => {
                self.result.parser_diagnostics.push(ParserDiagnostic {
                    span,
                    file: self.file.id(),
                    kind: ParserDiagnosticKind::UnexpectedToken,
                });
                None
            }
        }
    }

    fn parse_expr_postfix(&mut self, mut expr: Expr) -> Expr {
        while let Some(Token { kind, span }) = self.peek() {
            let op = match kind {
                TokenKind::PlusPlus => UnaryOp::PostInc,
                TokenKind::MinusMinus => UnaryOp::PostDec,
                _ => break,
            };
            self.next();
            expr = self.result.ast.expr.push(ExprData {
                span,
                ty: self.tys.void_ty.into(),
                kind: ExprKind::Unary { op, expr },
            });
        }
        expr
    }

    fn parse_args(&mut self) -> ExprList {
        let Some(token) = self.peek_some() else {
            return ExprList::default();
        };
        match token.kind {
            TokenKind::RParen => ExprList::default(),
            _ => {
                let base = self.expr_stack.len();
                if let Some(expr) = self.parse_expr(0) {
                    self.expr_stack.push(expr);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.peek()
                {
                    self.next();
                    if let Some(expr) = self.parse_expr(0) {
                        self.expr_stack.push(expr);
                    }
                }
                self.result.ast.exprs.extend(self.expr_stack.drain(base..))
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Auxiliary Methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn intern_span(&mut self, span: Span) -> Ident {
        self.interner
            .intern(self.file.slice(span).expect("expected span to be valid"))
    }

    #[inline]
    fn build_func_type(&mut self, params_slice: DeclList, ret: Ty<'ctx>) -> Ty<'ctx> {
        let mut params = Vec::with_capacity(params_slice.len());
        self.result.ast.decls[params_slice].iter().for_each(|d| {
            params.push(self.result.ast.decl[*d].ty);
        });
        self.tys.func(ret, params)
    }

    // ---------------------------------------------------------------------------
    // Lexer Helpers
    // ---------------------------------------------------------------------------

    #[inline]
    fn next(&mut self) -> Option<Token> {
        loop {
            match self.lexer.next() {
                None => return None,
                Some(Ok(token)) => return Some(token),
                Some(Err(diag)) => {
                    self.result.lexer_diagnostics.push(diag);
                }
            }
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<Token> {
        loop {
            match self.lexer.peek() {
                None => return None,
                Some(Ok(token)) => return Some(*token),
                Some(Err(diag)) => {
                    self.result.lexer_diagnostics.push(diag.clone());
                    self.lexer.next();
                }
            }
        }
    }

    #[inline]
    fn sync(&mut self, eat: TokenKind, stop: TokenKind) {
        while let Some(token) = self.peek() {
            if token.kind == eat {
                self.next();
                break;
            }
            if token.kind == stop {
                break;
            }
            self.next();
        }
    }

    #[inline]
    fn peek_span(&mut self) -> Span {
        self.peek().map(|t| t.span).unwrap_or_default()
    }

    #[inline]
    fn peek_some(&mut self) -> Option<Token> {
        if let Some(token) = self.peek() {
            Some(token)
        } else {
            self.result.parser_diagnostics.push(ParserDiagnostic {
                file: self.file.id(),
                span: Span::single(self.file.end_pos()),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.peek_some()?;
        if token.kind == kind {
            self.next()
        } else {
            self.result.parser_diagnostics.push(ParserDiagnostic {
                span: token.span,
                file: self.file.id(),
                kind: ParserDiagnosticKind::ExpectedToken(kind),
            });
            None
        }
    }

    #[inline]
    fn eat_some(&mut self) -> Option<Token> {
        self.next().or_else(|| {
            self.result.parser_diagnostics.push(ParserDiagnostic {
                file: self.file.id(),
                span: Span::single(self.file.end_pos()),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })
    }

    fn eat_identifier(&mut self) -> Option<(Span, Ident)> {
        let token = self.peek_some()?;
        match token.kind {
            TokenKind::Identifier => {
                let span = token.span;
                self.next();
                let symbol = self.intern_span(span);
                Some((span, symbol))
            }
            _ => {
                self.result.parser_diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    file: self.file.id(),
                    kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Identifier),
                });
                None
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Associativity
// ---------------------------------------------------------------------------

pub enum Associativity {
    /// Left associativity
    Left,
    /// Right associativity
    Right,
}

// ---------------------------------------------------------------------------
// InfixToken
// ---------------------------------------------------------------------------

pub enum InfixToken {
    /// Ternary operator
    Ternary,
    /// Binary operator
    Binary(BinaryOp),
}

// ---------------------------------------------------------------------------
// Precedence
// ---------------------------------------------------------------------------

struct Precedence {
    prec: u8,
    token: InfixToken,
    assoc: Associativity,
}

impl Precedence {
    const fn ternary(prec: u8) -> Self {
        Self {
            prec,
            token: InfixToken::Ternary,
            assoc: Associativity::Right,
        }
    }

    const fn binary_left(prec: u8, op: BinaryOp) -> Self {
        Self {
            prec,
            assoc: Associativity::Left,
            token: InfixToken::Binary(op),
        }
    }

    const fn binary_right(prec: u8, op: BinaryOp) -> Self {
        Self {
            prec,
            assoc: Associativity::Right,
            token: InfixToken::Binary(op),
        }
    }
}

impl From<TokenKind> for Option<Precedence> {
    fn from(token: TokenKind) -> Self {
        match token {
            // Group: Right-to-left Associativity
            TokenKind::Eq => Some(Precedence::binary_right(0, BinaryOp::Assign)),
            TokenKind::PlusEq => Some(Precedence::binary_right(0, BinaryOp::AddAssign)),
            TokenKind::MinusEq => Some(Precedence::binary_right(0, BinaryOp::SubAssign)),
            TokenKind::StarEq => Some(Precedence::binary_right(0, BinaryOp::MulAssign)),
            TokenKind::SlashEq => Some(Precedence::binary_right(0, BinaryOp::DivAssign)),
            TokenKind::PercentEq => Some(Precedence::binary_right(0, BinaryOp::RemAssign)),
            TokenKind::AmpEq => Some(Precedence::binary_right(0, BinaryOp::BitAndAssign)),
            TokenKind::PipeEq => Some(Precedence::binary_right(0, BinaryOp::BitOrAssign)),
            TokenKind::CaretEq => Some(Precedence::binary_right(0, BinaryOp::BitXorAssign)),
            TokenKind::LtLtEq => Some(Precedence::binary_right(0, BinaryOp::BitShlAssign)),
            TokenKind::GtGtEq => Some(Precedence::binary_right(0, BinaryOp::BitShrAssign)),

            // Group: Right-to-left Associativity
            TokenKind::Question => Some(Precedence::ternary(1)),

            // Group: Left-to-right Associativity
            TokenKind::PipePipe => Some(Precedence::binary_left(2, BinaryOp::LogOr)),

            // Group: Left-to-right Associativity
            TokenKind::AmpAmp => Some(Precedence::binary_left(3, BinaryOp::LogAnd)),

            // Group: Left-to-right Associativity
            TokenKind::Pipe => Some(Precedence::binary_left(4, BinaryOp::BitOr)),

            // Group: Left-to-right Associativity
            TokenKind::Caret => Some(Precedence::binary_left(5, BinaryOp::BitXor)),

            // Group: Left-to-right Associativity
            TokenKind::Amp => Some(Precedence::binary_left(6, BinaryOp::BitAnd)),

            // Group: Left-to-right Associativity
            TokenKind::EqEq => Some(Precedence::binary_left(7, BinaryOp::Eq)),
            TokenKind::BangEq => Some(Precedence::binary_left(7, BinaryOp::Ne)),

            // Group: Left-to-right Associativity
            TokenKind::Lt => Some(Precedence::binary_left(8, BinaryOp::Lt)),
            TokenKind::Gt => Some(Precedence::binary_left(8, BinaryOp::Gt)),
            TokenKind::LtEq => Some(Precedence::binary_left(8, BinaryOp::Le)),
            TokenKind::GtEq => Some(Precedence::binary_left(8, BinaryOp::Ge)),

            // Group: Left-to-right Associativity
            TokenKind::LtLt => Some(Precedence::binary_left(9, BinaryOp::BitShl)),
            TokenKind::GtGt => Some(Precedence::binary_left(9, BinaryOp::BitShr)),

            // Group: Left-to-right Associativity
            TokenKind::Plus => Some(Precedence::binary_left(10, BinaryOp::Add)),
            TokenKind::Minus => Some(Precedence::binary_left(10, BinaryOp::Sub)),

            // Group: Left-to-right Associativity
            TokenKind::Star => Some(Precedence::binary_left(11, BinaryOp::Mul)),
            TokenKind::Slash => Some(Precedence::binary_left(11, BinaryOp::Div)),
            TokenKind::Percent => Some(Precedence::binary_left(11, BinaryOp::Rem)),

            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// ParserResult
// ---------------------------------------------------------------------------

pub struct ParserResult<'ctx> {
    pub ast: Ast<'ctx>,
    pub lexer_diagnostics: Vec<LexerDiagnostic>,
    pub parser_diagnostics: Vec<ParserDiagnostic>,
}

impl ParserResult<'_> {
    pub fn new(file: FileId) -> Self {
        Self {
            ast: Ast::new(file),
            lexer_diagnostics: Vec::new(),
            parser_diagnostics: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// ParserDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub struct ParserDiagnostic {
    span: Span,
    file: FileId,
    kind: ParserDiagnosticKind,
}

// ---------------------------------------------------------------------------
// ParserDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub enum ParserDiagnosticKind {
    UnexpectedEof,
    UnexpectedToken,
    MissingTypeSpecifier,
    InvalidTypeSpecifier,
    DuplicateTypeSpecifier,
    ConflictingTypeSpecifiers,
    MultipleStorageClasses,
    ExpectedToken(TokenKind),
}

impl From<ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: ParserDiagnostic) -> Self {
        match diagnostic.kind {
            ParserDiagnosticKind::UnexpectedEof => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("unexpected end of file"),
                )
                .with_note("the file ended while parsing was still in progress; there may be missing closing braces, parentheses, or semicolons"),
            ParserDiagnosticKind::UnexpectedToken => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("unexpected token"),
                )
                .with_note("this token doesn't fit the expected syntax at this location"),
            ParserDiagnosticKind::ExpectedToken(token) => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message(format!("expected '{token}'")),
                )
                .with_note(format!("the parser expected to find '{token}' at this location")),
            ParserDiagnosticKind::DuplicateTypeSpecifier => Diagnostic::error()
                    .with_label(
                        Label::primary(diagnostic.file, diagnostic.span)
                            .with_message("duplicate type specifier"),
                    )
                    .with_note("a type specifier has been specified more than once in the same declaration"),
            ParserDiagnosticKind::MissingTypeSpecifier => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("missing type specifier"),
                )
                .with_note("declarations must include a type specifier such as 'int', 'char', 'void', etc."),
            ParserDiagnosticKind::ConflictingTypeSpecifiers => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("conflicting type specifiers"),
                )
                .with_note("signed and unsigned type specifiers cannot be used together in the same declaration"),
            ParserDiagnosticKind::InvalidTypeSpecifier => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("invalid type specifier combination"),
                )
                .with_note("this combination of type specifiers is invalid or conflicting (e.g., 'int char' or 'long short')"),
            ParserDiagnosticKind::MultipleStorageClasses => Diagnostic::error()
                .with_label(
                    Label::primary(diagnostic.file, diagnostic.span)
                        .with_message("multiple storage classes specified"),
                )
                .with_note("only one storage class specifier (static, extern, auto, register, typedef) is allowed per declaration"),
        }
    }
}
