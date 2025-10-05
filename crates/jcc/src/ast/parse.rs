use crate::{
    ast::{
        Ast, AstSymbol, BinaryOp, BlockItem, ConstValue, Decl, DeclKind, DeclRef, Expr, ExprKind,
        ExprRef, ForInit, Slice, Stmt, StmtKind, StmtRef, StorageClass, UnaryOp,
    },
    sema::{CompoundType, CompoundTypeRef, Type, TypeDict},
    tok::{Token, TokenKind},
    PassResult,
};

use jcc_ssa::{
    interner::{Interner, Symbol},
    sourcemap::{diag::Diagnostic, SourceMap, SourceSpan},
};

use std::{iter::Peekable, slice::Iter};

// ---------------------------------------------------------------------------
// ParserResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct ParserResult {
    pub ast: Ast,
    pub diagnostics: Vec<ParserDiagnostic>,
}

impl PassResult for ParserResult {
    fn diagnostics(&self) -> &[impl Into<jcc_ssa::sourcemap::diag::Diagnostic> + Clone] {
        &self.diagnostics
    }
}

// ---------------------------------------------------------------------------
// ParserDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct ParserDiagnostic {
    pub span: SourceSpan,
    pub kind: ParserDiagnosticKind,
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a> {
    file: &'a SourceMap,
    dict: &'a mut TypeDict,
    interner: &'a mut Interner,
    iter: Peekable<Iter<'a, Token>>,
    result: ParserResult,
    types: Vec<Type>,
    expr_stack: Vec<ExprRef>,
    decl_stack: Vec<DeclRef>,
    bitems_stack: Vec<BlockItem>,
}

impl<'a> Parser<'a> {
    pub fn new(
        file: &'a SourceMap,
        dict: &'a mut TypeDict,
        interner: &'a mut Interner,
        iter: Iter<'a, Token>,
    ) -> Self {
        Self {
            file,
            dict,
            interner,
            iter: iter.peekable(),
            result: ParserResult::default(),
            types: Vec::with_capacity(16),
            expr_stack: Vec::with_capacity(16),
            decl_stack: Vec::with_capacity(16),
            bitems_stack: Vec::with_capacity(16),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        while self.iter.peek().is_some() {
            match self.parse_decl() {
                Some(decl) => self.result.ast.root.push(decl),
                None => self.sync(TokenKind::Semi, TokenKind::KwInt),
            }
        }
        self.result
    }

    fn parse_type(&mut self) -> Type {
        let ty = match self.types[..] {
            [Type::Int] => Type::Int,
            [Type::Long] => Type::Long,
            [Type::Void] => Type::Void,
            [Type::Int, Type::Long] => Type::Long,
            [Type::Long, Type::Int] => Type::Long,
            [] => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: self
                        .iter
                        .peek()
                        .map(|t| t.span)
                        .unwrap_or(self.file.end_span()),
                    kind: ParserDiagnosticKind::MissingTypeSpecifier,
                });
                Type::default()
            }
            _ => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: self
                        .iter
                        .peek()
                        .map(|t| t.span)
                        .unwrap_or(self.file.end_span()),
                    kind: ParserDiagnosticKind::InvalidTypeSpecifier,
                });
                Type::default()
            }
        };
        self.types.clear();
        ty
    }

    fn parse_specifiers(&mut self) -> Option<(Type, Option<StorageClass>)> {
        let mut count = 0;
        let mut storage = None;
        while let Some(t) = self.iter.peek() {
            match t.kind {
                TokenKind::KwInt => {
                    self.types.push(Type::Int);
                }
                TokenKind::KwLong => {
                    self.types.push(Type::Long);
                }
                TokenKind::KwVoid => {
                    self.types.push(Type::Void);
                }
                TokenKind::KwStatic => {
                    storage = Some(StorageClass::Static);
                    count += 1;
                }
                TokenKind::KwExtern => {
                    storage = Some(StorageClass::Extern);
                    count += 1;
                }
                _ => break,
            }
            self.iter.next();
        }
        if count > 1 {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::MultipleStorageClasses,
            });
        }
        Some((self.parse_type(), storage))
    }

    fn parse_var_decl(&mut self) -> Option<DeclRef> {
        let (ty, storage) = self.parse_specifiers()?;
        let (span, name) = self.eat_identifier()?;
        let init = if let Some(Token {
            kind: TokenKind::Eq,
            ..
        }) = self.iter.peek()
        {
            self.iter.next();
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        self.eat(TokenKind::Semi)?;
        Some(self.result.ast.new_decl(Decl {
            ty,
            span,
            storage,
            kind: DeclKind::Var(init),
            name: AstSymbol::new(name),
        }))
    }

    fn parse_decl(&mut self) -> Option<DeclRef> {
        let (ty, storage) = self.parse_specifiers()?;
        let (span, name) = self.eat_identifier()?;
        let token = self.eat_some()?;
        match token.kind {
            TokenKind::Semi => Some(self.result.ast.new_decl(Decl {
                ty,
                span,
                storage,
                name: AstSymbol::new(name),
                ..Default::default()
            })),
            TokenKind::Eq => {
                let init = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_decl(Decl {
                    ty,
                    span,
                    storage,
                    name: AstSymbol::new(name),
                    kind: DeclKind::Var(Some(init)),
                }))
            }
            TokenKind::LParen => {
                let params = self.parse_params();
                self.eat(TokenKind::RParen)?;
                let body = if let Some(Token {
                    kind: TokenKind::Semi,
                    ..
                }) = self.iter.peek()
                {
                    self.iter.next();
                    None
                } else {
                    self.eat(TokenKind::LBrace)?;
                    let body = self.parse_body();
                    self.eat(TokenKind::RBrace)?;
                    Some(body)
                };
                let ty = self.build_func_type(params, ty);
                Some(self.result.ast.new_decl(Decl {
                    span,
                    storage,
                    ty: ty.into(),
                    name: AstSymbol::new(name),
                    kind: DeclKind::Func { params, body },
                }))
            }
            _ => {
                let diagnostic = ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Semi),
                };
                self.result.diagnostics.push(diagnostic);
                None
            }
        }
    }

    fn parse_param(&mut self, ty: Type) -> Option<DeclRef> {
        let (span, name) = self.eat_identifier()?;
        Some(self.result.ast.new_decl(Decl {
            ty,
            span,
            name: AstSymbol::new(name),
            ..Default::default()
        }))
    }

    fn parse_params(&mut self) -> Slice<DeclRef> {
        let Some(token) = self.peek_some() else {
            return Slice::default();
        };
        match token.kind {
            TokenKind::KwVoid => {
                self.iter.next();
                Slice::default()
            }
            TokenKind::RParen => Slice::default(),
            _ => {
                self.collect_types();
                let ty = self.parse_type();
                let base = self.decl_stack.len();
                if let Some(decl) = self.parse_param(ty) {
                    self.decl_stack.push(decl);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.iter.peek()
                {
                    self.iter.next();
                    self.collect_types();
                    let ty = self.parse_type();
                    if let Some(decl) = self.parse_param(ty) {
                        self.decl_stack.push(decl);
                    }
                }
                self.result.ast.new_decls(self.decl_stack.drain(base..))
            }
        }
    }

    fn parse_body(&mut self) -> Slice<BlockItem> {
        let base = self.bitems_stack.len();
        while let Some(Token { kind, .. }) = self.iter.peek() {
            match kind {
                TokenKind::RBrace => break,
                t if is_decl_start(*t) => match self.parse_decl() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(decl) => self.bitems_stack.push(BlockItem::Decl(decl)),
                },
                _ => match self.parse_stmt() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(expr) => self.bitems_stack.push(BlockItem::Stmt(expr)),
                },
            }
        }
        self.result.ast.new_bitems(self.bitems_stack.drain(base..))
    }

    fn parse_stmt(&mut self) -> Option<StmtRef> {
        let Token { kind, span } = self.iter.peek()?;
        match kind {
            TokenKind::Semi => {
                self.iter.next();
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Empty,
                    span: *span,
                }))
            }
            TokenKind::KwBreak => {
                self.iter.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Break(Default::default()),
                    span: *span,
                }))
            }
            TokenKind::KwContinue => {
                self.iter.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Continue(Default::default()),
                    span: *span,
                }))
            }
            TokenKind::KwGoto => {
                self.iter.next();
                let (_, label) = self.eat_identifier()?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Goto {
                        label,
                        stmt: Default::default(),
                    },
                    span: *span,
                }))
            }
            TokenKind::KwReturn => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Return(expr),
                    span: *span,
                }))
            }
            TokenKind::KwDefault => {
                self.iter.next();
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Default(stmt),
                    span: *span,
                }))
            }
            TokenKind::LBrace => {
                self.iter.next();
                let body = self.parse_body();
                self.eat(TokenKind::RBrace)?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Compound(body),
                    span: *span,
                }))
            }
            TokenKind::KwCase => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Case { expr, stmt },
                    span: *span,
                }))
            }
            TokenKind::KwWhile => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::While { cond, body },
                    span: *span,
                }))
            }
            TokenKind::KwSwitch => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Switch { cond, body },
                    span: *span,
                }))
            }
            TokenKind::KwDo => {
                self.iter.next();
                let body = self.parse_stmt()?;
                self.eat(TokenKind::KwWhile)?;
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::DoWhile { body, cond },
                    span: *span,
                }))
            }
            TokenKind::KwIf => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let then = self.parse_stmt()?;
                let otherwise = match self.iter.peek() {
                    Some(Token {
                        kind: TokenKind::KwElse,
                        ..
                    }) => {
                        self.iter.next();
                        Some(self.parse_stmt()?)
                    }
                    _ => None,
                };
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::If {
                        cond,
                        then,
                        otherwise,
                    },
                    span: *span,
                }))
            }
            TokenKind::KwFor => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let init = match self.iter.peek() {
                    None => None,
                    Some(Token { kind, .. }) => match kind {
                        t if is_decl_start(*t) => Some(ForInit::VarDecl(self.parse_var_decl()?)),
                        _ => self.parse_optional_expr(TokenKind::Semi).map(ForInit::Expr),
                    },
                };
                let cond = self.parse_optional_expr(TokenKind::Semi);
                let step = self.parse_optional_expr(TokenKind::RParen);
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::For {
                        init,
                        cond,
                        step,
                        body,
                    },
                    span: *span,
                }))
            }
            TokenKind::Identifier => match self.iter.clone().nth(1) {
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    self.iter.next();
                    self.iter.next();
                    let stmt = self.parse_stmt()?;
                    let label = self.intern_span(span);
                    Some(self.result.ast.new_stmt(Stmt {
                        kind: StmtKind::Label { label, stmt },
                        span: *span,
                    }))
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    Some(self.result.ast.new_stmt(Stmt {
                        kind: StmtKind::Expr(expr),
                        span: *span,
                    }))
                }
            },
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt {
                    kind: StmtKind::Expr(expr),
                    span: *span,
                }))
            }
        }
    }

    fn parse_optional_expr(&mut self, delim: TokenKind) -> Option<ExprRef> {
        match self.iter.peek() {
            Some(Token { kind, .. }) if *kind == delim => {
                self.iter.next();
                None
            }
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(delim)?;
                Some(expr)
            }
        }
    }

    fn parse_expr(&mut self, min_prec: u8) -> Option<ExprRef> {
        let mut lhs = self.parse_expr_prefix()?;
        while let Some(Token { kind, span }) = self.iter.peek() {
            match Option::<Precedence>::from(*kind) {
                None => break,
                Some(Precedence { token, prec, assoc }) => {
                    if prec < min_prec {
                        break;
                    }
                    self.iter.next();
                    let prec = match assoc {
                        Associativity::Right => prec,
                        Associativity::Left => prec + 1,
                    };
                    match token {
                        InfixToken::Binary(op) => {
                            let rhs = self.parse_expr(prec)?;
                            lhs = self
                                .result
                                .ast
                                .new_expr(Expr::new(ExprKind::Binary { op, lhs, rhs }, *span));
                        }
                        InfixToken::Ternary => {
                            let then = self.parse_expr(0)?;
                            self.eat(TokenKind::Colon)?;
                            let other = self.parse_expr(prec)?;
                            lhs = self.result.ast.new_expr(Expr::new(
                                ExprKind::Ternary {
                                    cond: lhs,
                                    then,
                                    other,
                                },
                                *span,
                            ));
                        }
                    }
                }
            }
        }
        Some(lhs)
    }

    fn parse_expr_prefix(&mut self) -> Option<ExprRef> {
        #[inline]
        fn parse_prefix_operator(
            parser: &mut Parser,
            op: UnaryOp,
            span: &SourceSpan,
        ) -> Option<ExprRef> {
            let expr = parser.parse_expr_prefix()?;
            Some(
                parser
                    .result
                    .ast
                    .new_expr(Expr::new(ExprKind::Unary { op, expr }, *span)),
            )
        }
        let Token { kind, span } = self.iter.next()?;
        match kind {
            TokenKind::Minus => parse_prefix_operator(self, UnaryOp::Neg, span),
            TokenKind::Tilde => parse_prefix_operator(self, UnaryOp::BitNot, span),
            TokenKind::Bang => parse_prefix_operator(self, UnaryOp::LogicalNot, span),
            TokenKind::PlusPlus => parse_prefix_operator(self, UnaryOp::PreInc, span),
            TokenKind::MinusMinus => parse_prefix_operator(self, UnaryOp::PreDec, span),
            TokenKind::LongIntNumber => {
                let n = self.file.slice(*span).expect("expected span to be valid");
                let n = &n[0..n.len() - 1]; // remove 'L' suffix
                let n = n.parse::<i64>().expect("expected number to be valid");
                Some(
                    self.result
                        .ast
                        .new_expr(Expr::new(ExprKind::Const(ConstValue::Int64(n)), *span)),
                )
            }
            TokenKind::IntNumber => {
                let n = self.file.slice(*span).expect("expected span to be valid");
                match n.parse::<i32>() {
                    Ok(n) => Some(
                        self.result
                            .ast
                            .new_expr(Expr::new(ExprKind::Const(ConstValue::Int32(n)), *span)),
                    ),
                    Err(_) => {
                        let n = n.parse::<i64>().expect("expected number to be valid");
                        Some(
                            self.result
                                .ast
                                .new_expr(Expr::new(ExprKind::Const(ConstValue::Int64(n)), *span)),
                        )
                    }
                }
            }
            TokenKind::Identifier => {
                let name = self.intern_span(span);
                let expr = self
                    .result
                    .ast
                    .new_expr(Expr::new(ExprKind::Var(AstSymbol::new(name)), *span));
                match self.iter.peek() {
                    Some(Token {
                        kind: TokenKind::LParen,
                        ..
                    }) => {
                        self.iter.next();
                        let args = self.parse_args();
                        self.eat(TokenKind::RParen)?;
                        Some(self.result.ast.new_expr(Expr::new(
                            ExprKind::Call {
                                args,
                                name: AstSymbol::new(name),
                            },
                            *span,
                        )))
                    }
                    _ => Some(self.parse_expr_postfix(expr)),
                }
            }
            TokenKind::LParen => {
                self.collect_types();
                if self.types.is_empty() {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::RParen)?;
                    let expr = self
                        .result
                        .ast
                        .new_expr(Expr::new(ExprKind::Grouped(expr), *span));
                    Some(self.parse_expr_postfix(expr))
                } else {
                    let ty = self.parse_type();
                    self.eat(TokenKind::RParen)?;
                    let expr = self.parse_expr_prefix()?;
                    let expr = self
                        .result
                        .ast
                        .new_expr(Expr::new(ExprKind::Cast { ty, expr }, *span));
                    Some(self.parse_expr_postfix(expr))
                }
            }
            _ => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: *span,
                    kind: ParserDiagnosticKind::UnexpectedToken,
                });
                None
            }
        }
    }

    fn parse_expr_postfix(&mut self, mut expr: ExprRef) -> ExprRef {
        fn parse_postfix_operator(
            parser: &mut Parser,
            expr: &mut ExprRef,
            op: UnaryOp,
            span: &SourceSpan,
        ) {
            parser.iter.next();
            *expr = parser
                .result
                .ast
                .new_expr(Expr::new(ExprKind::Unary { op, expr: *expr }, *span));
        }
        while let Some(Token { kind, span }) = self.iter.peek() {
            match kind {
                TokenKind::PlusPlus => {
                    parse_postfix_operator(self, &mut expr, UnaryOp::PostInc, span)
                }
                TokenKind::MinusMinus => {
                    parse_postfix_operator(self, &mut expr, UnaryOp::PostDec, span)
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_args(&mut self) -> Slice<ExprRef> {
        let Some(token) = self.peek_some() else {
            return Slice::default();
        };
        match token.kind {
            TokenKind::RParen => Slice::default(),
            _ => {
                let base = self.expr_stack.len();
                if let Some(expr) = self.parse_expr(0) {
                    self.expr_stack.push(expr);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.iter.peek()
                {
                    self.iter.next();
                    if let Some(expr) = self.parse_expr(0) {
                        self.expr_stack.push(expr);
                    }
                }
                self.result.ast.new_exprs(self.expr_stack.drain(base..))
            }
        }
    }

    // ---------------------------------------------------------------------------
    // Auxiliary Methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn intern_span(&mut self, span: &SourceSpan) -> Symbol {
        self.interner
            .intern(self.file.slice(*span).expect("expected span to be valid"))
    }

    #[inline]
    fn sync(&mut self, eat: TokenKind, stop: TokenKind) {
        while let Some(token) = self.iter.peek() {
            if token.kind == eat {
                self.iter.next();
                break;
            }
            if token.kind == stop {
                break;
            }
            self.iter.next();
        }
    }

    #[inline]
    fn peek_some(&mut self) -> Option<&&Token> {
        self.iter.peek().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })
    }

    #[inline]
    fn eat_some(&mut self) -> Option<&Token> {
        self.iter.next().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })
    }

    fn eat(&mut self, kind: TokenKind) -> Option<&Token> {
        let token = self.iter.peek().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })?;
        if token.kind == kind {
            self.iter.next()
        } else {
            self.result.diagnostics.push(ParserDiagnostic {
                span: token.span,
                kind: ParserDiagnosticKind::ExpectedToken(kind),
            });
            None
        }
    }

    fn eat_identifier(&mut self) -> Option<(SourceSpan, Symbol)> {
        let token = self.iter.peek().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })?;
        match token.kind {
            TokenKind::Identifier => {
                let span = token.span;
                self.iter.next();
                let symbol = self.intern_span(&span);
                Some((span, symbol))
            }
            _ => {
                self.result.diagnostics.push(ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::ExpectedToken(TokenKind::Identifier),
                });
                None
            }
        }
    }

    fn collect_types(&mut self) {
        while let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::KwInt => {
                    self.types.push(Type::Int);
                }
                TokenKind::KwLong => {
                    self.types.push(Type::Long);
                }
                TokenKind::KwVoid => {
                    self.types.push(Type::Void);
                }
                _ => break,
            }
            self.iter.next();
        }
    }

    #[inline]
    fn build_func_type(&mut self, params_slice: Slice<DeclRef>, ret: Type) -> CompoundTypeRef {
        let mut params = Vec::with_capacity(params_slice.len());
        self.result.ast.decls(params_slice).iter().for_each(|d| {
            params.push(self.result.ast.decl(*d).ty);
        });
        self.dict.intern(CompoundType::Func { ret, params })
    }
}

// ---------------------------------------------------------------------------
// Associativity
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    /// Left associativity
    Left,
    /// Right associativity
    Right,
}

// ---------------------------------------------------------------------------
// InfixToken
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            TokenKind::PipePipe => Some(Precedence::binary_left(2, BinaryOp::LogicalOr)),

            // Group: Left-to-right Associativity
            TokenKind::AmpAmp => Some(Precedence::binary_left(3, BinaryOp::LogicalAnd)),

            // Group: Left-to-right Associativity
            TokenKind::Pipe => Some(Precedence::binary_left(4, BinaryOp::BitOr)),

            // Group: Left-to-right Associativity
            TokenKind::Caret => Some(Precedence::binary_left(5, BinaryOp::BitXor)),

            // Group: Left-to-right Associativity
            TokenKind::Amp => Some(Precedence::binary_left(6, BinaryOp::BitAnd)),

            // Group: Left-to-right Associativity
            TokenKind::EqEq => Some(Precedence::binary_left(7, BinaryOp::Equal)),
            TokenKind::BangEq => Some(Precedence::binary_left(7, BinaryOp::NotEqual)),

            // Group: Left-to-right Associativity
            TokenKind::Lt => Some(Precedence::binary_left(8, BinaryOp::LessThan)),
            TokenKind::Gt => Some(Precedence::binary_left(8, BinaryOp::GreaterThan)),
            TokenKind::LtEq => Some(Precedence::binary_left(8, BinaryOp::LessEqual)),
            TokenKind::GtEq => Some(Precedence::binary_left(8, BinaryOp::GreaterEqual)),

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
// ParserDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParserDiagnosticKind {
    UnexpectedEof,
    UnexpectedToken,
    MissingTypeSpecifier,
    InvalidTypeSpecifier,
    MultipleStorageClasses,
    ExpectedToken(TokenKind),
}

impl From<ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: ParserDiagnostic) -> Self {
        match diagnostic.kind {
            ParserDiagnosticKind::UnexpectedEof => Diagnostic::error(
                diagnostic.span,
                "unexpected end of file",
                "expected more tokens",
            ),
            ParserDiagnosticKind::UnexpectedToken => Diagnostic::error(
                diagnostic.span,
                "unexpected token",
                "expected a different token",
            ),
            ParserDiagnosticKind::ExpectedToken(token) => Diagnostic::error(
                diagnostic.span,
                "unexpected token",
                format!("expected {token} instead"),
            ),
            ParserDiagnosticKind::MissingTypeSpecifier => Diagnostic::error(
                diagnostic.span,
                "missing type specifier",
                "expected a type specifier like 'int'",
            ),
            ParserDiagnosticKind::InvalidTypeSpecifier => Diagnostic::error(
                diagnostic.span,
                "invalid type specifier",
                "invalid or conflicting type specifier",
            ),
            ParserDiagnosticKind::MultipleStorageClasses => Diagnostic::error(
                diagnostic.span,
                "multiple storage classes",
                "only one storage class is allowed",
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Auxiliary Functions
// ---------------------------------------------------------------------------

#[inline]
fn is_decl_start(token: TokenKind) -> bool {
    matches!(
        token,
        TokenKind::KwInt
            | TokenKind::KwLong
            | TokenKind::KwVoid
            | TokenKind::KwStatic
            | TokenKind::KwExtern
    )
}
