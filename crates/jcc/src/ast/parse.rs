use crate::{
    ast::{
        ty::{Ty, TyCtx},
        Ast, AstSymbol, BinaryOp, BlockItem, ConstValue, Decl, DeclKind, DeclRef, Expr, ExprKind,
        ExprRef, ForInit, Slice, Stmt, StmtKind, StmtRef, StorageClass, UnaryOp,
    },
    token::{
        lex::{Lexer, LexerDiagnostic},
        Token, TokenKind,
    },
};

use jcc_ssa::{
    interner::{Interner, Symbol},
    sourcemap::{diag::Diagnostic, SourceMap, SourceSpan},
};

use std::iter::Peekable;

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub struct Parser<'a, 'ctx> {
    /// The source file being parsed.
    file: &'a SourceMap,
    /// The type context used for creating and interning types.
    tys: &'ctx TyCtx<'ctx>,
    /// The lexer that provides a stream of tokens.
    lexer: Peekable<Lexer<'a>>,
    /// The interner for string interning.
    interner: &'a mut Interner,
    /// A temporary buffer for collecting type specifiers.
    types: Vec<Ty<'ctx>>,
    /// The result of the parsing process, containing the AST and diagnostics.
    result: ParserResult<'ctx>,
    /// A stack used for building expression lists, like function arguments.
    expr_stack: Vec<ExprRef>,
    /// A stack used for building declaration lists, like function parameters.
    decl_stack: Vec<DeclRef>,
    /// A stack used for building block item lists, like function bodies.
    items_stack: Vec<BlockItem>,
}

impl<'a, 'ctx> Parser<'a, 'ctx> {
    pub fn new(
        lexer: Lexer<'a>,
        file: &'a SourceMap,
        tys: &'ctx TyCtx<'ctx>,
        interner: &'a mut Interner,
    ) -> Self {
        Self {
            tys,
            file,
            interner,
            lexer: lexer.peekable(),
            types: Vec::with_capacity(16),
            result: ParserResult::default(),
            expr_stack: Vec::with_capacity(16),
            decl_stack: Vec::with_capacity(16),
            items_stack: Vec::with_capacity(16),
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

    fn parse_type(&mut self) -> Ty<'ctx> {
        let ty = match self.types.as_slice() {
            [ty] if *ty == self.tys.int_ty => self.tys.int_ty,
            [ty] if *ty == self.tys.long_ty => self.tys.long_ty,
            [ty] if *ty == self.tys.void_ty => self.tys.void_ty,
            [ty1, ty2] if *ty1 == self.tys.int_ty && *ty2 == self.tys.long_ty => self.tys.long_ty,
            [ty1, ty2] if *ty1 == self.tys.long_ty && *ty2 == self.tys.int_ty => self.tys.long_ty,
            t => {
                let is_empty = t.is_empty();
                let span = self.peek_span();
                self.result.parser_diagnostics.push(ParserDiagnostic::new(
                    if is_empty {
                        ParserDiagnosticKind::MissingTypeSpecifier
                    } else {
                        ParserDiagnosticKind::InvalidTypeSpecifier
                    },
                    span,
                ));
                self.tys.void_ty
            }
        };
        self.types.clear();
        ty
    }

    fn parse_specifiers(&mut self) -> Option<(Ty<'ctx>, Option<StorageClass>)> {
        let mut count = 0;
        let mut storage = None;
        let start_span = self.peek_span();
        let mut end_span = start_span;
        while let Some(t) = self.peek() {
            match t.kind {
                TokenKind::KwInt => {
                    self.types.push(self.tys.int_ty);
                }
                TokenKind::KwLong => {
                    self.types.push(self.tys.long_ty);
                }
                TokenKind::KwVoid => {
                    self.types.push(self.tys.void_ty);
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
            end_span = t.span;
            self.next();
        }
        if count > 1 {
            self.result.parser_diagnostics.push(ParserDiagnostic::new(
                ParserDiagnosticKind::MultipleStorageClasses,
                start_span.merge(end_span),
            ));
        }
        Some((self.parse_type(), storage))
    }

    fn parse_var_decl(&mut self) -> Option<DeclRef> {
        let (ty, storage) = self.parse_specifiers()?;
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
        Some(self.result.ast.decls.push(Decl {
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
            TokenKind::Semi => Some(self.result.ast.decls.push(Decl {
                ty,
                span,
                storage,
                kind: DeclKind::Var(None),
                name: AstSymbol::new(name),
            })),
            TokenKind::Eq => {
                let init = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.decls.push(Decl {
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
                Some(self.result.ast.decls.push(Decl {
                    ty,
                    span,
                    storage,
                    name: AstSymbol::new(name),
                    kind: DeclKind::Func { params, body },
                }))
            }
            _ => {
                self.result.parser_diagnostics.push(ParserDiagnostic::new(
                    ParserDiagnosticKind::ExpectedToken(TokenKind::Semi),
                    token.span,
                ));
                None
            }
        }
    }

    fn parse_param(&mut self, ty: Ty<'ctx>) -> Option<DeclRef> {
        let (span, name) = self.eat_identifier()?;
        Some(self.result.ast.decls.push(Decl {
            ty,
            span,
            storage: None,
            kind: DeclKind::Var(None),
            name: AstSymbol::new(name),
        }))
    }

    fn parse_params(&mut self) -> Slice<DeclRef> {
        let Some(token) = self.peek_some() else {
            return Slice::default();
        };
        match token.kind {
            TokenKind::KwVoid => {
                self.next();
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
                }) = self.peek()
                {
                    self.next();
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
        let base = self.items_stack.len();
        while let Some(Token { kind, .. }) = self.peek() {
            match kind {
                TokenKind::RBrace => break,
                t if is_decl_start(t) => match self.parse_decl() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(decl) => self.items_stack.push(BlockItem::Decl(decl)),
                },
                _ => match self.parse_stmt() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(expr) => self.items_stack.push(BlockItem::Stmt(expr)),
                },
            }
        }
        self.result.ast.new_items(self.items_stack.drain(base..))
    }

    fn parse_stmt(&mut self) -> Option<StmtRef> {
        let Token { kind, span } = self.peek_some()?;
        match kind {
            TokenKind::Semi => {
                self.next();
                Some(self.result.ast.stmts.push(Stmt {
                    span,
                    kind: StmtKind::Empty,
                }))
            }
            TokenKind::KwBreak => {
                self.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmts.push(Stmt {
                    span,
                    kind: StmtKind::Break(Default::default()),
                }))
            }
            TokenKind::KwContinue => {
                self.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmts.push(Stmt {
                    span,
                    kind: StmtKind::Continue(Default::default()),
                }))
            }
            TokenKind::KwGoto => {
                self.next();
                let (_, label) = self.eat_identifier()?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmts.push(Stmt {
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
                Some(self.result.ast.stmts.push(Stmt {
                    span,
                    kind: StmtKind::Return(expr),
                }))
            }
            TokenKind::KwDefault => {
                self.next();
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.stmts.push(Stmt {
                    span,
                    kind: StmtKind::Default(stmt),
                }))
            }
            TokenKind::LBrace => {
                self.next();
                let body = self.parse_body();
                self.eat(TokenKind::RBrace)?;
                Some(self.result.ast.stmts.push(Stmt {
                    span,
                    kind: StmtKind::Compound(body),
                }))
            }
            TokenKind::KwCase => {
                self.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.stmts.push(Stmt {
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
                Some(self.result.ast.stmts.push(Stmt {
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
                Some(self.result.ast.stmts.push(Stmt {
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
                Some(self.result.ast.stmts.push(Stmt {
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
                Some(self.result.ast.stmts.push(Stmt {
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
                    Some(Token { kind, .. }) => match kind {
                        t if is_decl_start(t) => Some(ForInit::VarDecl(self.parse_var_decl()?)),
                        _ => self.parse_optional_expr(TokenKind::Semi).map(ForInit::Expr),
                    },
                };
                let cond = self.parse_optional_expr(TokenKind::Semi);
                let step = self.parse_optional_expr(TokenKind::RParen);
                let body = self.parse_stmt()?;
                Some(self.result.ast.stmts.push(Stmt {
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
                    Some(self.result.ast.stmts.push(Stmt {
                        span,
                        kind: StmtKind::Label { label, stmt },
                    }))
                } else {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    Some(self.result.ast.stmts.push(Stmt {
                        span,
                        kind: StmtKind::Expr(expr),
                    }))
                }
            }
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.stmts.push(Stmt {
                    span,
                    kind: StmtKind::Expr(expr),
                }))
            }
        }
    }

    fn parse_optional_expr(&mut self, delim: TokenKind) -> Option<ExprRef> {
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

    fn parse_expr(&mut self, min_prec: u8) -> Option<ExprRef> {
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
                            lhs = self.result.ast.exprs.push(Expr::new(
                                ExprKind::Binary { op, lhs, rhs },
                                span,
                                self.tys.void_ty,
                            ));
                        }
                        InfixToken::Ternary => {
                            let then = self.parse_expr(0)?;
                            self.eat(TokenKind::Colon)?;
                            let other = self.parse_expr(prec)?;
                            lhs = self.result.ast.exprs.push(Expr::new(
                                ExprKind::Ternary {
                                    cond: lhs,
                                    then,
                                    other,
                                },
                                span,
                                self.tys.void_ty,
                            ));
                        }
                    }
                }
            }
        }
        Some(lhs)
    }

    fn parse_expr_prefix(&mut self) -> Option<ExprRef> {
        let Token { kind, span } = self.eat_some()?;
        if let Some(op) = match kind {
            TokenKind::Minus => Some(UnaryOp::Neg),
            TokenKind::Tilde => Some(UnaryOp::BitNot),
            TokenKind::Bang => Some(UnaryOp::LogicalNot),
            TokenKind::PlusPlus => Some(UnaryOp::PreInc),
            TokenKind::MinusMinus => Some(UnaryOp::PreDec),
            _ => None,
        } {
            let expr = self.parse_expr_prefix()?;
            return Some(self.result.ast.exprs.push(Expr::new(
                ExprKind::Unary { op, expr },
                span,
                self.tys.void_ty,
            )));
        }
        match kind {
            TokenKind::LongIntNumber => {
                let n = self.file.slice(span).expect("expected span to be valid");
                let n = &n[0..n.len() - 1]; // remove 'L' suffix
                let n = n.parse::<i64>().expect("expected number to be valid");
                Some(self.result.ast.exprs.push(Expr::new(
                    ExprKind::Const(ConstValue::Int64(n)),
                    span,
                    self.tys.void_ty,
                )))
            }
            TokenKind::IntNumber => {
                let n = self.file.slice(span).expect("expected span to be valid");
                match n.parse::<i32>() {
                    Ok(n) => Some(self.result.ast.exprs.push(Expr::new(
                        ExprKind::Const(ConstValue::Int32(n)),
                        span,
                        self.tys.void_ty,
                    ))),
                    Err(_) => {
                        let n = n.parse::<i64>().expect("expected number to be valid");
                        Some(self.result.ast.exprs.push(Expr::new(
                            ExprKind::Const(ConstValue::Int64(n)),
                            span,
                            self.tys.void_ty,
                        )))
                    }
                }
            }
            TokenKind::Identifier => {
                let name = self.intern_span(span);
                let expr = self.result.ast.exprs.push(Expr::new(
                    ExprKind::Var(AstSymbol::new(name)),
                    span,
                    self.tys.void_ty,
                ));
                match self.peek() {
                    Some(Token {
                        kind: TokenKind::LParen,
                        ..
                    }) => {
                        self.next();
                        let args = self.parse_args();
                        self.eat(TokenKind::RParen)?;
                        Some(self.result.ast.exprs.push(Expr::new(
                            ExprKind::Call {
                                args,
                                name: AstSymbol::new(name),
                            },
                            span,
                            self.tys.void_ty,
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
                    let expr = self.result.ast.exprs.push(Expr::new(
                        ExprKind::Grouped(expr),
                        span,
                        self.tys.void_ty,
                    ));
                    Some(self.parse_expr_postfix(expr))
                } else {
                    let ty = self.parse_type();
                    self.eat(TokenKind::RParen)?;
                    let expr = self.parse_expr_prefix()?;
                    let expr = self.result.ast.exprs.push(Expr::new(
                        ExprKind::Cast { ty, expr },
                        span,
                        self.tys.void_ty,
                    ));
                    Some(self.parse_expr_postfix(expr))
                }
            }
            _ => {
                self.result.parser_diagnostics.push(ParserDiagnostic::new(
                    ParserDiagnosticKind::UnexpectedToken,
                    span,
                ));
                None
            }
        }
    }

    fn parse_expr_postfix(&mut self, mut expr: ExprRef) -> ExprRef {
        while let Some(Token { kind, span }) = self.peek() {
            let op = match kind {
                TokenKind::PlusPlus => UnaryOp::PostInc,
                TokenKind::MinusMinus => UnaryOp::PostDec,
                _ => break,
            };
            self.next();
            expr = self.result.ast.exprs.push(Expr::new(
                ExprKind::Unary { op, expr },
                span,
                self.tys.void_ty,
            ));
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
                }) = self.peek()
                {
                    self.next();
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
    fn intern_span(&mut self, span: SourceSpan) -> Symbol {
        self.interner
            .intern(self.file.slice(span).expect("expected span to be valid"))
    }

    #[inline]
    fn build_func_type(&mut self, params_slice: Slice<DeclRef>, ret: Ty<'ctx>) -> Ty<'ctx> {
        let mut params = Vec::with_capacity(params_slice.len());
        self.result.ast.decls(params_slice).iter().for_each(|d| {
            params.push(self.result.ast.decls[*d].ty);
        });
        self.tys.func(ret, params)
    }

    fn collect_types(&mut self) {
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::KwInt => {
                    self.types.push(self.tys.int_ty);
                }
                TokenKind::KwLong => {
                    self.types.push(self.tys.long_ty);
                }
                TokenKind::KwVoid => {
                    self.types.push(self.tys.void_ty);
                }
                _ => break,
            }
            self.next();
        }
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
    fn peek_span(&mut self) -> SourceSpan {
        self.peek()
            .map(|t| t.span)
            .unwrap_or_else(|| self.file.end_span())
    }

    #[inline]
    fn peek_some(&mut self) -> Option<Token> {
        if let Some(token) = self.peek() {
            Some(token)
        } else {
            self.result.parser_diagnostics.push(ParserDiagnostic::new(
                ParserDiagnosticKind::UnexpectedEof,
                self.file.end_span(),
            ));
            None
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.peek_some()?;
        if token.kind == kind {
            self.next()
        } else {
            self.result.parser_diagnostics.push(ParserDiagnostic::new(
                ParserDiagnosticKind::ExpectedToken(kind),
                token.span,
            ));
            None
        }
    }

    #[inline]
    fn eat_some(&mut self) -> Option<Token> {
        self.next().or_else(|| {
            self.result.parser_diagnostics.push(ParserDiagnostic::new(
                ParserDiagnosticKind::UnexpectedEof,
                self.file.end_span(),
            ));
            None
        })
    }

    fn eat_identifier(&mut self) -> Option<(SourceSpan, Symbol)> {
        let token = self.peek_some()?;
        match token.kind {
            TokenKind::Identifier => {
                let span = token.span;
                self.next();
                let symbol = self.intern_span(span);
                Some((span, symbol))
            }
            _ => {
                self.result.parser_diagnostics.push(ParserDiagnostic::new(
                    ParserDiagnosticKind::ExpectedToken(TokenKind::Identifier),
                    token.span,
                ));
                None
            }
        }
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

// ---------------------------------------------------------------------------
// ParserResult
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct ParserResult<'ctx> {
    pub ast: Ast<'ctx>,
    pub lexer_diagnostics: Vec<LexerDiagnostic>,
    pub parser_diagnostics: Vec<ParserDiagnostic>,
}

// ---------------------------------------------------------------------------
// ParserDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct ParserDiagnostic {
    span: SourceSpan,
    kind: ParserDiagnosticKind,
}

impl ParserDiagnostic {
    pub fn new(kind: ParserDiagnosticKind, span: SourceSpan) -> Self {
        Self { kind, span }
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
