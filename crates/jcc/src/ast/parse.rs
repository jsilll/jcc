use crate::{
    ast::{
        Ast, BinaryOp, BlockItem, Decl, DeclRef, Expr, ExprRef, ForInit, Slice, Stmt, StmtRef,
        UnaryOp,
    },
    lex::{Token, TokenKind},
};

use ssa::{
    source_file::{diag::Diagnostic, SourceFile, SourceSpan},
    Interner, Symbol,
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
    file: &'a SourceFile,
    interner: &'a mut Interner,
    iter: Peekable<Iter<'a, Token>>,
    result: ParserResult,
    args_stack: Vec<ExprRef>,
    params_stack: Vec<DeclRef>,
    block_items_stack: Vec<BlockItem>,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SourceFile, interner: &'a mut Interner, iter: Iter<'a, Token>) -> Self {
        Self {
            file,
            interner,
            iter: iter.peekable(),
            result: ParserResult::default(),
            args_stack: Vec::with_capacity(16),
            params_stack: Vec::with_capacity(16),
            block_items_stack: Vec::with_capacity(16),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        while let Some(_) = self.iter.peek() {
            match self.parse_func_decl() {
                Some(decl) => self.result.ast.root.push(decl),
                None => self.sync(TokenKind::Semi, TokenKind::KwInt),
            }
        }
        self.result
    }

    #[inline]
    fn intern_span(&mut self, span: &SourceSpan) -> Symbol {
        self.interner
            .get_or_intern(self.file.slice(*span).expect("expected span to be valid"))
    }

    fn parse_decl(&mut self) -> Option<DeclRef> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        let token = self.eat_some()?;
        match token.kind {
            TokenKind::Semi => Some(
                self.result
                    .ast
                    .new_decl(Decl::Var { name, init: None }, span),
            ),
            TokenKind::Eq => {
                let init = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_decl(
                    Decl::Var {
                        name,
                        init: Some(init),
                    },
                    span,
                ))
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
                Some(
                    self.result
                        .ast
                        .new_decl(Decl::Func { name, params, body }, span),
                )
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

    fn parse_var_decl(&mut self) -> Option<DeclRef> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        let init = match self.iter.peek() {
            Some(Token {
                kind: TokenKind::Eq,
                ..
            }) => {
                self.iter.next();
                Some(self.parse_expr(0)?)
            }
            _ => None,
        };
        self.eat(TokenKind::Semi)?;
        Some(self.result.ast.new_decl(Decl::Var { name, init }, span))
    }

    fn parse_func_decl(&mut self) -> Option<DeclRef> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        self.eat(TokenKind::LParen)?;
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
        Some(
            self.result
                .ast
                .new_decl(Decl::Func { name, params, body }, span),
        )
    }

    fn parse_params(&mut self) -> Slice<DeclRef> {
        let Some(token) = self.peek_some() else {
            return Slice::default();
        };
        match token.kind {
            TokenKind::RParen => Slice::default(),
            TokenKind::KwVoid => {
                self.iter.next();
                Slice::default()
            }
            TokenKind::KwInt => {
                let base = self.params_stack.len();
                if let Some(decl) = self.parse_param() {
                    self.params_stack.push(decl);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.iter.peek()
                {
                    self.iter.next();
                    if let Some(decl) = self.parse_param() {
                        self.params_stack.push(decl);
                    }
                }
                self.result.ast.new_params(self.params_stack.drain(base..))
            }
            _ => {
                let diagnostic = ParserDiagnostic {
                    span: token.span,
                    kind: ParserDiagnosticKind::ExpectedToken(TokenKind::RParen),
                };
                self.result.diagnostics.push(diagnostic);
                Slice::default()
            }
        }
    }

    fn parse_param(&mut self) -> Option<DeclRef> {
        self.eat(TokenKind::KwInt)?;
        let (span, name) = self.eat_identifier()?;
        Some(
            self.result
                .ast
                .new_decl(Decl::Var { name, init: None }, span),
        )
    }

    fn parse_body(&mut self) -> Slice<BlockItem> {
        let base = self.block_items_stack.len();
        while let Some(Token { kind, .. }) = self.iter.peek() {
            match kind {
                TokenKind::RBrace => break,
                TokenKind::KwInt => match self.parse_decl() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(decl) => self.block_items_stack.push(BlockItem::Decl(decl)),
                },
                _ => match self.parse_stmt() {
                    None => self.sync(TokenKind::Semi, TokenKind::RBrace),
                    Some(expr) => self.block_items_stack.push(BlockItem::Stmt(expr)),
                },
            }
        }
        self.result
            .ast
            .new_block_items(self.block_items_stack.drain(base..))
    }

    fn parse_stmt(&mut self) -> Option<StmtRef> {
        let Token { kind, span } = self.iter.peek()?;
        match kind {
            TokenKind::Semi => {
                self.iter.next();
                Some(self.result.ast.new_stmt(Stmt::Empty, *span))
            }
            TokenKind::KwBreak => {
                self.iter.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Break, *span))
            }
            TokenKind::KwContinue => {
                self.iter.next();
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Continue, *span))
            }
            TokenKind::KwGoto => {
                self.iter.next();
                let (_, name) = self.eat_identifier()?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Goto(name), *span))
            }
            TokenKind::KwReturn => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Return(expr), *span))
            }
            TokenKind::KwDefault => {
                self.iter.next();
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::Default(stmt), *span))
            }
            TokenKind::LBrace => {
                self.iter.next();
                let body = self.parse_body();
                self.eat(TokenKind::RBrace)?;
                Some(self.result.ast.new_stmt(Stmt::Compound(body), *span))
            }
            TokenKind::KwCase => {
                self.iter.next();
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::Case { expr, stmt }, *span))
            }
            TokenKind::KwWhile => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::While { cond, body }, *span))
            }
            TokenKind::KwSwitch => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(Stmt::Switch { cond, body }, *span))
            }
            TokenKind::KwDo => {
                self.iter.next();
                let body = self.parse_stmt()?;
                self.eat(TokenKind::KwWhile)?;
                self.eat(TokenKind::LParen)?;
                let cond = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                self.eat(TokenKind::Semi)?;
                Some(
                    self.result
                        .ast
                        .new_stmt(Stmt::DoWhile { body, cond }, *span),
                )
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
                Some(self.result.ast.new_stmt(
                    Stmt::If {
                        cond,
                        then,
                        otherwise,
                    },
                    *span,
                ))
            }
            TokenKind::KwFor => {
                self.iter.next();
                self.eat(TokenKind::LParen)?;
                let init = if let Some(Token {
                    kind: TokenKind::KwInt,
                    ..
                }) = self.iter.peek()
                {
                    Some(ForInit::VarDecl(self.parse_var_decl()?))
                } else {
                    self.parse_optional_expr(TokenKind::Semi).map(ForInit::Expr)
                };
                let cond = self.parse_optional_expr(TokenKind::Semi);
                let step = self.parse_optional_expr(TokenKind::RParen);
                let body = self.parse_stmt()?;
                Some(self.result.ast.new_stmt(
                    Stmt::For {
                        init,
                        cond,
                        step,
                        body,
                    },
                    *span,
                ))
            }
            TokenKind::Identifier => match self.iter.clone().skip(1).next() {
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    self.iter.next();
                    self.iter.next();
                    let stmt = self.parse_stmt()?;
                    let label = self.intern_span(span);
                    Some(self.result.ast.new_stmt(Stmt::Label { label, stmt }, *span))
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    self.eat(TokenKind::Semi)?;
                    Some(self.result.ast.new_stmt(Stmt::Expr(expr), *span))
                }
            },
            _ => {
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::Semi)?;
                Some(self.result.ast.new_stmt(Stmt::Expr(expr), *span))
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
                                .new_expr(Expr::Binary { op, lhs, rhs }, *span);
                        }
                        InfixToken::Ternary => {
                            let then = self.parse_expr(0)?;
                            self.eat(TokenKind::Colon)?;
                            let otherwise = self.parse_expr(prec)?;
                            lhs = self.result.ast.new_expr(
                                Expr::Ternary {
                                    cond: lhs,
                                    then,
                                    otherwise,
                                },
                                *span,
                            );
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
            Some(parser.result.ast.new_expr(Expr::Unary { op, expr }, *span))
        }
        let Token { kind, span } = self.iter.next()?;
        match kind {
            TokenKind::Minus => parse_prefix_operator(self, UnaryOp::Neg, span),
            TokenKind::Tilde => parse_prefix_operator(self, UnaryOp::BitNot, span),
            TokenKind::Bang => parse_prefix_operator(self, UnaryOp::LogicalNot, span),
            TokenKind::PlusPlus => parse_prefix_operator(self, UnaryOp::PreInc, span),
            TokenKind::MinusMinus => parse_prefix_operator(self, UnaryOp::PreDec, span),
            TokenKind::LParen => {
                let expr = self.parse_expr(0)?;
                self.eat(TokenKind::RParen)?;
                let expr = self.result.ast.new_expr(Expr::Grouped(expr), *span);
                Some(self.parse_expr_postfix(expr))
            }
            TokenKind::Number => {
                let number = self.file.slice(*span).expect("expected span to be valid");
                let number = number.parse().expect("expected number to be valid");
                Some(self.result.ast.new_expr(Expr::Const(number), *span))
            }
            TokenKind::Identifier => {
                let name = self.intern_span(span);
                let expr = self.result.ast.new_expr(Expr::Var(name), *span);
                match self.iter.peek() {
                    Some(Token {
                        kind: TokenKind::LParen,
                        ..
                    }) => {
                        self.iter.next();
                        let args = self.parse_args();
                        self.eat(TokenKind::RParen)?;
                        Some(self.result.ast.new_expr(Expr::Call { name, args }, *span))
                    }
                    _ => Some(self.parse_expr_postfix(expr)),
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
                .new_expr(Expr::Unary { op, expr: *expr }, *span);
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
                let base = self.args_stack.len();
                if let Some(expr) = self.parse_expr(0) {
                    self.args_stack.push(expr);
                }
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.iter.peek()
                {
                    self.iter.next();
                    if let Some(expr) = self.parse_expr(0) {
                        self.args_stack.push(expr);
                    }
                }
                self.result.ast.new_args(self.args_stack.drain(base..))
            }
        }
    }

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

    fn peek_some(&mut self) -> Option<&&Token> {
        self.iter.peek().or_else(|| {
            self.result.diagnostics.push(ParserDiagnostic {
                span: self.file.end_span(),
                kind: ParserDiagnosticKind::UnexpectedEof,
            });
            None
        })
    }

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
}

// ---------------------------------------------------------------------------
// Associativity
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

// ---------------------------------------------------------------------------
// InfixToken
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixToken {
    Ternary,
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

impl From<TokenKind> for Option<Precedence> {
    fn from(token: TokenKind) -> Self {
        match token {
            // Group: Right-to-left Associativity
            TokenKind::Eq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::Assign),
            }),
            TokenKind::PlusEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::AddAssign),
            }),
            TokenKind::MinusEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::SubAssign),
            }),
            TokenKind::StarEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::MulAssign),
            }),
            TokenKind::SlashEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::DivAssign),
            }),
            TokenKind::PercentEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::RemAssign),
            }),
            TokenKind::AmpEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitAndAssign),
            }),
            TokenKind::PipeEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitOrAssign),
            }),
            TokenKind::CaretEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitXorAssign),
            }),
            TokenKind::LtLtEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitShlAssign),
            }),
            TokenKind::GtGtEq => Some(Precedence {
                prec: 0,
                assoc: Associativity::Right,
                token: InfixToken::Binary(BinaryOp::BitShrAssign),
            }),
            // Group: Right-to-left Associativity
            TokenKind::Question => Some(Precedence {
                prec: 1,
                token: InfixToken::Ternary,
                assoc: Associativity::Right,
            }),
            // Group: Left-to-right Associativity
            TokenKind::PipePipe => Some(Precedence {
                prec: 2,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LogicalOr),
            }),
            // Group: Left-to-right Associativity
            TokenKind::AmpAmp => Some(Precedence {
                prec: 3,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LogicalAnd),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Pipe => Some(Precedence {
                prec: 4,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitOr),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Caret => Some(Precedence {
                prec: 5,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitXor),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Amp => Some(Precedence {
                prec: 6,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitAnd),
            }),
            // Group: Left-to-right Associativity
            TokenKind::EqEq => Some(Precedence {
                prec: 7,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Equal),
            }),
            TokenKind::BangEq => Some(Precedence {
                prec: 7,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::NotEqual),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Lt => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LessThan),
            }),
            TokenKind::Gt => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::GreaterThan),
            }),
            TokenKind::LtEq => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::LessEqual),
            }),
            TokenKind::GtEq => Some(Precedence {
                prec: 8,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::GreaterEqual),
            }),
            // Group: Left-to-right Associativity
            TokenKind::LtLt => Some(Precedence {
                prec: 9,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitShl),
            }),
            TokenKind::GtGt => Some(Precedence {
                prec: 9,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::BitShr),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Plus => Some(Precedence {
                prec: 10,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Add),
            }),
            TokenKind::Minus => Some(Precedence {
                prec: 10,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Sub),
            }),
            // Group: Left-to-right Associativity
            TokenKind::Star => Some(Precedence {
                prec: 11,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Mul),
            }),
            TokenKind::Slash => Some(Precedence {
                prec: 11,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Div),
            }),
            TokenKind::Percent => Some(Precedence {
                prec: 11,
                assoc: Associativity::Left,
                token: InfixToken::Binary(BinaryOp::Rem),
            }),
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
        }
    }
}
