use peeking_take_while::PeekableExt;

use tacky::{
    source_file::{diag::Diagnostic, SourceFile, SourceSpan},
    string_interner::{DefaultStringInterner, DefaultSymbol},
};

use std::{iter::Peekable, str::CharIndices};

// ---------------------------------------------------------------------------
// Token
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

// ---------------------------------------------------------------------------
// LexerDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct LexerDiagnostic {
    pub span: SourceSpan,
    pub kind: LexerDiagnosticKind,
}

// ---------------------------------------------------------------------------
// LexerResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub diagnostics: Vec<LexerDiagnostic>,
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

pub struct Lexer<'a> {
    file: &'a SourceFile,
    interner: &'a mut DefaultStringInterner,
    result: LexerResult,
    nesting: Vec<TokenKind>,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile, interner: &'a mut DefaultStringInterner) -> Self {
        Self {
            file,
            interner,
            result: LexerResult::default(),
            nesting: Vec::with_capacity(16),
            chars: file.data().char_indices().peekable(),
        }
    }

    pub fn lex(mut self) -> LexerResult {
        while let Some((begin, c)) = self.chars.next().map(|(begin, c)| (begin as u32, c)) {
            match c {
                c if c.is_whitespace() => continue,
                c if c.is_digit(10) => self.lex_number(begin),
                c if c.is_ascii_alphabetic() || c == '_' => self.lex_keyword_or_identifier(begin),
                ';' => self.lex_char(begin, TokenKind::Semi),
                '~' => self.lex_char(begin, TokenKind::Tilde),
                '=' => self.lex_char_double(begin, '=', TokenKind::EqEq, TokenKind::Eq),
                '!' => self.lex_char_double(begin, '=', TokenKind::BangEq, TokenKind::Bang),
                '*' => self.lex_char_double(begin, '=', TokenKind::StarEq, TokenKind::Star),
                '/' => self.lex_char_double(begin, '=', TokenKind::SlashEq, TokenKind::Slash),
                '^' => self.lex_char_double(begin, '=', TokenKind::CaretEq, TokenKind::Caret),
                '%' => self.lex_char_double(begin, '=', TokenKind::PercentEq, TokenKind::Percent),
                '(' => self.handle_nesting_open(begin, TokenKind::LParen, TokenKind::RParen),
                '{' => self.handle_nesting_open(begin, TokenKind::LBrace, TokenKind::RBrace),
                '[' => self.handle_nesting_open(begin, TokenKind::LBrack, TokenKind::RBrack),
                ')' => self.handle_nesting_close(begin, TokenKind::LParen, TokenKind::RParen),
                '}' => self.handle_nesting_close(begin, TokenKind::LBrace, TokenKind::RBrace),
                ']' => self.handle_nesting_close(begin, TokenKind::LBrack, TokenKind::RBrack),
                '+' => self.lex_char_double2(
                    begin,
                    ('+', TokenKind::PlusPlus),
                    ('=', TokenKind::PlusEq),
                    TokenKind::Plus,
                ),
                '-' => self.lex_char_double2(
                    begin,
                    ('-', TokenKind::MinusMinus),
                    ('=', TokenKind::MinusEq),
                    TokenKind::Minus,
                ),
                '&' => self.lex_char_double2(
                    begin,
                    ('&', TokenKind::AmpAmp),
                    ('=', TokenKind::AmpEq),
                    TokenKind::Amp,
                ),
                '|' => self.lex_char_double2(
                    begin,
                    ('|', TokenKind::PipePipe),
                    ('=', TokenKind::PipeEq),
                    TokenKind::Pipe,
                ),
                '<' => match self.chars.peek() {
                    Some((_, '<')) => {
                        self.chars.next();
                        self.lex_char_double(begin, '=', TokenKind::LtLtEq, TokenKind::LtLt)
                    }
                    Some((_, '=')) => {
                        self.chars.next();
                        self.lex_char(begin, TokenKind::LtEq)
                    }
                    _ => self.lex_char(begin, TokenKind::Lt),
                },
                '>' => match self.chars.peek() {
                    Some((_, '>')) => {
                        self.chars.next();
                        self.lex_char_double(begin, '=', TokenKind::GtGtEq, TokenKind::GtGt)
                    }
                    Some((_, '=')) => {
                        self.chars.next();
                        self.lex_char(begin, TokenKind::GtEq)
                    }
                    _ => self.lex_char(begin, TokenKind::Gt),
                },
                _ => self.result.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::UnexpectedCharacter,
                    span: self.file.span(begin..begin + 1).unwrap_or_default(),
                }),
            }
        }
        if !self.nesting.is_empty() {
            self.nesting.clone().into_iter().for_each(|kind| {
                self.insert_unbalanced_token(kind, None);
            });
        }
        self.result
    }

    fn lex_char(&mut self, begin: u32, kind: TokenKind) {
        self.result.tokens.push(Token {
            kind,
            span: self.file.span(begin..begin + 1).unwrap_or_default(),
        })
    }

    fn lex_char_double(&mut self, begin: u32, ch: char, kind: TokenKind, fallback: TokenKind) {
        match self.chars.peek() {
            Some((_, ch2)) if *ch2 == ch => {
                self.chars.next();
                self.result.tokens.push(Token {
                    kind,
                    span: self.file.span(begin..begin + 2).unwrap_or_default(),
                });
            }
            _ => self.result.tokens.push(Token {
                kind: fallback,
                span: self.file.span(begin..begin + 1).unwrap_or_default(),
            }),
        }
    }

    fn lex_char_double2(
        &mut self,
        begin: u32,
        alt1: (char, TokenKind),
        alt2: (char, TokenKind),
        fallback: TokenKind,
    ) {
        let (token, len) = match self.chars.peek() {
            Some((_, c)) if *c == alt1.0 => {
                self.chars.next();
                (alt1.1, 2)
            }
            Some((_, c)) if *c == alt2.0 => {
                self.chars.next();
                (alt2.1, 2)
            }
            _ => (fallback, 1),
        };
        self.result.tokens.push(Token {
            kind: token,
            span: self.file.span(begin..begin + len).unwrap_or_default(),
        });
    }

    fn lex_number(&mut self, begin: u32) {
        let end = self
            .chars
            .peeking_take_while(|(_, c)| c.is_digit(10))
            .last()
            .map(|(end, _)| end as u32)
            .unwrap_or_else(|| begin)
            + 1;
        let number = self.file.slice(begin..end).unwrap_or_default();
        let number = number.parse().unwrap_or_default();
        if let Some((_, c)) = self.chars.peek() {
            if c.is_ascii_alphabetic() {
                self.result.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::IdentifierStartsWithDigit,
                    span: self.file.span(begin..end).unwrap_or_default(),
                });
            }
        }
        self.result.tokens.push(Token {
            kind: TokenKind::Number(number),
            span: self.file.span(begin..end).unwrap_or_default(),
        });
    }

    fn lex_keyword_or_identifier(&mut self, begin: u32) {
        let end = self
            .chars
            .peeking_take_while(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
            .last()
            .map(|(end, _)| end as u32)
            .unwrap_or_else(|| begin)
            + 1;
        let ident = self.file.slice(begin..end).unwrap_or_default();
        let kind = KEYWORDS
            .get(ident)
            .copied()
            .unwrap_or(TokenKind::Identifier(self.interner.get_or_intern(ident)));
        self.result.tokens.push(Token {
            kind,
            span: self.file.span(begin..end).unwrap_or_default(),
        });
    }

    fn handle_nesting_open(&mut self, begin: u32, open: TokenKind, close: TokenKind) {
        self.nesting.push(close);
        self.lex_char(begin, open);
    }

    fn handle_nesting_close(&mut self, begin: u32, open: TokenKind, close: TokenKind) {
        let mut matched = false;
        while let Some(kind) = self.nesting.pop() {
            if kind == close {
                matched = true;
                break;
            }
            self.insert_unbalanced_token(kind, Some(begin));
        }
        if !matched {
            self.insert_unbalanced_token(open, Some(begin));
        }
        self.result.tokens.push(Token {
            kind: close,
            span: self.file.span(begin..begin + 1).unwrap_or_default(),
        });
    }

    fn insert_unbalanced_token(&mut self, kind: TokenKind, begin: Option<u32>) {
        let span = begin
            .map(|b| self.file.span(b..b + 1))
            .flatten()
            .unwrap_or(self.file.end_span());
        self.result.tokens.push(Token { kind, span });
        self.result.diagnostics.push(LexerDiagnostic {
            kind: LexerDiagnosticKind::UnbalancedToken(kind),
            span,
        });
    }
}

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map! {
    "int" => TokenKind::KwInt,
    "void" => TokenKind::KwVoid,
    "return" => TokenKind::KwReturn,
};

// ---------------------------------------------------------------------------
// TokenKind
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    /// The `&` token.
    Amp,
    /// The `&&` token.
    AmpAmp,
    /// The `&=` token.
    AmpEq,
    /// The `!` token.
    Bang,
    /// The `!=` token.
    BangEq,
    /// The `^` token.
    Caret,
    /// The `^=` token.
    CaretEq,
    /// The `=` token.
    Eq,
    /// The `==` token.
    EqEq,
    /// The `>` token.
    Gt,
    /// The `>=` token.
    GtEq,
    /// The `>>` token.
    GtGt,
    /// The `>>=` token.
    GtGtEq,
    /// The `{` token.
    LBrace,
    /// The `[` token.
    LBrack,
    /// The `(` token.
    LParen,
    /// The `<` token.
    Lt,
    /// The `<=` token.
    LtEq,
    /// The `<<` token.
    LtLt,
    /// The `<<=` token.
    LtLtEq,
    /// The `-` token.
    Minus,
    /// The `-=` token.
    MinusEq,
    /// The `--` token.
    MinusMinus,
    /// The `%` token.
    Percent,
    /// The `%=` token.
    PercentEq,
    /// The `|` token.
    Pipe,
    /// The `|=` token.
    PipeEq,
    /// The `||` token.
    PipePipe,
    /// The `+` token.
    Plus,
    /// The `+=` token.
    PlusEq,
    /// The `++` token.
    PlusPlus,
    /// The `}` token.
    RBrace,
    /// The `]` token.
    RBrack,
    /// The `)` token.
    RParen,
    /// The `;` token.
    Semi,
    /// The `/` token.
    Slash,
    /// The `/=` token.
    SlashEq,
    /// The `*` token.
    Star,
    /// The `*=` token.
    StarEq,
    /// The `~` token.
    Tilde,

    /// The `int` keyword.
    KwInt,
    /// The `void` keyword.
    KwVoid,
    /// The `return` keyword.
    KwReturn,

    /// A number.
    Number(u32),
    /// An identifier.
    Identifier(DefaultSymbol),
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Amp => write!(f, "'&'"),
            TokenKind::AmpAmp => write!(f, "'&&'"),
            TokenKind::AmpEq => write!(f, "'&='"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::BangEq => write!(f, "'!='"),
            TokenKind::Caret => write!(f, "'^'"),
            TokenKind::CaretEq => write!(f, "'^='"),
            TokenKind::Eq => write!(f, "'='"),
            TokenKind::EqEq => write!(f, "'=='"),
            TokenKind::Gt => write!(f, "'>'"),
            TokenKind::GtEq => write!(f, "'>='"),
            TokenKind::GtGt => write!(f, "'>>'"),
            TokenKind::GtGtEq => write!(f, "'>>='"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::LBrack => write!(f, "'['"),
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::Lt => write!(f, "'<'"),
            TokenKind::LtEq => write!(f, "'<='"),
            TokenKind::LtLt => write!(f, "'<<'"),
            TokenKind::LtLtEq => write!(f, "'<<='"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::MinusEq => write!(f, "'-='"),
            TokenKind::MinusMinus => write!(f, "'--'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::PercentEq => write!(f, "'%='"),
            TokenKind::Pipe => write!(f, "'|'"),
            TokenKind::PipeEq => write!(f, "'|='"),
            TokenKind::PipePipe => write!(f, "'||'"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::PlusEq => write!(f, "'+='"),
            TokenKind::PlusPlus => write!(f, "'++'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::RBrack => write!(f, "']'"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::Semi => write!(f, "';'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::SlashEq => write!(f, "'/='"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::StarEq => write!(f, "'*='"),
            TokenKind::Tilde => write!(f, "'~'"),

            TokenKind::KwInt => write!(f, "'int'"),
            TokenKind::KwVoid => write!(f, "'void'"),
            TokenKind::KwReturn => write!(f, "'return'"),

            TokenKind::Number(_) => write!(f, "a number"),
            TokenKind::Identifier(_) => write!(f, "an identifier"),
        }
    }
}

// ---------------------------------------------------------------------------
// LexerDiagnosticKind
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LexerDiagnosticKind {
    UnexpectedCharacter,
    IdentifierStartsWithDigit,
    UnbalancedToken(TokenKind),
}

impl From<LexerDiagnostic> for Diagnostic {
    fn from(diagnostic: LexerDiagnostic) -> Self {
        match diagnostic.kind {
            LexerDiagnosticKind::UnexpectedCharacter => Diagnostic::error(
                diagnostic.span,
                "unexpected character",
                "expected a valid character",
            ),
            LexerDiagnosticKind::IdentifierStartsWithDigit => Diagnostic::error(
                diagnostic.span,
                "identifier starts with digit",
                "identifiers cannot start with a digit",
            ),
            LexerDiagnosticKind::UnbalancedToken(token) => Diagnostic::error(
                diagnostic.span,
                "unbalanced token",
                format!("expected a matching {token}"),
            ),
        }
    }
}
