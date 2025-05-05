use tacky::{
    source_file::{diag::Diagnostic, SourceFile, SourceSpan},
    Interner, Symbol,
};

use peeking_take_while::PeekableExt;

use std::{iter::Peekable, str::CharIndices};

// ---------------------------------------------------------------------------
// LexerResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub diagnostics: Vec<LexerDiagnostic>,
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
// Token
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

pub struct Lexer<'a> {
    file: &'a SourceFile,
    interner: &'a mut Interner,
    idx: u32,
    result: LexerResult,
    nesting: Vec<TokenKind>,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile, interner: &'a mut Interner) -> Self {
        Self {
            file,
            idx: 0,
            interner,
            result: LexerResult::default(),
            nesting: Vec::with_capacity(16),
            chars: file.data().char_indices().peekable(),
        }
    }

    pub fn lex(mut self) -> LexerResult {
        while let Some((idx, c)) = self.chars.next().map(|(idx, c)| (idx as u32, c)) {
            self.idx = idx;
            match c {
                c if c.is_whitespace() => continue,
                c if c.is_digit(10) => self.handle_number(),
                c if c.is_ascii_alphabetic() || c == '_' => self.handle_word(),
                ';' => self.push_token(TokenKind::Semi, 1),
                ',' => self.push_token(TokenKind::Comma, 1),
                ':' => self.push_token(TokenKind::Colon, 1),
                '~' => self.push_token(TokenKind::Tilde, 1),
                '?' => self.push_token(TokenKind::Question, 1),
                '=' => self.try_match('=', TokenKind::EqEq, TokenKind::Eq),
                '!' => self.try_match('=', TokenKind::BangEq, TokenKind::Bang),
                '*' => self.try_match('=', TokenKind::StarEq, TokenKind::Star),
                '/' => self.try_match('=', TokenKind::SlashEq, TokenKind::Slash),
                '^' => self.try_match('=', TokenKind::CaretEq, TokenKind::Caret),
                '%' => self.try_match('=', TokenKind::PercentEq, TokenKind::Percent),
                '(' => self.handle_nesting_open(TokenKind::LParen, TokenKind::RParen),
                '{' => self.handle_nesting_open(TokenKind::LBrace, TokenKind::RBrace),
                '[' => self.handle_nesting_open(TokenKind::LBrack, TokenKind::RBrack),
                ')' => self.handle_nesting_close(TokenKind::LParen, TokenKind::RParen),
                '}' => self.handle_nesting_close(TokenKind::LBrace, TokenKind::RBrace),
                ']' => self.handle_nesting_close(TokenKind::LBrack, TokenKind::RBrack),
                '+' => self.try_match2(
                    ('=', TokenKind::PlusEq),
                    ('+', TokenKind::PlusPlus),
                    TokenKind::Plus,
                ),
                '-' => self.try_match2(
                    ('=', TokenKind::MinusEq),
                    ('-', TokenKind::MinusMinus),
                    TokenKind::Minus,
                ),
                '&' => self.try_match2(
                    ('=', TokenKind::AmpEq),
                    ('&', TokenKind::AmpAmp),
                    TokenKind::Amp,
                ),
                '|' => self.try_match2(
                    ('=', TokenKind::PipeEq),
                    ('|', TokenKind::PipePipe),
                    TokenKind::Pipe,
                ),
                '<' => self.try_match3(
                    ('=', TokenKind::LtEq),
                    ('<', TokenKind::LtLt),
                    ('=', TokenKind::LtLtEq),
                    TokenKind::Lt,
                ),
                '>' => self.try_match3(
                    ('=', TokenKind::GtEq),
                    ('>', TokenKind::GtGt),
                    ('=', TokenKind::GtGtEq),
                    TokenKind::Gt,
                ),
                _ => self.result.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::UnexpectedCharacter,
                    span: self.file.span(idx..idx + 1).unwrap_or_default(),
                }),
            }
        }
        if !self.nesting.is_empty() {
            for i in 0..self.nesting.len() {
                self.push_unbalanced_token(self.nesting[i], true);
            }
        }
        self.result
    }

    fn push_token(&mut self, kind: TokenKind, len: u32) {
        self.result.tokens.push(Token {
            kind,
            span: self.file.span(self.idx..self.idx + len).unwrap_or_default(),
        })
    }

    fn push_unbalanced_token(&mut self, kind: TokenKind, end: bool) {
        let span = if end {
            self.file.end_span()
        } else {
            self.file.span(self.idx..self.idx + 1).unwrap_or_default()
        };
        self.result.tokens.push(Token { kind, span });
        self.result.diagnostics.push(LexerDiagnostic {
            kind: LexerDiagnosticKind::UnbalancedToken(kind),
            span,
        });
    }

    fn try_match(&mut self, ch: char, kind: TokenKind, fallback: TokenKind) {
        match self.chars.peek() {
            Some((_, ch2)) if *ch2 == ch => {
                self.chars.next();
                self.result.tokens.push(Token {
                    kind,
                    span: self.file.span(self.idx..self.idx + 2).unwrap_or_default(),
                });
            }
            _ => self.result.tokens.push(Token {
                kind: fallback,
                span: self.file.span(self.idx..self.idx + 1).unwrap_or_default(),
            }),
        }
    }

    fn try_match2(
        &mut self,
        kind1: (char, TokenKind),
        kind2: (char, TokenKind),
        fallback: TokenKind,
    ) {
        let (token, len) = match self.chars.peek() {
            Some((_, c)) if *c == kind1.0 => {
                self.chars.next();
                (kind1.1, 2)
            }
            Some((_, c)) if *c == kind2.0 => {
                self.chars.next();
                (kind2.1, 2)
            }
            _ => (fallback, 1),
        };
        self.result.tokens.push(Token {
            kind: token,
            span: self.file.span(self.idx..self.idx + len).unwrap_or_default(),
        });
    }

    fn try_match3(
        &mut self,
        kind1: (char, TokenKind),
        kind2: (char, TokenKind),
        kind3: (char, TokenKind),
        fallback: TokenKind,
    ) {
        match self.chars.peek() {
            Some((_, c)) if *c == kind1.0 => {
                self.chars.next();
                self.push_token(kind1.1, 1);
            }
            Some((_, c)) if *c == kind2.0 => {
                self.chars.next();
                self.try_match(kind3.0, kind3.1, kind2.1)
            }
            _ => self.push_token(fallback, 1),
        }
    }

    fn handle_nesting_open(&mut self, open: TokenKind, close: TokenKind) {
        self.nesting.push(close);
        self.push_token(open, 1);
    }

    fn handle_nesting_close(&mut self, open: TokenKind, close: TokenKind) {
        let mut matched = false;
        while let Some(kind) = self.nesting.pop() {
            if kind == close {
                matched = true;
                break;
            }
            self.push_unbalanced_token(kind, false);
        }
        if !matched {
            self.push_unbalanced_token(open, false);
        }
        self.result.tokens.push(Token {
            kind: close,
            span: self.file.span(self.idx..self.idx + 1).unwrap_or_default(),
        });
    }

    fn handle_number(&mut self) {
        let end = self
            .chars
            .peeking_take_while(|(_, c)| c.is_digit(10))
            .last()
            .map(|(end, _)| end as u32)
            .unwrap_or_else(|| self.idx)
            + 1;
        let number = self.file.slice(self.idx..end).unwrap_or_default();
        let number = number.parse().unwrap_or_default();
        if let Some((_, c)) = self.chars.peek() {
            if c.is_ascii_alphabetic() {
                self.result.diagnostics.push(LexerDiagnostic {
                    kind: LexerDiagnosticKind::IdentifierStartsWithDigit,
                    span: self.file.span(self.idx..end).unwrap_or_default(),
                });
            }
        }
        self.result.tokens.push(Token {
            kind: TokenKind::Number(number),
            span: self.file.span(self.idx..end).unwrap_or_default(),
        });
    }

    fn handle_word(&mut self) {
        let end = self
            .chars
            .peeking_take_while(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
            .last()
            .map(|(end, _)| end as u32)
            .unwrap_or_else(|| self.idx)
            + 1;
        let ident = self.file.slice(self.idx..end).unwrap_or_default();
        let kind = KEYWORDS
            .get(ident)
            .copied()
            .unwrap_or(TokenKind::Identifier(self.interner.get_or_intern(ident)));
        self.result.tokens.push(Token {
            kind,
            span: self.file.span(self.idx..end).unwrap_or_default(),
        });
    }
}

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map! {
    "break" => TokenKind::KwBreak,
    "case" => TokenKind::KwCase,
    "continue" => TokenKind::KwContinue,
    "default" => TokenKind::KwDefault,
    "do" => TokenKind::KwDo,
    "else" => TokenKind::KwElse,
    "for" => TokenKind::KwFor,
    "goto" => TokenKind::KwGoto,
    "if" => TokenKind::KwIf,
    "int" => TokenKind::KwInt,
    "return" => TokenKind::KwReturn,
    "switch" => TokenKind::KwSwitch,
    "void" => TokenKind::KwVoid,
    "while" => TokenKind::KwWhile,
};

// ---------------------------------------------------------------------------
// TokenKind
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    /// The `&` token.
    Amp,
    /// The `&&` token.
    AmpAmp,
    /// The `&=` token.
    AmpEq,
    /// The `,` token.
    Comma,
    /// The `:` token.
    Colon,
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
    /// The `?` token.
    Question,
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
    /// The `break` keyword.
    KwBreak,
    /// The `case keyword.
    KwCase,
    /// The `continue` keyword.
    KwContinue,
    /// The `default` keyword.
    KwDefault,
    /// The `do` keyword.
    KwDo,
    /// The `else` keyword.
    KwElse,
    /// The `for` keyword.
    KwFor,
    /// The `goto` keyword.
    KwGoto,
    /// The `if` keyword.
    KwIf,
    /// The `int` keyword.
    KwInt,
    /// The `return` keyword.
    KwReturn,
    /// The `switch` keyword.
    KwSwitch,
    /// The `void` keyword.
    KwVoid,
    /// The `while` keyword.
    KwWhile,
    /// A number.
    Number(i64),
    /// An identifier.
    Identifier(Symbol),
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Amp => write!(f, "'&'"),
            TokenKind::AmpAmp => write!(f, "'&&'"),
            TokenKind::AmpEq => write!(f, "'&='"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Colon => write!(f, "':'"),
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
            TokenKind::Question => write!(f, "'?'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::RBrack => write!(f, "']'"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::Semi => write!(f, "';'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::SlashEq => write!(f, "'/='"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::StarEq => write!(f, "'*='"),
            TokenKind::Tilde => write!(f, "'~'"),
            TokenKind::KwBreak => write!(f, "'break'"),
            TokenKind::KwCase => write!(f, "'case'"),
            TokenKind::KwContinue => write!(f, "'continue'"),
            TokenKind::KwDefault => write!(f, "'default'"),
            TokenKind::KwDo => write!(f, "'do'"),
            TokenKind::KwElse => write!(f, "'else'"),
            TokenKind::KwFor => write!(f, "'for'"),
            TokenKind::KwGoto => write!(f, "'goto'"),
            TokenKind::KwIf => write!(f, "'if'"),
            TokenKind::KwInt => write!(f, "'int'"),
            TokenKind::KwReturn => write!(f, "'return'"),
            TokenKind::KwSwitch => write!(f, "'switch'"),
            TokenKind::KwVoid => write!(f, "'void'"),
            TokenKind::KwWhile => write!(f, "'while'"),
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
