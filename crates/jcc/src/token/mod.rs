pub mod lex;

use jcc_ssa::codemap::span::Span;

// ---------------------------------------------------------------------------
// Token
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

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
    /// The `extern` keyword.
    KwExtern,
    /// The `for` keyword.
    KwFor,
    /// The `goto` keyword.
    KwGoto,
    /// The `if` keyword.
    KwIf,
    /// The `int` keyword.
    KwInt,
    /// The `long` keyword.
    KwLong,
    /// The `return` keyword.
    KwReturn,
    /// The `signed` keyword.
    KwSigned,
    /// The `static` keyword.
    KwStatic,
    /// The `switch` keyword.
    KwSwitch,
    /// The `unsigned` keyword.
    KwUnsigned,
    /// The `void` keyword.
    KwVoid,
    /// The `while` keyword.
    KwWhile,
    /// A regular integer literal.
    NumInt,
    /// A long integer literal (e.g., 123L).
    NumLong,
    /// An unsigned integer literal (e.g. 123u).
    NumUInt,
    /// An unsigned long integer literal (e.g. 123ul).
    NumULong,
    /// An identifier.
    Identifier,
}

impl TokenKind {
    pub fn is_decl_start(self) -> bool {
        matches!(
            self,
            Self::KwInt
                | Self::KwLong
                | Self::KwVoid
                | Self::KwStatic
                | Self::KwExtern
                | Self::KwSigned
                | Self::KwUnsigned
        )
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::NumInt
            | TokenKind::NumLong
            | TokenKind::NumUInt
            | TokenKind::NumULong
            | TokenKind::Identifier => f.write_str(self.as_str()),
            _ => write!(f, "`{}`", self.as_str()),
        }
    }
}

impl TokenKind {
    pub const fn as_str(&self) -> &'static str {
        match self {
            TokenKind::Amp => "&",
            TokenKind::AmpAmp => "&&",
            TokenKind::AmpEq => "&=",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Bang => "!",
            TokenKind::BangEq => "!=",
            TokenKind::Caret => "^",
            TokenKind::CaretEq => "^=",
            TokenKind::Eq => "=",
            TokenKind::EqEq => "==",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::GtGt => ">>",
            TokenKind::GtGtEq => ">>=",
            TokenKind::LBrace => "{{",
            TokenKind::LBrack => "[",
            TokenKind::LParen => "(",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::LtLt => "<<",
            TokenKind::LtLtEq => "<<=",
            TokenKind::Minus => "-",
            TokenKind::MinusEq => "-=",
            TokenKind::MinusMinus => "--",
            TokenKind::Percent => "%",
            TokenKind::PercentEq => "%=",
            TokenKind::Pipe => "|",
            TokenKind::PipeEq => "|=",
            TokenKind::PipePipe => "||",
            TokenKind::Plus => "+",
            TokenKind::PlusEq => "+=",
            TokenKind::PlusPlus => "++",
            TokenKind::Question => "?",
            TokenKind::RBrace => "}}",
            TokenKind::RBrack => "]",
            TokenKind::RParen => ")",
            TokenKind::Semi => ";",
            TokenKind::Slash => "/",
            TokenKind::SlashEq => "/=",
            TokenKind::Star => "*",
            TokenKind::StarEq => "*=",
            TokenKind::Tilde => "~",
            TokenKind::KwBreak => "break",
            TokenKind::KwCase => "case",
            TokenKind::KwContinue => "continue",
            TokenKind::KwDefault => "default",
            TokenKind::KwDo => "do",
            TokenKind::KwElse => "else",
            TokenKind::KwExtern => "extern",
            TokenKind::KwFor => "for",
            TokenKind::KwGoto => "goto",
            TokenKind::KwIf => "if",
            TokenKind::KwInt => "int",
            TokenKind::KwLong => "long",
            TokenKind::KwReturn => "return",
            TokenKind::KwSigned => "signed",
            TokenKind::KwStatic => "static",
            TokenKind::KwSwitch => "switch",
            TokenKind::KwUnsigned => "unsigned",
            TokenKind::KwVoid => "void",
            TokenKind::KwWhile => "while",
            TokenKind::NumInt => "a number",
            TokenKind::NumLong => "a long number",
            TokenKind::NumUInt => "an unsigned number",
            TokenKind::NumULong => "an unsigned long number",
            TokenKind::Identifier => "an identifier",
        }
    }
}
