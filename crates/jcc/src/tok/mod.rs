pub mod lex;

use jcc_ssa::sourcemap::SourceSpan;

// ---------------------------------------------------------------------------
// Token
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
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
    /// The `static` keyword.
    KwStatic,
    /// The `switch` keyword.
    KwSwitch,
    /// The `void` keyword.
    KwVoid,
    /// The `while` keyword.
    KwWhile,
    /// A regular integer literal.
    IntNumber,
    /// A long integer literal (e.g., 123L).
    LongIntNumber,
    /// An identifier.
    Identifier,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::IntNumber | TokenKind::LongIntNumber | TokenKind::Identifier => {
                f.write_str(self.as_str())
            }
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
            TokenKind::KwStatic => "static",
            TokenKind::KwSwitch => "switch",
            TokenKind::KwVoid => "void",
            TokenKind::KwWhile => "while",
            TokenKind::IntNumber => "a number",
            TokenKind::LongIntNumber => "a long number",
            TokenKind::Identifier => "an identifier",
        }
    }
}
