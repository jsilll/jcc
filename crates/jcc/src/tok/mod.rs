pub mod lex;

use jcc_ssa::sourcemap::SourceSpan;

// ---------------------------------------------------------------------------
// Token
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
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
    /// A number.
    Number,
    /// An identifier.
    Identifier,
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
            TokenKind::KwExtern => write!(f, "'extern'"),
            TokenKind::KwFor => write!(f, "'for'"),
            TokenKind::KwGoto => write!(f, "'goto'"),
            TokenKind::KwIf => write!(f, "'if'"),
            TokenKind::KwInt => write!(f, "'int'"),
            TokenKind::KwReturn => write!(f, "'return'"),
            TokenKind::KwStatic => write!(f, "'static'"),
            TokenKind::KwSwitch => write!(f, "'switch'"),
            TokenKind::KwVoid => write!(f, "'void'"),
            TokenKind::KwWhile => write!(f, "'while'"),
            TokenKind::Number => write!(f, "a number"),
            TokenKind::Identifier => write!(f, "an identifier"),
        }
    }
}