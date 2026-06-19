use jcc_codemap::span::Span;

use crate::ir::{
    inst::{BinaryOp, FCmpOp, ICmpOp, UnaryOp},
    ty::Ty,
};

// ---------------------------------------------------------------------------
// Token
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

// ---------------------------------------------------------------------------
// TokenKind
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // =========================================================================
    // Declarations
    // =========================================================================
    /// The `@define` keyword.
    Define,
    /// The `@global` keyword.
    Global,

    // =========================================================================
    // Function Calls
    // =========================================================================
    /// The `call` keyword.
    Call,
    /// The `icall` keyword.
    ICall,

    // =========================================================================
    // Comparison Operations
    // =========================================================================
    /// The `icmp` keyword.
    Icmp,
    /// The `fcmp` keyword.
    Fcmp,

    // =========================================================================
    // Unary Operations
    // =========================================================================
    /// The `not` keyword.
    Not,
    /// The `neg` keyword.
    Neg,
    /// The `fneg` keyword.
    FNeg,

    // =========================================================================
    // SSA
    // =========================================================================
    /// The `phi` keyword.
    Phi,
    /// The `param` keyword.
    Param,
    /// The `select` keyword.
    Select,
    /// The `upsilon` keyword.
    Upsilon,

    // =========================================================================
    // Control Flow
    // =========================================================================
    /// The `br` keyword.
    Br,
    /// The `ret` keyword.
    Ret,
    /// The `switch` keyword.
    Switch,
    /// The `default` keyword.
    Default,
    /// The `unreachable` keyword.
    Unreachable,

    // =========================================================================
    // Values
    // =========================================================================
    /// The `null` keyword.
    Null,
    /// The `true` keyword.
    True,
    /// The `false` keyword.
    False,
    /// The `const` keyword.
    Const,
    /// The `zeroinitializer` keyword.
    ZeroInitializer,

    // =========================================================================
    // Memory
    // =========================================================================
    /// The `alloca` keyword.
    Alloca,
    /// The `load` keyword.
    Load,
    /// The `store` keyword.
    Store,
    /// The `gep` keyword.
    Gep,
    /// The `align` keyword.
    Align,
    /// The `addrof` keyword.
    GlobalAddr,

    // =========================================================================
    // Bitwise Operations
    // =========================================================================
    /// The `and` keyword.
    And,
    /// The `or` keyword.
    Or,
    /// The `xor` keyword.
    Xor,

    /// The `shl` keyword.
    Shl,
    /// The `lshr` keyword.
    Lshr,
    /// The `ashr` keyword.
    Ashr,

    // =========================================================================
    // Types
    // =========================================================================
    /// The `void` keyword.
    Void,
    /// The `i1` keyword.
    I1,
    /// The `i8` keyword.
    I8,
    /// The `i16` keyword.
    I16,
    /// The `i32` keyword.
    I32,
    /// The `i64` keyword.
    I64,
    /// The `f32` keyword.
    F32,
    /// The `f64` keyword.
    F64,
    /// The `ptr` keyword.
    Ptr,

    // =========================================================================
    // Punctuation
    // =========================================================================
    /// The `-` token.
    Minus,
    /// The `=` token.
    Assign,
    /// The `,` token.
    Comma,
    /// The `:` token.
    Colon,
    /// The `->` token.
    Arrow,
    /// The `(` token.
    LParen,
    /// The `{` token.
    LBrace,
    /// The `[` token.
    LBrack,
    /// The `)` token.
    RParen,
    /// The `}` token.
    RBrace,
    /// The `]` token.
    RBrack,

    // =========================================================================
    // Binary Arithmetic
    // =========================================================================
    /// The `add` keyword.
    Add,
    /// The `sub` keyword.
    Sub,
    /// The `mul` keyword.
    Mul,

    /// The `sdiv` keyword.
    SDiv,
    /// The `udiv` keyword.
    UDiv,
    /// The `srem` keyword.
    SRem,
    /// The `urem` keyword.
    URem,

    /// The `fadd` keyword.
    FAdd,
    /// The `fsub` keyword.
    FSub,
    /// The `fmul` keyword.
    FMul,
    /// The `fdiv` keyword.
    FDiv,
    /// The `frem` keyword.
    FRem,

    // =========================================================================
    // Conversions
    // =========================================================================
    /// The `trunc` keyword.
    Trunc,
    /// The `ftrunc` keyword.
    FTrunc,
    /// The `bitcast` keyword.
    Bitcast,

    /// The `zext` keyword.
    ZExt,
    /// The `sext` keyword.
    SExt,
    /// The `fext` keyword.
    FExt,

    /// The `fptosi` keyword.
    FpToSi,
    /// The `fptoui` keyword.
    FpToUi,
    /// The `sitofp` keyword.
    SiToFp,
    /// The `uitofp` keyword.
    UiToFp,

    /// The `inttoptr` keyword.
    IntToPtr,
    /// The `ptrtoint` keyword.
    PtrToInt,

    // =========================================================================
    // Integer Comparison Predicates
    // =========================================================================
    /// The `eq` keyword.
    Eq,
    /// The `ne` keyword.
    Ne,

    /// The `slt` keyword.
    Slt,
    /// The `sle` keyword.
    Sle,
    /// The `sgt` keyword.
    Sgt,
    /// The `sge` keyword.
    Sge,

    /// The `ult` keyword.
    Ult,
    /// The `ule` keyword.
    Ule,
    /// The `ugt` keyword.
    Ugt,
    /// The `uge` keyword.
    Uge,

    // =========================================================================
    // Floating-Point Comparison Predicates
    // =========================================================================
    /// The `ord` keyword.
    Ord,
    /// The `uno` keyword.
    Uno,

    /// The `oeq` keyword.
    Oeq,
    /// The `one` keyword.
    One,
    /// The `ogt` keyword.
    Ogt,
    /// The `oge` keyword.
    Oge,
    /// The `olt` keyword.
    Olt,
    /// The `ole` keyword.
    Ole,

    /// The `ueq` keyword.
    Ueq,
    /// The `une` keyword.
    Une,

    // =========================================================================
    // Misc.
    // =========================================================================
    /// The `to` keyword.
    To,
    /// The `noop` keyword.
    Noop,

    // =========================================================================
    // Literals
    // =========================================================================
    /// A floating-point literal.
    Float,
    /// An integer literal.
    Integer,

    // =========================================================================
    // References
    // =========================================================================
    /// An identifier starting with '@'.
    AtIdent,
    /// A value reference.
    ValueRef,
    /// A block reference.
    BlockRef,
    /// A parameter index.
    ParamIndex,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Float
            | TokenKind::AtIdent
            | TokenKind::Integer
            | TokenKind::ValueRef
            | TokenKind::BlockRef
            | TokenKind::ParamIndex => f.write_str(self.as_str()),
            _ => write!(f, "`{}`", self.as_str()),
        }
    }
}

impl TryFrom<TokenKind> for UnaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Not => Ok(Self::Not),
            TokenKind::Neg => Ok(Self::Neg),
            TokenKind::FNeg => Ok(Self::FNeg),
            _ => Err(()),
        }
    }
}

impl TryFrom<TokenKind> for Ty {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::I1 => Ok(Self::I1),
            TokenKind::I8 => Ok(Self::I8),
            TokenKind::I16 => Ok(Self::I16),
            TokenKind::I32 => Ok(Self::I32),
            TokenKind::I64 => Ok(Self::I64),
            TokenKind::F32 => Ok(Self::F32),
            TokenKind::F64 => Ok(Self::F64),
            TokenKind::Ptr => Ok(Self::Ptr),
            TokenKind::Void => Ok(Self::Void),
            _ => Err(()),
        }
    }
}

impl TryFrom<TokenKind> for ICmpOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Eq => Ok(Self::Eq),
            TokenKind::Ne => Ok(Self::Ne),
            TokenKind::Slt => Ok(Self::Lt),
            TokenKind::Sle => Ok(Self::Le),
            TokenKind::Sgt => Ok(Self::Gt),
            TokenKind::Sge => Ok(Self::Ge),
            TokenKind::Ult => Ok(Self::Ult),
            TokenKind::Ule => Ok(Self::Ule),
            TokenKind::Ugt => Ok(Self::Ugt),
            TokenKind::Uge => Ok(Self::Uge),
            _ => Err(()),
        }
    }
}

impl TryFrom<TokenKind> for FCmpOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Ord => Ok(Self::Ord),
            TokenKind::Uno => Ok(Self::Uno),
            TokenKind::Oeq => Ok(Self::Oeq),
            TokenKind::One => Ok(Self::One),
            TokenKind::Ogt => Ok(Self::Ogt),
            TokenKind::Oge => Ok(Self::Oge),
            TokenKind::Olt => Ok(Self::Olt),
            TokenKind::Ole => Ok(Self::Ole),
            TokenKind::Ueq => Ok(Self::Ueq),
            TokenKind::Une => Ok(Self::Une),
            TokenKind::True => Ok(Self::True),
            TokenKind::False => Ok(Self::False),
            _ => Err(()),
        }
    }
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Or => Ok(Self::Or),
            TokenKind::And => Ok(Self::And),
            TokenKind::Xor => Ok(Self::Xor),
            TokenKind::Shl => Ok(Self::Shl),
            TokenKind::Lshr => Ok(Self::Shr),
            TokenKind::Ashr => Ok(Self::AShr),
            TokenKind::Add => Ok(Self::Add),
            TokenKind::Sub => Ok(Self::Sub),
            TokenKind::Mul => Ok(Self::Mul),
            TokenKind::SDiv => Ok(Self::SDiv),
            TokenKind::UDiv => Ok(Self::UDiv),
            TokenKind::SRem => Ok(Self::SRem),
            TokenKind::URem => Ok(Self::URem),
            TokenKind::FAdd => Ok(Self::FAdd),
            TokenKind::FSub => Ok(Self::FSub),
            TokenKind::FMul => Ok(Self::FMul),
            TokenKind::FDiv => Ok(Self::FDiv),
            TokenKind::FRem => Ok(Self::FRem),
            _ => Err(()),
        }
    }
}

impl TokenKind {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Define => "define",
            Self::Global => "global",

            Self::Call => "call",
            Self::ICall => "icall",

            Self::Icmp => "icmp",
            Self::Fcmp => "fcmp",

            Self::Not => "not",
            Self::Neg => "neg",
            Self::FNeg => "fneg",

            Self::Phi => "phi",
            Self::Param => "param",
            Self::Select => "select",
            Self::Upsilon => "upsilon",

            Self::Br => "br",
            Self::Ret => "ret",
            Self::Switch => "switch",
            Self::Default => "default",
            Self::Unreachable => "unreachable",

            Self::Null => "null",
            Self::True => "true",
            Self::False => "false",
            Self::Const => "const",
            Self::ZeroInitializer => "zeroinitializer",

            Self::Alloca => "alloca",
            Self::Load => "load",
            Self::Store => "store",
            Self::Gep => "gep",
            Self::Align => "align",
            Self::GlobalAddr => "global.addr",

            Self::And => "and",
            Self::Or => "or",
            Self::Xor => "xor",
            Self::Shl => "shl",
            Self::Lshr => "lshr",
            Self::Ashr => "ashr",

            Self::Void => "void",
            Self::I1 => "i1",
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Ptr => "ptr",
            Self::To => "to",

            Self::Minus => "-",
            Self::Assign => "=",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::Arrow => "->",
            Self::LParen => "(",
            Self::LBrace => "{",
            Self::LBrack => "[",
            Self::RParen => ")",
            Self::RBrace => "}",
            Self::RBrack => "]",

            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::SDiv => "sdiv",
            Self::UDiv => "udiv",
            Self::SRem => "srem",
            Self::URem => "urem",
            Self::FAdd => "fadd",
            Self::FSub => "fsub",
            Self::FMul => "fmul",
            Self::FDiv => "fdiv",
            Self::FRem => "frem",

            Self::Trunc => "trunc",
            Self::FTrunc => "ftrunc",
            Self::Bitcast => "bitcast",
            Self::ZExt => "zext",
            Self::SExt => "sext",
            Self::FExt => "fext",
            Self::FpToSi => "fptosi",
            Self::FpToUi => "fptoui",
            Self::SiToFp => "sitofp",
            Self::UiToFp => "uitofp",
            Self::IntToPtr => "inttoptr",
            Self::PtrToInt => "ptrtoint",

            Self::Noop => "noop",

            Self::Eq => "eq",
            Self::Ne => "ne",
            Self::Slt => "slt",
            Self::Sle => "sle",
            Self::Sgt => "sgt",
            Self::Sge => "sge",
            Self::Ult => "ult",
            Self::Ule => "ule",
            Self::Ugt => "ugt",
            Self::Uge => "uge",

            Self::Ord => "ord",
            Self::Uno => "uno",
            Self::Oeq => "oeq",
            Self::One => "one",
            Self::Ogt => "ogt",
            Self::Oge => "oge",
            Self::Olt => "olt",
            Self::Ole => "ole",
            Self::Ueq => "ueq",
            Self::Une => "une",

            Self::Float => "a float literal",
            Self::AtIdent => "an '@' identifier",
            Self::Integer => "an integer literal",
            Self::ValueRef => "a value reference",
            Self::BlockRef => "a block reference",
            Self::ParamIndex => "a parameter index",
        }
    }

    pub fn from_keyword(s: &str) -> Option<Self> {
        match s {
            "define" => Some(Self::Define),
            "global" => Some(Self::Global),

            "call" => Some(Self::Call),
            "icall" => Some(Self::ICall),

            "icmp" => Some(Self::Icmp),
            "fcmp" => Some(Self::Fcmp),

            "not" => Some(Self::Not),
            "neg" => Some(Self::Neg),
            "fneg" => Some(Self::FNeg),

            "phi" => Some(Self::Phi),
            "param" => Some(Self::Param),
            "select" => Some(Self::Select),
            "upsilon" => Some(Self::Upsilon),

            "br" => Some(Self::Br),
            "ret" => Some(Self::Ret),
            "switch" => Some(Self::Switch),
            "default" => Some(Self::Default),
            "unreachable" => Some(Self::Unreachable),

            "null" => Some(Self::Null),
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "const" => Some(Self::Const),
            "zeroinitializer" => Some(Self::ZeroInitializer),

            "alloca" => Some(Self::Alloca),
            "load" => Some(Self::Load),
            "store" => Some(Self::Store),
            "gep" => Some(Self::Gep),
            "align" => Some(Self::Align),
            "global.addr" => Some(Self::GlobalAddr),

            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "xor" => Some(Self::Xor),
            "shl" => Some(Self::Shl),
            "lshr" => Some(Self::Lshr),
            "ashr" => Some(Self::Ashr),

            "void" => Some(Self::Void),
            "i1" => Some(Self::I1),
            "i8" => Some(Self::I8),
            "i16" => Some(Self::I16),
            "i32" => Some(Self::I32),
            "i64" => Some(Self::I64),
            "f32" => Some(Self::F32),
            "f64" => Some(Self::F64),
            "ptr" => Some(Self::Ptr),
            "to" => Some(Self::To),

            "add" => Some(Self::Add),
            "sub" => Some(Self::Sub),
            "mul" => Some(Self::Mul),
            "sdiv" => Some(Self::SDiv),
            "udiv" => Some(Self::UDiv),
            "srem" => Some(Self::SRem),
            "urem" => Some(Self::URem),
            "fadd" => Some(Self::FAdd),
            "fsub" => Some(Self::FSub),
            "fmul" => Some(Self::FMul),
            "fdiv" => Some(Self::FDiv),
            "frem" => Some(Self::FRem),

            "trunc" => Some(Self::Trunc),
            "ftrunc" => Some(Self::FTrunc),
            "bitcast" => Some(Self::Bitcast),
            "zext" => Some(Self::ZExt),
            "sext" => Some(Self::SExt),
            "fext" => Some(Self::FExt),
            "fptosi" => Some(Self::FpToSi),
            "fptoui" => Some(Self::FpToUi),
            "sitofp" => Some(Self::SiToFp),
            "uitofp" => Some(Self::UiToFp),
            "inttoptr" => Some(Self::IntToPtr),
            "ptrtoint" => Some(Self::PtrToInt),

            "noop" => Some(Self::Noop),

            "eq" => Some(Self::Eq),
            "ne" => Some(Self::Ne),
            "slt" => Some(Self::Slt),
            "sle" => Some(Self::Sle),
            "sgt" => Some(Self::Sgt),
            "sge" => Some(Self::Sge),
            "ult" => Some(Self::Ult),
            "ule" => Some(Self::Ule),
            "ugt" => Some(Self::Ugt),
            "uge" => Some(Self::Uge),

            "ord" => Some(Self::Ord),
            "uno" => Some(Self::Uno),
            "oeq" => Some(Self::Oeq),
            "one" => Some(Self::One),
            "ogt" => Some(Self::Ogt),
            "oge" => Some(Self::Oge),
            "olt" => Some(Self::Olt),
            "ole" => Some(Self::Ole),
            "ueq" => Some(Self::Ueq),
            "une" => Some(Self::Une),

            _ => None,
        }
    }
}
