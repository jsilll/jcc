pub mod infra;
pub mod ir;

pub use jcc_codemap as codemap;
pub use jcc_interner as interner;

/// The target operating system for which the code is being generated.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetOs {
    /// Linux operating system.
    Linux,
    /// Macos operating system.
    Macos,
}

impl std::str::FromStr for TargetOs {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "linux" => Ok(TargetOs::Linux),
            "macos" => Ok(TargetOs::Macos),
            _ => Err(format!("invalid target OS '{s}'")),
        }
    }
}

impl std::fmt::Display for TargetOs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TargetOs::Linux => write!(f, "linux"),
            TargetOs::Macos => write!(f, "macos"),
        }
    }
}

/// A zero-sized marker type used to distinguish Identifier symbols from other symbols.
///
/// This type is never instantiated at runtime. It is used strictly as a generic
/// parameter for `interner::Symbol<T>` to enforce type safety, ensuring that
/// symbols for identifiers are not accidentally mixed with symbols for other
/// string types (e.g., string literals).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IdentMarker;

/// A lightweight, interned handle for a source code identifier.
///
/// This is a specialized `Symbol` that is guaranteed to represent an identifier
/// (variable name, function name, etc.). It is efficient to copy (4 bytes) and
/// equality checks are O(1).
pub type Ident = interner::Symbol<IdentMarker>;

/// The dedicated interner for source code identifiers.
///
/// Use this interner to store and retrieve the string representations of
/// `Ident` handles. Separating this from other interners (like those for string literals)
/// improves cache locality for identifier lookups.
pub type IdentInterner = interner::Interner<IdentMarker>;
