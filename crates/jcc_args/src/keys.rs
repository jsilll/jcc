//! Command-line argument keys.
//!
//! A [`Keys`] holds up to two flag strings — one short (`-v`) and one long (`--verbose`) —
//! that refer to the same option. Callers construct one via the [`From`] impls.

/// A pair of command-line flag strings: `[short, long]`.
///
/// The short form uses a single `-` prefix (e.g. `-v`, `-vv`).
/// The long form uses a `--` prefix (e.g. `--verbose`).
/// When only one form is needed, the unused slot is an empty string.
#[derive(Debug, Clone, Copy)]
pub struct Keys([&'static str; 2]);

impl Keys {
    #[inline]
    pub fn as_slice(&self) -> &[&'static str] {
        if self.0[1].is_empty() {
            &self.0[..1]
        } else {
            &self.0
        }
    }
}

impl From<&'static str> for Keys {
    /// Creates a `Keys` from a single short (`-v`) or long (`--verbose`) flag.
    #[inline]
    fn from(v: &'static str) -> Self {
        debug_assert!(v.starts_with('-'), "an argument should start with '-'");
        if !v.starts_with("--") {
            validate_shortflag(v);
        }
        Keys([v, ""])
    }
}

impl From<[&'static str; 2]> for Keys {
    /// Creates a `Keys` from a `[short, long]` pair, e.g. `["-v", "--verbose"]`.
    #[inline]
    fn from(v: [&'static str; 2]) -> Self {
        debug_assert!(v[0].starts_with('-'), "an argument should start with '-'");
        debug_assert!(
            !v[0].starts_with("--"),
            "the first argument should be the short form"
        );
        debug_assert!(
            v[1].starts_with("--"),
            "the second argument should be the long form"
        );
        validate_shortflag(v[0]);
        Keys(v)
    }
}

/// Asserts that `short` is a valid short flag: a single character or a run of the same
/// character repeated (e.g. `-v`, `-vv` for verbosity levels).
fn validate_shortflag(short: &'static str) {
    let mut chars = short[1..].chars();
    if let Some(first) = chars.next() {
        debug_assert!(
            chars.all(|c| c == first),
            "short keys should be a single character or a repeated character (e.g. -v, -vv)"
        );
    }
}
