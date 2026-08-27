//! A custom assertion crate that provides a diff-based `assert_eq!` macro.
//!
//! Unlike the standard library's `assert_eq!`, which prints the raw debug
//! representations of both values sequentially on failure, this crate computes
//! and displays an ANSI-colored text diff. This makes it easier to spot the exact
//! differences between large structs, strings, or collections.

mod diff;
mod fmt;
mod unescape;

/// Asserts that two expressions are equal to each other (using [`PartialEq`]).
///
/// On panic, this macro will print an ANSI-colored diff of the [`Debug`]
/// representations of the two values. This is especially useful for comparing
/// complex, multi-line data structures.
#[macro_export]
macro_rules! assert_eq {
    ($left:expr, $right:expr $(,)?) => {
        $crate::assert_eq!(@internal $left, $right, "")
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
        $crate::assert_eq!(@internal $left, $right, format!(": {}", format_args!($($arg)+)))
    };
    (@internal $left:expr, $right:expr, $msg:expr) => {
        match (&$left, &$right) {
            (left_val, right_val) => {
                $crate::assert_eq_impl(left_val, right_val);
            }
        }
    };
}

/// The underlying assertion implementation that generates and prints the diff.
///
/// This function is called by the [`assert_eq!`] macro when an equality check fails.
/// It formats both values using the alternate debug formatter (`{:#?}`), unescapes
/// string characters (like newlines) to format them cleanly, computes the differences,
/// and panics with an ANSI-colored string.
///
/// *Note: This function is generally not meant to be called directly. Use the
/// [`assert_eq!`] macro instead.*
///
/// # Panics
///
/// This function will always panic unless the two values provided evaluate to true under `l == r`.
#[allow(clippy::panic)]
pub fn assert_eq_impl<T: std::fmt::Debug + PartialEq>(l: &T, r: &T) {
    if l != r {
        let l = format!("{l:#?}");
        let r = format!("{r:#?}");
        let l = unescape::unescape_debug_str(&l);
        let r = unescape::unescape_debug_str(&r);
        let buf_size = l.len() + r.len();
        let mut buf = termcolor::Ansi::new(Vec::with_capacity(buf_size));
        fmt::diff(&mut buf, &l, &r).expect("Failed to format diff");
        panic!(
            "{}",
            String::from_utf8(buf.into_inner()).expect("Failed to convert diff to string")
        );
    }
}
