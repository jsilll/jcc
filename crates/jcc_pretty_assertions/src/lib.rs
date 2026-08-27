mod diff;
mod fmt;
mod unescape;

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
                if !(*left_val == *right_val) {
                    $crate::assert_eq_impl(left_val, right_val);
                }
            }
        }
    };
}

pub fn assert_eq_impl<T: std::fmt::Debug + PartialEq>(l: &T, r: &T) {
    if l != r {
        let l = format!("{:#?}", l);
        let r = format!("{:#?}", r);
        let l = unescape::unescape_debug_str(&l);
        let r = unescape::unescape_debug_str(&r);
        let mut buf = termcolor::Ansi::new(Vec::new());
        fmt::diff(&mut buf, &l, &r).expect("Failed to format diff");
        panic!(
            "{}",
            String::from_utf8(buf.into_inner()).expect("Failed to convert diff to string")
        );
    }
}
