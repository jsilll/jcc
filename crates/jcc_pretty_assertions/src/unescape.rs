use std::borrow::Cow;

pub(crate) fn unescape_debug_str(s: &str) -> Cow<'_, str> {
    match s.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
        None => Cow::Borrowed(s),
        Some(s) if !s.contains('\\') => Cow::Borrowed(s),
        Some(s) => {
            let mut chars = s.chars();
            let mut res = String::with_capacity(s.len());
            while let Some(c) = chars.next() {
                if c == '\\' {
                    match chars.next() {
                        None | Some('\\') => res.push('\\'),
                        Some('"') => res.push('"'),
                        Some('n') => res.push('\n'),
                        Some('r') => res.push('\r'),
                        Some('t') => res.push('\t'),
                        Some('0') => res.push('\0'),
                        Some(c) => {
                            res.push('\\');
                            res.push(c);
                        }
                    }
                } else {
                    res.push(c);
                }
            }
            Cow::Owned(res)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normal() {
        let input = "just_a_string";
        let res = unescape_debug_str(input);
        assert_eq!(res, Cow::Borrowed("just_a_string"));
    }

    #[test]
    fn zero_allocation() {
        let input = "\"hello world\"";
        let res = unescape_debug_str(input);
        assert_eq!(res, Cow::Borrowed("hello world"));
    }

    #[test]
    fn str_escapes() {
        let input = "\"line1\\nline2\\t\\\"quoted\\\"\\\\slash\"";
        let res = unescape_debug_str(input);
        assert_eq!(
            res,
            Cow::<str>::Owned("line1\nline2\t\"quoted\"\\slash".to_owned())
        );
    }
}
