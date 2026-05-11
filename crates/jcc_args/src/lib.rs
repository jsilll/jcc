//! A simple command-line argument parser.
//!
//! This crate provides a flexible and easy-to-use API for parsing command-line arguments.
//! It supports flags, options with values, and positional arguments.

pub mod error;
mod keys;

use crate::{error::Error, keys::Keys};

/// A command-line argument parser.
#[derive(Clone, Debug)]
pub struct Args(Vec<std::ffi::OsString>);

impl Args {
    /// Creates a parser from [`env::args_os`].
    ///
    /// The executable path will be removed.
    ///
    /// [`env::args_os`]: https://doc.rust-lang.org/stable/std/env/fn.args_os.html
    pub fn from_env() -> Self {
        Args(std::env::args_os().skip(1).collect())
    }

    /// Checks that all arguments were consumed during parsing.
    ///
    /// Call this after all expected flags and values have been parsed.
    ///
    /// # Errors
    ///
    /// When there are unexpected arguments that were not parsed.
    pub fn finish(self) -> Result<(), Error> {
        match self.0.into_iter().next() {
            None => Ok(()),
            Some(arg) => Err(Error::UnexpectedArgument(
                arg.to_string_lossy().into_owned(),
            )),
        }
    }

    /// Checks that arguments contain a specified flag.
    ///
    /// Searches through all arguments, not only the next one.
    ///
    /// Calling this method "consumes" the flag: if a flag is present `n`
    /// times then the first `n` calls to `contains` for that flag will
    /// return `true`, and subsequent calls will return `false`.
    pub fn contains<A: Into<Keys>>(&mut self, keys: A) -> bool {
        self.index_of(keys.into())
            .map(|(idx, _)| {
                self.0.remove(idx);
            })
            .is_some()
    }

    /// Parses a free-standing argument using `FromStr` trait.
    ///
    /// This is a shorthand for `free_from_fn(FromStr::from_str)`
    pub fn free_from_str<T>(&mut self) -> Result<T, Error>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Display,
    {
        self.free_from_fn(std::str::FromStr::from_str)
    }

    /// Parses a free-standing argument using a specified function.
    ///
    /// Parses the first argument from the list of remaining arguments.
    /// Therefore, it's up to the caller to check if the argument is actually
    /// a free-standing one and not an unused flag/option.
    ///
    /// Sadly, there is no way to automatically check for flag/option.
    /// `-`, `--`, `-1`, `-0.5`, `--.txt` - all of these arguments can have different
    /// meaning depending on the caller requirements.
    ///
    /// Must be used only once for each argument.
    ///
    /// # Errors
    ///
    /// - When argument is not a UTF-8 string.
    /// - When argument parsing failed.
    /// - When argument is not present.
    pub fn free_from_fn<T, E: std::fmt::Display>(
        &mut self,
        f: fn(&str) -> Result<T, E>,
    ) -> Result<T, Error> {
        self.opt_free_from_fn(f)?.ok_or(Error::MissingArg)
    }

    /// Parses an optional key-value pair using `FromStr` trait.
    ///
    /// This is a shorthand for `opt_value_from_fn("--key", FromStr::from_str)`
    pub fn opt_value_from_str<A, T>(&mut self, keys: A) -> Result<Option<T>, Error>
    where
        A: Into<Keys>,
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Display,
    {
        self.opt_value_from_fn(keys, std::str::FromStr::from_str)
    }

    /// Parses multiple key-value pairs into the `Vec` using `FromStr` trait.
    ///
    /// This is a shorthand for `values_from_fn("--key", FromStr::from_str)`
    pub fn values_from_str<A, T>(&mut self, keys: A) -> Result<Vec<T>, Error>
    where
        A: Into<Keys>,
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Display,
    {
        self.values_from_fn(keys, std::str::FromStr::from_str)
    }

    /// Parses an optional free-standing argument using a specified function.
    ///
    /// The same as [`Self::free_from_fn`], but returns `Ok(None)` when argument is not present.
    pub fn opt_free_from_fn<T, E: std::fmt::Display>(
        &mut self,
        f: fn(&str) -> Result<T, E>,
    ) -> Result<Option<T>, Error> {
        if self.0.is_empty() {
            Ok(None)
        } else {
            let value = self.0.remove(0);
            let value = value.to_str().ok_or(Error::NonUtf8Arg)?;
            match f(&value) {
                Ok(value) => Ok(Some(value)),
                Err(e) => Err(Error::Utf8ArgParsingFailed {
                    cause: e.to_string(),
                    value: value.to_string(),
                }),
            }
        }
    }

    /// Parses an optional key-value pair using a specified function.
    ///
    /// Returns `Ok(None)` when the option is not present.
    pub fn opt_value_from_fn<A: Into<Keys>, T, E: std::fmt::Display>(
        &mut self,
        keys: A,
        f: fn(&str) -> Result<T, E>,
    ) -> Result<Option<T>, Error> {
        match self.find_value(keys.into())? {
            None => Ok(None),
            Some((value, idx)) => match f(value) {
                Ok(value) => {
                    self.0.remove(idx);
                    self.0.remove(idx);
                    Ok(Some(value))
                }
                Err(e) => Err(Error::Utf8ArgParsingFailed {
                    cause: e.to_string(),
                    value: value.to_string(),
                }),
            },
        }
    }

    /// Parses multiple key-value pairs into the `Vec` using a specified function.
    ///
    /// This function can be used to parse arguments like:<br>
    /// `--file /path1 --file /path2 --file /path3`<br>
    /// But not `--file /path1 /path2 /path3`.
    ///
    /// Arguments can also be separated: `--file /path1 --some-flag --file /path2`
    ///
    /// This method simply executes [`Self::opt_value_from_fn`] multiple times.
    ///
    /// An empty `Vec` is not an error.
    pub fn values_from_fn<A: Into<Keys>, T, E: std::fmt::Display>(
        &mut self,
        keys: A,
        f: fn(&str) -> Result<T, E>,
    ) -> Result<Vec<T>, Error> {
        let keys = keys.into();
        let mut values = Vec::new();
        loop {
            match self.opt_value_from_fn(keys, f) {
                Ok(None) => break,
                Err(e) => return Err(e),
                Ok(Some(v)) => values.push(v),
            }
        }
        Ok(values)
    }

    fn index_of(&self, keys: Keys) -> Option<(usize, &'static str)> {
        for key in keys.as_slice() {
            if let Some(idx) = self.0.iter().position(|v| v == key) {
                return Some((idx, key));
            }
        }
        None
    }

    fn find_value(&mut self, keys: Keys) -> Result<Option<(&str, usize)>, Error> {
        if let Some((idx, key)) = self.index_of(keys) {
            let value = self.0.get(idx + 1).ok_or(Error::OptionWithoutValue(key))?;
            let value = value.to_str().ok_or(Error::NonUtf8Arg)?;
            Ok(Some((value, idx)))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsString;

    fn args(slice: &[&str]) -> Args {
        Args(slice.iter().map(OsString::from).collect())
    }

    #[test]
    fn finish_empty() {
        let a = args(&[]);
        assert!(a.finish().is_ok());
    }

    #[test]
    fn finish_unexpected() {
        let a = args(&["--unknown"]);
        let err = a.finish().unwrap_err();
        assert!(matches!(err, Error::UnexpectedArgument(s) if s == "--unknown"));
    }

    #[test]
    fn contains_short() {
        let mut a = args(&["-v"]);
        assert!(a.contains("-v"));
        assert!(!a.contains("-v"));
    }

    #[test]
    fn contains_long() {
        let mut a = args(&["--verbose"]);
        assert!(a.contains("--verbose"));
        assert!(!a.contains("--verbose"));
    }

    #[test]
    fn contains_missing() {
        let mut a = args(&["--other"]);
        assert!(!a.contains("--verbose"));
    }

    #[test]
    fn contains_short_or_long() {
        let mut a = args(&["--verbose"]);
        assert!(a.contains(["-v", "--verbose"]));
    }

    #[test]
    fn contains_consumes_one_at_a_time() {
        let mut a = args(&["-v", "-v"]);
        assert!(a.contains("-v"));
        assert!(a.contains("-v"));
        assert!(!a.contains("-v"));
    }

    #[test]
    fn free_from_str_ok() {
        let mut a = args(&["42"]);
        let v = a.free_from_str::<u32>().unwrap();
        assert_eq!(v, 42);
    }

    #[test]
    fn free_from_str_empty() {
        let mut a = args(&[]);
        let err = a.free_from_str::<u32>().unwrap_err();
        assert!(matches!(err, Error::MissingArg));
    }

    #[test]
    fn free_from_str_parse_failure() {
        let mut a = args(&["not-a-number"]);
        let err = a.free_from_str::<u32>().unwrap_err();
        assert!(matches!(err, Error::Utf8ArgParsingFailed { .. }));
    }

    #[test]
    fn opt_free_from_fn_empty() {
        let mut a = args(&[]);
        let v = a.opt_free_from_fn(str::parse::<u32>).unwrap();
        assert!(v.is_none());
    }

    #[test]
    fn opt_value_from_str_absent() {
        let mut a = args(&["--other", "3"]);
        let v = a.opt_value_from_str::<_, u32>("--count").unwrap();
        assert!(v.is_none());
    }

    #[test]
    fn opt_value_from_str_present() {
        let mut a = args(&["--count", "3"]);
        let v = a.opt_value_from_str::<_, u32>("--count").unwrap();
        assert_eq!(v, Some(3));
    }

    #[test]
    fn opt_value_from_str_without_value() {
        let mut a = args(&["--count"]);
        let err = a.opt_value_from_str::<_, u32>("--count").unwrap_err();
        assert!(matches!(err, Error::OptionWithoutValue("--count")));
    }

    #[test]
    fn opt_value_from_str_parse_failure() {
        let mut a = args(&["--count", "abc"]);
        let err = a.opt_value_from_str::<_, u32>("--count").unwrap_err();
        assert!(matches!(err, Error::Utf8ArgParsingFailed { .. }));
    }

    #[test]
    fn values_from_str_empty() {
        let mut a = args(&[]);
        let v: Vec<String> = a.values_from_str("--file").unwrap();
        assert!(v.is_empty());
    }

    #[test]
    fn values_from_str_multiple() {
        let mut a = args(&["--file", "a.txt", "--file", "b.txt"]);
        let v = a.values_from_str::<_, String>("--file").unwrap();
        assert_eq!(v, vec!["a.txt", "b.txt"]);
    }

    #[test]
    fn values_from_str_interleaved() {
        let mut a = args(&["--file", "a.txt", "--verbose", "--file", "b.txt"]);

        let v = a.values_from_str::<_, String>("--file").unwrap();
        assert_eq!(v, vec!["a.txt", "b.txt"]);

        let verbose = a.contains("--verbose");
        assert!(verbose);
    }
}
