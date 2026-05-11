//! Errors that can occur during command line argument parsing.

/// An error caused by parsing the command line arguments.
#[derive(Clone, Debug)]
pub enum Error {
    /// An argument could not be decoded as UTF-8.
    NonUtf8Arg,
    /// A required free-standing argument was not present.
    MissingArg,
    /// An unrecognized argument was found.
    UnexpectedArgument(String),
    /// An option was found but not followed by a value.
    OptionWithoutValue(&'static str),
    /// An argument was found but the parse function failed.
    Utf8ArgParsingFailed {
        /// The error returned by the parse function.
        cause: String,
        /// The raw argument string that was passed to the parse function.
        value: String,
    },
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::NonUtf8Arg => write!(f, "argument is not a UTF-8 string"),
            Error::MissingArg => write!(f, "free-standing argument is missing"),
            Error::UnexpectedArgument(arg) => write!(f, "unexpected argument '{arg}'"),
            Error::OptionWithoutValue(key) => write!(f, "the '{key}' option requires a value"),
            Error::Utf8ArgParsingFailed { value, cause } => {
                write!(f, "failed to parse '{value}': {cause}")
            }
        }
    }
}
