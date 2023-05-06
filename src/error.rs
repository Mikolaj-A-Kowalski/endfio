//! Defines the error type used by endfio
//!
use std::fmt::Display;
use thiserror::Error;

///
/// Highest level error from endfio
///
#[derive(Error, Debug)]
pub enum EndfError {
    /// Raised when parsing of the any of the ENDF numbers have failed
    #[error("ENDF: Encountered parsing error:\n  {source}")]
    Parse {
        #[from]
        source: ParseError,
    },

    /// Raised when encountered an I/O error
    #[error("ENDF: Encountered I/O error:\n  {source}")]
    IOError {
        #[from]
        source: std::io::Error,
    },

    /// Raised when there are problems with formatting or data in the file
    #[error("ENDF: There was problem with the file format:\n  {source}")]
    Format {
        #[from]
        source: FormatError,
    },

    /// Raised when reached unexpected end of ENDF section when reading the data
    #[error("Unexpected end of iterator when reading data")]
    EndOfSection,
}

///
/// Covers failed conversions to number from ENDF-formatted string
///
#[derive(Error, Debug)]
pub enum ParseError {
    /// Failed float conversion
    #[error("Failed conversion of '{context}' due to: {kind}")]
    Float {
        kind: std::num::ParseFloatError,
        context: String,
    },
    /// Failed int conversion
    #[error("Failed conversion of '{context}' due to: {kind}")]
    Int {
        kind: std::num::ParseIntError,
        context: String,
    },
}

///
/// Lists all defined types of ENDF file/data fromatting error
///
#[derive(Error, Debug)]
pub enum FormatErrorKind {
    /// Raised when ENDF requires a specific length of a line, but it does not
    /// match the expectation
    #[error("Line must be {expected} characters in length is: {length}")]
    WrongLineLength { expected: usize, length: usize },

    /// Raised if reading of the control characters at the end of an ENDF line
    /// failed
    #[error("Failed to properly parse contol (MAT, MF, MT) entries.")]
    InvalidControl,

    /// Raised when MAT number exceeds its range
    #[error("Invalid range of MAT number: {mat}")]
    InvalidMATRange { mat: i32 },

    /// Raised when
    #[error("Invalid EVAL, DIST or REV format")]
    InvalidDateFormat,
    #[error("Contains non-ASCII characters")]
    NonASCII,

    /// It is used when dealing with very specific errors for which there is
    /// no benefit defining new error category
    #[error("Miscellaneous error. See context for error message")]
    Misc,
}

///
/// Covers a case when there is a problem with the format of endf file or data.
///
#[derive(Error, Debug)]
pub struct FormatError {
    kind: FormatErrorKind,
    context: String,
}

impl FormatError {
    /// Create new instance of the error
    pub fn new(kind: FormatErrorKind, context: &str) -> Self {
        Self {
            kind,
            context: context.to_string(),
        }
    }

    /// Shortcut to create an instance of miscellaneous error
    pub fn misc(context: &str) -> Self {
        Self {
            kind: FormatErrorKind::Misc,
            context: context.to_string(),
        }
    }
}

impl Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{}\n  Error:\n {}\n  ", self.context, self.kind)
    }
}
