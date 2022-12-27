//! Defines the error type used by endfio
//!
use std::fmt::Display;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum EndfError {
    ///
    ///
    ///
    #[error("ENDF: Encountered parsing error:\n  {source}")]
    Parse {
        #[from]
        source: ParseError,
    },

    ///
    /// Encountered an I/O error
    ///
    #[error("ENDF: Encountered I/O error:\n  {source}")]
    IOError {
        #[from]
        source: std::io::Error,
    },
    ///
    ///
    #[error("ENDF: There was problem with the file format:\n  {source}")]
    Format {
        #[from]
        source: FormatError,
    },

    ///
    ///
    ///
    #[error("Unexpected end of iterator when reading data")]
    EndOfSection,

    ///
    /// An error that does not fit any defined category
    ///
    /// A case to catch errors that do not fir any defined category
    ///
    #[error("ENDF: Encountered some error:\n  {source}")]
    Generic { source: Box<dyn std::error::Error> },
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Failed conversion of '{context}' due to: {kind}")]
    Float {
        kind: std::num::ParseFloatError,
        context: String,
    },
    #[error("Failed conversion of '{context}' due to: {kind}")]
    Int {
        kind: std::num::ParseIntError,
        context: String,
    },
}

#[derive(Error, Debug)]
pub enum FormatErrorKind {
    #[error("Line must be {expected} characters in length is: {length}")]
    WrongLineLength { expected: usize, length: usize },
    #[error("Failed to properly parse contol (MAT, MF, MT) entries.")]
    InvalidControl,
    #[error("Invalid range of MAT number: {mat}")]
    InvalidMATRange { mat: i32 },
    #[error("Invalid EVAL, DIST or REV format")]
    InvalidDateFormat,
    #[error("Contains non-ASCII characters")]
    NonASCII,
    #[error("Miscellaneous error. See context for error message")]
    Misc,
}

///
/// Error for a case there was a problem reading the ENDF file
///
///
///
#[derive(Error, Debug)]
pub struct FormatError {
    kind: FormatErrorKind,
    context: String,
}

impl FormatError {
    pub fn new(kind: FormatErrorKind, context: &str) -> Self {
        Self {
            kind,
            context: context.to_string(),
        }
    }

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

impl From<String> for EndfError {
    fn from(value: String) -> Self {
        EndfError::Generic {
            source: value.into(),
        }
    }
}
