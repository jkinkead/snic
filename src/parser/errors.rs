use nom::{Context, Err as NomErr, ErrorKind as NomErrorKind};
use std;
use std::io;

use input::Span;

/// Error during parsing.
pub struct Error<'a> {
    kind: ErrorKind,
    span: Span<'a>,
    /// The cause of the error. Currently only set for import IO errors.
    cause: Option<&'a std::error::Error>,
}

impl<'a> From<NomErr<Span<'a>>> for Error<'a> {
    fn from(nom_err: NomErr<Span<'a>>) -> Self {
        match nom_err {
            NomErr::Error(Context::Code(span, NomErrorKind::Custom(kind_code))) |
            NomErr::Failure(Context::Code(span, NomErrorKind::Custom(kind_code))) => {
                let kind = ErrorKind::from(kind_code);
                Error{kind, span, cause: None}
            },
            NomErr::Error(Context::Code(span, _)) | NomErr::Failure(Context::Code(span, _)) => {
                Error{kind: ErrorKind::Unknown, span: Span::from(""), cause: None}
            },
            _ => unreachable!("Incomplete should be impossible with Span"),
        }
    }
}

impl<'a> From<(&'a io::Error, Span<'a>)> for Error<'a> {
    fn from(spanned_error: (&'a io::Error, Span<'a>)) -> Self {
        let (error, span) = spanned_error;
        Error {
            kind: ErrorKind::BadImport,
            span,
            cause: Some(error),
        }
    }
}

/// Kinds of errors encountered in parsing. Used directly to communicate out of nom parsers.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    // NOTE: Values must be kept in sync with From<u32> implementation.
    /// A key was definied with an illegal name.
    BadKey = 0,
    /// A string wasn't terminated.
    UnterminatedString = 1,
    /// Newline was required but missing.
    ExpectedNewline = 2,
    /// Equals was required but missing.
    ExpectedEquals = 3,
    /// Start-of-map (`{`) was required but missing.
    ExpectedMapStart = 4,
    /// Number could not be parsed.
    MalformedNumber = 6,
    /// Expected a value, but got something else.
    ExpectedValue = 7,
    /// Expected another assignment or a map end.
    ExpectedAssignmentOrMapEnd = 8,
    /// Expected another item or a list end.
    ExpectedValueOrListEnd = 9,
    /// Failure resolving import.
    BadImport = 10,
    /// Unknown error code passed from a parser. Should not be returned.
    Unknown = 999,
}

// Required implementation for fix_error, used to make nom integration less painful.
impl From<u32> for ErrorKind {
    /// Returns the ErrorKind for the given code.
    fn from(i: u32) -> Self {
        match i {
            0 => ErrorKind::BadKey,
            1 => ErrorKind::UnterminatedString,
            2 => ErrorKind::ExpectedNewline,
            3 => ErrorKind::ExpectedEquals,
            4 => ErrorKind::ExpectedMapStart,
            6 => ErrorKind::MalformedNumber,
            7 => ErrorKind::ExpectedValue,
            8 => ErrorKind::ExpectedAssignmentOrMapEnd,
            9 => ErrorKind::ExpectedValueOrListEnd,
            10 => ErrorKind::BadImport,
            _ => ErrorKind::Unknown,
        }
    }
}

impl ErrorKind {
    /// Convert the parse error to a nom error.
    pub fn to_err<I, T>(self, input: T) -> Result<I, NomErr<T>> {
        Err(NomErr::Error(
            Context::Code(input, NomErrorKind::Custom(self as u32)),
        ))
    }

    /// Convert the parse error to a nom failure.
    pub fn to_fail<I, T>(self, input: T) -> Result<I, NomErr<T>> {
        Err(NomErr::Failure(
            Context::Code(input, NomErrorKind::Custom(self as u32)),
        ))
    }
}
