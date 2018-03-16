use nom::{Context, Err as NomErr, ErrorKind as NomErrorKind};

/// Errors from parsing.
#[derive(Debug, PartialEq)]
pub enum ParseError {
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
    /// Dot (`.`) was required but missing.
    ExpectedDot = 5,
    /// Unknown error code passed from a parser. Should not be returned.
    Unknown = 999,
}

// Required implementation for fix_error, used to make nom integration less painful.
impl From<u32> for ParseError {
    /// Returns the ParseError for the given code.
    fn from(i: u32) -> Self {
        match i {
            0 => ParseError::BadKey,
            1 => ParseError::UnterminatedString,
            2 => ParseError::ExpectedNewline,
            3 => ParseError::ExpectedEquals,
            4 => ParseError::ExpectedMapStart,
            5 => ParseError::ExpectedDot,
            _ => ParseError::Unknown,
        }
    }
}

impl ParseError {
    /// Convert the parse error to a nom error.
    pub fn to_err<I, T>(self, input: T) -> Result<I, NomErr<T, ParseError>> {
        Err(NomErr::Error(
            Context::Code(input, NomErrorKind::Custom(self)),
        ))
    }

    /// Convert the parse error to a nom failure.
    // TODO(jkinkead): Delete.
    pub fn to_fail_old<I, T>(self, input: T) -> Result<I, NomErr<T, ParseError>> {
        Err(NomErr::Failure(
            Context::Code(input, NomErrorKind::Custom(self)),
        ))
    }

    /// Convert the parse error to a nom failure.
    pub fn to_fail<I, T>(self, input: T) -> Result<I, NomErr<T>> {
        Err(NomErr::Failure(
            Context::Code(input, NomErrorKind::Custom(self as u32)),
        ))
    }
}
