use nom::{Context, Err as NomErr, ErrorKind as NomErrorKind};

use parser::input::Span;

/// Error during first-pass parsing, encapsulating the location the error happened.
pub struct Error<'a> {
    kind: ErrorKind,
    pub span: Span<'a>,
}

impl<'a> Error<'a> {
    pub fn new(kind: ErrorKind, span: Span) -> Error {
        Error { kind, span }
    }

    /// Returns a short message based on the error kind.
    pub fn short_message(&self) -> String {
        match self.kind {
            ErrorKind::BadKey => String::from("illegal config key"),
            ErrorKind::ExpectedAssignment => String::from("expected assignment"),
            ErrorKind::ExpectedAssignmentOrMapEnd => String::from("expected '}' or assignment"),
            ErrorKind::ExpectedEquals => String::from("expected '='"),
            ErrorKind::ExpectedImportOrAssignment => String::from("expected import or assignment"),
            ErrorKind::ExpectedMapStart => String::from("expected '{'"),
            ErrorKind::ExpectedValue => String::from("expected config value"),
            ErrorKind::ExpectedValueOrListEnd => String::from("expected ']' or config value"),
            ErrorKind::MalformedNumber => String::from("malformed number"),
            ErrorKind::UnterminatedString => String::from("unterminated string"),
            ErrorKind::Unknown => String::from("unknown parse error"),
        }
    }
}

impl<'a> From<NomErr<Span<'a>>> for Error<'a> {
    fn from(nom_err: NomErr<Span<'a>>) -> Self {
        match nom_err {
            NomErr::Error(Context::Code(span, NomErrorKind::Custom(kind_code)))
            | NomErr::Failure(Context::Code(span, NomErrorKind::Custom(kind_code))) => {
                let kind = ErrorKind::from(kind_code);
                Error { kind, span }
            }
            NomErr::Error(Context::Code(span, _)) | NomErr::Failure(Context::Code(span, _)) => {
                Error {
                    kind: ErrorKind::Unknown,
                    span,
                }
            }
            _ => unreachable!("Incomplete should be impossible with Span"),
        }
    }
}

/// Kinds of errors encountered in parsing. Used directly to communicate out of nom parsers.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    // NOTE: Values must be kept in sync with From<u32> implementation.
    /// A key was definied with an illegal name.
    BadKey = 0,
    /// Expected another assignment in the file.
    ExpectedAssignment,
    /// Expected another assignment or a map end.
    ExpectedAssignmentOrMapEnd,
    /// Equals was required but missing.
    ExpectedEquals,
    /// Expected another import or assignment.
    ExpectedImportOrAssignment,
    /// Start-of-map (`{`) was required but missing.
    ExpectedMapStart,
    /// Expected a value, but got something else.
    ExpectedValue,
    /// Expected another item or a list end.
    ExpectedValueOrListEnd,
    /// Number could not be parsed.
    MalformedNumber,
    /// A string wasn't terminated.
    UnterminatedString,
    /// Unknown error code passed from a parser. Should not be returned.
    Unknown,
}

// Required implementation for fix_error, used to make nom integration less painful.
impl From<u32> for ErrorKind {
    /// Returns the ErrorKind for the given code.
    fn from(i: u32) -> Self {
        match i {
            0 => ErrorKind::BadKey,
            1 => ErrorKind::ExpectedAssignment,
            2 => ErrorKind::ExpectedAssignmentOrMapEnd,
            3 => ErrorKind::ExpectedEquals,
            4 => ErrorKind::ExpectedImportOrAssignment,
            5 => ErrorKind::ExpectedMapStart,
            6 => ErrorKind::ExpectedValue,
            7 => ErrorKind::ExpectedValueOrListEnd,
            8 => ErrorKind::MalformedNumber,
            9 => ErrorKind::UnterminatedString,
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
