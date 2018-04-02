use nom::{Context, Err as NomErr, ErrorKind as NomErrorKind};

use input::Span;

/// Error during parsing.
pub struct Error<'a> {
    kind: ErrorKind,
    span: Span<'a>,
}

impl<'a> Error<'a> {
    pub fn new(kind: ErrorKind, span: Span) -> Error {
        Error { kind, span }
    }

    /// Returns a short message based on the error kind.
    pub fn short_message(&self) -> String {
        match self.kind {
            ErrorKind::BadKey => String::from("illegal config key"),
            ErrorKind::ExpectedAssignmentOrMapEnd => String::from("expected '}' or assignment"),
            ErrorKind::ExpectedEquals => String::from("expected '='"),
            ErrorKind::ExpectedMapStart => String::from("expected '{'"),
            ErrorKind::ExpectedStatement => String::from("expected start of statement"),
            ErrorKind::ExpectedValue => String::from("expected config value"),
            ErrorKind::ExpectedValueOrListEnd => String::from("expected ']' or config value"),
            ErrorKind::MalformedNumber => String::from("malformed number"),
            ErrorKind::UnterminatedString => String::from("unterminated string"),
            _ => String::from("unknown parse error"),
        }
    }

    /// Returns a detailed message about the location of the error.
    // TODO: Add filename below.
    pub fn location(&self) -> String {
        let (column, line) = self.span.get_column_and_line();
        String::from(format!(
            "at {}:{}\n{}\n{}^\n",
            self.span.line,
            column,
            line,
            " ".repeat(column - 1)
        ))
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
    /// Expected another assignment or a map end.
    ExpectedAssignmentOrMapEnd = 1,
    /// Equals was required but missing.
    ExpectedEquals = 2,
    /// Start-of-map (`{`) was required but missing.
    ExpectedMapStart = 3,
    /// Expected another statement in the file.
    ExpectedStatement = 4,
    /// Expected a value, but got something else.
    ExpectedValue = 5,
    /// Expected another item or a list end.
    ExpectedValueOrListEnd = 6,
    /// Number could not be parsed.
    MalformedNumber = 7,
    /// A string wasn't terminated.
    UnterminatedString = 8,
    /// Unknown error code passed from a parser. Should not be returned.
    Unknown = 999,
}

// Required implementation for fix_error, used to make nom integration less painful.
impl From<u32> for ErrorKind {
    /// Returns the ErrorKind for the given code.
    fn from(i: u32) -> Self {
        match i {
            0 => ErrorKind::BadKey,
            1 => ErrorKind::ExpectedAssignmentOrMapEnd,
            2 => ErrorKind::ExpectedEquals,
            3 => ErrorKind::ExpectedMapStart,
            4 => ErrorKind::ExpectedStatement,
            5 => ErrorKind::ExpectedValue,
            6 => ErrorKind::ExpectedValueOrListEnd,
            7 => ErrorKind::MalformedNumber,
            8 => ErrorKind::UnterminatedString,
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
