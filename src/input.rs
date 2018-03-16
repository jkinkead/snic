use located::LocatedSpan;
use nom::types::CompleteStr;

/// The input type used for the parser.
pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

/// Wraps a string slice, for ease of testing.
impl<'a> From<&'a str> for Span<'a> {
    fn from(from: &str) -> Span {
        Span::new(CompleteStr(from))
    }
}

impl<'a> Span<'a> {
    /// Create a new span with the given fields. Useful for testing.
    pub fn from_values(string: &str, offset: usize, line: u32) -> Span {
        Span {
            fragment: CompleteStr(string),
            offset,
            line,
        }
    }
}
