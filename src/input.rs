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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_column_works() {
        let base = "foo\nbar\ngaz\n";
        let span = Span::from_values(&base[9..], 9, 3);
        assert_eq!(span.get_column(), 2);
        assert_eq!(span.naive_get_utf8_column(), 2);
    }

    #[test]
    fn span_get_column_and_line_works() {
        let base = "foo\nbar\ngaz\n";
        let span = Span::from_values(&base[9..], 9, 3);
        assert_eq!(span.get_column(), 2);
        assert_eq!(span.naive_get_utf8_column(), 2);
        let (col, line) = span.get_column_and_line();
        assert_eq!(col, 2, "get_column_and_line returned bad column");
        assert_eq!(
            line,
            String::from("gaz"),
            "get_column_and_line returned line"
        );
    }

    #[test]
    fn span_get_column_and_line_works_no_newline() {
        let base = "fooble";
        let span = Span::from_values(&base[2..], 2, 1);
        assert_eq!(span.get_column(), 3);
        assert_eq!(span.naive_get_utf8_column(), 3);
        let (col, line) = span.get_column_and_line();
        assert_eq!(col, 3, "get_column_and_line returned bad column");
        assert_eq!(
            line,
            String::from("fooble"),
            "get_column_and_line returned line"
        );
    }
}
