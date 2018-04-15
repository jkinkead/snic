//! Input type for the parser module.

use nom::types::CompleteStr;

use parser::located::LocatedSpan;

/// The input type used for the parser.
pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

/// Wraps a string slice.
impl<'a> From<&'a str> for Span<'a> {
    fn from(from: &str) -> Span {
        Span::new(CompleteStr(from))
    }
}

impl<'a> Span<'a> {
    /// Create a new span with the given fields. Deeply unsafe, so used only in testing.
    #[cfg(test)]
    pub fn from_values(string: &str, offset: usize, line: u32) -> Span {
        Span {
            fragment: CompleteStr(string),
            offset,
            line,
        }
    }

    /// Create a detailed location string for this span.
    pub fn location(&self) -> String {
        let (column, line) = self.get_column_and_line();
        String::from(format!(
            "{}:{}\n{}\n{}^\n",
            self.line,
            column,
            line,
            " ".repeat(column - 1)
        ))
    }

    /// Return the wrapped string.
    pub fn as_str(&self) -> &str {
        self.fragment.0
    }
}
