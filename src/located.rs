// Nom 4.0 implementation of nom_located.
// TODO: Separate out the CompleteStr implementations.

use std;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::str::{CharIndices, Chars, FromStr};

use bytecount;
use memchr;
use memchr::Memchr;
use nom::{AsBytes, AtEof, Compare, CompareResult, FindSubstring, FindToken, InputIter,
          InputLength, InputTake, Offset, ParseTo, Slice, UnspecializedInput};
use nom::types::CompleteStr;

/// A LocatedSpan is a set of meta information about the location of a token.
///
/// The `LocatedSpan` structure can be used as an input of the nom parsers.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct LocatedSpan<T> {
    /// The offset represents the position of the fragment relatively to
    /// the input of the parser. It starts at offset 0.
    pub offset: usize,

    /// The line number of the fragment relatively to the input of the
    /// parser. It starts at line 1.
    pub line: u32,

    /// The fragment that is spanned.
    /// The fragment represents a part of the input of the parser.
    pub fragment: T,
}

impl<T: AsBytes> LocatedSpan<T> {
    /// Create a span for a particular input with default `offset` and
    /// `line` values. You can compute the column through the `get_column` or `get_utf8_column`
    /// methods.
    ///
    /// `offset` starts at 0, `line` starts at 1, and `column` starts at 1.
    pub fn new(program: T) -> LocatedSpan<T> {
        LocatedSpan {
            line: 1,
            offset: 0,
            fragment: program,
        }
    }

    /// Returns the column index and the full current line, excluding newlines.
    pub fn get_column_and_line(&self) -> (usize, String) {
        let self_bytes = self.fragment.as_bytes();
        let self_ptr = self_bytes.as_ptr();
        let before_self = unsafe {
            assert!(
                self.offset <= isize::max_value() as usize,
                "offset is too big"
            );
            let orig_input_ptr = self_ptr.offset(-(self.offset as isize));
            std::slice::from_raw_parts(orig_input_ptr, self.offset)
        };

        let column = match memchr::memrchr(b'\n', before_self) {
            None => self.offset + 1,
            Some(pos) => self.offset - pos,
        };

        let line_end = match memchr::memchr(b'\n', self_bytes) {
            None => self_bytes.len(),
            Some(pos) => pos,
        };

        let line_start = &before_self[self.offset - (column - 1)..];
        let mut line_text = String::from_utf8(line_start.to_vec()).unwrap();
        line_text.push_str(&String::from_utf8(self_bytes[..line_end].to_vec()).unwrap());

        let char_column = bytecount::naive_num_chars(line_start) + 1;

        (char_column, line_text)
    }
}

impl<T: InputLength> InputLength for LocatedSpan<T> {
    fn input_len(&self) -> usize {
        self.fragment.input_len()
    }
}

impl<'a> InputIter for LocatedSpan<CompleteStr<'a>> {
    type Item = char;
    type RawItem = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;
    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.fragment.iter_indices()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.fragment.iter_elements()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::RawItem) -> bool,
    {
        self.fragment.position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        self.fragment.slice_index(count)
    }
}

#[macro_export]
macro_rules! impl_compare {
    ( $fragment_type:ty, $compare_to_type:ty ) => {
        impl<'a,'b> Compare<$compare_to_type> for LocatedSpan<$fragment_type> {
            #[inline(always)]
            fn compare(&self, t: $compare_to_type) -> CompareResult {
                self.fragment.compare(t)
            }

            #[inline(always)]
            fn compare_no_case(&self, t: $compare_to_type) -> CompareResult {
                self.fragment.compare_no_case(t)
            }
        }
    }
}

impl_compare!(CompleteStr<'a>, &'a str);

macro_rules! impl_slice_range {
    ( $fragment_type:ty, $range_type:ty ) => {
        impl<'a> Slice<$range_type> for LocatedSpan<$fragment_type> {
            fn slice(&self, range: $range_type) -> Self {
                let next_fragment = self.fragment.slice(range);
                self.new_from_fragment(next_fragment)
            }
        }
    }
}

#[macro_export]
macro_rules! impl_slice_ranges {
    ( $fragment_type:ty ) => {
        impl<'a> LocatedSpan<$fragment_type> {
          fn new_from_fragment(&self, next_fragment: $fragment_type) -> Self {
            if next_fragment == self.fragment {
              return *self;
            }
            let consumed_len = self.fragment.offset(&next_fragment);
            if consumed_len == 0 {
              return LocatedSpan {
                line: self.line,
                offset: self.offset,
                fragment: next_fragment
              };
            }

            let consumed = self.fragment.slice(..consumed_len);
            let next_offset = self.offset + consumed_len;

            let consumed_as_bytes = consumed.as_bytes();
            let iter = Memchr::new(b'\n', consumed_as_bytes);
            let number_of_lines = iter.count() as u32;
            let next_line = self.line + number_of_lines;

            LocatedSpan {
              line: next_line,
              offset: next_offset,
              fragment: next_fragment
            }
          }
        }
        impl_slice_range! {$fragment_type, Range<usize>}
        impl_slice_range! {$fragment_type, RangeTo<usize>}
        impl_slice_range! {$fragment_type, RangeFrom<usize>}
        impl_slice_range! {$fragment_type, RangeFull}
    }
}

impl_slice_ranges! {CompleteStr<'a>}

impl<'a> FindToken<char> for LocatedSpan<CompleteStr<'a>> {
    fn find_token(&self, token: char) -> bool {
        self.fragment.find_token(token)
    }
}

impl<'a, T> FindSubstring<&'a str> for LocatedSpan<T>
where
    T: FindSubstring<&'a str>,
{
    #[inline]
    fn find_substring(&self, substr: &'a str) -> Option<usize> {
        self.fragment.find_substring(substr)
    }
}

impl<T: AtEof> AtEof for LocatedSpan<T> {
    #[inline]
    fn at_eof(&self) -> bool {
        self.fragment.at_eof()
    }
}

impl<R: FromStr, T> ParseTo<R> for LocatedSpan<T>
where
    T: ParseTo<R>,
{
    #[inline]
    fn parse_to(&self) -> Option<R> {
        self.fragment.parse_to()
    }
}

impl<'a> Offset for LocatedSpan<CompleteStr<'a>> {
    fn offset(&self, second: &LocatedSpan<CompleteStr>) -> usize {
        self.fragment.offset(&second.fragment)
    }
}

// NOTE: This requires the new_from_fragment implementation generated by impl_slice_ranges.
impl<'a> InputTake for LocatedSpan<CompleteStr<'a>> {
    fn take(&self, count: usize) -> Self {
        let next_fragment = self.fragment.take(count);

        self.new_from_fragment(next_fragment)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left_fragment, right_fragment) = self.fragment.take_split(count);

        // Confusingly, this returns the taken items on the right, remaining on the left.
        let right_span = LocatedSpan {
            line: self.line,
            offset: self.offset,
            fragment: right_fragment,
        };
        let left_span = self.new_from_fragment(left_fragment);
        (left_span, right_span)
    }
}

impl<'a> ToString for LocatedSpan<CompleteStr<'a>> {
    fn to_string(&self) -> String {
        self.fragment.0.to_string()
    }
}

impl<'a> UnspecializedInput for LocatedSpan<CompleteStr<'a>> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_get_column_and_line_works() {
        let base = "foo\nbar\ngaz\n";
        let span = LocatedSpan {
            fragment: CompleteStr(&base[9..]),
            offset: 9,
            line: 3,
        };
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
        let span = LocatedSpan {
            fragment: CompleteStr(&base[2..]),
            offset: 2,
            line: 1,
        };
        let (col, line) = span.get_column_and_line();
        assert_eq!(col, 3, "get_column_and_line returned bad column");
        assert_eq!(
            line,
            String::from("fooble"),
            "get_column_and_line returned line"
        );
    }
}
