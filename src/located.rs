// Nom 4.0 implementation of nom_located.
// TODO: Separate out the CompleteStr implementations.

use std;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::str::{CharIndices, Chars, FromStr};

use memchr;
use memchr::Memchr;
use nom::{AsBytes, AtEof, Compare, CompareResult, FindSubstring, FindToken, InputIter,
          InputLength, InputTake, Offset, ParseTo, Slice};
use nom::types::CompleteStr;
use bytecount::{naive_num_chars, num_chars};

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

    fn get_columns_and_bytes_before(&self) -> (usize, &[u8]) {
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

        (column, &before_self[self.offset - (column - 1)..])
    }

    /// Return the column index, assuming 1 byte = 1 column.
    ///
    /// Use it for ascii text, or use get_utf8_column for UTF8.
    pub fn get_column(&self) -> usize {
        self.get_columns_and_bytes_before().0
    }

    /// Return the column index for UTF8 text. Return value is unspecified for non-utf8 text.
    ///
    /// This version uses bytecount's hyper algorithm to count characters. This is much faster
    /// for long lines, but is non-negligibly slower for short slices (below around 100 bytes).
    /// This is also sped up significantly more depending on architecture and enabling the simd
    /// feature gates. If you expect primarily short lines, you may get a noticeable speedup in
    /// parsing by using `naive_get_utf8_column` instead. Benchmark your specific use case!
    pub fn get_utf8_column(&self) -> usize {
        let before_self = self.get_columns_and_bytes_before().1;
        num_chars(before_self) + 1
    }

    /// Return the column index for UTF8 text. Return value is unspecified for non-utf8 text.
    ///
    /// A simpler implementation of `get_utf8_column` that may be faster on shorter lines.
    /// If benchmarking shows that this is faster, you can use it instead of `get_utf8_column`.
    /// Prefer defaulting to `get_utf8_column` unless this legitimately is a performance bottleneck.
    pub fn naive_get_utf8_column(&self) -> usize {
        let before_self = self.get_columns_and_bytes_before().1;
        naive_num_chars(before_self) + 1
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
