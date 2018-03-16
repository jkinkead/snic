//! `tokens` contains parsers for simple syntax items. These produce Span values rather than more
//! complicated data structures.

use parser::errors::ParseError;
use input::Span;

use nom::{ErrorKind as NomErrorKind, IResult};

// Naming conventions:
// req_ prefix means the method will hard-fail if unmatched (nom::Err::Failure).
// opt_ prefix means it will always match, possibly empty.
// No prefix means default backtracking behavior on no match (nom::Err::Error).

/// Matches a single, required character, returning the given error (as a nom failure).
fn req_char(input: Span, to_match: char, if_missing_err: ParseError) -> IResult<Span, Span> {
    use nom::{InputIter, Slice};
    let mut iter = input.iter_elements();
    match iter.next() {
        Some(c) if c == to_match => Ok((input.slice(1..), input.slice(0..1))),
        _ => if_missing_err.to_fail(input),
    }
}

/// Single `=`. Used in assignment and template.
named!(pub req_equals<Span, Span>, call!(req_char, '=', ParseError::ExpectedEquals));

/// Single newline. Used in assignment, template, and import.
named!(pub req_newline<Span, Span>, call!(req_char, '\n', ParseError::ExpectedNewline));

/// Start of a map. Used in template and from.
named!(pub req_map_start<Span, Span>, call!(req_char, '{', ParseError::ExpectedMapStart));

/// Single '.' in ref. Used for "super" and "self" refs (non-static refs).
named!(pub req_dot<Span, Span>, call!(req_char, '.', ParseError::ExpectedDot));

/// Comment, without terminal newlines.
named!(comment<Span, Span>, recognize!(do_parse!(char!('#') >> is_not!("\n") >> (()) )));

/// Generic whitespace, used between most tokens.
named!(pub opt_whitespace<Span, Span>, recognize!(many0!(alt!(
    comment | 
    // Newline whitespace.
    is_a!(" \r\t\n")))));

/// Whitespace with a newline appended. Used to match whitespace at the end of statements.
named!(pub req_whitespace_with_newline<Span, Span>, recognize!(
        do_parse!(opt!(is_a!(" \r\t")) >> opt!(comment) >> req_newline >> (()) )));


#[cfg(test)]
mod tests {
    use nom::types::CompleteStr;

    use super::*;

    #[test]
    fn req_char_ok() {
        let result = req_char(Span::from("ab"), 'a', ParseError::Unknown);
        let ok_val = result.expect("'a' should have matched 'ab'");
        assert_eq!(ok_val.0, Span::from_values("b", 1, 1));
        assert_eq!(ok_val.1, Span::from("a"));
    }

    #[test]
    fn req_char_err() {
        let result = req_char(Span::from("a="), '=', ParseError::ExpectedEquals);
        assert_eq!(result, ParseError::ExpectedEquals.to_fail(Span::from("a=")));
    }

    #[test]
    fn opt_whitespace_comment_ok() {
        let result = opt_whitespace(Span::from("# my comment\n"));
        let ok_val = result.expect("comment should have parsed");
        assert_eq!(ok_val.0, Span::from_values("", 13, 2));
        assert_eq!(ok_val.1, Span::from("# my comment\n"));
    }

    // Tests that a whitespace string composed of all elements is OK.
    #[test]
    fn opt_whitespace_mixed_ok() {
        let result = opt_whitespace(Span::from("\t  \n# this is my comment\nit.is = long"));
        let ok_val = result.expect("whitespace should have parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr("it.is = long"));
        assert_eq!(
            ok_val.1.fragment,
            CompleteStr("\t  \n# this is my comment\n")
        );
    }

    // Tests that a "bad" string still produces a valid match.
    #[test]
    fn opt_whitespace_no_match() {
        let result = opt_whitespace(Span::from("not.good = true"));
        let ok_val = result.expect("non-whitespace should have parsed empty");
        assert_eq!(ok_val.0.fragment, CompleteStr("not.good = true"));
        assert_eq!(ok_val.1.fragment, CompleteStr(""));
    }

    // Tests that line-terminating whitespace matches without a comment.
    #[test]
    fn req_whitespace_with_newline_ok_without_comment() {
        let result = req_whitespace_with_newline(Span::from("   \t\nfoo = 1\n"));
        let ok_val = result.expect("whitespace should have parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr("foo = 1\n"));
        assert_eq!(ok_val.1.fragment, CompleteStr("   \t\n"));
    }

    // Tests that line-terminating whitespace matches with a comment.
    #[test]
    fn req_whitespace_with_newline_ok_with_comment() {
        let result =
            req_whitespace_with_newline(Span::from(" # end-of-line comment\nnext.line = true"));
        let ok_val = result.expect("whitespace should have parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr("next.line = true"));
        assert_eq!(ok_val.1.fragment, CompleteStr(" # end-of-line comment\n"));
    }

    // Tests that whitespace without a newline returns a failure.
    #[test]
    fn req_whitespace_with_newline_err_no_newline() {
        let result = req_whitespace_with_newline(Span::from("  another.statement = true"));
        assert_eq!(
            result,
            ParseError::ExpectedNewline
                .to_fail(Span::from_values("another.statement = true", 2, 1))
        );
    }
}
