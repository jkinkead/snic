//! `tokens` contains parsers for simple syntax items. These produce Span values rather than more
//! complicated data structures.

use input::Span;
use parser::errors::ParseError;

use nom::{Context, Err as NomErr, ErrorKind as NomErrorKind, IResult};

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

/// Start of a map.
named!(pub req_map_start<Span, Span>, call!(req_char, '{', ParseError::ExpectedMapStart));

/// End of a map. Note the error type; this could be a map ending or another assignment.
named!(pub req_map_end<Span, Span>, call!(req_char, '}', ParseError::ExpectedAssignmentOrMapEnd));

/// End of a list. Note the error type; this could be a list ending or another value.
named!(pub req_list_end<Span, Span>, call!(req_char, ']', ParseError::ExpectedValueOrListEnd));

/// Single '.' in ref. Used for "super" and "self" refs (non-static refs).
named!(pub req_dot<Span, Span>, call!(req_char, '.', ParseError::ExpectedDot));

/// Comment, without terminal newlines.
named!(comment<Span, Span>, recognize!(do_parse!(char!('#') >> is_not!("\n") >> (()) )));

/// Single "unit" of whitespace, comment or whitespace string.
named!(whitespace_unit<Span, Span>, alt!(comment | is_a!(" \t\r\n")));

/// Generic non-empty whitespace, used between word tokens.
named!(pub whitespace<Span, Span>, recognize!(many1!(whitespace_unit)));

/// Generic optional whitespace, used between non-word tokens.
named!(pub opt_whitespace<Span, Span>, recognize!(many0!(whitespace_unit)));

/// Whitespace with a newline appended. Used to match whitespace at the end of statements.
named!(pub whitespace_with_newline<Span, Span>, recognize!(
        do_parse!(opt!(is_a!(" \r\t")) >> opt!(comment) >> char!('\n') >> opt_whitespace >>
            (()) )));

/// Matches zero or more escaped characters in a string with the given quote character. This returns
/// the full string span, including escapes and quotes.
pub fn escaped_chars(input: Span, quote: char) -> IResult<Span, Span> {
    use nom::{InputIter, Slice};
    let mut iter = input.iter_indices();
    // Expect start-quote.
    match iter.next() {
        Some((i, c)) if c != quote => {
            return Err(NomErr::Error(Context::Code(input, NomErrorKind::Char)))
        }
        None => return Err(NomErr::Error(Context::Code(input, NomErrorKind::Eof))),
        // Was quote.
        _ => (),
    }
    loop {
        match iter.next() {
            Some((i, c)) if c == quote => {
                return Ok((input.slice((i + 1)..), input.slice(..(i + 1))))
            }
            // Escaped char, skip checking next char for quote or escape.
            Some((_, '\\')) => match iter.next() {
                None => return ParseError::UnterminatedString.to_fail(input),
                // Ok!
                _ => (),
            },
            Some(_) => (),
            None => return ParseError::UnterminatedString.to_fail(input),
        }
    }
}

/// Matches a single alphabetic character.
fn letter1(input: Span) -> IResult<Span, Span> {
    use nom::{InputIter, Slice};
    match input.iter_elements().next() {
        Some(c) if char::is_alphabetic(c) => Ok((input.slice(1..), input.slice(..1))),
        _ => ParseError::BadKey.to_err(input),
    }
}

/// Matches zero or more identifier continuation characters (alphanumeric and _).
fn identifier_cont0(input: Span) -> IResult<Span, Span> {
    use nom::{InputIter, InputLength, Slice};
    let end_pos = input.position(|c| !(char::is_alphanumeric(c) || c == '_'));
    match end_pos {
        Some(p) => Ok((input.slice(p..), input.slice(..p))),
        None => Ok((
            input.slice(input.input_len()..),
            input.slice(..input.input_len()),
        )),
    }
}

/// An identifier token. This can be a key segment or keyword.
named!(pub id_token<Span, Span>, recognize!(tuple!(letter1, identifier_cont0)));

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
    fn whitespace_with_newline_ok_without_comment() {
        let result = whitespace_with_newline(Span::from("   \t\n  foo = 1\n"));
        let ok_val = result.expect("whitespace should have parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr("foo = 1\n"));
        assert_eq!(ok_val.1.fragment, CompleteStr("   \t\n  "));
    }

    // Tests that line-terminating whitespace matches with a comment.
    #[test]
    fn whitespace_with_newline_ok_with_comment() {
        let result = whitespace_with_newline(Span::from(
            " # end-of-line comment\n# nother\n\nnext.line = true",
        ));
        let ok_val = result.expect("whitespace should have parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr("next.line = true"));
        assert_eq!(
            ok_val.1.fragment,
            CompleteStr(" # end-of-line comment\n# nother\n\n")
        );
    }

    // Tests that whitespace without a newline returns an error.
    #[test]
    fn whitespace_with_newline_err_no_newline() {
        let result = whitespace_with_newline(Span::from("  another.statement = true"));
        assert_eq!(
            result,
            Err(NomErr::Error(Context::Code(
                Span::from_values("another.statement = true", 2, 1),
                NomErrorKind::Char
            )))
        );
    }

    #[test]
    fn escaped_chars_ok_with_escapes() {
        let result = escaped_chars(Span::from("\"this is a \\\"string\\\"\"\nnext=true"), '"');
        let ok_val = result.expect("string should parse");
        assert_eq!(ok_val.0.fragment, CompleteStr("\nnext=true"));
        assert_eq!(ok_val.1, Span::from("\"this is a \\\"string\\\"\""));
    }

    #[test]
    fn escaped_chars_err_unterminated() {
        let input = Span::from("\"this had no en");
        let result = escaped_chars(input, '"');
        assert_eq!(
            result,
            Err(NomErr::Failure(Context::Code(
                input,
                NomErrorKind::Custom(ParseError::UnterminatedString as u32)
            )))
        );
    }

    // Tests an unterminated string with an escape at the end.
    #[test]
    fn escaped_chars_err_escape_unterminated() {
        let input = Span::from("\"this escape is bad: \\");
        let result = escaped_chars(input, '"');
        assert_eq!(
            result,
            Err(NomErr::Failure(Context::Code(
                input,
                NomErrorKind::Custom(ParseError::UnterminatedString as u32)
            )))
        );
    }

    #[test]
    fn id_token_ok() {
        let result = id_token(Span::from("foo.bar"));
        let ok_val = result.expect("id should parse");
        assert_eq!(ok_val.0.fragment, CompleteStr(".bar"));
        assert_eq!(ok_val.1, Span::from("foo"));
    }

    #[test]
    fn id_token_err_starting_number() {
        let input = Span::from("1foo");
        let result = id_token(input);
        assert_eq!(result, ParseError::BadKey.to_err(input));
    }
}
