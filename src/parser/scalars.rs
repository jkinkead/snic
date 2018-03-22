//! `scalars` contains parsers for number and string values in a config file. All public methods
//! produce instances of RawConfigValue.

use input::Span;
use parser::errors::ParseError;
use parser::tokens::escaped_chars;
use parser::types::RawConfigValue;

use nom;
use nom::IResult;

// Naming conventions:
// req_ prefix means the method will hard-fail if unmatched (nom::Err::Failure).
// opt_ prefix means it will always match, possibly empty.
// No prefix means default backtracking behavior on no match (nom::Err::Error).

/// Integer portion of a number. This parses a JSON integer, not a rust integer (no octal or hex).
/// Note that this does not enforce any and-of-token requirements.
named!(integer_str<Span, Span>, recognize!(
        tuple!(
            opt!(one_of!("+-")),
            alt!(
                value!((), char!('0')) |
                value!((), tuple!(one_of!("123456789"), nom::digit0))
            )
        )
    ));
named!(integer<Span, RawConfigValue>, do_parse!(
    value: integer_str >> (RawConfigValue::Integer(value))));

/// Exponent of a float.
named!(exponent_str<Span, Span>, recognize!(
    tuple!(one_of!("eE"), opt!(one_of!("+-")), nom::digit1)));

/// Float number, in JSON format. Note that this does not enforce any and-of-token requirements.
named!(float<Span, RawConfigValue>, do_parse!(
    float_str: recognize!(
        alt!(
          // Decimal with optional exponent.
          value!((), tuple!(integer_str, tuple!(char!('.'), nom::digit1, opt!(exponent_str)))) |
          // Integer with required exponent.
          value!((), tuple!(integer_str, exponent_str))
    )) >>
    (RawConfigValue::Float(float_str))
    ));

/// Float or integer. This fails with a malformed number error if the number is followed by a letter
/// or number. This helps clarify some confusing parse errors.
pub fn number(input: Span) -> IResult<Span, RawConfigValue> {
    float(input)
        .or_else(|_| integer(input))
        .and_then(|(rest, value)| {
            use nom::InputIter;
            let mut iter = rest.iter_elements();
            match iter.next() {
                // EOF is fine.
                None => Ok((rest, value)),
                Some(c) if char::is_alphanumeric(c) => ParseError::MalformedNumber.to_fail(input),
                _ => Ok((rest, value)),
            }
        })
}

/// Matches double-quoted string content.
named!(pub string<Span, RawConfigValue>,
    map!(call!(escaped_chars, '"'), |value| RawConfigValue::String(value)));

#[cfg(test)]
mod tests {
    use nom::{Context, Err as NomErr, ErrorKind as NomErrorKind};
    use nom::types::CompleteStr;

    use super::*;

    #[test]
    fn integer_ok_simple() {
        let result = integer(Span::from("123"));
        let ok_val = result.expect("123 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Integer(Span::from("123")));
    }

    #[test]
    fn integer_ok_positive() {
        let result = integer(Span::from("+23"));
        let ok_val = result.expect("+23 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Integer(Span::from("+23")));
    }

    #[test]
    fn integer_ok_negative() {
        let result = integer(Span::from("-23"));
        let ok_val = result.expect("-23 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Integer(Span::from("-23")));
    }

    // Test that integers will only grab the first of a series of zeros.
    #[test]
    fn integer_ok_multiple_zeros() {
        let result = integer(Span::from("0123"));
        let ok_val = result.expect("0123 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr("123"));
        assert_eq!(ok_val.1, RawConfigValue::Integer(Span::from("0")));
    }

    #[test]
    fn float_ok_no_exponent() {
        let result = float(Span::from("1.5"));
        let ok_val = result.expect("1.5 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Float(Span::from("1.5")));
    }

    #[test]
    fn float_ok_decimal_exponent() {
        let result = float(Span::from("1.5e10"));
        let ok_val = result.expect("1.5e10 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Float(Span::from("1.5e10")));
    }

    #[test]
    fn float_ok_integer_exponent() {
        let result = float(Span::from("15e10"));
        let ok_val = result.expect("15e10 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Float(Span::from("15e10")));
    }

    #[test]
    fn float_err_integer() {
        let result = float(Span::from("150"));
        assert!(result.is_err());
    }

    #[test]
    fn number_ok_integer() {
        let result = number(Span::from("-23"));
        let ok_val = result.expect("-23 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Integer(Span::from("-23")));
    }

    #[test]
    fn number_ok_float() {
        let result = number(Span::from("-1.5e10"));
        let ok_val = result.expect("-1.5e10 should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(ok_val.1, RawConfigValue::Float(Span::from("-1.5e10")));
    }

    #[test]
    fn number_ok_newline_ending() {
        let result = number(Span::from("10\nnext=1"));
        let ok_val = result.expect("number should parse");
        assert_eq!(ok_val.0.fragment, CompleteStr("\nnext=1"));
        assert_eq!(ok_val.1, RawConfigValue::Integer(Span::from("10")));
    }

    #[test]
    fn number_err_malformed_has_letter() {
        let input = Span::from("10a\nnext=1");
        let result = number(input);
        assert_eq!(
            result,
            Err(NomErr::Failure(Context::Code(
                input,
                NomErrorKind::Custom(ParseError::MalformedNumber as u32)
            )))
        );
    }

    #[test]
    fn number_err_malformed_two_zeros() {
        let input = Span::from("00\nnext=1");
        let result = number(input);
        assert_eq!(
            result,
            Err(NomErr::Failure(Context::Code(
                input,
                NomErrorKind::Custom(ParseError::MalformedNumber as u32)
            )))
        );
    }
}
