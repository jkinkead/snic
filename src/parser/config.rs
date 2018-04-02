//! The main value parsers for the first-pass parser. This contains rules for building non-scalar
//! config types, as well as processing syntax items.

use input::Span;
use parser::scalars;
use parser::tokens;
use parser::types::{ConfigKeyLike, ConfigKeySegment, RawConfigMap, RawConfigValue, Statement};

/// Parses a single config key segment.
named!(config_key_segment<Span, ConfigKeySegment>,
    alt!(map!(tokens::id_token, |token| ConfigKeySegment::Unquoted(token)) |
        map!(call!(tokens::escaped_chars, '`'), |quoted| ConfigKeySegment::Quoted(quoted))));

/// Parses a ConfigKeyLike.
named!(pub config_key_like<Span, ConfigKeyLike>, map!(separated_nonempty_list!(
        tuple!(tokens::opt_whitespace, char!('.'), tokens::opt_whitespace),
        config_key_segment
    ),
    |segments| ConfigKeyLike{segments}));


/// Parses a RefLike value (key reference or single-token literal).
named!(ref_like<Span, RawConfigValue>, map!(
    config_key_like,
    |value| RawConfigValue::RefLike(value)));

named!(template<Span, Statement>, do_parse!(
        tag!("template") >> value!((), tokens::whitespace) >>
        name: config_key_like >>
        tuple!(tokens::opt_whitespace, tokens::req_equals, tokens::opt_whitespace) >>
        value: alt!(map_with_template | map_no_template) >>
        (Statement::Template{name, value})));

named!(assignment<Span, Statement>, do_parse!(
        target: config_key_like >>
        // tuple! call is to avoid do_parse! finickiness.
        tuple!(tokens::opt_whitespace, tokens::req_equals, tokens::opt_whitespace) >>
        value: req_config_value >>
        (Statement::Assignment{target, value})));

named!(pub statement<Span, Statement>, alt!(assignment | template));

/// Parses a literal "from" followed by a config key, returning the key.
named!(from_key<Span, ConfigKeyLike>, do_parse!(
    // value! call is to avoid do_parse! finickiness.
    tag!("from") >> value!((), tokens::whitespace) >> value: config_key_like >> (value)
    ));

/// Parses a map literal with a "from" clause.
named!(map_with_template<Span, RawConfigMap>, do_parse!(
        template: from_key >>
        tuple!(tokens::opt_whitespace, tokens::req_map_start, tokens::opt_whitespace) >>
        values: separated_list!(tokens::whitespace_with_newline, statement) >>
        tuple!(tokens::opt_whitespace, tokens::req_map_end) >>
        (RawConfigMap{template: Some(template), values})));

/// Parses a map literal without a "from" clause.
named!(map_no_template<Span, RawConfigMap>, do_parse!(
        char!('{') >> value!((), tokens::opt_whitespace) >>
        values: separated_list!(tokens::whitespace_with_newline, statement) >>
        tuple!(tokens::opt_whitespace, tokens::req_map_end) >>
        (RawConfigMap{template: None, values})));

/// Parses a map value as a RawConfigValue.
named!(map_config_value<Span, RawConfigValue>, do_parse!(
        map_value: alt!(map_with_template | map_no_template) >>
        (RawConfigValue::Map(map_value))));

/// Parses a list literal.
named!(config_list<Span, RawConfigValue>, do_parse!(
        char!('[') >> value!((), tokens::opt_whitespace) >>
        values: separated_list!(tuple!(tokens::opt_whitespace, char!(','), tokens::opt_whitespace),
            config_value) >>
        tuple!(tokens::opt_whitespace, opt!(char!(',')), tokens::req_list_end) >>
        (RawConfigValue::List{values})));

/// Parses a config value. Valid for statement values, or values in a list.
named!(config_value<Span, RawConfigValue>,
    // NOTE: This MUST have map_config_value before ref_like, or `from` parsing will fail.
    alt!(map_config_value | config_list | scalars::number | scalars::string | ref_like));

/// Parses a config value. Valid for statement values, or values in a list. Fails with
/// ExpectedValue if item is not a config value.
named!(req_config_value<Span, RawConfigValue>,
    return_error!(
        // Note that the macro imports ErrorKind from nom, so we need to fully-qualify the parser
        // symbol below. We also use an un-qualified nom ErrorKind to suppress a compiler warning.
        ErrorKind::Custom(super::errors::ErrorKind::ExpectedValue as u32), config_value));

#[cfg(test)]
mod tests {
    use nom::{Context, Err as NomErr, ErrorKind as NomErrorKind};
    use nom::types::CompleteStr;

    use super::*;
    use parser::errors::ErrorKind;

    #[test]
    fn config_key_segment_ok_unquoted() {
        let result = config_key_segment(Span::from("fooey bar"));
        let ok_val = result.expect("unquoted token should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(" bar"));
        assert_eq!(ok_val.1, ConfigKeySegment::Unquoted(Span::from("fooey")));
    }

    #[test]
    fn config_key_segment_ok_quoted() {
        let result = config_key_segment(Span::from("`foo`.ey bar"));
        let ok_val = result.expect("quoted token should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(".ey bar"));
        assert_eq!(ok_val.1, ConfigKeySegment::Quoted(Span::from("`foo`")));
    }

    #[test]
    fn ref_like_ok() {
        let result = ref_like(Span::from("`foo`.ey . \nbar gaz"));
        let ok_val = result.expect("ref like key should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(" gaz"));
        assert_eq!(
            ok_val.1,
            RawConfigValue::RefLike(ConfigKeyLike {
                segments: vec![
                    ConfigKeySegment::Quoted(Span::from("`foo`")),
                    ConfigKeySegment::Unquoted(Span::from_values("ey", 6, 1)),
                    ConfigKeySegment::Unquoted(Span::from_values("bar", 12, 2)),
                ],
            })
        );
    }

    #[test]
    fn ref_like_err() {
        let input = Span::from("123.ey");
        let result = ref_like(input);
        assert_eq!(
            result,
            Err(NomErr::Error(Context::Code(input, NomErrorKind::Alt)))
        );
    }

    #[test]
    fn assignment_ok_int_value() {
        let result = assignment(Span::from("my.`key`= \n123\n"));
        let ok_val = result.expect("assignment should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr("\n"));
        assert_eq!(
            ok_val.1,
            Statement::Assignment {
                target: ConfigKeyLike {
                    segments: vec![
                        ConfigKeySegment::Unquoted(Span::from("my")),
                        ConfigKeySegment::Quoted(Span::from_values("`key`", 3, 1)),
                    ],
                },
                value: RawConfigValue::Integer(Span::from_values("123", 11, 2)),
            }
        );
    }

    #[test]
    fn assignment_err_no_equals() {
        let result = assignment(Span::from("foo.bar 123"));
        assert_eq!(
            result,
            ErrorKind::ExpectedEquals.to_fail(Span::from_values("123", 8, 1))
        );
    }

    #[test]
    fn assignment_err_bad_value() {
        let result = assignment(Span::from("foo.bar = = true"));
        assert_eq!(
            result,
            ErrorKind::ExpectedValue.to_fail(Span::from_values("= true", 10, 1))
        );
    }

    #[test]
    fn from_key_ok() {
        let result = from_key(Span::from("from \n`foo`.bar {"));
        let ok_val = result.expect("from expression should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(" {"));
        assert_eq!(
            ok_val.1,
            ConfigKeyLike {
                segments: vec![
                    ConfigKeySegment::Quoted(Span::from_values("`foo`", 6, 2)),
                    ConfigKeySegment::Unquoted(Span::from_values("bar", 12, 2)),
                ],
            }
        );
    }

    #[test]
    fn from_key_err_requires_whitespace() {
        let input = Span::from("fromfoo.bar");
        let result = from_key(input);
        assert_eq!(
            result,
            Err(NomErr::Error(Context::Code(
                Span::from_values("foo.bar", 4, 1),
                NomErrorKind::Many1,
            )))
        );
    }

    #[test]
    fn map_with_template_ok() {
        let result = map_with_template(Span::from("from `foo`.bar { a = 1 }"));
        let ok_val = result.expect("map with template should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(
            ok_val.1,
            RawConfigMap {
                template: Some(ConfigKeyLike {
                    segments: vec![
                        ConfigKeySegment::Quoted(Span::from_values("`foo`", 5, 1)),
                        ConfigKeySegment::Unquoted(Span::from_values("bar", 11, 1)),
                    ],
                }),
                values: vec![
                    Statement::Assignment {
                        target: ConfigKeyLike {
                            segments: vec![
                                ConfigKeySegment::Unquoted(Span::from_values("a", 17, 1)),
                            ],
                        },
                        value: RawConfigValue::Integer(Span::from_values("1", 21, 1)),
                    },
                ],
            }
        );
    }

    #[test]
    fn map_no_template_ok() {
        let result = map_no_template(Span::from("{ a = 1\nb\n=\n2\n}"));
        let ok_val = result.expect("map with template should have been parsed");
        assert_eq!(ok_val.0.fragment, CompleteStr(""));
        assert_eq!(
            ok_val.1,
            RawConfigMap {
                template: None,
                values: vec![
                    Statement::Assignment {
                        target: ConfigKeyLike {
                            segments: vec![
                                ConfigKeySegment::Unquoted(Span::from_values("a", 2, 1)),
                            ],
                        },
                        value: RawConfigValue::Integer(Span::from_values("1", 6, 1)),
                    },
                    Statement::Assignment {
                        target: ConfigKeyLike {
                            segments: vec![
                                ConfigKeySegment::Unquoted(Span::from_values("b", 8, 2)),
                            ],
                        },
                        value: RawConfigValue::Integer(Span::from_values("2", 12, 4)),
                    },
                ],
            }
        );
    }
}
