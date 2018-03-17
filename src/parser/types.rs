//! Types used in parsing a config document. This is what the first-pass parser generates.

use input::Span;

/// A document, with imports resolved into statements.
pub struct Document<'a> {
    statements: Vec<Statement<'a>>,
}

/// A key, split into segments. Unquoted key segments will be valid keys (won't be value literals).
#[derive(Debug, PartialEq)]
pub struct ConfigKey<'a> {
    pub key: Vec<Span<'a>>,
}

/// A segment of a key.
pub enum ConfigKeySegment<'a> {
    /// A raw quoted key, with backticks intact.
    Quoted(Span<'a>),
    /// An unquoted key segment.
    Unquoted(Span<'a>),
}

/// A statement in a config file.
pub enum Statement<'a> {
    Assignment {
        target: ConfigKey<'a>,
        value: RawConfigValue<'a>,
    },
    Template {
        name: ConfigKey<'a>,
        value: RawConfigMap<'a>,
    },
}

#[derive(Debug, PartialEq)]
pub struct RawConfigMap<'a> {
    /// In-order parsed values.
    values: Vec<(ConfigKey<'a>, RawConfigValue<'a>)>,
    /// The parent template, if there is one.
    template: Option<ConfigKey<'a>>,
}

/// A config value as parsed from a config file. This may include unresolved references, templates,
/// and uninterpolated strings.
#[derive(Debug, PartialEq)]
pub enum RawConfigValue<'a> {
    /// Literal "true".
    True(Span<'a>),
    /// Literal "false".
    False(Span<'a>),
    /// Integer literal, suitable for parsing with i32::from_str.
    Integer(Span<'a>),
    /// Floating-point literal, suitable for parsing with f64::from_str.
    Float(Span<'a>),
    /// Quoted string literal, with escapes intact. Interpolation and escape processing happens
    /// after the first-pass parse.
    String(Span<'a>),
    /// A config map without references resolved or template merged.
    Map(RawConfigMap<'a>),
    /// A list without references resolved.
    List { values: Vec<RawConfigValue<'a>> },
    /// A reference to another key.
    Ref(ConfigKey<'a>),
    /// Literal undefined value, only valid in templates.
    Undefined(Span<'a>),
}
