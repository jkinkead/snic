//! Types used in parsing a config document. This is what the first-pass parser generates.

use parser::input::Span;

/// A full config document for a single file.
pub struct Document<'a> {
    pub imports: Vec<Import<'a>>,
    pub statements: Vec<Statement<'a>>,
}

/// A sequence of key segments or other tokens. Length-one unquoted keys will be translated into
/// literal (true, false, undefined) values as needed.
#[derive(Debug, PartialEq)]
pub struct ConfigKeyLike<'a> {
    pub segments: Vec<ConfigKeySegment<'a>>,
}

/// A segment of a key.
#[derive(Debug, PartialEq)]
pub enum ConfigKeySegment<'a> {
    /// A raw quoted key, with backticks intact.
    Quoted(Span<'a>),
    /// An unquoted key segment.
    Unquoted(Span<'a>),
}

impl<'a> ConfigKeySegment<'a> {
    pub fn span(&self) -> &Span {
        match self {
            &ConfigKeySegment::Quoted(ref span) => span,
            &ConfigKeySegment::Unquoted(ref span) => span,
        }
    }
}

/// Import line in a document.
pub struct Import<'a> {
    /// Path to import.
    pub filename: Span<'a>,
    /// Optional key to import into.
    pub target: Option<ConfigKeyLike<'a>>,
}

/// A statement in a config file.
#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Assignment {
        target: ConfigKeyLike<'a>,
        value: RawConfigValue<'a>,
    },
    Template {
        name: ConfigKeyLike<'a>,
        value: RawConfigMap<'a>,
    },
}

#[derive(Debug, PartialEq)]
pub struct RawConfigMap<'a> {
    /// In-order parsed values.
    pub values: Vec<Statement<'a>>,
    /// The parent template, if there is one.
    pub template: Option<ConfigKeyLike<'a>>,
}

/// A config value as parsed from a config file. This may include unresolved references, templates,
/// and uninterpolated strings.
#[derive(Debug, PartialEq)]
pub enum RawConfigValue<'a> {
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
    /// A reference to another key, or a literal token.
    RefLike(ConfigKeyLike<'a>),
    /// True literal.
    True(Span<'a>),
    /// False literal.
    False(Span<'a>),
    /// Undefined literal.
    Undefined(Span<'a>),
}
