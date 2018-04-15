use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::fs::File;
use std::io::{ErrorKind as IoErrorKind, Read};
use std::path::{Path, MAIN_SEPARATOR};

use config::{ConfigMap, ConfigValue};
use parser::document;
use parser::errors::Error as ParseError;
use parser::input::Span;
use parser::types::{ConfigKeyLike, ConfigKeySegment, RawConfigMap, RawConfigValue, Statement};

#[derive(Debug)]
pub struct Error {
    /// Short message, suitable for printing out.
    pub message: String,
    /// Filename. Set at the top-level.
    filename: Option<String>,
    /// Location message, if the error came from parsing.
    location: Option<String>,
}

impl Error {
    /// Create an error with no location or associated file.
    fn with_message(message: String) -> Error {
        Error {
            message,
            filename: None,
            location: None,
        }
    }

    /// Create an error that occurred at a given location.
    fn from_span(message: &str, span: Span) -> Error {
        Error {
            message: String::from(message),
            location: Some(span.location()),
            filename: None,
        }
    }

    /// Create an error that occurred at a given location.
    fn from_location(message: &str, location: String) -> Error {
        Error {
            message: String::from(message),
            location: Some(location),
            filename: None,
        }
    }

    /// Sets the filename on this error if it is not already set.
    fn set_filename(&mut self, filename: &str) {
        if self.filename.is_none() {
            self.filename = Some(filename.to_string());
        }
    }
}

impl<'a> From<ParseError<'a>> for Error {
    fn from(err: ParseError) -> Error {
        Error {
            message: err.short_message(),
            filename: None,
            location: Some(err.span.location()),
        }
    }
}

impl Display for Error {
    /// Displays the error with the message and any location.
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match (&self.filename, &self.location) {
            (&Some(ref filename), &Some(ref location)) => {
                write!(f, "{}\n{}:{}", self.message, filename, location)
            }
            (&None, &Some(ref location)) | (&Some(ref location), &None) => {
                write!(f, "{}\n{}", self.message, location)
            }
            (&None, &None) => write!(f, "{}", self.message),
        }
    }
}

/// A config key reference with a location string.
#[derive(Debug)]
struct ConfigKeyRef {
    location: String,
    path: Vec<String>,
    /// The length of the relative ref prefix. Zero for absolute refs. This is used to disambiguate
    /// paths that have quoted `super` or `self` values at the beginning.
    ref_length: usize,
}

impl ConfigKeyRef {
    /// Returns the ref for the given path, or an error if the path contains unquoted illegal
    /// values. `is_target` should be true if this is on the LHS of an assignment, and will treat
    /// `super` and `self` as errors.
    fn from_raw(raw_path: &ConfigKeyLike, is_target: bool) -> Result<ConfigKeyRef, Error> {
        // Tracks if we're validating the start of a path that could be a relative ref. Relative
        // refs are only valid for non-target keys, so init to is_target.
        let mut ref_ended = is_target;
        let mut ref_length: usize = 0;
        let path_result: Result<Vec<String>, Error> = raw_path
            .segments
            .iter()
            // First, validate the path. It may not have any literals unquoted, and may not have
            // (super|self) values if it's a target.
            .map(|segment| match segment {
                &ConfigKeySegment::Quoted(Span { fragment, .. }) => {
                    // Quoted keys are always non-relative.
                    ref_ended = true;
                    Ok(String::from(&fragment.0[1..fragment.0.len() - 1]))
                }
                &ConfigKeySegment::Unquoted(span @ Span { .. }) => match span.fragment.0 {
                    "true" | "false" | "undefined" => {
                        Err(Error::from_span("value literal in path", span))
                    }
                    value @ "super" | value @ "self" if is_target => {
                        Err(Error::from_span(&format!("can't assign to \"{}\" value", value), span))
                    }
                    value @ "super" => {
                        if ref_ended {
                            Err(Error::from_span("\"super\" may only appear at the start of a path",
                                                 span))
                        } else {
                            // Any number of super chains are allowed.
                            ref_length += 1;
                            Ok(String::from(value))
                        }
                    },
                    value @ "self" => {
                        if ref_ended || ref_length > 0 {
                            Err(Error::from_span(
                                    "\"self\" may only appear once, at the start of a path",
                                    span))
                        } else {
                            // Only one "self" is allowed.
                            ref_ended = true;
                            ref_length += 1;
                            Ok(String::from(value))
                        }
                    },
                    value => {
                        ref_ended = true;
                        Ok(String::from(value))
                    }
                },
            })
            .collect();
        path_result.map(|path| {
            let location = raw_path.segments[0].span().location();
            ConfigKeyRef {
                location,
                path,
                ref_length,
            }
        })
    }
}

impl Display for ConfigKeyRef {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.path.join("."))
    }
}

/// An extend config value, holding unresolved references and undefined values.
enum LoadedValue {
    /// A literal that's fully-resolved (no lists or maps).
    Literal(ConfigValue),
    /// A reference to another value.
    Reference(ConfigKeyRef),
    /// An undefined literal.
    Undefined { location: String },
    /// A list that's unresolved.
    List { values: Vec<LoadedValue> },
    /// A map that's unresolved.
    Map(LoadedMap),
}

/// A loaded map object that hasn't been flattened or had refs removed yet.
struct LoadedMap {
    /// True if this map itself is a template.
    pub is_template: bool,
    /// The path to the template ("from" value) for this map.
    pub template: Option<ConfigKeyRef>,
    /// The values stored in this map.
    values: HashMap<String, LoadedValue>,
}

impl LoadedMap {
    fn new(is_template: bool, template: Option<ConfigKeyRef>) -> LoadedMap {
        LoadedMap {
            is_template,
            template,
            values: HashMap::new(),
        }
    }

    /// Returns true if the given key is in this map.
    fn contains_key(&self, key: &String) -> bool {
        self.values.contains_key(key)
    }

    /// Returns true if the given path is in this map.
    fn contains_path(&self, path: &Vec<String>) -> bool {
        match path.split_first() {
            Some((first, rest)) => match self.values.get(first) {
                Some(child_value) => if rest.len() == 0 {
                    true
                } else {
                    match child_value {
                        &LoadedValue::Map(ref child_map) => child_map.contains_path(&rest.to_vec()),
                        _ => false,
                    }
                },
                None => false,
            },
            None => true,
        }
    }

    /// Inserts the given value at the given path in this map. Panics if the value already
    /// exists or if the path is empty.
    fn insert(&mut self, path: &Vec<String>, value: LoadedValue) {
        match path.split_first() {
            Some((first, rest)) => {
                if self.values.contains_key(first) {
                    panic!("duplicate key definition for key {}", first);
                } else {
                    // No value; last path item.
                    if rest.len() == 0 {
                        self.values.insert(first.to_string(), value);
                    } else {
                        // No value; recursive insert.
                        let mut new_map = LoadedMap::new(self.is_template, None);
                        new_map.insert(&rest.to_vec(), value);
                        self.values
                            .insert(first.to_string(), LoadedValue::Map(new_map));
                    }
                }
            }
            None => panic!("insert called with empty path"),
        }
    }

    /// Converts this LoadedMap into a ConfigMap. Panics if any non-template part of this map
    /// contains Undefined or Reference values.
    fn to_config_map(self) -> ConfigMap {
        let mut self_map = ConfigMap::new();
        for (key, value) in self.values.into_iter() {
            match value {
                LoadedValue::Literal(config_value) => {
                    self_map.values.insert(key, config_value);
                }
                // TODO: Implement lists!
                LoadedValue::List { values } => unimplemented!(),
                LoadedValue::Map(child) => {
                    self_map
                        .values
                        .insert(key, ConfigValue::Map(child.to_config_map()));
                }
                _ => panic!("to_config_map called on non-flattened map"),
            }
        }
        self_map
    }
}

/// Import resolver. This is called to translate an import path into file contents.
pub trait ImportResolver {
    /// Resolve the given import path into its contents.
    fn resolve(&self, path: &str) -> Result<String, Error>;
}

/// Reads the file with the given filename, converting the error appropriately.
fn read_string<P: AsRef<Path>>(filename: P) -> Result<String, Error> {
    File::open(&filename)
        .map_err(|e| match e.kind() {
            IoErrorKind::NotFound => {
                Error::with_message(format!("file '{}' not found", filename.as_ref().display()))
            }
            IoErrorKind::PermissionDenied => Error::with_message(format!(
                "permission deined reading '{}'",
                filename.as_ref().display()
            )),
            _ => Error::with_message(format!("could not open '{}'", filename.as_ref().display())),
        })
        .and_then(|mut file| {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|_| {
                    Error::with_message(format!("error reading {}", filename.as_ref().display()))
                })
                .and(Ok(contents))
        })
}

/// Base implementation for a resolver operating with a base path.
impl<'a> ImportResolver for &'a Path {
    /// Resolves the given import path relative to this path.
    fn resolve(&self, path: &str) -> Result<String, Error> {
        // Split the import path into components.
        let elements: Vec<&str> = path.split("/").collect();
        // TODO: Make separator string constant?
        let import_path = elements.join(&format!("{}", MAIN_SEPARATOR));
        // Note that `join` on an absolute path will return the absolute path.
        let file_path = self.join(import_path);
        read_string(&file_path)
    }
}

/// Loads the given file.
pub fn load(filename: String) -> Result<ConfigMap, Error> {
    let contents = read_string(&filename)?;

    // TODO: Make this function take an ImportResolver instead!
    let base_path = Path::new(&filename).parent().unwrap_or(Path::new("."));

    load_template_config(&contents, &base_path)
        .map(|loaded_map| loaded_map.to_config_map())
        .map_err(|mut err| {
            err.set_filename(&filename);
            err
        })
}

/// Processes the given file contents into a fully-validated config with templates.
fn load_template_config<R>(contents: &str, resolver: &R) -> Result<LoadedMap, Error>
where
    R: ImportResolver,
{
    let raw_document = document::load(&contents).map_err(|e| Error::from(e))?;

    let mut resolved_imports: Vec<LoadedMap> = vec![];
    for import in raw_document.imports.iter() {
        // Resolve and load the next import.
        let import_filename = import.filename.fragment.0;
        let result: Result<(), Error> = resolver.resolve(import_filename).and_then(|contents| {
            load_template_config(&contents, resolver)
                .map(|new_config| {
                    resolved_imports.push(new_config);
                })
                .map_err(|mut err| {
                    err.set_filename(import_filename);
                    err
                })
        });
        // Exit if we couldn't load the import.
        if result.is_err() {
            return Err(result.unwrap_err());
        }
    }
    // Flatten imports, handling 'as' clauses.
    /*
    let imports_as_assignments: Vec<Statement<'a>> =
        imports.into_iter().zip(resolved_imports.into_iter()).flat_map(
        |(import, document)| {
            match import.target {
                Some(target) => {
                    let doc_as_map = RawConfigValue::Map(RawConfigMap{
                        values: document.statements,
                        template: None,
                    });
                    vec!(Statement::Assignment{target, value: doc_as_map})
                },
                None => document.statements,
            }
        }).collect();
    */

    let mut config = LoadedMap::new(false, None);

    let second_pass_result: Result<(), Error> = raw_document
        .statements
        .into_iter()
        .map(|statement| load_statement(statement, None, &mut config))
        .collect();

    // TODO:
    // Third-pass algo:
    // - flatten parent templates into children recursively, validating undefined values
    // - resolve relative refs recursively, tracking current chain for circular dependency check

    second_pass_result.map(|()| config)
}

/// Loads the given statement in the given context into the the given config. Returns an error
/// if there was a validation problem. Root config is used to validate absolute references, and is
/// set for non-root loads.
fn load_statement(
    statement: Statement,
    root_config: Option<&LoadedMap>,
    config: &mut LoadedMap,
) -> Result<(), Error> {
    // Extract target, value, and template info.
    let (target, value, is_template) = match statement {
        Statement::Assignment { target, value } => (target, value, false),
        Statement::Template { name, value } => (name, RawConfigValue::Map(value), true),
    };
    ConfigKeyRef::from_raw(&target, true).and_then(|key_ref| {
        let path = key_ref.path;
        if config.contains_key(&path[0]) {
            let err_span = target.segments[0].span();
            Err(Error::from_span(
                &format!("key \"{}\" already exists", path[0]),
                *err_span,
            ))
        } else {
            match value {
                RawConfigValue::False(_) => {
                    config.insert(&path, LoadedValue::Literal(ConfigValue::Bool(false)));
                    Ok(())
                }
                RawConfigValue::True(_) => {
                    config.insert(&path, LoadedValue::Literal(ConfigValue::Bool(true)));
                    Ok(())
                }
                RawConfigValue::Undefined(span) => if !config.is_template {
                    Err(Error::from_span("undefined value in non-template", span))
                } else {
                    config.insert(
                        &path,
                        LoadedValue::Undefined {
                            location: span.location(),
                        },
                    );
                    Ok(())
                },
                RawConfigValue::Integer(span) => {
                    let int_value = str::parse::<i32>(span.as_str()).unwrap();
                    config.insert(&path, LoadedValue::Literal(ConfigValue::Int(int_value)));
                    Ok(())
                }
                RawConfigValue::Float(span) => {
                    let float_value = str::parse::<f64>(span.as_str()).unwrap();
                    config.insert(&path, LoadedValue::Literal(ConfigValue::Float(float_value)));
                    Ok(())
                }
                RawConfigValue::String(span) => {
                    let span_str = span.as_str();
                    // Strip quotes off of strings.
                    // TODO: Interpolation goes here.
                    let string_value = String::from(&span_str[1..span_str.len() - 1]);
                    config.insert(
                        &path,
                        LoadedValue::Literal(ConfigValue::String(string_value)),
                    );
                    Ok(())
                }
                RawConfigValue::Map(RawConfigMap {
                    values,
                    template: template_opt,
                }) => {
                    // Validate template.
                    let template_result = template_opt
                        .map(|template_key| {
                            ConfigKeyRef::from_raw(&template_key, false).and_then(|key_ref| {
                                // Check that the template exists.
                                let root = root_config.unwrap_or(config);
                                if root.contains_path(&key_ref.path) {
                                    Ok(Some(key_ref))
                                } else {
                                    Err(Error::from_location(
                                        &format!("could not find template \"{}\"", key_ref),
                                        key_ref.location,
                                    ))
                                }
                            })
                        })
                        .unwrap_or(Ok(None));

                    template_result.and_then(|template_opt| {
                        let child_is_template = config.is_template || is_template;
                        let mut child_config = LoadedMap::new(child_is_template, template_opt);
                        let recurse_result: Result<(), Error> = values
                            .into_iter()
                            .map(|child| {
                                load_statement(
                                    child,
                                    root_config.or(Some(config)),
                                    &mut child_config,
                                )
                            })
                            .collect();
                        recurse_result.map(|()| {
                            config.insert(&path, LoadedValue::Map(child_config));
                        })
                    })
                }
                RawConfigValue::List { values } => {
                    // TODO: Handle. This probably means refactoring this inner match into a helper.
                    Ok(())
                }
                RawConfigValue::RefLike(_) => {
                    // TODO: If a length-one ref-like, check for constant. Otherwise, it's a ref!
                    // TODO: Error if undefined value and not in_template.
                    Ok(())
                }
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_key_ref_from_raw_target_ok() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Quoted(Span::from("`one`")),
                ConfigKeySegment::Unquoted(Span::from("two")),
                ConfigKeySegment::Quoted(Span::from("`three`")),
            ],
        };
        let key_ref = ConfigKeyRef::from_raw(&test_key, true).unwrap();
        assert_eq!(key_ref.path, &["one", "two", "three"]);
        assert_eq!(key_ref.ref_length, 0);
    }

    #[test]
    fn config_key_ref_from_raw_value_ok_self() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("self")),
                ConfigKeySegment::Unquoted(Span::from("value")),
            ],
        };
        let key_ref = ConfigKeyRef::from_raw(&test_key, false).unwrap();
        assert_eq!(key_ref.path, &["self", "value"]);
        assert_eq!(key_ref.ref_length, 1);
    }

    #[test]
    fn config_key_ref_from_raw_value_ok_super() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("super")),
                ConfigKeySegment::Unquoted(Span::from("super")),
                ConfigKeySegment::Unquoted(Span::from("super")),
                ConfigKeySegment::Unquoted(Span::from("value")),
            ],
        };
        let key_ref = ConfigKeyRef::from_raw(&test_key, false).unwrap();
        assert_eq!(key_ref.path, &["super", "super", "super", "value"]);
        assert_eq!(key_ref.ref_length, 3);
    }

    #[test]
    fn config_key_ref_from_raw_target_ok_quoted_illegal() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Quoted(Span::from("`true`")),
                ConfigKeySegment::Unquoted(Span::from("two")),
                ConfigKeySegment::Quoted(Span::from("`super`")),
            ],
        };
        let key_ref = ConfigKeyRef::from_raw(&test_key, true).unwrap();
        assert_eq!(key_ref.path, &["true", "two", "super"]);
        assert_eq!(key_ref.ref_length, 0);
    }

    #[test]
    fn config_key_ref_from_raw_target_err_illegal() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("one")),
                ConfigKeySegment::Unquoted(Span::from("two")),
                ConfigKeySegment::Unquoted(Span::from("false")),
            ],
        };
        let err = ConfigKeyRef::from_raw(&test_key, true)
            .expect_err("'false' should have been illegal key");
        assert_eq!(err.location, Some(String::from("1:1\nfalse\n^\n")));
    }

    #[test]
    fn config_key_ref_from_raw_target_err_self_ref() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("self")),
                ConfigKeySegment::Unquoted(Span::from("false")),
            ],
        };
        let err = ConfigKeyRef::from_raw(&test_key, true)
            .expect_err("'self' should have been illegal key in a target");
        assert_eq!(err.location, Some(String::from("1:1\nself\n^\n")));
    }

    #[test]
    fn config_key_ref_from_raw_value_err_late_super() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("super")),
                ConfigKeySegment::Unquoted(Span::from("value")),
                ConfigKeySegment::Unquoted(Span::from("super")),
            ],
        };
        let err = ConfigKeyRef::from_raw(&test_key, false)
            .expect_err("'super' should not have been allowed mid-path");
        assert_eq!(err.location, Some(String::from("1:1\nsuper\n^\n")));
    }

    #[test]
    fn config_key_ref_from_raw_value_err_late_self() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("super")),
                ConfigKeySegment::Unquoted(Span::from("self")),
                ConfigKeySegment::Unquoted(Span::from("value")),
            ],
        };
        let err = ConfigKeyRef::from_raw(&test_key, false)
            .expect_err("'self' should not have been allowed after 'super'");
        assert_eq!(err.location, Some(String::from("1:1\nself\n^\n")));
    }

    #[test]
    fn config_key_ref_from_raw_value_err_double_self() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("self")),
                ConfigKeySegment::Unquoted(Span::from("self")),
                ConfigKeySegment::Unquoted(Span::from("value")),
            ],
        };
        let err = ConfigKeyRef::from_raw(&test_key, false)
            .expect_err("'self' should not have been allowed after 'self'");
        assert_eq!(err.location, Some(String::from("1:1\nself\n^\n")));
    }

    #[test]
    fn config_key_ref_from_raw_value_err_self_super() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("self")),
                ConfigKeySegment::Unquoted(Span::from("super")),
                ConfigKeySegment::Unquoted(Span::from("value")),
            ],
        };
        let err = ConfigKeyRef::from_raw(&test_key, false)
            .expect_err("'super' should not have been allowed after 'self'");
        assert_eq!(err.location, Some(String::from("1:1\nsuper\n^\n")));
    }

    #[test]
    fn load_statement_ok() {
        let test_resolver = Path::new(".");
        let result = load_template_config(
            "
foo = 1
bar = {
  a = 1
  b = 2
  nested = {
    again = \"true\"
    `true` = false
  }
}",
            &test_resolver,
        );
        let loaded_config = result.expect("simple config should've parsed");
        let config = loaded_config.to_config_map();
        assert_eq!(config.get_int("foo").unwrap(), 1);
        assert_eq!(config.get_int("bar.a").unwrap(), 1);
        assert_eq!(config.get_int("bar.b").unwrap(), 2);
        assert_eq!(
            config.get_string("bar.nested.again").unwrap(),
            &String::from("true")
        );
        assert_eq!(config.get_bool("bar.nested.true").unwrap(), false);
    }
}
