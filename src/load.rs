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
struct ConfigKeyRef {
    location: String,
    path: Vec<String>,
}

impl ConfigKeyRef {
    fn from_raw(raw_path: &ConfigKeyLike) -> Result<ConfigKeyRef, Error> {
        let path_result: Result<Vec<String>, Error> = raw_path
            .segments
            .iter()
            .map(|segment| match segment {
                &ConfigKeySegment::Quoted(Span { fragment, .. }) => {
                    Ok(String::from(&fragment.0[1..fragment.0.len() - 1]))
                }
                &ConfigKeySegment::Unquoted(span @ Span { .. }) => match span.fragment.0 {
                    "true" | "false" | "undefined" => {
                        Err(Error::from_span("value literal in path", span))
                    }
                    "super" | "self" => Err(Error::from_span("illegal value in path", span)),
                    value => Ok(String::from(value)),
                },
            })
            .collect();
        path_result.map(|path| {
            let location = raw_path.segments[0].span().location();
            ConfigKeyRef { location, path }
        })
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

    /// Converts this LoadedMap into a ConfigMap. Panics if this map contains any Undefined values.
    fn to_config_map(&self) -> ConfigMap {
        unimplemented!()
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
    create_config_path(&target).and_then(|path| {
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
                            ConfigKeyRef::from_raw(&template_key).and_then(|key_ref| {
                                // TODO: Check that this exists.
                                Ok(Some(key_ref))
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
                                    root_config.or(Some(&config)),
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

/// Validates a config path, and returns the parsed path.
fn create_config_path(raw_path: &ConfigKeyLike) -> Result<Vec<String>, Error> {
    raw_path
        .segments
        .iter()
        .map(|segment| match segment {
            &ConfigKeySegment::Quoted(Span { fragment, .. }) => {
                Ok(String::from(&fragment.0[1..fragment.0.len() - 1]))
            }
            &ConfigKeySegment::Unquoted(span @ Span { .. }) => match span.fragment.0 {
                "true" | "false" | "undefined" => {
                    Err(Error::from_span("value literal in path", span))
                }
                "super" | "self" => Err(Error::from_span("illegal value in path", span)),
                value => Ok(String::from(value)),
            },
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_config_path_ok() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Quoted(Span::from("`one`")),
                ConfigKeySegment::Unquoted(Span::from("two")),
                ConfigKeySegment::Quoted(Span::from("`three`")),
            ],
        };
        assert_eq!(
            create_config_path(&test_key).unwrap(),
            &["one", "two", "three"]
        );
    }

    #[test]
    fn create_config_path_ok_quoted_illegal() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Quoted(Span::from("`true`")),
                ConfigKeySegment::Unquoted(Span::from("two")),
                ConfigKeySegment::Quoted(Span::from("`false`")),
            ],
        };
        assert_eq!(
            create_config_path(&test_key).unwrap(),
            &["true", "two", "false"]
        );
    }

    #[test]
    fn create_config_path_err_illegal() {
        let test_key = ConfigKeyLike {
            segments: vec![
                ConfigKeySegment::Unquoted(Span::from("one")),
                ConfigKeySegment::Unquoted(Span::from("two")),
                ConfigKeySegment::Unquoted(Span::from("false")),
            ],
        };
        let err = create_config_path(&test_key).expect_err("'false' should have been illegal key");
        assert_eq!(err.location, Some(String::from("1:1\nfalse\n^\n")));
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
        let config = result.expect("simple config should've parsed");
        // TODO:
        /*
        assert_eq!(config.config.get_int("foo").unwrap(), 1);
        assert_eq!(config.config.get_int("bar.a").unwrap(), 1);
        assert_eq!(config.config.get_int("bar.b").unwrap(), 2);
        assert_eq!(config.config.get_string("bar.nested.again").unwrap(), &String::from("true"));
        assert_eq!(config.config.get_bool("bar.nested.true").unwrap(), false);
        */    }
}
