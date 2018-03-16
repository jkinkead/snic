// Core configuration objects.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;

/// A value in a config.
#[derive(Debug, PartialEq)]
pub enum ConfigValue {
    /// true & false literals.
    Bool(bool),
    /// Quoted strings, post-interpolation.
    String(String),
    /// Integral values in the config. Numbers without a decimal point will be parsed as IntValue.
    Int(i32),
    /// Decimal values in the config. Numbers with a decimal point will be parsed as FloatValue.
    Float(f64),
    /// An array.
    List(ConfigList),
    /// A map. The root config is a map value.
    Map(ConfigMap),
}

impl ConfigValue {
    /// Returns a string representation of the kind of this value.
    fn kind_string(&self) -> String {
        match self {
            &ConfigValue::Bool(_) => String::from("bool"),
            &ConfigValue::String(_) => String::from("string"),
            &ConfigValue::Int(_) => String::from("int"),
            &ConfigValue::Float(_) => String::from("float"),
            &ConfigValue::List(_) => String::from("list"),
            &ConfigValue::Map(_) => String::from("map"),
        }
    }
}

/// An error looking up a key in a config.
#[derive(Debug, PartialEq)]
pub struct KeyError {
    /// The key which triggered the error.
    key: String,
    /// The path that led to this key.
    path: VecDeque<String>,
    /// Failure type.
    pub kind: KeyErrorKind,
}

impl KeyError {
    /// Create a new error for the given path key with the given kind.
    fn new(key: String, kind: KeyErrorKind) -> KeyError {
        KeyError {
            key,
            kind,
            path: VecDeque::new(),
        }
    }

    /// Create a new WrongType error for the given type names and path.
    fn wrong_type(expected: String, actual: String, path_elements: &[&str]) -> KeyError {
        let len = path_elements.len();
        let mut path: VecDeque<String> = VecDeque::new();
        for i in 1..len {
            path.push_back(path_elements[i].to_string());
        }
        KeyError {
            key: path[len - 1].to_string(),
            path,
            kind: KeyErrorKind::WrongType { expected, actual },
        }
    }
}

impl fmt::Display for KeyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let path = join_key(&self.path);
        match self.kind {
            KeyErrorKind::BadIndex => write!(f, "bad index '{}' in list at {}", self.key, path),
            KeyErrorKind::MissingKey => write!(f, "no key \"{}\" found at {}", self.key, path),
            KeyErrorKind::Dereference => write!(
                f,
                "value {} is a primitive, can't access key \"{}\"",
                self.key,
                path
            ),
            KeyErrorKind::WrongType {
                ref expected,
                ref actual,
            } => write!(
                f,
                "expected a {}, got a {} for key \"{}\" in {}",
                expected,
                actual,
                self.key,
                path
            ),
        }
    }
}

/// The types of errors encountered when looking up a value in a ConfigMap or ConfigList.
#[derive(Debug, PartialEq)]
pub enum KeyErrorKind {
    /// An index wasn't found in a ConfigList, or wasn't a positive integer.
    BadIndex,
    /// A key wasn't found in a ConfigMap.
    MissingKey,
    /// The key dereferenced a non-Map type.
    Dereference,
    /// The key wasn't of the requested type.
    WrongType { expected: String, actual: String },
}

/// An result of fetching a value from a configuration.
pub type ConfigResult<'a> = Result<&'a ConfigValue, KeyError>;

/// Helper to split a config key into component parts (on periods).
fn split_key(key: &str) -> Vec<&str> {
    // TODO(jkinkead): Support keys with dots (quoted keys).
    key.split('.').collect()
}
/// Joins a list of key segments into a full key.
fn join_key(segments: &VecDeque<String>) -> String {
    let mut iter = segments.iter();
    match iter.next() {
        None => String::new(),
        Some(first) => {
            let mut result = String::new();
            // TODO(jkinkead): Quote dotted keys.
            result += first;
            for segment in iter {
                result += ".";
                result += segment;
            }
            result
        }
    }
}

/// A trait for config values which contain other values (ConfigList and ConfigMap).
/// Parameterized on the key type.
trait ConfigContainer<K: ToString> {
    /// Return the value at the given key, or None if the key doesn't exist in this container.
    fn get_at_key(&self, key: &K) -> Option<&ConfigValue>;

    /// Return the given path element as a key value.
    fn str_to_key(element: &str) -> Result<K, KeyError>;

    /// Return the value at the given path.
    fn get_by_path(&self, path: &[&str]) -> ConfigResult {
        // Get the value for the first key element.
        Self::str_to_key(path[0]).and_then(|first_key| match self.get_at_key(&first_key) {
            // No such key in this collection.
            None => Err(KeyError::new(
                first_key.to_string(),
                KeyErrorKind::MissingKey,
            )),
            // No more path elements; item found!
            Some(value) if path.len() == 1 => Ok(value),
            // Nested item; recurse. Any new container types will need to be added below.
            // This is a bad design for extensibility, since it requires enumerating all
            // implementations of this trait...but is probably fine for this usage.
            Some(container @ &ConfigValue::Map(_)) | Some(container @ &ConfigValue::List(_)) => {
                match container {
                    &ConfigValue::Map(ref value) => value.get_by_path(&path[1..]),
                    &ConfigValue::List(ref value) => value.get_by_path(&path[1..]),
                    _ => unreachable!(),
                }.or_else(|mut err| {
                    // Prepend the current key to error path.
                    err.path.push_front(first_key.to_string());
                    Err(err)
                })
            }
            // More path elements, but item is not a container.
            _ => Err(KeyError::new(
                first_key.to_string(),
                KeyErrorKind::Dereference,
            )),
        })
    }
}

/// A list parsed from a config. This can be converted into a list of homogenous types, or be
/// accessed by index.
#[derive(Debug, PartialEq)]
pub struct ConfigList {
    values: Vec<ConfigValue>,
}

impl ConfigContainer<usize> for ConfigList {
    fn get_at_key(&self, key: &usize) -> Option<&ConfigValue> {
        self.values.get(*key)
    }

    fn str_to_key(element: &str) -> Result<usize, KeyError> {
        usize::from_str_radix(element, 10).or_else(|_| {
            Err(KeyError::new(element.to_string(), KeyErrorKind::BadIndex))
        })
    }
}

impl ConfigList {
    pub fn empty() -> ConfigList {
        ConfigList { values: vec![] }
    }

    /// Return the value at the given index.
    pub fn get(&self, index: usize) -> ConfigResult {
        self.values
            .get(index)
            .ok_or_else(|| KeyError::new(index.to_string(), KeyErrorKind::BadIndex))
    }

    /// Appends the given value to the config list.
    pub fn push(&mut self, value: ConfigValue) {
        self.values.push(value)
    }

    // TODO(jkinkead): Methods to convert the list to a slice of a specific type.
}

#[derive(Debug, PartialEq)]
pub struct ConfigMap {
    values: HashMap<String, ConfigValue>,
}

impl ConfigContainer<String> for ConfigMap {
    fn get_at_key(&self, key: &String) -> Option<&ConfigValue> {
        self.values.get(key)
    }

    fn str_to_key(element: &str) -> Result<String, KeyError> {
        Ok(element.to_string())
    }
}

/// A full configuration.
impl ConfigMap {
    /// Returns a new empty map.
    pub fn empty() -> ConfigMap {
        ConfigMap {
            values: HashMap::new(),
        }
    }

    /// Return the value at the given key.
    pub fn get(&self, key: &str) -> ConfigResult {
        let full_path = split_key(key);
        self.get_by_path(&full_path)
    }

    pub fn insert(&mut self, key: &str, value: ConfigValue) {
        self.values.insert(key.to_string(), value);
    }

    pub fn get_bool(&self, key: &str) -> Result<bool, KeyError> {
        let full_path = split_key(key);
        self.get_by_path(&full_path).and_then(|value| match value {
            &ConfigValue::Bool(v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("bool"),
                value.kind_string(),
                &full_path,
            )),
        })
    }
    pub fn get_string(&self, key: &str) -> Result<&String, KeyError> {
        let full_path = split_key(key);
        self.get_by_path(&full_path).and_then(|value| match value {
            &ConfigValue::String(ref v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("string"),
                value.kind_string(),
                &full_path,
            )),
        })
    }
    pub fn get_int(&self, key: &str) -> Result<i32, KeyError> {
        let full_path = split_key(key);
        self.get_by_path(&full_path).and_then(|value| match value {
            &ConfigValue::Int(v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("int"),
                value.kind_string(),
                &full_path,
            )),
        })
    }

    /// Returns the float value at the given key. This will automatically convert integer values.
    pub fn get_float(&self, key: &str) -> Result<f64, KeyError> {
        let full_path = split_key(key);
        self.get_by_path(&full_path).and_then(|value| match value {
            &ConfigValue::Int(v) => Ok(f64::from(v)),
            &ConfigValue::Float(v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("float"),
                value.kind_string(),
                &full_path,
            )),
        })
    }
    pub fn get_list(&self, key: &str) -> Result<&ConfigList, KeyError> {
        let full_path = split_key(key);
        self.get_by_path(&full_path).and_then(|value| match value {
            &ConfigValue::List(ref v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("list"),
                value.kind_string(),
                &full_path,
            )),
        })
    }
    pub fn get_map(&self, key: &str) -> Result<&ConfigMap, KeyError> {
        let full_path = split_key(key);
        self.get_by_path(&full_path).and_then(|value| match value {
            &ConfigValue::Map(ref v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("map"),
                value.kind_string(),
                &full_path,
            )),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_missing_key() {
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Bool(true));
        let result = map.get("foo");
        assert_eq!(result.err().map(|e| e.kind), Some(KeyErrorKind::MissingKey));
    }

    #[test]
    fn get_bad_key() {
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Bool(true));
        let result = map.get("test.it");
        assert_eq!(
            result.err(),
            Some(KeyError::new(
                String::from("test"),
                KeyErrorKind::Dereference
            ))
        );
    }

    #[test]
    fn get_bad_subkey() {
        let child_map = ConfigMap::empty();
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Map(child_map));
        let result = map.get("test.it");
        assert_eq!(
            result.err(),
            Some(KeyError {
                key: String::from("it"),
                path: VecDeque::from(vec![String::from("test")]),
                kind: KeyErrorKind::MissingKey,
            })
        );
    }

    #[test]
    fn get_existing_key() {
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Bool(true));
        let result = map.get("test");
        assert_eq!(result.unwrap(), &ConfigValue::Bool(true));
    }

    #[test]
    fn get_bool() {
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Bool(true));
        let result = map.get_bool("test");
        assert_eq!(result.unwrap(), true);
    }

    #[test]
    fn get_string() {
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::String(String::from("value")));
        let result = map.get_string("test");
        assert_eq!(*result.unwrap(), String::from("value"));
    }

    #[test]
    fn get_int() {
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Int(123));
        let result = map.get_int("test");
        assert_eq!(result.unwrap(), 123);
    }

    #[test]
    fn get_float() {
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Float(1.5));
        let result = map.get_float("test");
        assert_eq!(result.unwrap(), 1.5);
    }

    #[test]
    fn get_list() {
        let child_list = ConfigList::empty();
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::List(child_list));
        let result = map.get_list("test");
        assert_eq!(*result.unwrap(), ConfigList::empty());
    }

    #[test]
    fn get_map() {
        let child_map = ConfigMap::empty();
        let mut map = ConfigMap::empty();
        map.insert("test", ConfigValue::Map(child_map));
        let result = map.get_map("test");
        assert_eq!(*result.unwrap(), ConfigMap::empty());
    }

    // TODO(jkinkead): Tests for bad values fetched using get_* methods.
}
