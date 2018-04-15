//! Configuration objects. A configuration file is parsed into a single ConfigMap.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::ops::Deref;

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

    /// Gets the given key as a list index.
    fn to_index(key: &str) -> Result<usize, KeyError> {
        usize::from_str_radix(key, 10).or_else(|_| {
            Err(KeyError::new(key.to_string(), KeyErrorKind::BadIndex))
        })
    }

    /// Helper for mutable / immatable gets. Uses the given getters for map and list access from the
    /// config value. Returns an error if the parent item is not a list or map, if the key isn't
    /// present, or if a list key is not an int.
    #[inline]
    fn get_key_helper<V, M, L>(
        value: V,
        key: &str,
        mut map_getter: M,
        mut list_getter: L,
    ) -> Result<V, KeyError>
    where
        V: Deref<Target = ConfigValue>,
        M: FnMut(V, &str) -> Option<V>,
        L: FnMut(V, usize) -> Option<V>,
    {
        match *value {
            ConfigValue::Map(_) => map_getter(value, key)
                .ok_or_else(|| KeyError::new(key.to_string(), KeyErrorKind::MissingKey)),
            ConfigValue::List(_) => ConfigValue::to_index(key).and_then(|index| {
                list_getter(value, index)
                    .ok_or_else(|| KeyError::new(key.to_string(), KeyErrorKind::MissingKey))
            }),
            _ => Err(KeyError::new(key.to_string(), KeyErrorKind::Dereference)),
        }
    }

    /// Gets a reference to the value at the given key literal. Returns an error if this item is not
    /// a list or map, if the key isn't present, or if a list key is not an int.
    pub fn get_key(&self, key: &str) -> Result<&ConfigValue, KeyError> {
        ConfigValue::get_key_helper(
            self,
            key,
            |map, key| match map {
                &ConfigValue::Map(ref map) => map.values.get(key),
                _ => panic!("closure should not have been called with non-map"),
            },
            |list, index| match list {
                &ConfigValue::List(ref list) => list.values.get(index),
                _ => panic!("closure should not have been called with non-list"),
            },
        )
    }

    /// Gets a mutable reference to the value at the given key literal. Returns an error if this
    /// item is not a list or map, if the key isn't present, or if a list key is not an int.
    pub fn get_key_mut(&mut self, key: &str) -> Result<&mut ConfigValue, KeyError> {
        ConfigValue::get_key_helper(
            self,
            key,
            |map, key| match map {
                &mut ConfigValue::Map(ref mut map) => map.values.get_mut(key),
                _ => panic!("closure should not have been called with non-map"),
            },
            |list, index| match list {
                &mut ConfigValue::List(ref mut list) => list.values.get_mut(index),
                _ => panic!("closure should not have been called with non-list"),
            },
        )
    }

    /// Gets a the value stored in the given path, using the given getter function as a helper.
    /// Returns an error if this value is not a list or map, if the key isn't present, if a list
    /// key is not an int, or if the path is empty.
    #[inline]
    fn get_path_helper<V, K>(value: V, path: &[&str], mut key_getter: K) -> Result<V, KeyError>
    where
        V: Deref<Target = ConfigValue>,
        K: FnMut(V, &str) -> Result<V, KeyError>,
    {
        match path.split_first() {
            Some((first, rest)) => {
                let result = key_getter(value, first);
                if rest.len() == 0 {
                    result
                } else {
                    result
                        .and_then(|child| {
                            ConfigValue::get_path_helper(child, rest, key_getter)
                        })
                        .or_else(|mut err| {
                            // Prepend the current key to error path.
                            err.chain(first);
                            Err(err)
                        })
                }
            }
            None => Err(KeyError::new(String::from(""), KeyErrorKind::EmptyKey)),
        }
    }

    /// Gets a reference to the value stored in the given path. Returns an error if this value is
    /// not a list or map, if the key isn't present, if a list key is not an int, or if the path is
    /// empty.
    pub fn get_path(&self, path: &[&str]) -> Result<&ConfigValue, KeyError> {
        ConfigValue::get_path_helper(self, path, ConfigValue::get_key)
    }

    /// Gets a mutable reference to the value stored in the given path. Returns an error if this
    /// value is not a list or map, if the key isn't present, if a list key is not an int, or if the
    /// path is empty.
    pub fn get_path_mut(&mut self, path: &[&str]) -> Result<&mut ConfigValue, KeyError> {
        ConfigValue::get_path_helper(self, path, ConfigValue::get_key_mut)
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
            key: path_elements[len - 1].to_string(),
            path,
            kind: KeyErrorKind::WrongType { expected, actual },
        }
    }

    /// Chain a key error by appending the given path to the current path.
    fn chain(&mut self, key: &str) {
        self.path.push_front(key.to_string());
    }
}

impl Display for KeyError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let path = join_key(&self.path);
        match self.kind {
            KeyErrorKind::BadIndex => write!(f, "bad index '{}' in list at {}", self.key, path),
            KeyErrorKind::MissingKey => write!(f, "no key \"{}\" found at {}", self.key, path),
            KeyErrorKind::Dereference => write!(
                f,
                "value at {} is a primitive, can't access key \"{}\"",
                path,
                self.key
            ),
            KeyErrorKind::EmptyKey => write!(f, "empty key given"),
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
    /// A key dereferenced a non-Map type.
    Dereference,
    /// A key path was zero-length.
    EmptyKey,
    /// The key wasn't of the requested type.
    WrongType { expected: String, actual: String },
}

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

/// A list parsed from a config. This can be converted into a list of homogenous types, or be
/// accessed by index.
#[derive(Debug, PartialEq)]
pub struct ConfigList {
    values: Vec<ConfigValue>,
}

impl ConfigList {
    /// Returns an new empty ConfigList.
    pub fn empty() -> ConfigList {
        ConfigList { values: vec![] }
    }

    /// Return the value at the given index.
    pub fn get(&self, index: usize) -> Result<&ConfigValue, KeyError> {
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

/// A full configuration.
impl ConfigMap {
    /// Returns a new empty ConfigMap.
    pub fn empty() -> ConfigMap {
        ConfigMap {
            values: HashMap::new(),
        }
    }

    /// Return the value at the given key.
    pub fn get(&self, key: &str) -> Result<&ConfigValue, KeyError> {
        let full_path = split_key(key);
        let (first, rest) = full_path.split_first().unwrap();
        match self.values.get(*first) {
            Some(value) if rest.len() == 0 => Ok(value),
            Some(value) => value.get_path(rest).or_else(|mut err| {
                // Prepend the current key to error path.
                err.chain(first);
                Err(err)
            }),
            None => Err(KeyError::new(first.to_string(), KeyErrorKind::MissingKey)),
        }
    }

    /// Inserts a new value at the given path, overwriting any existing value. Panics if given an
    /// empty path, or if the path is already set.
    #[test]
    fn insert(&mut self, path: &Vec<&str>, value: ConfigValue) {
        match path.split_first() {
            Some((first, rest)) => {
                if self.values.contains_key(*first) {
                    panic!("a value already exists for key {}", first);
                } else {
                    // No value; last path item.
                    if rest.len() == 0 {
                        self.values.insert(first.to_string(), value);
                    } else {
                        // No value; recursive insert.
                        let mut new_map = ConfigMap::empty();
                        new_map.insert(&rest.to_vec(), value);
                        self.values
                            .insert(first.to_string(), ConfigValue::Map(new_map));
                    }
                }
            }
            None => panic!("insert called with empty path"),
        }
    }

    /// Gets the bool value at the given key, returning an error if the key was bad or if the value
    /// at the key was not a bool.
    pub fn get_bool(&self, key: &str) -> Result<bool, KeyError> {
        self.get(key).and_then(|value| match value {
            &ConfigValue::Bool(v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("bool"),
                value.kind_string(),
                &split_key(key),
            )),
        })
    }

    /// Gets the String value at the given key, returning an error if the key was bad or if the
    /// value at the key was not a String.
    pub fn get_string(&self, key: &str) -> Result<&String, KeyError> {
        self.get(key).and_then(|value| match value {
            &ConfigValue::String(ref v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("string"),
                value.kind_string(),
                &split_key(key),
            )),
        })
    }

    /// Gets the int value at the given key, returning an error if the key was bad or if the
    /// value at the key was not an int.
    pub fn get_int(&self, key: &str) -> Result<i32, KeyError> {
        self.get(key).and_then(|value| match value {
            &ConfigValue::Int(v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("int"),
                value.kind_string(),
                &split_key(key),
            )),
        })
    }

    /// Gets the float value at the given key, automatically converting integers to floats, and
    /// returning an error if the key was bad or if the value at the key was not a number.
    pub fn get_float(&self, key: &str) -> Result<f64, KeyError> {
        self.get(&key).and_then(|value| match value {
            &ConfigValue::Int(v) => Ok(f64::from(v)),
            &ConfigValue::Float(v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("float"),
                value.kind_string(),
                &split_key(key),
            )),
        })
    }

    /// Gets the list value at the given key, returning an error if the key was bad or if the
    /// value at the key was not a list.
    pub fn get_list(&self, key: &str) -> Result<&ConfigList, KeyError> {
        self.get(key).and_then(|value| match value {
            &ConfigValue::List(ref v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("list"),
                value.kind_string(),
                &split_key(key),
            )),
        })
    }

    /// Gets the map value at the given key, returning an error if the key was bad or if the
    /// value at the key was not a map.
    pub fn get_map(&self, key: &str) -> Result<&ConfigMap, KeyError> {
        self.get(key).and_then(|value| match value {
            &ConfigValue::Map(ref v) => Ok(v),
            _ => Err(KeyError::wrong_type(
                String::from("map"),
                value.kind_string(),
                &split_key(key),
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
        map.insert(&vec!["test"], ConfigValue::Bool(true));
        let result = map.get("foo");
        assert_eq!(result.err().map(|e| e.kind), Some(KeyErrorKind::MissingKey));
    }

    #[test]
    fn get_bad_deref() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Bool(true));
        let result = map.get("test.it");
        assert_eq!(
            result.err(),
            Some(KeyError {
                key: String::from("it"),
                path: VecDeque::from(vec![String::from("test")]),
                kind: KeyErrorKind::Dereference,
            })
        );
    }

    #[test]
    fn get_bad_subkey() {
        let child_map = ConfigMap::empty();
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Map(child_map));
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
        map.insert(&vec!["test"], ConfigValue::Bool(true));
        let result = map.get("test");
        assert_eq!(result.unwrap(), &ConfigValue::Bool(true));
    }

    #[test]
    fn get_nested_key() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test", "it", "now"], ConfigValue::Bool(true));

        let mut first_nested_map = ConfigMap::empty();
        first_nested_map.insert(&vec!["it", "now"], ConfigValue::Bool(true));
        assert_eq!(map.get_map("test").unwrap(), &first_nested_map);

        let mut second_nested_map = ConfigMap::empty();
        second_nested_map.insert(&vec!["now"], ConfigValue::Bool(true));
        assert_eq!(map.get_map("test.it").unwrap(), &second_nested_map);

        assert_eq!(map.get("test.it.now").unwrap(), &ConfigValue::Bool(true));
    }

    #[test]
    fn get_bool_ok() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Bool(true));
        let result = map.get_bool("test");
        assert_eq!(result.unwrap(), true);
    }

    #[test]
    fn get_bool_err() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Int(1));
        let result = map.get_bool("test");
        assert_eq!(
            result,
            Err(KeyError::wrong_type(
                String::from("bool"),
                String::from("int"),
                &["test"]
            ))
        );
    }

    #[test]
    fn get_string_ok() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::String(String::from("value")));
        let result = map.get_string("test");
        assert_eq!(*result.unwrap(), String::from("value"));
    }

    #[test]
    fn get_string_err() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Bool(true));
        let result = map.get_string("test");
        assert_eq!(
            result,
            Err(KeyError::wrong_type(
                String::from("string"),
                String::from("bool"),
                &["test"]
            ))
        );
    }

    #[test]
    fn get_int_ok() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Int(123));
        let result = map.get_int("test");
        assert_eq!(result.unwrap(), 123);
    }

    #[test]
    fn get_int_err() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Float(123.5));
        let result = map.get_int("test");
        assert_eq!(
            result,
            Err(KeyError::wrong_type(
                String::from("int"),
                String::from("float"),
                &["test"]
            ))
        );
    }

    #[test]
    fn get_float_ok() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Float(1.5));
        let result = map.get_float("test");
        assert_eq!(result.unwrap(), 1.5);
    }

    #[test]
    fn get_float_ok_int() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Int(1));
        let result = map.get_float("test");
        assert_eq!(result.unwrap(), 1.0);
    }

    #[test]
    fn get_float_err() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Map(ConfigMap::empty()));
        let result = map.get_float("test");
        assert_eq!(
            result,
            Err(KeyError::wrong_type(
                String::from("float"),
                String::from("map"),
                &["test"]
            ))
        );
    }

    #[test]
    fn get_list_ok() {
        let child_list = ConfigList::empty();
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::List(child_list));
        let result = map.get_list("test");
        assert_eq!(*result.unwrap(), ConfigList::empty());
    }

    #[test]
    fn get_list_err() {
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Bool(true));
        let result = map.get_list("test");
        assert_eq!(
            result,
            Err(KeyError::wrong_type(
                String::from("list"),
                String::from("bool"),
                &["test"]
            ))
        );
    }

    #[test]
    fn get_map_ok() {
        let child_map = ConfigMap::empty();
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::Map(child_map));
        let result = map.get_map("test");
        assert_eq!(*result.unwrap(), ConfigMap::empty());
    }

    #[test]
    fn get_map_err() {
        let child_list = ConfigList::empty();
        let mut map = ConfigMap::empty();
        map.insert(&vec!["test"], ConfigValue::List(child_list));
        let result = map.get_map("test");
        assert_eq!(
            result,
            Err(KeyError::wrong_type(
                String::from("map"),
                String::from("list"),
                &["test"]
            ))
        );
    }
}
