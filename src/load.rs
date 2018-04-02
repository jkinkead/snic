use config::ConfigMap;
use input::Span;
use parser::document;
use parser::errors::Error as ParseError;

use nom::types::CompleteStr;

use std::fs::File;
use std::io::{ErrorKind as IoErrorKind, Read};
use std::path::{Path, MAIN_SEPARATOR};

#[derive(Debug)]
pub struct Error {
    /// Short message, suitable for printing out.
    pub message: String,
    /// Location message, if it came from parsing.
    location: Option<String>,
}

impl Error {
    fn with_message(message: String) -> Error {
        Error {
            message,
            location: None,
        }
    }
}

impl<'a> From<ParseError<'a>> for Error {
    fn from(err: ParseError) -> Self {
        Error {
            message: err.short_message(),
            location: Some(err.location()),
        }
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
impl ImportResolver for Path {
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
    let raw_document = document::load(Span::new(CompleteStr(&contents)))?;

    // TODO: Make this function take an ImportResolver; default to using the Path
    // implementation.
    let base_path = Path::new(&filename).parent().unwrap_or(Path::new("."));

    /*
    let mut resolved_imports: Vec<Document<'a>> = vec!();
    for import in imports.iter() {
        // Resolve and load the next import.
        let result: Result<(), LoadError> =
            resolver.resolve(import.filename.fragment.0).and_then(|contents| {
                let contents_span = Span::new(CompleteStr(&contents));
                load(contents_span, resolver).map(|new_doc| {
                    resolved_imports.push(new_doc);
                })
            });
        // Exit if we couldn't load the import.
        if (result.is_err()) {
            return Err(result.unwrap_err());
        }
    }
    // Flatten imports, handling 'as' clauses.
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
    let (assignments_rest, mut assignments) =
        assignments(document).map_err(|e| LoadError::from(e))?;
    // TODO: If assignments_rest is not empty, error.

    // Concat imports-as-statements and assignments.
    let mut all_statements = imports_as_assignments;
    all_statements.append(&mut assignments);
    Ok(Document{statements: all_statements})
        */
    unimplemented!();
}
