//! The entrypoint for the first-pass parser.

use std::io;

use input::Span;
use parser::config;
use parser::errors::Error;
use parser::scalars;
use parser::tokens;
use parser::errors::ErrorKind;
use parser::types::{ConfigKeyLike, Document, RawConfigMap, RawConfigValue, Statement};

/// Struct holding an import.
struct Import<'a> {
    /// Path to import.
    filename: Span<'a>,
    /// Optional key to import into.
    target: Option<ConfigKeyLike<'a>>,
}

/// Parses a single import statement.
named!(import<Span, Import>, do_parse!(
    tag!("import") >> value!((), tokens::opt_whitespace) >>
    filename: call!(tokens::escaped_chars, '"') >>
    target: opt!(
        map!(tuple!(tokens::opt_whitespace, config::config_key_like), |(_, target)| target)) >>
    (Import{filename, target})));

/// Parses all of the imports at the top of the file.
named!(imports<Span, Vec<Import>>, do_parse!(
    value!((), tokens::opt_whitespace) >>
    imports: separated_list!(tokens::whitespace_with_newline, import) >>
    value!((), tokens::opt_whitespace) >>
    (imports)));

/// Parses the assignment portion of a document.
named!(assignments<Span, Vec<Statement>>, do_parse!(
    value!((), tokens::opt_whitespace) >>
    assignments: separated_list!(tokens::whitespace_with_newline, config::assignment) >>
    value!((), tokens::opt_whitespace) >>
    (assignments)));

/// Does a first-pass parse of the given config string using the given import resolver.
pub fn load<'a, R>(document: Span<'a>, resolver: R) -> Result<Document<'a>, Error>
  where R: Fn(&'a str) -> Result<Span<'a>, Error> {
    let (imports_rest, imports) = imports(document).map_err(|e| Error::from(e))?;
    let mut resolved_imports: Vec<Document<'a>> = vec!();
    for import in imports.iter() {
        // Resolve and load the next import.
        let result: Result<(), Error> = resolver(import.filename.fragment.0).and_then(|contents|
            load_document(contents, &resolver).map(|new_doc| {
                resolved_imports.push(new_doc);
            }));
        // Exit if we couldn't load the import.
        if (result.is_err()) {
            return Err(result.unwrap_err());
        }
    }
    // Flatten imports, handling 'as' clauses.
    let imports_as_assignments: Vec<Statement<'a>> = imports.into_iter().zip(resolved_imports.into_iter()).flat_map(
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

    let (assignments_rest, mut assignments) = assignments(document).map_err(|e| Error::from(e))?;
    // TODO: If assignments_rest is not empty, error.

    // Concat imports-as-statements and assignments.
    let mut all_statements = imports_as_assignments;
    all_statements.append(&mut assignments);
    Ok(Document{statements: all_statements})
}
