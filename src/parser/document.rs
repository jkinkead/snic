//! The entrypoint for the first-pass parser.

use nom::InputLength;

use parser::config;
use parser::errors::{Error, ErrorKind};
use parser::input::Span;
use parser::tokens;
use parser::types::{Document, Import};

/// Parses a single import statement.
named!(import<Span, Import>, do_parse!(
    tag!("import") >> value!((), tokens::opt_whitespace) >>
    filename: call!(tokens::escaped_chars, '"') >>
    target: opt!(
        map!(tuple!(tokens::opt_whitespace, config::config_key_like), |(_, target)| target)) >>
    (Import{filename, target})));

/// Parses a document.
named!(document<Span, Document>, do_parse!(
    value!((), tokens::opt_whitespace) >>
    imports: separated_list!(tokens::whitespace_with_newline, import) >>
    value!((), tokens::opt_whitespace) >>
    statements: separated_list!(tokens::whitespace_with_newline, config::statement) >>
    value!((), tokens::opt_whitespace) >>
    (Document{imports, statements})
    ));


/// Does a first-pass parse of the given config string.
pub fn load<'a>(config_string: &'a str) -> Result<Document<'a>, Error> {
    let input = Span::from(config_string);
    document(input)
        .map_err(|e| Error::from(e))
        .and_then(|(rest, document)| {
            if rest.input_len() > 0 {
                if document.statements.len() > 0 {
                    Err(Error::new(ErrorKind::ExpectedAssignment, rest))
                } else {
                    Err(Error::new(ErrorKind::ExpectedImportOrAssignment, rest))
                }
            } else {
                Ok(document)
            }
        })
}
