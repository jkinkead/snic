package snic

import (
	"io"
	"unicode"
)

// A lexer produces a Token stream.

// Possible config token types
type TokenType int

const (
	// A portion of a config path.
	KEY TokenType = iota

	// Literal values.
	INTEGER
	DECIMAL
	STRING
	// "_" value for documentation-only values in templates.
	PLACEHOLDER_VALUE

	// Syntax elements.
	// =
	EQUALS
	// {
	LBRACE
	// }
	RBRACE
	// [
	LBRACKET
	// ]
	RBRACKET
	// .
	DOT

	// Keywords.
	SUPER
	SELF
	IMPORT
	TEMPLATE

	COMMENT
	// Whitespace (including comments)
	WHITESPACE

	// End of stream token.
	EOF
	// Illegal token.
	ILLEGAL
)

// A single token in a config file.
type Token struct {
	TokenType TokenType
	Contents  string
	// TODO(jkinkead): Add position here.
}

type Lexer struct {
	// The reader being consumed.
	reader io.RuneScanner
	// The current character in the reader. Only valid if eof is false.
	curr rune
	// True if we're at the end-of-file.
	eof bool
	// The current character number in the line.
	charNumber int
	// The current line number.
	lineNumber int
}

// Constructs a Lexer using the given reader.
// The lexer will consume the reader; a copy is not created.
func NewLexer(reader io.RuneScanner) *Lexer {
	return &Lexer{reader: reader, charNumber: 1, lineNumber: 1}
}

// Get the next token from the lexer, or an error if a read problem occurs.
func (l *Lexer) Next() (Token, error) {
	if l.eof {
		return Token{EOF, ""}, nil
	}

	err := l.nextRune()
	if err != nil {
		return Token{EOF, ""}, err
	}

	var token Token

	switch l.curr {
	// Single-character tokens.
	case '{':
		token = Token{LBRACE, "{"}
	case '}':
		token = Token{RBRACE, "}"}
	case '[':
		token = Token{LBRACKET, "["}
	case ']':
		token = Token{RBRACKET, "]"}
	case '.':
		token = Token{DOT, "."}
	case '#':
		l.reader.UnreadRune()
		var value string
		value, err = l.readComment()
		token = Token{COMMENT, value}
	case '_':
		// Key or underscore literal.
		// TODO: Read next rune, and check if the value's a letter or number!
		token = Token{PLACEHOLDER_VALUE, "_"}
	default:
		if unicode.IsSpace(l.curr) {
			token = Token{WHITESPACE, "TODO"}
			// TODO: Read whitespace!
		} else if unicode.IsDigit(l.curr) {
			// TODO: Read number! Could be decimal or integer.
			token = Token{INTEGER, "TODO"}
		} else if unicode.IsLetter(l.curr) {
			// TODO: Read key!
			token = Token{KEY, "TODO"}
		} else {
			// Unknown character; report an error.
			token = Token{ILLEGAL, string(l.curr)}
		}
	}

	return token, nil
}

// Fetches the next rune from the embedded reader, and puts it in `curr`. If no more runes exist,
// instead sets eof.  Returns an error if the underying reader returns an error.
func (l *Lexer) nextRune() error {
	read, _, err := l.reader.ReadRune()
	l.curr = read
	if err != nil {
		l.eof = true
		if err == io.EOF {
			// Don't treat EOF as a read error.
			err = nil
		}
	} else {
		// TODO(jkinkead): Do line numbers.
		l.charNumber++
	}
	return err
}

// Reads until end-of-line for a comment.
func (l *Lexer) readComment() (string, error) {
	// Reasonable comment length. Note that append will resize as-needed anyway.
	value := make([]rune, 0, 110)
	var err error = nil
	for err = l.nextRune(); l.curr != '\n' && !l.eof && err == nil; err = l.nextRune() {
		value = append(value, l.curr)
	}
	if !l.eof {
		value = append(value, l.curr)
	}
	return string(value), err
}
