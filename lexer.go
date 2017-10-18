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
	// TODO(jkinkead): Add position here?
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
	return &Lexer{reader: reader, charNumber: 0, lineNumber: 1}
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

	var tokenType TokenType
	var contents string

	switch l.curr {
	// Single-character tokens.
	case '{':
		tokenType = LBRACE
		contents = "{"
	case '}':
		tokenType = RBRACE
		contents = "}"
	case '[':
		tokenType = LBRACKET
		contents = "["
	case ']':
		tokenType = RBRACKET
		contents = "]"
	case '.':
		tokenType = DOT
		contents = "."
	case '#':
		err = l.unreadRune()
		if err == nil {
			tokenType = COMMENT
			contents, err = l.readComment()
		}
	case '_':
		// Key or underscore literal.
		// TODO: Read key, and check for PLACEHOLDER_VALUE instead.
		tokenType = PLACEHOLDER_VALUE
		contents = "_"
	default:
		if unicode.IsSpace(l.curr) {
			// Read whitespace.
			tokenType = WHITESPACE
			contents, err = l.readWhitespace()
		} else if unicode.IsDigit(l.curr) {
			// TODO: Read number! Could be decimal or integer.
			tokenType = INTEGER
		} else if unicode.IsLetter(l.curr) {
			// TODO: Read key!
			tokenType = KEY
		} else {
			// Unknown character; report an error.
			tokenType = ILLEGAL
			contents = string(l.curr)
		}
	}

	token := Token{tokenType, contents}
	return token, nil
}

// Fetches the next rune from the embedded reader, and puts it in `curr`. If no more runes exist,
// instead sets eof.  Returns an error if the underying reader returns an error.
func (l *Lexer) nextRune() error {
	read, _, err := l.reader.ReadRune()
	// Advance the newline / character counters.
	if l.curr == '\n' {
		l.lineNumber++
		l.charNumber = 1
	} else {
		l.charNumber++
	}
	l.curr = read
	if err != nil {
		l.eof = true
		if err == io.EOF {
			// Don't treat EOF as a read error.
			err = nil
		}
	}
	return err
}

// Pushes the last-read rune back onto the input reader. Calling this more than once between calls
// to nextRune will result in an error.
func (l *Lexer) unreadRune() error {
	// This is a little odd with line numbers; backing onto a newline will put us at char 0 on the
	// next line, instead of on the last char of the previous line as would be correct.
	l.charNumber--
	return l.reader.UnreadRune()
}

// Reads until end-of-line for a comment.
func (l *Lexer) readComment() (string, error) {
	// Reasonable comment length. Note that append will resize as-needed anyway.
	value := make([]rune, 0, 110)
	var err error = nil
	for err = l.nextRune(); !l.eof && err == nil && l.curr != '\n'; err = l.nextRune() {
		value = append(value, l.curr)
	}
	// Append any end-of-line character read.
	if !l.eof {
		value = append(value, l.curr)
	}
	return string(value), err
}

// Skips whitespace until a non-whitespace character is encountered.
func (l *Lexer) readWhitespace() (string, error) {
	value := make([]rune, 0, 10)
	var err error = nil
	for err = l.nextRune(); !l.eof && err == nil && unicode.IsSpace(l.curr); err = l.nextRune() {
		value = append(value, l.curr)
	}
	if err == nil {
		err = l.unreadRune()
	}
	return string(value), err
}
