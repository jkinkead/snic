package snic

import (
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"unicode"
)

// A lexer produces a Token stream.

// Possible config token types
type TokenType int

const (
	// A portion of a config path, or a keyword.
	BAREWORD TokenType = iota

	// Literal values.
	INTEGER
	DECIMAL
	STRING
	TRUE
	FALSE

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
	case '"':
		err = l.unreadRune()
		if err == nil {
			tokenType = STRING
			contents, err = l.readEscapedString()
		}
	case '-':
		// Negative number. Read.
		err = l.unreadRune()
		if err == nil {
			tokenType, contents, err = l.readNumber()
		}
	case '_':
		// Key or underscore literal.
		l.unreadRune()
		tokenType = BAREWORD
		contents, err = l.readBareword()
	default:
		if unicode.IsSpace(l.curr) {
			// Read whitespace.
			tokenType = WHITESPACE
			contents, err = l.readWhitespace()
		} else if unicode.IsDigit(l.curr) {
			// Read number.
			err = l.unreadRune()
			if err == nil {
				tokenType, contents, err = l.readNumber()
			}
		} else if unicode.IsLetter(l.curr) {
			tokenType = BAREWORD
			contents, err = l.readBareword()
		} else {
			// Unknown character; report an error.
			tokenType = ILLEGAL
			contents = string(l.curr)
		}
	}

	if err == io.EOF {
		tokenType = EOF
	}

	token := Token{tokenType, contents}
	return token, err
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
	}
	return err
}

// Pushes the last-read rune back onto the input reader. Calling this more than once between calls
// to nextRune will result in an error.
func (l *Lexer) unreadRune() error {
	if l.eof {
		// No-op.
		return nil
	}
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
	for err = l.nextRune(); err == nil && l.curr != '\n'; err = l.nextRune() {
		value = append(value, l.curr)
	}
	// Append any end-of-line character read.
	if err == nil {
		value = append(value, l.curr)
	} else if err == io.EOF {
		// EOF can terminate a comment.
		err = nil
	}
	return string(value), err
}

// Reads a double-quoted string, processing backslash escapes.
func (l *Lexer) readEscapedString() (string, error) {
	err := l.nextRune()
	if err != nil {
		return "", err
	}
	if l.curr != '"' {
		// Shouldn't happen unless readEscapedString is called incorrectly.
		return "", fmt.Errorf("Unexpected character at start of string: %q", l.curr)
	}
	// Reasonable string length. Note that append will resize as-needed anyway.
	value := make([]rune, 0, 100)
	for err = l.nextRune(); err == nil && l.curr != '"'; err = l.nextRune() {
		// Handle escapes.
		if l.curr == '\\' {
			err = l.nextRune()
			if err != nil {
				if err == io.EOF {
					err = fmt.Errorf("expected escaped character; got EOF")
				}
				return string(value), err
			}
			switch l.curr {
			case '"', '\\':
				value = append(value, l.curr)
			case 'n':
				value = append(value, '\n')
			case 'r':
				value = append(value, '\r')
			case 't':
				value = append(value, '\t')
			case 'u':
				// Read hex digits.
				var evaluatedRune rune
				evaluatedRune, err = l.readHexEscape()
				if err != nil {
					return string(value), err
				}
				value = append(value, evaluatedRune)
			default:
				// All other escapes trigger an error.
				return string(value), fmt.Errorf("illegal escape code: \\%c", l.curr)
			}
		} else {
			value = append(value, l.curr)
		}
	}
	if err == io.EOF {
		err = errors.New("expecting end-of-string; got EOF")
	}
	return string(value), err
}

// Reads a four-character unicode escape, and translates it into a rune.
func (l *Lexer) readHexEscape() (rune, error) {
	chars := make([]rune, 4)
	for i := 0; i < 4; i++ {
		err := l.nextRune()
		if err != nil {
			if err == io.EOF {
				err = errors.New("expected hexadecimal digit; got EOF")
			}
			return '\u0000', err
		}
		switch l.curr {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A',
			'B', 'C', 'D', 'E', 'F':
			chars[i] = l.curr
		default:
			return '\u0000', fmt.Errorf("expected hexadecimal digit; got %q", l.curr)
		}
	}
	result, err := hex.DecodeString(string(chars))
	if err != nil {
		// Shouldn't happen; we've ensured it's only hex characters.
		return '\u0000', fmt.Errorf("unexpected error decoding hexadecimal escape: %s", err)
	}
	return (rune(result[0]) << 8) | rune(result[1]), nil
}

// Reads whitespace until a non-whitespace character is encountered.
func (l *Lexer) readWhitespace() (string, error) {
	value := make([]rune, 0, 10)
	var err error = nil
	for err = l.nextRune(); err == nil && unicode.IsSpace(l.curr); err = l.nextRune() {
		value = append(value, l.curr)
	}
	if err == nil {
		err = l.unreadRune()
	}
	return string(value), err
}

// Reads a number. This could be an integer or a decimal number. The type read, contents, and any
// error are returned.
// This parses any number which matches the JSON spec.
// Regex: -?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?
func (l *Lexer) readNumber() (TokenType, string, error) {
	value := make([]rune, 0, 10)
	err := l.nextRune()
	if err != nil {
		return INTEGER, "", err
	}

	// Optional negative sign.
	if l.curr == '-' {
		value = append(value, l.curr)
		err = l.nextRune()
		if err != nil {
			if err == io.EOF {
				err = errors.New("expected digit; got EOF")
			}
			return INTEGER, "", err
		}
	}

	// Require leading digit.
	if !unicode.IsDigit(l.curr) {
		return INTEGER, "", fmt.Errorf("illegal start of number: %q", l.curr)
	}
	value = append(value, l.curr)

	if l.curr == '0' {
		// No digit allowed.
		err = l.nextRune()
		if err != nil {
			if err == io.EOF {
				err = nil
			}
			return INTEGER, string(value), err
		}
		if unicode.IsDigit(l.curr) {
			return INTEGER, string(value), errors.New("illegal number format (no octal)")
		}
	} else {
		// Read integral portion.
		for err = l.nextRune(); err == nil && unicode.IsDigit(l.curr); err = l.nextRune() {
			value = append(value, l.curr)
		}
	}

	if err != nil && err != io.EOF {
		return INTEGER, string(value), err
	}

	if l.curr != '.' {
		// Ignore EOF; it's caught next time.
		l.unreadRune()
		return INTEGER, string(value), nil
	}
	value = append(value, l.curr)

	// Read decimal portion.
	err = l.nextRune()
	if err != nil {
		if err == io.EOF {
			err = fmt.Errorf("expected digit; got EOF")
		}
		return DECIMAL, string(value), err
	}
	if !unicode.IsDigit(l.curr) {
		return DECIMAL, string(value), fmt.Errorf("expected digit; got %q", l.curr)
	}
	value = append(value, l.curr)
	for err = l.nextRune(); !l.eof && err == nil && unicode.IsDigit(l.curr); err = l.nextRune() {
		value = append(value, l.curr)
	}

	// TODO: Handle exponents.

	l.unreadRune()

	// EOF is OK here.
	if err == io.EOF {
		err = nil
	}

	return DECIMAL, string(value), err
}

// Reads a bareword. Barewords must be a letter, followed by zero or more letters or digits.
func (l *Lexer) readBareword() (string, error) {
	value := make([]rune, 0, 10)
	err := l.nextRune()
	if err != nil {
		return "", err
	}
	if !unicode.IsLetter(l.curr) && l.curr != '_' {
		return "", fmt.Errorf("expected letter, got %q", l.curr)
	}

	value = append(value, l.curr)
	for err = l.nextRune(); !l.eof && err == nil && (unicode.IsLetter(l.curr) || unicode.IsDigit(l.curr) || l.curr == '_'); err = l.nextRune() {
		value = append(value, l.curr)
	}
	l.unreadRune()

	// EOF is OK here.
	if err == io.EOF {
		err = nil
	}

	return string(value), err
}
