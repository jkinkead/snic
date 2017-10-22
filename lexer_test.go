package snic

import (
	"bytes"
	"io"
	"testing"
)

func TestNextRune(t *testing.T) {
	fixtures := []struct {
		name           string
		bytes          []byte
		expected_errs  []error
		expected_runes []rune
	}{
		{"GoodUtf", []byte("ok"), []error{nil, nil}, []rune{'o', 'k'}},
		{"BadUtf", []byte{0x20, 0xff}, []error{nil, nil}, []rune{' ', '\ufffd'}},
	}
	for _, fixture := range fixtures {
		t.Run(fixture.name, func(t *testing.T) {
			l := NewLexer(bytes.NewReader(fixture.bytes))
			for i, expected_err := range fixture.expected_errs {
				got_err := l.nextRune()
				if got_err != expected_err {
					t.Errorf("Expected %s; got %s", expected_err, got_err)
				}
				if expected_err == nil && l.curr != fixture.expected_runes[i] {
					t.Errorf("Expected rune %q; got %q", fixture.expected_runes[i], l.curr)
				}
			}
			got_err := l.nextRune()
			if !l.eof {
				t.Error("Expected EOF after test case")
			}
			if got_err != io.EOF {
				t.Errorf("Expected EOF error after test case, got %s", got_err)
			}
		})
	}
}

func TestCounts(t *testing.T) {
	// Tests that counts are handled correctly when nextRune / unreadRune are called.
	input := "one\ntwo\nthree\nEND"
	l := NewLexer(bytes.NewReader([]byte(input)))
	runes := []rune(input)
	counts := []struct{ charNumber, lineNumber int }{
		// First line.
		{1, 1}, {2, 1}, {3, 1}, {4, 1},
		// Second line.
		{1, 2}, {2, 2}, {3, 2}, {4, 2},
		// Third line.
		{1, 3}, {2, 3}, {3, 3}, {4, 3}, {5, 3}, {6, 3},
		// Final line.
		{1, 4}, {2, 4}, {3, 4},
	}
	count := 0
	for err := l.nextRune(); !l.eof; err = l.nextRune() {
		if err != nil {
			t.Errorf("Unexpected error at char %d: %s", count, err)
		}
		value := l.curr
		if runes[count] != value {
			t.Errorf("Expected %q at char %d; got %q", runes[count], count, value)
		}
		if counts[count].charNumber != l.charNumber {
			t.Errorf("Expected char number %d, got %d at char %d",
				counts[count].charNumber, l.charNumber, count)
		}
		if counts[count].lineNumber != l.lineNumber {
			t.Errorf("Expected line number %d, got %d at char %d",
				counts[count].lineNumber, l.lineNumber, count)
		}
		count++
	}
}

// Tests that unreadRune will back up character / line counts as expected.
func TestUnread(t *testing.T) {
	input := "aa\nb\nc"
	l := NewLexer(bytes.NewReader([]byte(input)))
	l.nextRune()
	l.nextRune()
	l.unreadRune()
	if l.lineNumber != 1 {
		t.Errorf("Expected line one after first backup")
	}
	if l.charNumber != 1 {
		t.Errorf("Expected char one after first backup")
	}
	l.nextRune()
	l.nextRune()
	l.nextRune()
	// At 'b'
	if l.curr != 'b' {
		t.Errorf("Should be at 'b'")
	}
	if l.lineNumber != 2 {
		t.Errorf("Expected line two at 'b'")
	}
	if l.charNumber != 1 {
		t.Errorf("Expected char one at 'b'")
	}
	l.unreadRune()
	if l.lineNumber != 2 {
		t.Errorf("Expected line two before 'b'")
	}
	if l.charNumber != 0 {
		t.Errorf("Expected char zero before 'b'")
	}
	l.nextRune()
	l.nextRune()
	l.nextRune()
	if l.curr != 'c' {
		t.Errorf("Should be at 'c'")
	}
	if l.lineNumber != 3 {
		t.Errorf("Expected line three at 'c'")
	}
	if l.charNumber != 1 {
		t.Errorf("Expected char one at 'c'")
	}
}

func TestReadComment(t *testing.T) {
	fixtures := []struct {
		name  string
		input string
		value string
		err   error
		// Next rune after a call to nextRune().
		next   rune
		is_eof bool
	}{
		{"SimpleComment", "# comment\n1", "# comment\n", nil, '1', false},
		{"CommentEof", "# eof after comment", "# eof after comment", nil, '\u0000', true},
		{"CommentNewlineEof", "# comment newline\n", "# comment newline\n", nil, '\u0000', true},
	}
	for _, fixture := range fixtures {
		t.Run(fixture.name, func(t *testing.T) {
			l := NewLexer(bytes.NewReader([]byte(fixture.input)))
			value, err := l.readComment()
			if value != fixture.value {
				t.Errorf("Expected value %q; got %q", fixture.value, value)
			}
			if err != fixture.err {
				t.Errorf("Expected %s; got %s", fixture.err, err)
			}
			err = l.nextRune()
			if err != nil {
				if err != io.EOF {
					t.Errorf("Unexpected error: %s", err)
				}
			}
			if l.curr != fixture.next {
				t.Errorf("Expected %q; got %q", fixture.next, l.curr)
			}
			if l.eof != fixture.is_eof {
				t.Errorf("Expected EOF = %t, got EOF = %t", fixture.is_eof, l.eof)
			}
			if fixture.is_eof && err != io.EOF {
				t.Error("Expected EOF error")
			}
		})
	}
}

func TestReadNumber(t *testing.T) {
	fixtures := []struct {
		name      string
		input     string
		tokenType TokenType
		value     string
		err       error
		// Next rune after a call to nextRune().
		next rune
	}{
		{"Integer", "123 4", INTEGER, "123", nil, ' '},
		{"Decimal", "1.23\n", DECIMAL, "1.23", nil, '\n'},
		{"DecimalNoExponent", "12.", DECIMAL, "12.", io.EOF, '\u0000'},
	}
	for _, fixture := range fixtures {
		t.Run(fixture.name, func(t *testing.T) {
			l := NewLexer(bytes.NewReader([]byte(fixture.input)))
			tokenType, value, err := l.readNumber()
			if tokenType != fixture.tokenType {
				t.Errorf("Expected token type %d; got %d", fixture.tokenType, tokenType)
			}
			if value != fixture.value {
				t.Errorf("Expected value %q; got %q", fixture.value, value)
			}
			if err != fixture.err {
				t.Errorf("Expected %s; got %s", fixture.err, err)
			}
			err = l.nextRune()
			if err != nil && err != io.EOF {
				t.Errorf("Unexpected error: %s", err)
			}
			if l.curr != fixture.next {
				t.Errorf("Expected %q; got %q", fixture.next, l.curr)
			}
		})
	}
}

func TestReadString(t *testing.T) {
	fixtures := []struct {
		name      string
		input     string
		value     string
		errString string
		// Next rune after a call to nextRune().
		next rune
	}{
		{"Simple", `"foo" `, "foo", "", ' '},
		{"UnterminatedString", `"foo`, "foo", "expecting end-of-string; got EOF", '\u0000'},
		{"BasicEscapes", `"\t\n\\\"foo\"" `, "\t\n\\\"foo\"", "", ' '},
		{"IllegalEscape", `"\g" `, "", "illegal escape code: \\g", '"'},
		{"UnterminatedEscape", `"foo\`, "foo", "expected escaped character; got EOF", '\u0000'},
		{"UnicodeEscapes", `"\u0020- \u03a9, \u03C9" `, " - Ω, ω", "", ' '},
		{"BadHex", `"\ufoobar`, "", "expected hexadecimal digit; got o", 'o'},
		{"UnterminatedHex", `"\ufff`, "", "expected hexadecimal digit; got EOF", '\u0000'},
	}
	for _, fixture := range fixtures {
		t.Run(fixture.name, func(t *testing.T) {
			l := NewLexer(bytes.NewReader([]byte(fixture.input)))
			value, err := l.readEscapedString()
			if value != fixture.value {
				t.Errorf("Expected value %q; got %q", fixture.value, value)
			}
			if fixture.errString != "" && err.Error() != fixture.errString {
				t.Errorf("Expected %s; got %s", fixture.errString, err)
			}
			err = l.nextRune()
			if err != nil && err != io.EOF {
				t.Errorf("Unexpected error: %s", err)
			}
			if l.curr != fixture.next {
				t.Errorf("Expected %q; got %q", fixture.next, l.curr)
			}
		})
	}
}
