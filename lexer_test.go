package snic

import (
	"bytes"
	"testing"
)

func TestNextRune(t *testing.T) {
	fixtures := []struct {
		bytes          []byte
		expected_errs  []error
		expected_runes []rune
	}{
		{[]byte("ok"), []error{nil, nil}, []rune{'o', 'k'}},
		{[]byte{0x20, 0xff}, []error{nil, nil}, []rune{' ', '\ufffd'}},
	}
	for _, fixture := range fixtures {
		lexer := NewLexer(bytes.NewReader(fixture.bytes))
		for i, expected_err := range fixture.expected_errs {
			got_err := lexer.nextRune()
			if got_err != expected_err {
				t.Errorf("Expected %s; got %s", expected_err, got_err)
			}
			if expected_err == nil && lexer.curr != fixture.expected_runes[i] {
				t.Errorf("Expected rune %q; got %q", fixture.expected_runes[i], lexer.curr)
			}
		}
		got_err := lexer.nextRune()
		if !lexer.eof {
			t.Error("Expected EOF after test case")
		}
		if got_err != nil {
			t.Errorf("Expected nil error after test case, got %s", got_err)
		}
	}
}

func TestReadComment(t *testing.T) {
	fixtures := []struct {
		input string
		value string
		err   error
		// Next run after a call to nextRune().
		next   rune
		is_eof bool
	}{
		{"# a long comment\n1", "# a long comment\n", nil, '1', false},
		{"# eof after comment", "# eof after comment", nil, '\u0000', true},
		{"# eof after comment newline\n", "# eof after comment newline\n", nil, '\u0000', true},
	}
	for _, fixture := range fixtures {
		lexer := NewLexer(bytes.NewReader([]byte(fixture.input)))
		value, err := lexer.readComment()
		if value != fixture.value {
			t.Errorf("Expected value %q; got %q", fixture.value, value)
		}
		if err != fixture.err {
			t.Errorf("Expected %s; got %s", fixture.err, err)
		}
		err = lexer.nextRune()
		if err != nil {
			t.Errorf("Unexpected error: %s", err)
		}
		if lexer.curr != fixture.next {
			t.Errorf("Expected %q; got %q", fixture.next, lexer.curr)
		}
		if lexer.eof != fixture.is_eof {
			t.Errorf("Expected EOF = %s, got EOF = %s", fixture.is_eof, lexer.eof)
		}
	}
}
