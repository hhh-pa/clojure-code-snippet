package measure

import (
	"errors"
	"strings"
)

// SplitCommand ports shlex.split(template, posix=True): POSIX shell
// words with single/double quotes and backslash escapes, no comment
// or expansion handling.
func SplitCommand(text string) ([]string, error) {
	var tokens []string
	var current strings.Builder
	inToken := false
	i := 0
	flush := func() {
		if inToken {
			tokens = append(tokens, current.String())
			current.Reset()
			inToken = false
		}
	}
	for i < len(text) {
		c := text[i]
		switch {
		case c == ' ' || c == '\t' || c == '\n' || c == '\r':
			flush()
			i++
		case c == '\'' || c == '"':
			quote := c
			i++
			closed := false
			for i < len(text) {
				d := text[i]
				if d == quote {
					closed = true
					i++
					break
				}
				if quote == '"' && d == '\\' && i+1 < len(text) &&
					(text[i+1] == '"' || text[i+1] == '\\') {
					current.WriteByte(text[i+1])
					inToken = true
					i += 2
					continue
				}
				current.WriteByte(d)
				inToken = true
				i++
			}
			if !closed {
				return nil, errors.New("No closing quotation")
			}
			// A quote pair starts a token even when empty.
			inToken = true
		case c == '\\':
			if i+1 >= len(text) {
				return nil, errors.New("No escaped character")
			}
			current.WriteByte(text[i+1])
			inToken = true
			i += 2
		default:
			current.WriteByte(c)
			inToken = true
			i++
		}
	}
	flush()
	return tokens, nil
}
