// Package cljread splits Clojure source into top-level forms for the
// AOT transformer. It scans bracket depth with string, character
// literal, and line-comment awareness; every repository script is
// header comments plus balanced paren forms, and anything else
// (unbalanced input, read-eval) is rejected loudly rather than
// transformed wrong.
package cljread

import (
	"errors"
	"strings"
)

// Split returns the top-level forms of src in order (exact source
// spans, comments between forms dropped). At least one form must
// exist and all brackets must balance.
func Split(src string) ([]string, error) {
	var forms []string
	i, n := 0, len(src)
	for {
		i = skipSpace(src, i)
		if i >= n {
			break
		}
		end, err := scanForm(src, i)
		if err != nil {
			return nil, err
		}
		forms = append(forms, src[i:end])
		i = end
	}
	if len(forms) == 0 {
		return nil, errors.New("cljread: no top-level forms")
	}
	return forms, nil
}

// Head returns the leading symbol of a list form ("defn" for
// "(defn foo ...)"), or "" when the form is not a list or the head
// is not a plain symbol.
func Head(form string) string {
	rest := strings.TrimSpace(form)
	if !strings.HasPrefix(rest, "(") {
		return ""
	}
	rest = rest[1:]
	// Skip reader prefixes that cannot start a definition head.
	for len(rest) > 0 && strings.ContainsRune("'@`~", rune(rest[0])) {
		rest = rest[1:]
	}
	var head strings.Builder
	for _, r := range rest {
		if r == '(' || r == ')' || r == '[' || r == ']' || r == '{' || r == '}' ||
			r == '"' || r == ';' || r == ' ' || r == '\t' || r == '\n' || r == '\r' || r == ',' {
			break
		}
		head.WriteRune(r)
	}
	out := head.String()
	if out == "" || strings.HasPrefix(out, ":") || strings.HasPrefix(out, "#") {
		return ""
	}
	return out
}

// skipSpace advances past whitespace, commas, and line comments.
func skipSpace(src string, i int) int {
	for i < len(src) {
		c := src[i]
		if c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' {
			i++
			continue
		}
		if c == ';' {
			for i < len(src) && src[i] != '\n' {
				i++
			}
			continue
		}
		return i
	}
	return i
}

// scanForm returns the end offset of the form starting at i.
func scanForm(src string, i int) (int, error) {
	c := src[i]
	switch {
	case c == '(' || c == '[' || c == '{':
		return scanBalanced(src, i)
	case c == '"':
		return scanString(src, i)
	case c == '#' && i+1 < len(src) && src[i+1] == '_':
		// Discard prefix plus the discarded form stay one span:
		// splitting them apart would resurrect dead code.
		j := skipSpace(src, i+2)
		if j >= len(src) {
			return 0, errors.New("cljread: dangling #_")
		}
		return scanForm(src, j)
	case c == '#' && i+1 < len(src) && src[i+1] == '=':
		return 0, errors.New("cljread: read-eval #= is not AOT-safe")
	case c == '#' || c == '\'' || c == '@' || c == '`' || c == '~' || c == '^':
		// Reader prefix: the form continues after it.
		j := skipSpace(src, i+1)
		if j >= len(src) {
			return 0, errors.New("cljread: dangling reader prefix")
		}
		return scanForm(src, j)
	default:
		return scanAtom(src, i)
	}
}

// scanBalanced scans a bracketed span, string/char/comment aware.
func scanBalanced(src string, i int) (int, error) {
	depth := 0
	for i < len(src) {
		c := src[i]
		switch {
		case c == '"':
			end, err := scanString(src, i)
			if err != nil {
				return 0, err
			}
			i = end
			continue
		case c == ';':
			for i < len(src) && src[i] != '\n' {
				i++
			}
			continue
		case c == '\\' && depth > 0:
			// Character literal: the next char is opaque.
			// (A backslash also escapes inside strings, but
			// strings are consumed wholesale above.)
			i += 2
			continue
		case c == '(' || c == '[' || c == '{':
			depth++
		case c == ')' || c == ']' || c == '}':
			depth--
			if depth == 0 {
				return i + 1, nil
			}
		}
		i++
	}
	return 0, errors.New("cljread: unbalanced brackets")
}

// scanString scans a string literal starting at the quote.
func scanString(src string, i int) (int, error) {
	i++ // opening quote
	for i < len(src) {
		switch src[i] {
		case '\\':
			i += 2
		case '"':
			return i + 1, nil
		default:
			i++
		}
	}
	return 0, errors.New("cljread: unterminated string")
}

// scanAtom scans a symbol/keyword/number/char literal to its delimiter.
func scanAtom(src string, i int) (int, error) {
	if src[i] == '\\' {
		// Top-level char literal: backslash plus one char name.
		i++
		for i < len(src) && isSymChar(src[i]) {
			i++
		}
		return i, nil
	}
	for i < len(src) && isSymChar(src[i]) {
		i++
	}
	return i, nil
}

func isSymChar(c byte) bool {
	switch c {
	case ' ', '\t', '\n', '\r', ',', ';', '"', '(', ')', '[', ']', '{', '}':
		return false
	}
	return true
}
