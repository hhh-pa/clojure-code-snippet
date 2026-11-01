package snipcore

import (
	"strings"
)

// LastLine ports the Python `tail[-1] if tail else "no stderr"` idiom
// for subprocess failure diagnostics.
func LastLine(stderr []byte) string {
	text := strings.TrimSpace(string(stderr))
	if text == "" {
		return "no stderr"
	}
	lines := strings.Split(text, "\n")
	return strings.TrimSpace(lines[len(lines)-1])
}

// TruncateChars cuts text to max runes (Python [:max] on str).
func TruncateChars(text string, max int) string {
	runes := []rune(text)
	if len(runes) > max {
		return string(runes[:max])
	}
	return text
}
