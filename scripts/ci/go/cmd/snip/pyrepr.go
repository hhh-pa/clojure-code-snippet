package main

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// pyRepr formats text the way Python's repr() formats str: single
// quotes unless the text contains a quote but no double quote, with
// short escapes for whitespace and \x/\u/\U escapes for the rest.
// Only failure diagnostics use it, so mismatch lines diff cleanly
// against the Python tools.
func pyRepr(text string) string {
	quote := "'"
	if strings.Contains(text, "'") && !strings.Contains(text, `"`) {
		quote = `"`
	}
	var sb strings.Builder
	sb.WriteString(quote)
	for _, r := range text {
		switch r {
		case '\\':
			sb.WriteString(`\\`)
		case '\n':
			sb.WriteString(`\n`)
		case '\t':
			sb.WriteString(`\t`)
		case '\r':
			sb.WriteString(`\r`)
		default:
			if rune(quote[0]) == r {
				sb.WriteByte('\\')
				sb.WriteRune(r)
			} else if r < utf8.RuneSelf && (r < 0x20 || r == 0x7f) {
				fmt.Fprintf(&sb, `\x%02x`, r)
			} else if unicode.IsPrint(r) {
				sb.WriteRune(r)
			} else if r < 0x100 {
				fmt.Fprintf(&sb, `\x%02x`, r)
			} else if r < 0x10000 {
				fmt.Fprintf(&sb, `\u%04x`, r)
			} else {
				fmt.Fprintf(&sb, `\U%08x`, r)
			}
		}
	}
	sb.WriteString(quote)
	return sb.String()
}
