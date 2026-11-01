package snipcore

import (
	"strings"
)

// asciiSpace is the bytes.strip() set: space, tab, newline,
// carriage return, vertical tab, form feed. Unicode spaces such
// as \x85 are NOT stripped (probed on CPython 3.9).
const asciiSpace = " \t\n\r\x0b\x0c"

// Normalize compares program output the way tests/run_tests.py does:
// bytes.splitlines() (which splits ONLY on \r\n, \r, \n — probed,
// not assumed) with ASCII-whitespace stripping of every line.
func Normalize(content []byte) []byte {
	text := string(content)
	text = strings.ReplaceAll(text, "\r\n", "\n")
	text = strings.ReplaceAll(text, "\r", "\n")
	lines := strings.Split(text, "\n")
	// A trailing line break ends the last line without adding an
	// empty one ("a\n" -> ["a"], "a\n\n" -> ["a", ""]).
	if strings.HasSuffix(text, "\n") {
		lines = lines[:len(lines)-1]
	}
	for i, line := range lines {
		lines[i] = strings.Trim(line, asciiSpace)
	}
	return []byte(strings.Join(lines, "\n"))
}
