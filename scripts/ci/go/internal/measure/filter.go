package measure

import (
	"fmt"
	"regexp"
	"strings"
)

// SplitFilterList splits a comma-separated --only/--exclude value
// into trimmed non-empty patterns. A single value without commas
// behaves exactly like the old plain-substring flag.
func SplitFilterList(s string) []string {
	var out []string
	for _, part := range strings.Split(s, ",") {
		if trimmed := strings.TrimSpace(part); trimmed != "" {
			out = append(out, trimmed)
		}
	}
	return out
}

// FilterMatch reports whether any pattern selects the (label, stem)
// task. A pattern matches when the label or the label/stem
// benchmark name contains it, so slow legs can pin one benchmark
// (e.g. graph/shortest_path/dijkstrashortreach) without catching
// its label siblings (bellman_ford, bfsshortreach). An empty
// pattern list matches nothing.
func FilterMatch(label, stem string, patterns []string) bool {
	if len(patterns) == 0 {
		return false
	}
	name := label + "/" + stem
	for _, p := range patterns {
		if strings.Contains(label, p) || strings.Contains(name, p) {
			return true
		}
	}
	return false
}

var callgrindXmxRe = regexp.MustCompile(`^[0-9]+[gGmM]$`)

// EpsilonHeapFlags returns EpsilonFlags with the fixed 2g heap
// replaced by size (both -Xms and -Xmx, like the default recipe),
// validating a --callgrind-xmx value. Only the size spellings the
// JVM accepts are allowed (digits plus one M/G suffix).
func EpsilonHeapFlags(size string) ([]string, error) {
	if !callgrindXmxRe.MatchString(size) {
		return nil, fmt.Errorf("bad --callgrind-xmx %q: want <digits><M|G>, e.g. 8g", size)
	}
	out := append([]string{}, EpsilonFlags...)
	for i, f := range out {
		if strings.HasPrefix(f, "-Xms") {
			out[i] = "-Xms" + size
		}
		if strings.HasPrefix(f, "-Xmx") {
			out[i] = "-Xmx" + size
		}
	}
	return out, nil
}
