package measure

import (
	"bytes"
	"strings"
	"testing"
)

// TestSelfTestGreen runs the full CLI self-test suite in-process:
// every check must pass with no JVM, time, perf, or valgrind.
func TestSelfTestGreen(t *testing.T) {
	var buf bytes.Buffer
	if code := SelfTest(&buf); code != 0 {
		t.Fatalf("SelfTest exit %d:\n%s", code, buf.String())
	}
	if !strings.Contains(buf.String(), "self-test: 0 failed") {
		t.Fatalf("missing summary:\n%s", buf.String())
	}
}

func TestIsCallgrindTimeout(t *testing.T) {
	if !IsCallgrindTimeout("TIMEOUT (>900s)") {
		t.Fatal("timeout not detected")
	}
	if !IsCallgrindTimeout("alloc-shim TIMEOUT (>900s)") {
		t.Fatal("shim timeout not detected")
	}
	for _, other := range []string{"", "exit 1: boom", "callgrind output unreadable: x",
		"callgrind summary not found", "epsilon log unreadable: x"} {
		if IsCallgrindTimeout(other) {
			t.Fatalf("false positive: %q", other)
		}
	}
}

func TestCallgrindSkipReason(t *testing.T) {
	if r := CallgrindSkipReason("search/beam_search", true); r == "" {
		t.Fatal("exempt label must skip on the JVM leg")
	}
	if r := CallgrindSkipReason("sort/bubble_sort", true); r != "" {
		t.Fatalf("ordinary label must not skip: %q", r)
	}
	if r := CallgrindSkipReason("search/beam_search", false); r != "" {
		t.Fatalf("native leg has no fixed heap, must not skip: %q", r)
	}
}
