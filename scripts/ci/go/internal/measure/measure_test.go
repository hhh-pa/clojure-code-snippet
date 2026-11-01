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
