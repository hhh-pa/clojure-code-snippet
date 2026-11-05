package measure

import (
	"bytes"
	"os/exec"
	"runtime"
	"strings"
	"testing"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
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

func TestParsePinCount(t *testing.T) {
	if n, err := ParsePinCount("Count 846018869\n"); err != nil || n != 846018869 {
		t.Fatalf("single line: got %d, %v", n, err)
	}
	if n, err := ParsePinCount("Pin 3.31\nCount 1\nCount 42\n"); err != nil || n != 42 {
		t.Fatalf("later Count must win: got %d, %v", n, err)
	}
	for _, bad := range []string{"", "no counts here\n", "Count abc\n", "Count\n", "Count 1 2\n"} {
		if n, err := ParsePinCount(bad); err == nil {
			t.Fatalf("must reject %q: got %d", bad, n)
		}
	}
}

func TestParsePinAlloc(t *testing.T) {
	if n, err := ParsePinAlloc("Count 1\nAlloc 4681445241\n"); err != nil || n != 4681445241 {
		t.Fatalf("paired lines: got %d, %v", n, err)
	}
	if n, err := ParsePinAlloc("Alloc 7\nAlloc 9\n"); err != nil || n != 9 {
		t.Fatalf("later Alloc must win: got %d, %v", n, err)
	}
	for _, bad := range []string{"", "Count 1\n", "Alloc xyz\n", "Alloc\n", "Alloc 1 2\n"} {
		if n, err := ParsePinAlloc(bad); err == nil {
			t.Fatalf("must reject %q: got %d", bad, n)
		}
	}
}

func TestPinLauncher(t *testing.T) {
	got := pinLauncher("/opt/pin/pin")
	if got[len(got)-1] != "/opt/pin/pin" {
		t.Fatalf("launcher must end with the pin binary: %q", got)
	}
	if runtime.GOOS == "linux" {
		if _, err := exec.LookPath("setarch"); err != nil {
			if len(got) != 1 {
				t.Fatalf("without setarch must run pin directly: %q", got)
			}
		} else if len(got) != 3 || got[1] != "-R" || !strings.HasSuffix(got[0], "setarch") {
			t.Fatalf("with setarch must prefix [setarch -R]: %q", got)
		}
		if !PinNoASLRUsable() {
			t.Fatal("PinNoASLRUsable must hold where setarch exists")
		}
	} else if len(got) != 1 {
		t.Fatalf("non-linux must run pin directly: %q", got)
	}
}

func TestPinUsableMissing(t *testing.T) {
	if PinUsable("", "") {
		t.Fatal("empty pin paths must be unusable")
	}
	if PinUsable("/nonexistent/pin", "/nonexistent/tool.so") {
		t.Fatal("missing pin files must be unusable")
	}
}

func TestIsPinTimeout(t *testing.T) {
	if !IsPinTimeout("TIMEOUT (>900s)") {
		t.Fatal("timeout not detected")
	}
	for _, other := range []string{"", "exit 1: boom", "pin output unreadable: x", "pin count unreadable: x"} {
		if IsPinTimeout(other) {
			t.Fatalf("false positive: %q", other)
		}
	}
}

func TestPinOnlySummary(t *testing.T) {
	pin := MinMedMax{Min: pyjson.IntNum(100), Median: pyjson.IntNum(150), Max: pyjson.IntNum(200)}
	alloc := MinMedMax{Min: pyjson.IntNum(1000), Median: pyjson.IntNum(1500), Max: pyjson.IntNum(2000)}
	res := CaseResult{Label: "l", Stem: "s", Case: "c", Ok: true,
		Summary: &CaseSummary{Runs: 0, PinInstructions: &pin, PinAllocatedBytes: &alloc}}
	var buf bytes.Buffer
	PrintTable(&buf, []CaseResult{res}, nil, nil)
	if !strings.Contains(buf.String(), "l/s/c: ok pin_min=100 pin_med=150 pin_max=200 alloc_min=1000 alloc_med=1500 alloc_max=2000") {
		t.Fatalf("pin-only table line wrong:\n%s", buf.String())
	}
	if bmf := BuildBMF([]CaseResult{res}, nil); len(bmf) != 0 {
		t.Fatalf("pin-only results must stay out of BMF: %+v", bmf)
	}
	var nilBuf bytes.Buffer
	PrintTable(&nilBuf, []CaseResult{{Label: "l", Stem: "s", Case: "c", Ok: true}}, nil, nil)
	if !strings.Contains(nilBuf.String(), "l/s/c: ok no timed runs") {
		t.Fatalf("nil-summary table line wrong:\n%s", nilBuf.String())
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
