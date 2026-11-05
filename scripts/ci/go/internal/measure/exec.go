package measure

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/aot"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// ExpandTemplate ports str.format for the {cp}/{script}/{stem} holes
// (plus {{/}} escapes). Unknown holes and malformed templates are
// errors shaped like the Python KeyError/ValueError texts.
func ExpandTemplate(template, cp, script, stem string) (string, error) {
	holes := map[string]string{"cp": cp, "script": script, "stem": stem}
	var sb strings.Builder
	i := 0
	for i < len(template) {
		c := template[i]
		switch c {
		case '{':
			if i+1 < len(template) && template[i+1] == '{' {
				sb.WriteByte('{')
				i += 2
				continue
			}
			end := strings.IndexByte(template[i:], '}')
			if end < 0 {
				return "", errors.New("Single '{' encountered in format string")
			}
			name := template[i+1 : i+end]
			if name == "" || strings.ContainsAny(name, "!:{") {
				return "", fmt.Errorf("unsupported hole {%s}", name)
			}
			value, ok := holes[name]
			if !ok {
				return "", fmt.Errorf("%s", snipcore.PyRepr(name))
			}
			sb.WriteString(value)
			i += end + 1
		case '}':
			if i+1 < len(template) && template[i+1] == '}' {
				sb.WriteByte('}')
				i += 2
				continue
			}
			return "", errors.New("Single '}' encountered in format string")
		default:
			sb.WriteByte(c)
			i++
		}
	}
	return sb.String(), nil
}

// BuildInnerCommand ports build_inner_command.
func BuildInnerCommand(template, cp, script, stem string) ([]string, error) {
	expanded, err := ExpandTemplate(template, cp, script, stem)
	if err != nil {
		return nil, fmt.Errorf("bad command template: %v", err)
	}
	argv, err := SplitCommand(expanded)
	if err != nil {
		return nil, fmt.Errorf("bad command template: %v", err)
	}
	if len(argv) == 0 {
		return nil, errors.New("bad command template: expands to empty argv")
	}
	return argv, nil
}

// AllocSupported ports alloc_supported.
func AllocSupported(template, cp, script, stem string) bool {
	inner, err := BuildInnerCommand(template, cp, script, stem)
	if err != nil || len(inner) == 0 {
		return false
	}
	return filepath.Base(inner[0]) == "java"
}

// ResolveTimeBin ports resolve_time_bin.
func ResolveTimeBin(explicit string) (string, error) {
	path := explicit
	if path == "" {
		if _, err := os.Stat("/usr/bin/time"); err == nil {
			path = "/usr/bin/time"
		} else if found, err := exec.LookPath("time"); err == nil {
			path = found
		}
	}
	if path == "" {
		return "", errors.New("GNU time not found: install it (Debian/Ubuntu: apt install time) or pass --time-bin")
	}
	stdout, _, err := snipcore.Run([]string{path, "--version"}, nil, 10)
	if err != nil {
		return "", fmt.Errorf("cannot execute time binary %s: %v", snipcore.PyRepr(path), err)
	}
	if !strings.Contains(string(stdout), "GNU") {
		return "", fmt.Errorf("%s is not GNU time (need -f/-o support); install GNU time or pass --time-bin", snipcore.PyRepr(path))
	}
	return path, nil
}

// PerfUsable ports perf_usable.
func PerfUsable() bool {
	if _, err := exec.LookPath("perf"); err != nil {
		return false
	}
	tmp, err := os.CreateTemp("", "perf-probe-")
	if err != nil {
		return false
	}
	name := tmp.Name()
	tmp.Close()
	defer os.Remove(name)
	_, _, err = snipcore.Run([]string{"perf", "stat", "-e", PerfEvents, "-x,", "-o", name, "--", "true"}, nil, 30)
	if err != nil {
		return false
	}
	raw, err := os.ReadFile(name)
	if err != nil {
		return false
	}
	counters := ParsePerfText(string(raw))
	return counters.Instructions != nil || counters.Cycles != nil
}

// CallgrindUsable reports whether valgrind callgrind runs can execute.
func CallgrindUsable() bool {
	path, err := exec.LookPath("valgrind")
	if err != nil {
		return false
	}
	_, _, err = snipcore.Run([]string{path, "--version"}, nil, 30)
	return err == nil
}

// BuildCommand ports build_command: GNU time wraps the snippet
// directly (perf outside, time inside).
func BuildCommand(timeBin, timeFile string, usePerf bool, perfFile string, inner []string) []string {
	timed := append([]string{timeBin, "-f", TimeFormat, "-o", timeFile}, inner...)
	if usePerf {
		return append([]string{"perf", "stat", "-e", PerfEvents, "-x,", "-o", perfFile, "--"}, timed...)
	}
	return timed
}

// MeasureOnce ports measure_once: one run under GNU time (+perf).
func MeasureOnce(timeBin string, usePerf bool, inner []string, inputPath string, timeout int) ([]byte, *RunMetrics, string) {
	input, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, nil, "open input: " + err.Error()
	}
	timeFile, err := tempFile("snip-time-")
	if err != nil {
		return nil, nil, "time output unreadable: " + err.Error()
	}
	defer os.Remove(timeFile)
	var perfFile string
	if usePerf {
		perfFile, err = tempFile("snip-perf-")
		if err != nil {
			return nil, nil, "perf output unreadable: " + err.Error()
		}
		defer os.Remove(perfFile)
	}
	cmd := BuildCommand(timeBin, timeFile, usePerf, perfFile, inner)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, nil, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, nil, fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	timeRaw, err := os.ReadFile(timeFile)
	if err != nil {
		return nil, nil, "time output unreadable: " + err.Error()
	}
	parsed, err := ParseTimeText(string(timeRaw))
	if err != nil {
		return nil, nil, "time output unreadable: " + err.Error()
	}
	metrics := &RunMetrics{
		WallS: pyjson.FloatNum(parsed.WallS), UserS: pyjson.FloatNum(parsed.UserS),
		SysS: pyjson.FloatNum(parsed.SysS), CpuS: pyjson.FloatNum(parsed.CpuS),
		MaxRSSKb: parsed.MaxRSSKb, CpuPct: pyjson.FloatNum(parsed.CpuPct),
		MajorFaults: parsed.MajorFault, MinorFaults: parsed.MinorFault,
	}
	if usePerf {
		perfRaw, err := os.ReadFile(perfFile)
		if err != nil {
			return nil, nil, "perf output unreadable: " + err.Error()
		}
		counters := ParsePerfText(string(perfRaw))
		metrics.Instructions = counters.Instructions
		metrics.Cycles = counters.Cycles
	}
	return stdout, metrics, ""
}

// MeasureAllocOnce ports measure_alloc_once: one EpsilonGC run.
func MeasureAllocOnce(inner []string, inputPath string, timeout int) ([]byte, int64, string) {
	input, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, 0, "open input: " + err.Error()
	}
	gcFile, err := tempFile("snip-gc-")
	if err != nil {
		return nil, 0, "epsilon log unreadable: " + err.Error()
	}
	defer os.Remove(gcFile)
	cmd := append([]string{inner[0]}, EpsilonFlags...)
	cmd = append(cmd, "-Xlog:gc:"+gcFile)
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		errText := string(stderr)
		msg := fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
		if strings.Contains(errText, "OutOfMemoryError") {
			msg += " (Epsilon heap exhausted; raise -Xmx in EPSILON_FLAGS)"
		}
		return nil, 0, msg
	}
	gcRaw, err := os.ReadFile(gcFile)
	if err != nil {
		return nil, 0, "epsilon log unreadable: " + err.Error()
	}
	used, err := ParseEpsilonUsed(string(gcRaw))
	if err != nil {
		return nil, 0, "epsilon log unreadable: " + err.Error()
	}
	return stdout, used, ""
}

// ResolveArgv0 finds the real binary behind PATH/symlinks: under
// valgrind the /bin/java alternatives symlink loses libjli.so
// (exit 127), so callgrind runs need the resolved path. Used for
// JVM and native legs alike.
func ResolveArgv0(argv0 string) string {
	path := argv0
	if !strings.Contains(path, string(os.PathSeparator)) {
		if found, err := exec.LookPath(path); err == nil {
			path = found
		}
	}
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		return resolved
	}
	return argv0
}

// IsCallgrindTimeout reports whether a CallgrindOnce error text is a
// deadline timeout rather than a crash or output mismatch: timeouts
// mean the input exceeds the instrument's budget, so the case
// degrades to a skip instead of failing the run.
func IsCallgrindTimeout(runErr string) bool {
	return strings.Contains(runErr, "TIMEOUT")
}

// IsAllocShimSkip reports whether a CallgrindOnce error text is a
// native-leg counter problem (missing toolchain, timeout, unreadable
// total) rather than a program failure: the Ir leg already gated
// correctness, so counter problems degrade to a skip.
func IsAllocShimSkip(runErr string) bool {
	return strings.HasPrefix(runErr, "alloc-shim ")
}

// CallgrindSkipReason returns the skip reason when the callgrind
// phase must not run for this label, or "" to proceed. On the JVM
// leg, EpsilonGC exempt workloads exceed the fixed callgrind heap
// by definition (the callgrind recipe uses the same EpsilonGC
// flags), so they skip outright instead of OOMing. The native leg
// has no fixed heap, so it never skips for size reasons.
func CallgrindSkipReason(label string, javaLeg bool) string {
	if javaLeg && allocExempt(label) {
		return "EpsilonGC-exempt workload exceeds the fixed callgrind heap"
	}
	return ""
}

// PinJVMFlags keeps HotSpot stable under Intel Pin: -Xrs stops
// the JVM from installing its full OS signal handlers (which
// would fight Pin's signal interception) and
// -XX:+AllowUserSignalHandlers lets Pin's own handlers coexist
// with the remaining JVM signal use (implicit null checks, thread
// coordination).
var PinJVMFlags = []string{"-Xrs", "-XX:+AllowUserSignalHandlers"}

// PinUsable reports whether Pin count runs can execute: the pin
// launcher resolves and the inscount tool file exists.
func PinUsable(pinBin, tool string) bool {
	if pinBin == "" || tool == "" {
		return false
	}
	if _, err := os.Stat(tool); err != nil {
		return false
	}
	if strings.Contains(pinBin, string(os.PathSeparator)) {
		info, err := os.Stat(pinBin)
		return err == nil && info.Mode()&0o111 != 0
	}
	_, err := exec.LookPath(pinBin)
	return err == nil
}

// PinVersion probes `pin -version` for report provenance, or nil
// when the launcher cannot answer. Compared runs must share one
// Pin version: counts are only comparable within a version.
func PinVersion(pinBin string) *string {
	stdout, _, err := snipcore.Run([]string{pinBin, "-version"}, nil, 30)
	if err != nil {
		return nil
	}
	for _, line := range strings.Split(string(stdout), "\n") {
		if trimmed := strings.TrimSpace(line); trimmed != "" {
			return &trimmed
		}
	}
	return nil
}

// IsPinTimeout reports whether a PinOnce error text is a deadline
// timeout rather than a crash: timeouts mean the input exceeds
// the instrument's budget, so the case degrades to a skip.
func IsPinTimeout(runErr string) bool {
	return strings.Contains(runErr, "TIMEOUT")
}

// PinOnce runs inner once under Intel Pin with the inscount tool
// and returns stdout plus the total instruction count,
// dispatching on the resolved argv0: the JVM recipe (below) for
// java, the plain recipe (no JVM flags) otherwise.
func PinOnce(pinBin, tool string, inner []string, inputPath string, timeout int) ([]byte, int64, string) {
	if !InnerIsJava(inner) {
		return nativePinOnce(pinBin, tool, inner, inputPath, timeout)
	}
	return jvmPinOnce(pinBin, tool, inner, inputPath, timeout)
}

// jvmPinOnce runs a JVM argv once under Pin with PinJVMFlags
// injected right after the resolved java binary (JVM options must
// precede -cp and the main class).
func jvmPinOnce(pinBin, tool string, inner []string, inputPath string, timeout int) ([]byte, int64, string) {
	input, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, 0, "open input: " + err.Error()
	}
	outFile, err := tempFile("snip-pin-")
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	defer os.Remove(outFile)
	cmd := []string{pinBin, "-t", tool, "-o", outFile, "--", ResolveArgv0(inner[0])}
	cmd = append(cmd, PinJVMFlags...)
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, 0, fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	raw, err := os.ReadFile(outFile)
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	count, err := ParsePinCount(string(raw))
	if err != nil {
		return nil, 0, err.Error()
	}
	return stdout, count, ""
}

// nativePinOnce runs a native argv once under Pin with the
// inscount tool (same -o plumbing, no JVM flags).
func nativePinOnce(pinBin, tool string, inner []string, inputPath string, timeout int) ([]byte, int64, string) {
	input, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, 0, "open input: " + err.Error()
	}
	outFile, err := tempFile("snip-pin-")
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	defer os.Remove(outFile)
	cmd := []string{pinBin, "-t", tool, "-o", outFile, "--", ResolveArgv0(inner[0])}
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, 0, fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	raw, err := os.ReadFile(outFile)
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	count, err := ParsePinCount(string(raw))
	if err != nil {
		return nil, 0, err.Error()
	}
	return stdout, count, ""
}

// CallgrindOnce runs inner once under valgrind callgrind and
// returns stdout plus total Ir and total allocation, dispatching on
// the resolved argv0: the JVM recipe (below) for java, the native
// recipe (no JVM flags, LD_PRELOAD-counted allocation) otherwise.
func CallgrindOnce(inner []string, inputPath string, timeout int) ([]byte, int64, int64, string) {
	if !InnerIsJava(inner) {
		return nativeCallgrindOnce(inner, inputPath, timeout)
	}
	return jvmCallgrindOnce(inner, inputPath, timeout)
}

// jvmCallgrindOnce runs inner once under valgrind callgrind with the
// deterministic recipe (-Xint, EpsilonGC, fixed heap, no perfdata)
// and returns stdout plus total Ir and EpsilonGC total allocation.
// The allocation comes from the JVM's own GC log, not from valgrind:
// valgrind-side heap profilers (Massif/DHAT) see only HotSpot's
// bulk-mmaped heap reservation, never Java objects.
func jvmCallgrindOnce(inner []string, inputPath string, timeout int) ([]byte, int64, int64, string) {
	input, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, 0, 0, "open input: " + err.Error()
	}
	outFile, err := tempFile("snip-cg-")
	if err != nil {
		return nil, 0, 0, "callgrind output unreadable: " + err.Error()
	}
	defer os.Remove(outFile)
	gcFile, err := tempFile("snip-cg-gc-")
	if err != nil {
		return nil, 0, 0, "epsilon log unreadable: " + err.Error()
	}
	defer os.Remove(gcFile)
	jvmFlags := append([]string{"-Xint"}, EpsilonFlags...)
	jvmFlags = append(jvmFlags, "-XX:-UsePerfData", "-Xlog:gc:"+gcFile)
	cmd := []string{"valgrind", "--tool=callgrind",
		"--cache-sim=no", "--branch-sim=no", "--collect-jumps=no",
		"--callgrind-out-file=" + outFile, ResolveArgv0(inner[0])}
	cmd = append(cmd, jvmFlags...)
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, 0, 0, fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	raw, err := os.ReadFile(outFile)
	if err != nil {
		return nil, 0, 0, "callgrind output unreadable: " + err.Error()
	}
	ir, err := ParseCallgrindSummary(string(raw))
	if err != nil {
		return nil, 0, 0, err.Error()
	}
	gcRaw, err := os.ReadFile(gcFile)
	if err != nil {
		return nil, 0, 0, "epsilon log unreadable: " + err.Error()
	}
	used, err := ParseEpsilonUsed(string(gcRaw))
	if err != nil {
		return nil, 0, 0, "epsilon log unreadable: " + err.Error()
	}
	return stdout, ir, used, ""
}

// nativeCallgrindOnce runs a native binary once under valgrind
// callgrind (same no-sim flags, no JVM flags) for Ir, plus one
// near-native-speed LD_PRELOAD counting run for allocation. The Ir
// leg is authoritative — its crashes fail like the JVM path — while
// every counter-leg problem degrades to an "alloc-shim ..." skip
// reason (a broken counter must never fail correct runs).
func nativeCallgrindOnce(inner []string, inputPath string, timeout int) ([]byte, int64, int64, string) {
	input, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, 0, 0, "open input: " + err.Error()
	}
	outFile, err := tempFile("snip-cg-")
	if err != nil {
		return nil, 0, 0, "callgrind output unreadable: " + err.Error()
	}
	defer os.Remove(outFile)
	cmd := []string{"valgrind", "--tool=callgrind",
		"--cache-sim=no", "--branch-sim=no", "--collect-jumps=no",
		"--callgrind-out-file=" + outFile, ResolveArgv0(inner[0])}
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, 0, 0, fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	raw, err := os.ReadFile(outFile)
	if err != nil {
		return nil, 0, 0, "callgrind output unreadable: " + err.Error()
	}
	ir, err := ParseCallgrindSummary(string(raw))
	if err != nil {
		return nil, 0, 0, err.Error()
	}
	used, skip := MeasureShimAllocOnce(inner, input, stdout, timeout)
	if skip != "" {
		return nil, 0, 0, skip
	}
	return stdout, ir, used, ""
}

// MeasureShimAllocOnce runs inner once under the LD_PRELOAD
// counting shim and returns total allocated bytes. wantStdout is
// the Ir leg's stdout: a shim-run mismatch means the counter run
// diverged, which degrades like any other counter problem. Every
// failure returns a skip reason (never a hard error); "" means
// success.
func MeasureShimAllocOnce(inner []string, input, wantStdout []byte, timeout int) (int64, string) {
	so, err := EnsureAllocShim()
	if err != nil {
		return 0, "alloc-shim unavailable: " + err.Error()
	}
	stdout, stderr, err := snipcore.RunEnv(inner, input, timeout, []string{"LD_PRELOAD=" + so})
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return 0, fmt.Sprintf("alloc-shim TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return 0, fmt.Sprintf("alloc-shim exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	if string(snipcore.Normalize(stdout)) != string(snipcore.Normalize(wantStdout)) {
		return 0, "alloc-shim output mismatch: counter run diverged from the callgrind run"
	}
	used, err := ParseAllocShimTotal(string(stderr))
	if err != nil {
		return 0, err.Error()
	}
	return used, ""
}

func tempFile(prefix string) (string, error) {
	fd, err := os.CreateTemp("", prefix)
	if err != nil {
		return "", err
	}
	name := fd.Name()
	fd.Close()
	return name, nil
}

// AOTCache compiles each script at most once per run (plus the
// persistent content-hash cache underneath). Concurrent builds of
// different scripts proceed in parallel; concurrent builds of the
// same script collapse into one.
type AOTCache struct {
	Root      string
	CacheDir  string
	Classpath string
	Java      string
	Timeout   int

	mu    sync.Mutex
	built map[string]*aot.Built
	errs  map[string]error
	calls map[string]*aotCall
}

type aotCall struct {
	done  chan struct{}
	built *aot.Built
	err   error
}

// Get returns the built artifact for a script, compiling on first use.
func (c *AOTCache) Get(label, stem, script string) (*aot.Built, error) {
	c.mu.Lock()
	if c.built == nil {
		c.built = map[string]*aot.Built{}
		c.errs = map[string]error{}
		c.calls = map[string]*aotCall{}
	}
	if built, ok := c.built[script]; ok {
		c.mu.Unlock()
		return built, nil
	}
	if err, failed := c.errs[script]; failed {
		c.mu.Unlock()
		return nil, err
	}
	if call, running := c.calls[script]; running {
		c.mu.Unlock()
		<-call.done
		return call.built, call.err
	}
	call := &aotCall{done: make(chan struct{})}
	c.calls[script] = call
	c.mu.Unlock()

	built, err := aot.Compile(c.Root, c.CacheDir, c.Classpath, c.Java, label, stem, script, c.Timeout)

	c.mu.Lock()
	if err != nil {
		c.errs[script] = err
	} else {
		c.built[script] = built
	}
	call.built, call.err = built, err
	delete(c.calls, script)
	close(call.done)
	c.mu.Unlock()
	return built, err
}

// CaseOpts bundles MeasureCase inputs.
type CaseOpts struct {
	TimeBin         string
	UsePerf         bool
	Template        string
	Classpath       string
	Label           string
	Stem            string
	Script          string
	Case            string
	InputPath       string
	OutputPath      string
	Repeat          int
	AllocRepeat     int
	CallgrindRepeat int
	PinBin          string
	PinTool         string
	PinRepeat       int
	PinTimeout      int
	Timeout         int
	Root            string
	AOT             *AOTCache // nil for script mode
}

// innerCommand resolves the snippet argv for script or AOT mode.
func (o CaseOpts) innerCommand() ([]string, error) {
	if o.AOT != nil {
		built, err := o.AOT.Get(o.Label, o.Stem, o.Script)
		if err != nil {
			return nil, err
		}
		return built.Argv(o.Classpath), nil
	}
	return BuildInnerCommand(o.Template, o.Classpath, o.Script, o.Stem)
}

// MeasureCase ports measure_case with the callgrind phase appended.
func MeasureCase(o CaseOpts) CaseResult {
	res := CaseResult{
		Label: o.Label, Stem: o.Stem, Case: o.Case,
		Runs: []RunMetrics{}, AllocRuns: []int64{},
	}
	rel, err := filepath.Rel(o.Root, o.Script)
	if err != nil {
		rel = o.Script
	}
	res.Script = rel
	expected, err := os.ReadFile(o.OutputPath)
	if err != nil {
		msg := fmt.Sprintf("%s/%s: open expected: %v", o.Label, o.Case, err)
		res.Error = &msg
		return res
	}
	want := snipcore.Normalize(expected)
	inner, err := o.innerCommand()
	if err != nil {
		msg := fmt.Sprintf("%s/%s: aot: %v", o.Label, o.Case, err)
		if o.AOT == nil {
			msg = fmt.Sprintf("%s/%s: %v", o.Label, o.Case, err)
		}
		res.Error = &msg
		return res
	}
	for i := 0; i < o.Repeat; i++ {
		stdout, metrics, runErr := MeasureOnce(o.TimeBin, o.UsePerf, inner, o.InputPath, o.Timeout)
		if runErr != "" {
			msg := fmt.Sprintf("%s/%s: %s", o.Label, o.Case, runErr)
			res.Error = &msg
			return res
		}
		if i == 0 && string(snipcore.Normalize(stdout)) != string(want) {
			got := snipcore.TruncateChars(decodeIgnore(snipcore.Normalize(stdout)), 200)
			wantT := snipcore.TruncateChars(decodeIgnore(want), 200)
			msg := fmt.Sprintf("%s/%s: output mismatch: expected %s got %s",
				o.Label, o.Case, snipcore.PyRepr(wantT), snipcore.PyRepr(got))
			res.Error = &msg
			return res
		}
		res.Runs = append(res.Runs, *metrics)
	}
	res.Summary = SummarizeCase(res.Runs)
	if o.AllocRepeat > 0 && !allocExempt(o.Label) {
		for i := 0; i < o.AllocRepeat; i++ {
			stdout, used, runErr := MeasureAllocOnce(inner, o.InputPath, o.Timeout)
			if runErr != "" {
				msg := fmt.Sprintf("%s/%s: alloc: %s", o.Label, o.Case, runErr)
				res.Error = &msg
				return res
			}
			if i == 0 && string(snipcore.Normalize(stdout)) != string(want) {
				got := snipcore.TruncateChars(decodeIgnore(snipcore.Normalize(stdout)), 200)
				wantT := snipcore.TruncateChars(decodeIgnore(want), 200)
				msg := fmt.Sprintf("%s/%s: alloc output mismatch: expected %s got %s",
					o.Label, o.Case, snipcore.PyRepr(wantT), snipcore.PyRepr(got))
				res.Error = &msg
				return res
			}
			res.AllocRuns = append(res.AllocRuns, used)
		}
		res.Summary.AllocatedBytes = SummarizeAlloc(res.AllocRuns)
	}
	if o.CallgrindRepeat > 0 {
		if reason := CallgrindSkipReason(o.Label, InnerIsJava(inner)); reason != "" {
			res.CallgrindSkipped = &reason
			fmt.Fprintf(os.Stderr, "warning: %s/%s/%s: callgrind skipped (%s)\n",
				o.Label, o.Stem, o.Case, reason)
			res.Ok = true
			return res
		}
		var irs []int64
		var cgAllocs []int64
		for i := 0; i < o.CallgrindRepeat; i++ {
			stdout, ir, used, runErr := CallgrindOnce(inner, o.InputPath, o.Timeout)
			if runErr != "" {
				if IsCallgrindTimeout(runErr) || IsAllocShimSkip(runErr) {
					skip := runErr
					res.CallgrindSkipped = &skip
					fmt.Fprintf(os.Stderr, "warning: %s/%s/%s: callgrind skipped (%s)\n",
						o.Label, o.Stem, o.Case, runErr)
					break
				}
				msg := fmt.Sprintf("%s/%s: callgrind: %s", o.Label, o.Case, runErr)
				res.Error = &msg
				return res
			}
			if i == 0 && string(snipcore.Normalize(stdout)) != string(want) {
				got := snipcore.TruncateChars(decodeIgnore(snipcore.Normalize(stdout)), 200)
				wantT := snipcore.TruncateChars(decodeIgnore(want), 200)
				msg := fmt.Sprintf("%s/%s: callgrind output mismatch: expected %s got %s",
					o.Label, o.Case, snipcore.PyRepr(wantT), snipcore.PyRepr(got))
				res.Error = &msg
				return res
			}
			irs = append(irs, ir)
			cgAllocs = append(cgAllocs, used)
		}
		if res.CallgrindSkipped == nil {
			s := SummarizeInts(irs)
			res.Summary.CallgrindInstructions = &s
			res.Summary.CallgrindAllocatedBytes = SummarizeAlloc(cgAllocs)
		}
	}
	if o.PinRepeat > 0 {
		var counts []int64
		for i := 0; i < o.PinRepeat; i++ {
			stdout, count, runErr := PinOnce(o.PinBin, o.PinTool, inner, o.InputPath, o.PinTimeout)
			if runErr != "" {
				if IsPinTimeout(runErr) {
					skip := runErr
					res.PinSkipped = &skip
					fmt.Fprintf(os.Stderr, "warning: %s/%s/%s: pin skipped (%s)\n",
						o.Label, o.Stem, o.Case, runErr)
					break
				}
				msg := fmt.Sprintf("%s/%s: pin: %s", o.Label, o.Case, runErr)
				res.Error = &msg
				return res
			}
			if i == 0 && string(snipcore.Normalize(stdout)) != string(want) {
				got := snipcore.TruncateChars(decodeIgnore(snipcore.Normalize(stdout)), 200)
				wantT := snipcore.TruncateChars(decodeIgnore(want), 200)
				msg := fmt.Sprintf("%s/%s: pin output mismatch: expected %s got %s",
					o.Label, o.Case, snipcore.PyRepr(wantT), snipcore.PyRepr(got))
				res.Error = &msg
				return res
			}
			counts = append(counts, count)
		}
		if res.PinSkipped == nil {
			s := SummarizeInts(counts)
			res.Summary.PinInstructions = &s
		}
	}
	res.Ok = true
	return res
}

func allocExempt(label string) bool {
	if AllocExempt[label] {
		return true
	}
	for stem := range AllocExempt {
		if strings.HasPrefix(label, stem+"/") {
			return true
		}
	}
	return false
}

func decodeIgnore(raw []byte) string {
	return strings.ToValidUTF8(string(raw), "")
}

// SelectShard ports select_shard: snippet-cohesive deterministic
// filtering by (label, stem) groups.
func SelectShard[T any](tasks []T, key func(T) (string, string), index, total int) []T {
	groups := map[[2]string][]T{}
	var order [][2]string
	for _, t := range tasks {
		label, stem := key(t)
		k := [2]string{label, stem}
		if _, seen := groups[k]; !seen {
			order = append(order, k)
		}
		groups[k] = append(groups[k], t)
	}
	sort.Slice(order, func(i, j int) bool {
		if order[i][0] != order[j][0] {
			return order[i][0] < order[j][0]
		}
		return order[i][1] < order[j][1]
	})
	keep := map[[2]string]bool{}
	for i, g := range order {
		if i%total == index {
			keep[g] = true
		}
	}
	var out []T
	for _, t := range tasks {
		label, stem := key(t)
		if keep[[2]string{label, stem}] {
			out = append(out, t)
		}
	}
	return out
}
