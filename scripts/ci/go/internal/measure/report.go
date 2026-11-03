package measure

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"sort"
	"strconv"
	"strings"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/slope"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// Host describes the measuring machine.
type Host struct {
	Hostname   string  `json:"hostname"`
	Os         string  `json:"os"`
	Nproc      int     `json:"nproc"`
	CpuModel   *string `json:"cpu_model"`
	MemTotalKb *int64  `json:"mem_total_kb"`
}

// Runtime describes the measured runtime.
type Runtime struct {
	Name            string  `json:"name"`
	CommandTemplate string  `json:"command_template"`
	JavaVersion     *string `json:"java_version"`
	RuntimeVersion  *string `json:"runtime_version"`
	Classpath       string  `json:"classpath"`
	TimeBin         string  `json:"time_bin"`
	PerfUsed        bool    `json:"perf_used"`
	CallgrindUsed   bool    `json:"callgrind_used"`
}

// Config records the run configuration. AllocRepeat is a pointer
// because old reports may omit it (rendered "n/a" like Python's
// .get default); live runs always set it.
type Config struct {
	Repeat              int     `json:"repeat"`
	Jobs                int     `json:"jobs"`
	Timeout             int     `json:"timeout"`
	Only                string  `json:"only"`
	Exclude             string  `json:"exclude"`
	Shard               *string `json:"shard"`
	AllocRepeat         *int    `json:"alloc_repeat"`
	Slope               bool    `json:"slope"`
	SlopeRepeat         int     `json:"slope_repeat"`
	CallgrindRepeat     int     `json:"callgrind_repeat"`
	CallgrindXmx        string  `json:"callgrind_xmx"`
	CallgrindInclExempt bool    `json:"callgrind_include_exempt"`
	Aot                 bool    `json:"aot"`
}

// Report is the full perf-report JSON document.
type Report struct {
	Tool         string                 `json:"tool"`
	Version      int                    `json:"version"`
	TimestampUTC string                 `json:"timestamp_utc"`
	Host         Host                   `json:"host"`
	Runtime      Runtime                `json:"runtime"`
	Config       Config                 `json:"config"`
	Results      []CaseResult           `json:"results"`
	Failures     []string               `json:"failures"`
	Missing      []string               `json:"missing"`
	Complexity   map[string]slope.Entry `json:"complexity"`
}

// HostInfo ports host_info.
func HostInfo() Host {
	out := Host{Nproc: runtime.NumCPU()}
	if name, err := os.Hostname(); err == nil {
		out.Hostname = name
	}
	out.Os = runtime.GOOS + "/" + runtime.GOARCH
	if raw, err := os.ReadFile("/proc/cpuinfo"); err == nil {
		for _, line := range strings.Split(string(raw), "\n") {
			for _, key := range []string{"model name", "Model", "Hardware"} {
				if strings.HasPrefix(line, key) {
					_, value, _ := strings.Cut(line, ":")
					model := strings.TrimSpace(value)
					out.CpuModel = &model
					break
				}
			}
			if out.CpuModel != nil {
				break
			}
		}
	}
	if raw, err := os.ReadFile("/proc/meminfo"); err == nil {
		for _, line := range strings.Split(string(raw), "\n") {
			if strings.HasPrefix(line, "MemTotal:") {
				fields := strings.Fields(line)
				if len(fields) >= 2 {
					if kb, err := strconv.ParseInt(fields[1], 10, 64); err == nil {
						out.MemTotalKb = &kb
					}
				}
				break
			}
		}
	}
	return out
}

// JavaVersion ports java_version (first line or nil). Like Python,
// only a spawn failure or timeout yields nil; the exit code is
// ignored and java's stderr is parsed (that is where -version
// writes).
func JavaVersion() *string {
	_, stderr, err := snipcore.Run([]string{"java", "-version"}, nil, 30)
	if err != nil {
		var timeout *snipcore.TimeoutError
		var exitErr *exec.ExitError
		if errors.As(err, &timeout) || !errors.As(err, &exitErr) {
			return nil
		}
	}
	for _, line := range strings.Split(string(stderr), "\n") {
		if trimmed := strings.TrimSpace(line); trimmed != "" {
			return &trimmed
		}
	}
	return nil
}

// RuntimeVersion probes a native runtime binary's `<argv0>
// --version`, returning the first non-empty output line (stdout,
// then stderr) or nil when the probe fails. Best-effort
// provenance for non-JVM legs, mirroring JavaVersion's tolerance.
func RuntimeVersion(argv0 string) *string {
	stdout, stderr, err := snipcore.Run([]string{argv0, "--version"}, nil, 30)
	if err != nil {
		var timeout *snipcore.TimeoutError
		var exitErr *exec.ExitError
		if errors.As(err, &timeout) || !errors.As(err, &exitErr) {
			return nil
		}
	}
	for _, raw := range [][]byte{stdout, stderr} {
		for _, line := range strings.Split(string(raw), "\n") {
			if trimmed := strings.TrimSpace(line); trimmed != "" {
				return &trimmed
			}
		}
	}
	return nil
}

// MergeReports ports merge_reports: concatenate full-report JSONs
// into one (report, bmf) pair with dedupe, sorted failures/missing,
// unioned complexity, and shard "merged".
func MergeReports(paths []string) (Report, BMF, error) {
	merged := []CaseResult{}
	seen := map[resultKey]bool{}
	var failures []string
	missing := map[string]bool{}
	complexity := map[string]slope.Entry{}
	var base Report
	haveBase := false
	for _, path := range paths {
		raw, err := os.ReadFile(path)
		if err != nil {
			return Report{}, nil, fmt.Errorf("cannot read report %s: %v", path, err)
		}
		var rep Report
		if err := json.Unmarshal(raw, &rep); err != nil {
			return Report{}, nil, fmt.Errorf("cannot read report %s: %v", path, err)
		}
		if !haveBase {
			base = rep
			haveBase = true
		}
		for _, res := range rep.Results {
			key := resultKey{res.Label, res.Stem, res.Case}
			if seen[key] {
				continue
			}
			seen[key] = true
			merged = append(merged, res)
		}
		failures = append(failures, rep.Failures...)
		for _, m := range rep.Missing {
			missing[m] = true
		}
		for name, entry := range rep.Complexity {
			if _, dup := complexity[name]; !dup {
				complexity[name] = entry
			}
		}
	}
	sort.Slice(merged, func(i, j int) bool {
		if merged[i].Label != merged[j].Label {
			return merged[i].Label < merged[j].Label
		}
		if merged[i].Stem != merged[j].Stem {
			return merged[i].Stem < merged[j].Stem
		}
		return merged[i].Case < merged[j].Case
	})
	report := base
	report.Results = merged
	report.Failures = sortedUnique(failures)
	report.Missing = sortedKeys(missing)
	report.Complexity = complexity
	shard := "merged"
	report.Config.Shard = &shard
	return report, BuildBMF(merged, complexity), nil
}

type resultKey struct {
	label, stem, cas string
}

func sortedUnique(in []string) []string {
	set := map[string]bool{}
	for _, v := range in {
		set[v] = true
	}
	return sortedKeys(set)
}

func sortedKeys(set map[string]bool) []string {
	out := make([]string, 0, len(set))
	for v := range set {
		out = append(out, v)
	}
	sort.Strings(out)
	return out
}
