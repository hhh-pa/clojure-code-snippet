package measure

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// TimeMetrics is one parsed GNU time line.
type TimeMetrics struct {
	WallS      float64
	UserS      float64
	SysS       float64
	CpuS       float64
	MaxRSSKb   int64
	CpuPct     float64
	MajorFault int64
	MinorFault int64
}

// ParseTimeText ports parse_time_text.
func ParseTimeText(text string) (TimeMetrics, error) {
	var m TimeMetrics
	fields := map[string]string{}
	for _, token := range strings.Fields(text) {
		key, value, found := strings.Cut(token, "=")
		if !found {
			continue
		}
		fields[key] = value
	}
	wall, err1 := strconv.ParseFloat(fields["wall"], 64)
	user, err2 := strconv.ParseFloat(fields["user"], 64)
	sysS, err3 := strconv.ParseFloat(fields["sys"], 64)
	maxrss, err4 := strconv.ParseInt(fields["maxrss_kb"], 10, 64)
	cpuPct, err5 := strconv.ParseFloat(strings.TrimSuffix(fields["cpu_pct"], "%"), 64)
	major, err6 := strconv.ParseInt(fields["major_faults"], 10, 64)
	minor, err7 := strconv.ParseInt(fields["minor_faults"], 10, 64)
	// Missing keys yield "" which fails parsing, mirroring the
	// Python KeyError/ValueError path (note: Python int() accepts
	// surrounding whitespace and signs; Atoi-equivalent here).
	if err1 != nil || err2 != nil || err3 != nil || err4 != nil || err5 != nil || err6 != nil || err7 != nil {
		return m, fmt.Errorf("unparseable GNU time output: %s", snipcore.PyRepr(strings.TrimSpace(text)))
	}
	m.WallS = wall
	m.UserS = user
	m.SysS = sysS
	m.CpuS = user + sysS
	m.MaxRSSKb = maxrss
	m.CpuPct = cpuPct
	m.MajorFault = major
	m.MinorFault = minor
	return m, nil
}

// PerfCounters holds user-space perf counts (nil when unavailable).
type PerfCounters struct {
	Instructions *int64
	Cycles       *int64
}

// ParsePerfText ports parse_perf_text.
func ParsePerfText(text string) PerfCounters {
	var out PerfCounters
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		parts := strings.Split(line, ",")
		if len(parts) < 3 {
			continue
		}
		raw := strings.TrimSpace(parts[0])
		base, _, _ := strings.Cut(strings.TrimSpace(parts[2]), ":")
		if base != "instructions" && base != "cycles" {
			continue
		}
		var value *int64
		if raw != "" && raw != "not counted" && raw != "<not supported>" && raw != "<not counted>" {
			if n, err := strconv.ParseInt(raw, 10, 64); err == nil {
				value = &n
			}
		}
		if base == "instructions" {
			out.Instructions = value
		} else {
			out.Cycles = value
		}
	}
	return out
}

var unitMultipliers = map[string]int64{"B": 1, "K": 1024, "M": 1024 * 1024, "G": 1024 * 1024 * 1024, "T": 1024 * 1024 * 1024 * 1024}

var epsilonUsedRe = regexp.MustCompile(`(\d+(?:\.\d+)?)\s*([BKMGT])(?:\s*\([^)]*\))?\s+used\b`)

// ParseEpsilonUsed ports parse_epsilon_used: heap-used-at-exit in
// bytes from the LAST Heap: line.
func ParseEpsilonUsed(text string) (int64, error) {
	var found []string
	for _, line := range strings.Split(text, "\n") {
		if !strings.Contains(line, "Heap:") {
			continue
		}
		for _, match := range epsilonUsedRe.FindAllStringSubmatch(line, -1) {
			found = match
		}
	}
	if found == nil {
		tail := strings.TrimSpace(text)
		if runes := []rune(tail); len(runes) > 200 {
			tail = string(runes[len(runes)-200:])
		}
		return 0, fmt.Errorf("no Epsilon Heap used value in: %s", snipcore.PyRepr(tail))
	}
	amount, err := strconv.ParseFloat(found[1], 64)
	if err != nil {
		return 0, fmt.Errorf("no Epsilon Heap used value in: %s", snipcore.PyRepr(found[0]))
	}
	return int64(amount * float64(unitMultipliers[found[2]])), nil
}

// ParseCallgrindSummary extracts the total Ir from a callgrind.out
// file (the `summary: N` line).
func ParseCallgrindSummary(text string) (int64, error) {
	for _, line := range strings.Split(text, "\n") {
		if !strings.HasPrefix(line, "summary:") {
			continue
		}
		n, err := strconv.ParseInt(strings.TrimSpace(strings.TrimPrefix(line, "summary:")), 10, 64)
		if err != nil {
			return 0, fmt.Errorf("callgrind summary unreadable: %s", snipcore.PyRepr(strings.TrimSpace(line)))
		}
		return n, nil
	}
	return 0, fmt.Errorf("callgrind summary unreadable: no summary line")
}

// ParsePinCount reads the total from an inscount-style tool
// output file (one "Count N" line). Later Count lines win so
// per-thread dumps still resolve to their trailing total.
func ParsePinCount(text string) (int64, error) {
	found := false
	var total int64
	for _, line := range strings.Split(text, "\n") {
		fields := strings.Fields(line)
		if len(fields) != 2 || fields[0] != "Count" {
			continue
		}
		n, err := strconv.ParseInt(fields[1], 10, 64)
		if err != nil {
			return 0, fmt.Errorf("pin count unreadable: %s", snipcore.PyRepr(strings.TrimSpace(line)))
		}
		total, found = n, true
	}
	if !found {
		return 0, fmt.Errorf("pin count unreadable: no Count line")
	}
	return total, nil
}

// ParsePinAlloc reads the cumulative malloc-family bytes from an
// inscount-style tool output file (one "Alloc M" line). Later
// Alloc lines win, mirroring ParsePinCount.
func ParsePinAlloc(text string) (int64, error) {
	found := false
	var total int64
	for _, line := range strings.Split(text, "\n") {
		fields := strings.Fields(line)
		if len(fields) != 2 || fields[0] != "Alloc" {
			continue
		}
		n, err := strconv.ParseInt(fields[1], 10, 64)
		if err != nil {
			return 0, fmt.Errorf("pin alloc unreadable: %s", snipcore.PyRepr(strings.TrimSpace(line)))
		}
		total, found = n, true
	}
	if !found {
		return 0, fmt.Errorf("pin alloc unreadable: no Alloc line")
	}
	return total, nil
}
