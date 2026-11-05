package measure

import (
	"sort"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
)

// RunMetrics is one measured execution. Field order matches the
// Python metrics dict (wall_s first, perf counters last). Times are
// Num so integral values keep Python's ".0" in JSON.
type RunMetrics struct {
	WallS        pyjson.Num `json:"wall_s"`
	UserS        pyjson.Num `json:"user_s"`
	SysS         pyjson.Num `json:"sys_s"`
	CpuS         pyjson.Num `json:"cpu_s"`
	MaxRSSKb     int64      `json:"maxrss_kb"`
	CpuPct       pyjson.Num `json:"cpu_pct"`
	MajorFaults  int64      `json:"major_faults"`
	MinorFaults  int64      `json:"minor_faults"`
	Instructions *int64     `json:"instructions"`
	Cycles       *int64     `json:"cycles"`
}

// MinMedMax aggregates one metric across repeats.
type MinMedMax struct {
	Min    pyjson.Num `json:"min"`
	Median pyjson.Num `json:"median"`
	Max    pyjson.Num `json:"max"`
}

// AllocSummary aggregates allocated-byte counts. The median keeps
// statistics.median semantics (even counts average to a float).
type AllocSummary struct {
	Min           int64      `json:"min"`
	Median        pyjson.Num `json:"median"`
	Max           int64      `json:"max"`
	Deterministic bool       `json:"deterministic"`
}

// CaseSummary aggregates one case's runs. Field order matches
// summarize_case plus the appended allocated_bytes and the
// callgrind_instructions / callgrind_allocated_bytes signals.
// (Pin instruction counts live in snip test-instrumentation, not
// in measure reports.)
type CaseSummary struct {
	CpuS                    MinMedMax     `json:"cpu_s"`
	MaxRSSKb                MinMedMax     `json:"maxrss_kb"`
	WallS                   MinMedMax     `json:"wall_s"`
	Runs                    int           `json:"runs"`
	Instructions            *MinMedMax    `json:"instructions"`
	Cycles                  *MinMedMax    `json:"cycles"`
	AllocatedBytes          *AllocSummary `json:"allocated_bytes"`
	CallgrindInstructions   *MinMedMax    `json:"callgrind_instructions"`
	CallgrindAllocatedBytes *AllocSummary `json:"callgrind_allocated_bytes"`
}

// CaseResult is one measured case. Error is nil (null) on success.
// CallgrindSkipped records a degraded callgrind phase: the timed
// data is valid (Ok stays true) but the case exceeded the callgrind
// budget, so no callgrind summaries were recorded.
type CaseResult struct {
	Label            string       `json:"label"`
	Stem             string       `json:"stem"`
	Script           string       `json:"script"`
	Case             string       `json:"case"`
	Ok               bool         `json:"ok"`
	Error            *string      `json:"error"`
	Runs             []RunMetrics `json:"runs"`
	AllocRuns        []int64      `json:"alloc_runs"`
	Summary          *CaseSummary `json:"summary"`
	CallgrindSkipped *string      `json:"callgrind_skipped,omitempty"`
}

// SummarizeFloats aggregates floats (odd counts keep the middle
// value; even counts average, like statistics.median).
func SummarizeFloats(values []float64) MinMedMax {
	sorted := append([]float64(nil), values...)
	sort.Float64s(sorted)
	return MinMedMax{
		Min:    pyjson.FloatNum(sorted[0]),
		Median: pyjson.MedianFloat64(sorted),
		Max:    pyjson.FloatNum(sorted[len(sorted)-1]),
	}
}

// SummarizeInts aggregates ints (odd-count medians stay integral).
func SummarizeInts(values []int64) MinMedMax {
	sorted := append([]int64(nil), values...)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i] < sorted[j] })
	return MinMedMax{
		Min:    pyjson.IntNum(sorted[0]),
		Median: pyjson.MedianInt64(sorted),
		Max:    pyjson.IntNum(sorted[len(sorted)-1]),
	}
}

// SummarizeCase ports summarize_case (nil for no runs).
func SummarizeCase(runs []RunMetrics) *CaseSummary {
	if len(runs) == 0 {
		return nil
	}
	cpu := make([]float64, 0, len(runs))
	rss := make([]int64, 0, len(runs))
	wall := make([]float64, 0, len(runs))
	var insn, cyc []int64
	for _, r := range runs {
		cpu = append(cpu, r.CpuS.Float64())
		rss = append(rss, r.MaxRSSKb)
		wall = append(wall, r.WallS.Float64())
		if r.Instructions != nil {
			insn = append(insn, *r.Instructions)
		}
		if r.Cycles != nil {
			cyc = append(cyc, *r.Cycles)
		}
	}
	out := &CaseSummary{
		CpuS:     SummarizeFloats(cpu),
		MaxRSSKb: SummarizeInts(rss),
		WallS:    SummarizeFloats(wall),
		Runs:     len(runs),
	}
	if len(insn) > 0 {
		s := SummarizeInts(insn)
		out.Instructions = &s
	}
	if len(cyc) > 0 {
		s := SummarizeInts(cyc)
		out.Cycles = &s
	}
	return out
}

// SummarizeAlloc ports summarize_alloc (one-sided noise: the BMF
// uses the minimum as the point estimate).
func SummarizeAlloc(values []int64) *AllocSummary {
	sorted := append([]int64(nil), values...)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i] < sorted[j] })
	det := true
	for _, v := range sorted[1:] {
		if v != sorted[0] {
			det = false
			break
		}
	}
	return &AllocSummary{
		Min:           sorted[0],
		Median:        pyjson.MedianInt64(sorted),
		Max:           sorted[len(sorted)-1],
		Deterministic: det,
	}
}
