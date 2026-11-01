package measure

import (
	"sort"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/slope"
)

// Measure is one BMF measure (value with lower/upper bounds).
type Measure struct {
	Value      pyjson.Num `json:"value"`
	LowerValue pyjson.Num `json:"lower_value"`
	UpperValue pyjson.Num `json:"upper_value"`
}

// Benchmark is one snippet's BMF entry. The time/memory measures
// are pointers so slope-only runs (no regular results) emit an
// entry with just complexity_exponent, like setdefault does.
type Benchmark struct {
	CpuTime               *Measure `json:"cpu_time,omitempty"`
	PeakMemory            *Measure `json:"peak_memory,omitempty"`
	AllocatedBytes        *Measure `json:"allocated_bytes,omitempty"`
	CallgrindInstructions *Measure `json:"callgrind_instructions,omitempty"`
	ComplexityExponent    *Measure `json:"complexity_exponent,omitempty"`
}

// BMF maps benchmark name to measures.
type BMF map[string]Benchmark

// BuildBMF ports build_bmf: one Benchmark per snippet with
// cpu_time/peak_memory plus allocated_bytes and
// callgrind_instructions when every case reports them, plus the
// sparse complexity_exponent for successful slope fits.
func BuildBMF(results []CaseResult, complexity map[string]slope.Entry) BMF {
	perSnippet := map[string][]*CaseSummary{}
	for i := range results {
		res := &results[i]
		if !res.Ok || res.Summary == nil {
			continue
		}
		name := res.Label + "/" + res.Stem
		perSnippet[name] = append(perSnippet[name], res.Summary)
	}
	out := BMF{}
	for name, summaries := range perSnippet {
		bench := Benchmark{
			CpuTime: &Measure{
				Value:      pyjson.Sum(pluck(summaries, func(s *CaseSummary) pyjson.Num { return s.CpuS.Median })),
				LowerValue: pyjson.Sum(pluck(summaries, func(s *CaseSummary) pyjson.Num { return s.CpuS.Min })),
				UpperValue: pyjson.Sum(pluck(summaries, func(s *CaseSummary) pyjson.Num { return s.CpuS.Max })),
			},
			PeakMemory: &Measure{
				Value:      pyjson.Max(pluck(summaries, func(s *CaseSummary) pyjson.Num { return s.MaxRSSKb.Median })).Mul(pyjson.IntNum(1024)),
				LowerValue: pyjson.Max(pluck(summaries, func(s *CaseSummary) pyjson.Num { return s.MaxRSSKb.Min })).Mul(pyjson.IntNum(1024)),
				UpperValue: pyjson.Max(pluck(summaries, func(s *CaseSummary) pyjson.Num { return s.MaxRSSKb.Max })).Mul(pyjson.IntNum(1024)),
			},
		}
		var allocs []*AllocSummary
		var cgs []*MinMedMax
		for _, s := range summaries {
			if s.AllocatedBytes != nil {
				allocs = append(allocs, s.AllocatedBytes)
			}
			if s.CallgrindInstructions != nil {
				cgs = append(cgs, s.CallgrindInstructions)
			}
		}
		if len(allocs) == len(summaries) && len(summaries) > 0 {
			mins := make([]pyjson.Num, 0, len(allocs))
			maxs := make([]pyjson.Num, 0, len(allocs))
			for _, a := range allocs {
				mins = append(mins, pyjson.IntNum(a.Min))
				maxs = append(maxs, pyjson.IntNum(a.Max))
			}
			bench.AllocatedBytes = &Measure{
				Value:      pyjson.Sum(mins),
				LowerValue: pyjson.Sum(mins),
				UpperValue: pyjson.Sum(maxs),
			}
		}
		// Callgrind noise is one-sided like allocation (cold
		// paths only add instructions), so the minimum is the
		// point estimate here too.
		if len(cgs) == len(summaries) && len(summaries) > 0 {
			mins := make([]pyjson.Num, 0, len(cgs))
			maxs := make([]pyjson.Num, 0, len(cgs))
			for _, c := range cgs {
				mins = append(mins, c.Min)
				maxs = append(maxs, c.Max)
			}
			bench.CallgrindInstructions = &Measure{
				Value:      pyjson.Sum(mins),
				LowerValue: pyjson.Sum(mins),
				UpperValue: pyjson.Sum(maxs),
			}
		}
		out[name] = bench
	}
	for name, entry := range complexity {
		if !entry.Ok || entry.Fit == nil || !entry.Fit.Ok {
			continue
		}
		bench := out[name]
		bench.ComplexityExponent = &Measure{
			Value:      *entry.Fit.Slope,
			LowerValue: *entry.Fit.Lower,
			UpperValue: *entry.Fit.Upper,
		}
		out[name] = bench
	}
	return out
}

func pluck(summaries []*CaseSummary, fn func(*CaseSummary) pyjson.Num) []pyjson.Num {
	out := make([]pyjson.Num, 0, len(summaries))
	for _, s := range summaries {
		out = append(out, fn(s))
	}
	return out
}

// SortedNames returns BMF benchmark names in order.
func SortedNames(bmf BMF) []string {
	names := make([]string, 0, len(bmf))
	for name := range bmf {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}
