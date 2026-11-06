package measure

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

// ComparedStats is one side of a compared case: the representative
// timed signals (README's primaries: cpu_s min-of-repeats, maxrss
// max-of-repeats, wall_s median), plus perf-counter medians when
// both sides recorded them.
type ComparedStats struct {
	CpuS    float64  `json:"cpu_s"`
	WallS   float64  `json:"wall_s"`
	MaxRSSK float64  `json:"maxrss_kb"`
	Instrs  *float64 `json:"instructions"`
	Cycles  *float64 `json:"cycles"`
}

// CompareCase matches one (label, stem, case) across the from/to
// reports. A nil side means the case had no summary there (absent
// from that tree, or failed); ratios are nil unless both sides
// have stats and the from value is positive.
type CompareCase struct {
	Label     string         `json:"label"`
	Stem      string         `json:"stem"`
	Case      string         `json:"case"`
	From      *ComparedStats `json:"from"`
	To        *ComparedStats `json:"to"`
	CpuRatio  *float64       `json:"cpu_ratio"`
	WallRatio *float64       `json:"wall_ratio"`
	RssRatio  *float64       `json:"rss_ratio"`
}

// Comparison is the full from/to diff: matched cases in label,
// stem, case order plus the per-side missing lists.
type Comparison struct {
	FromLabel   string        `json:"from_label"`
	ToLabel     string        `json:"to_label"`
	Cases       []CompareCase `json:"cases"`
	FromMissing []string      `json:"from_missing"`
	ToMissing   []string      `json:"to_missing"`
	Matched     int           `json:"matched"`
	FromOnly    int           `json:"from_only"`
	ToOnly      int           `json:"to_only"`
}

// CompareReports matches two full reports by (label, stem, case).
// Correspondence gaps are data (nil side, "missing" stats), never
// errors: trees legitimately add and drop snippets across commits.
func CompareReports(from, to Report, fromLabel, toLabel string) Comparison {
	fromByKey := indexResults(from.Results)
	toByKey := indexResults(to.Results)
	keys := map[resultKey]bool{}
	for k := range fromByKey {
		keys[k] = true
	}
	for k := range toByKey {
		keys[k] = true
	}
	ordered := make([]resultKey, 0, len(keys))
	for k := range keys {
		ordered = append(ordered, k)
	}
	sort.Slice(ordered, func(i, j int) bool {
		if ordered[i].label != ordered[j].label {
			return ordered[i].label < ordered[j].label
		}
		if ordered[i].stem != ordered[j].stem {
			return ordered[i].stem < ordered[j].stem
		}
		return ordered[i].cas < ordered[j].cas
	})
	out := Comparison{
		FromLabel:   fromLabel,
		ToLabel:     toLabel,
		FromMissing: sortedKeysOf(from.Missing),
		ToMissing:   sortedKeysOf(to.Missing),
	}
	for _, k := range ordered {
		f, fok := fromByKey[k]
		t, tok := toByKey[k]
		var fs, ts *ComparedStats
		if fok {
			fs = comparedStats(f)
		}
		if tok {
			ts = comparedStats(t)
		}
		cc := CompareCase{Label: k.label, Stem: k.stem, Case: k.cas, From: fs, To: ts}
		if fs != nil && ts != nil {
			cc.CpuRatio = ratio(fs.CpuS, ts.CpuS)
			cc.WallRatio = ratio(fs.WallS, ts.WallS)
			cc.RssRatio = ratio(fs.MaxRSSK, ts.MaxRSSK)
		}
		if fok && tok {
			out.Matched++
		} else if fok {
			out.FromOnly++
		} else {
			out.ToOnly++
		}
		out.Cases = append(out.Cases, cc)
	}
	return out
}

func indexResults(results []CaseResult) map[resultKey]CaseResult {
	out := map[resultKey]CaseResult{}
	for _, res := range results {
		k := resultKey{res.Label, res.Stem, res.Case}
		if _, dup := out[k]; !dup {
			out[k] = res
		}
	}
	return out
}

func comparedStats(res CaseResult) *ComparedStats {
	s := res.Summary
	if s == nil {
		return nil
	}
	out := &ComparedStats{
		CpuS:    s.CpuS.Min.Float64(),
		WallS:   s.WallS.Median.Float64(),
		MaxRSSK: s.MaxRSSKb.Max.Float64(),
	}
	if s.Instructions != nil {
		v := s.Instructions.Median.Float64()
		out.Instrs = &v
	}
	if s.Cycles != nil {
		v := s.Cycles.Median.Float64()
		out.Cycles = &v
	}
	return out
}

func ratio(from, to float64) *float64 {
	if from <= 0 {
		return nil
	}
	v := to / from
	return &v
}

func sortedKeysOf(in []string) []string {
	out := append([]string{}, in...)
	sort.Strings(out)
	return out
}

// AppendCompareSummary appends the comparison block to a step-summary
// file: counts, the top-N slowdowns and speedups by cpu ratio, and
// the capped added/removed case lists. Report-only: it never fails
// on the data, only on IO.
func AppendCompareSummary(path string, c Comparison, top int) error {
	fd, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
	if err != nil {
		return err
	}
	defer fd.Close()
	var b strings.Builder
	fmt.Fprintf(&b, "## Perf comparison (`snip compare`): %s -> %s\n\n", c.FromLabel, c.ToLabel)
	fmt.Fprintf(&b, "matched %d, only in from %d, only in to %d (stats missing where unmatched)\n\n",
		c.Matched, c.FromOnly, c.ToOnly)
	ranked := make([]CompareCase, 0, len(c.Cases))
	for _, cc := range c.Cases {
		if cc.CpuRatio != nil {
			ranked = append(ranked, cc)
		}
	}
	down := append([]CompareCase{}, ranked...)
	sort.Slice(down, func(i, j int) bool { return *down[i].CpuRatio > *down[j].CpuRatio })
	up := append([]CompareCase{}, ranked...)
	sort.Slice(up, func(i, j int) bool { return *up[i].CpuRatio < *up[j].CpuRatio })
	writeTop(&b, "Largest slowdowns (to/from cpu_s)", down, top)
	writeTop(&b, "Largest speedups (to/from cpu_s)", up, top)
	writeOnly(&b, "Only in from (removed or unmeasured)", c.Cases, top, true)
	writeOnly(&b, "Only in to (added or unmeasured)", c.Cases, top, false)
	_, err = fd.WriteString(b.String())
	return err
}

func writeTop(b *strings.Builder, title string, cases []CompareCase, top int) {
	fmt.Fprintf(b, "### %s\n\n", title)
	if len(cases) == 0 {
		b.WriteString("none\n\n")
		return
	}
	b.WriteString("| benchmark | case | from cpu_s | to cpu_s | ratio |\n")
	b.WriteString("| --- | --- | --- | --- | --- |\n")
	shown := cases
	if len(shown) > top {
		shown = shown[:top]
	}
	for _, cc := range shown {
		fmt.Fprintf(b, "| %s/%s | %s | %.3f | %.3f | %.2fx |\n",
			cc.Label, cc.Stem, cc.Case, cc.From.CpuS, cc.To.CpuS, *cc.CpuRatio)
	}
	if len(cases) > top {
		fmt.Fprintf(b, "\n+%d more\n\n", len(cases)-top)
	} else {
		b.WriteString("\n")
	}
}

func writeOnly(b *strings.Builder, title string, cases []CompareCase, top int, fromSide bool) {
	fmt.Fprintf(b, "### %s\n\n", title)
	var names []string
	for _, cc := range cases {
		if fromSide && cc.From != nil && cc.To == nil {
			names = append(names, cc.Label+"/"+cc.Stem+"/"+cc.Case)
		}
		if !fromSide && cc.From == nil && cc.To != nil {
			names = append(names, cc.Label+"/"+cc.Stem+"/"+cc.Case)
		}
	}
	if len(names) == 0 {
		b.WriteString("none\n\n")
		return
	}
	shown := names
	if len(shown) > top {
		shown = shown[:top]
	}
	for _, n := range shown {
		fmt.Fprintf(b, "- %s\n", n)
	}
	if len(names) > top {
		fmt.Fprintf(b, "- +%d more\n", len(names)-top)
	}
	b.WriteString("\n")
}
