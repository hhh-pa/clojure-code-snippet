package measure

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
)

func compareCase(label, stem, cas string, cpuMin, wallMed, rssMax float64) CaseResult {
	return CaseResult{Label: label, Stem: stem, Case: cas, Ok: true,
		Summary: &CaseSummary{
			CpuS:     MinMedMax{Min: pyjson.FloatNum(cpuMin), Median: pyjson.FloatNum(cpuMin), Max: pyjson.FloatNum(cpuMin)},
			WallS:    MinMedMax{Min: pyjson.FloatNum(wallMed), Median: pyjson.FloatNum(wallMed), Max: pyjson.FloatNum(wallMed)},
			MaxRSSKb: MinMedMax{Min: pyjson.FloatNum(rssMax), Median: pyjson.FloatNum(rssMax), Max: pyjson.FloatNum(rssMax)},
			Runs:     1,
		}}
}

func TestCompareReportsMatch(t *testing.T) {
	from := Report{Results: []CaseResult{
		compareCase("sort/bubble", "solution", "test_case_1", 1.0, 1.2, 100),
		compareCase("sort/bubble", "solution", "test_case_2", 2.0, 2.2, 200),
	}}
	to := Report{Results: []CaseResult{
		compareCase("sort/bubble", "solution", "test_case_2", 1.0, 1.1, 100),
		compareCase("sort/bubble", "solution", "test_case_1", 2.0, 2.4, 400),
	}}
	c := CompareReports(from, to, "aaa", "bbb")
	if c.Matched != 2 || c.FromOnly != 0 || c.ToOnly != 0 {
		t.Fatalf("counts: %+v", c)
	}
	if len(c.Cases) != 2 || c.Cases[0].Case != "test_case_1" || c.Cases[1].Case != "test_case_2" {
		t.Fatalf("order: %+v", c.Cases)
	}
	first := c.Cases[0]
	if first.From.CpuS != 1.0 || first.To.CpuS != 2.0 {
		t.Fatalf("cpu sides: %+v %+v", first.From, first.To)
	}
	if first.CpuRatio == nil || *first.CpuRatio != 2.0 {
		t.Fatalf("cpu ratio: %+v", first.CpuRatio)
	}
	if first.WallRatio == nil || *first.WallRatio != 2.0 {
		t.Fatalf("wall ratio: %+v", first.WallRatio)
	}
	if first.RssRatio == nil || *first.RssRatio != 4.0 {
		t.Fatalf("rss ratio: %+v", first.RssRatio)
	}
	second := c.Cases[1]
	if second.CpuRatio == nil || *second.CpuRatio != 0.5 {
		t.Fatalf("speedup ratio: %+v", second.CpuRatio)
	}
}

func TestCompareStatSelection(t *testing.T) {
	from := Report{Results: []CaseResult{{Label: "l", Stem: "s", Case: "c", Ok: true,
		Summary: &CaseSummary{
			CpuS:     MinMedMax{Min: pyjson.FloatNum(1), Median: pyjson.FloatNum(9), Max: pyjson.FloatNum(9)},
			WallS:    MinMedMax{Min: pyjson.FloatNum(9), Median: pyjson.FloatNum(2), Max: pyjson.FloatNum(9)},
			MaxRSSKb: MinMedMax{Min: pyjson.FloatNum(9), Median: pyjson.FloatNum(9), Max: pyjson.FloatNum(3)},
			Runs:     3,
		}}}}
	to := Report{Results: []CaseResult{compareCase("l", "s", "c", 1, 1, 8)}}
	c := CompareReports(from, to, "f", "t")
	got := c.Cases[0].From
	if got.CpuS != 1 || got.WallS != 2 || got.MaxRSSK != 3 {
		t.Fatalf("representative stats: %+v", got)
	}
}

func TestCompareReportsMissing(t *testing.T) {
	errMsg := "boom"
	from := Report{
		Results: []CaseResult{
			compareCase("l", "s", "kept", 1, 1, 8),
			compareCase("l", "s", "removed", 1, 1, 8),
			{Label: "l", Stem: "s", Case: "failed", Ok: false, Error: &errMsg},
		},
		Missing: []string{"b", "a"},
	}
	to := Report{
		Results: []CaseResult{
			compareCase("l", "s", "kept", 2, 2, 8),
			compareCase("l", "s", "added", 1, 1, 8),
			{Label: "l", Stem: "s", Case: "failed", Ok: false, Error: &errMsg},
		},
		Missing: []string{"c"},
	}
	c := CompareReports(from, to, "f", "t")
	if c.Matched != 2 || c.FromOnly != 1 || c.ToOnly != 1 {
		t.Fatalf("counts: %+v", c)
	}
	byCase := map[string]CompareCase{}
	for _, cc := range c.Cases {
		byCase[cc.Case] = cc
	}
	if byCase["removed"].To != nil || byCase["removed"].CpuRatio != nil {
		t.Fatalf("removed side: %+v", byCase["removed"])
	}
	if byCase["added"].From != nil || byCase["added"].CpuRatio != nil {
		t.Fatalf("added side: %+v", byCase["added"])
	}
	failed := byCase["failed"]
	if failed.From != nil || failed.To != nil || failed.CpuRatio != nil {
		t.Fatalf("failed stats must stay missing: %+v", failed)
	}
	if len(c.FromMissing) != 2 || c.FromMissing[0] != "a" || c.FromMissing[1] != "b" {
		t.Fatalf("from missing: %q", c.FromMissing)
	}
	if len(c.ToMissing) != 1 || c.ToMissing[0] != "c" {
		t.Fatalf("to missing: %q", c.ToMissing)
	}
}

func TestCompareZeroFrom(t *testing.T) {
	from := Report{Results: []CaseResult{compareCase("l", "s", "c", 0, 0, 0)}}
	to := Report{Results: []CaseResult{compareCase("l", "s", "c", 1, 1, 8)}}
	c := CompareReports(from, to, "f", "t")
	cc := c.Cases[0]
	if cc.CpuRatio != nil || cc.WallRatio != nil || cc.RssRatio != nil {
		t.Fatalf("zero from must yield null ratios: %+v", cc)
	}
	if c.Matched != 1 {
		t.Fatalf("counts: %+v", c)
	}
}

func TestAppendCompareSummary(t *testing.T) {
	from := Report{Results: []CaseResult{
		compareCase("l", "s", "slow", 1.0, 1.0, 8),
		compareCase("l", "s", "fast", 4.0, 4.0, 8),
		compareCase("l", "s", "gone", 1.0, 1.0, 8),
	}}
	to := Report{Results: []CaseResult{
		compareCase("l", "s", "slow", 3.0, 3.0, 8),
		compareCase("l", "s", "fast", 1.0, 1.0, 8),
		compareCase("l", "s", "new", 1.0, 1.0, 8),
	}}
	c := CompareReports(from, to, "aaa1111", "bbb2222")
	path := filepath.Join(t.TempDir(), "summary.md")
	if err := AppendCompareSummary(path, c, 1); err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	text := string(raw)
	for _, want := range []string{
		"## Perf comparison (`snip compare`): aaa1111 -> bbb2222",
		"matched 2, only in from 1, only in to 1",
		"### Largest slowdowns (to/from cpu_s)",
		"| l/s | slow | 1.000 | 3.000 | 3.00x |",
		"### Largest speedups (to/from cpu_s)",
		"| l/s | fast | 4.000 | 1.000 | 0.25x |",
		"+1 more",
		"### Only in from (removed or unmeasured)",
		"- l/s/gone",
		"### Only in to (added or unmeasured)",
		"- l/s/new",
	} {
		if !strings.Contains(text, want) {
			t.Fatalf("summary lacks %q:\n%s", want, text)
		}
	}
}

func TestAppendCompareSummaryEmpty(t *testing.T) {
	c := CompareReports(Report{}, Report{}, "f", "t")
	path := filepath.Join(t.TempDir(), "summary.md")
	if err := AppendCompareSummary(path, c, 20); err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if text := string(raw); !strings.Contains(text, "matched 0") || !strings.Contains(text, "none\n") {
		t.Fatalf("empty summary:\n%s", text)
	}
}
