package measure

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/aot"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/cljread"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/generators"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/slope"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// SelfTest runs the durable parser/aggregator checks (no JVM, time,
// perf, or valgrind needed), printing PASS/FAIL lines like the
// Python --self-test. It returns 1 when any check fails.
func SelfTest(w io.Writer) int {
	checks := append(selfChecks(), slope.SelfChecks()...)
	failed := 0
	for _, c := range checks {
		status := "PASS"
		if !c.Ok {
			status = "FAIL"
			failed++
		}
		fmt.Fprintf(w, "%-4s %s\n", status, c.Name)
		if !c.Ok && c.Detail != "" {
			fmt.Fprintf(w, "     %s\n", c.Detail)
		}
	}
	fmt.Fprintf(w, "self-test: %d failed\n", failed)
	if failed > 0 {
		return 1
	}
	return 0
}

func selfChecks() []snipcore.Check {
	var out []snipcore.Check
	check := func(name string, ok bool, detail string) {
		out = append(out, snipcore.Check{Name: name, Ok: ok, Detail: detail})
	}

	m, err := ParseTimeText("wall=1.23 user=0.90 sys=0.10 maxrss_kb=123456 cpu_pct=81% major_faults=2 minor_faults=300\n")
	check("time-parse", err == nil && m.WallS == 1.23 && m.CpuS == 1.0 &&
		m.MaxRSSKb == 123456 && m.CpuPct == 81.0 && m.MajorFault == 2 && m.MinorFault == 300,
		fmt.Sprintf("%+v %v", m, err))
	_, err = ParseTimeText("garbage line\n")
	check("time-parse-rejects", err != nil, "no error raised")

	p := ParsePerfText("134041,,instructions:u,525440,100.00,0.50,insn per cycle\n266896,,cycles:u,525440,100.00,,\n")
	check("perf-parse", p.Instructions != nil && *p.Instructions == 134041 &&
		p.Cycles != nil && *p.Cycles == 266896, fmt.Sprintf("%+v", p))
	p2 := ParsePerfText("<not supported>,,instructions:u,,,\nnot counted,,cycles:u,,,\n")
	check("perf-parse-unsupported", p2.Instructions == nil && p2.Cycles == nil, fmt.Sprintf("%+v", p2))

	inner, err := BuildInnerCommand(DefaultCommandTemplate, "/a/b.jar", "/s/x.clj", "x")
	check("template-default", err == nil && equalStrings(inner,
		[]string{"java", "-cp", "/a/b.jar", "clojure.main", "/s/x.clj"}),
		fmt.Sprintf("%q %v", inner, err))
	inner2, err := BuildInnerCommand("./build/{stem}", "/a", "/s/y.clj", "y")
	check("template-stem", err == nil && equalStrings(inner2, []string{"./build/y"}),
		fmt.Sprintf("%q %v", inner2, err))
	_, err = BuildInnerCommand("{bogus}", "/a", "/s", "s")
	check("template-rejects-unknown", err != nil, "no error raised")
	_, err = BuildInnerCommand("", "/a", "/s", "s")
	check("template-rejects-empty", err != nil, "no error raised")
	expanded, err := ExpandTemplate("{{literal}} {cp}", "/a", "/s", "s")
	check("template-escapes", err == nil && expanded == "{literal} /a",
		fmt.Sprintf("%q %v", expanded, err))

	idx, total, err := snipcore.ParseMeasureShard("1/4")
	check("shard-parse", err == nil && idx == 1 && total == 4, fmt.Sprintf("%d %d %v", idx, total, err))
	bad := 0
	for _, text := range []string{"4/4", "-1/4", "0/0", "x", "1/2/3", ""} {
		if _, _, err := snipcore.ParseMeasureShard(text); err != nil {
			bad++
		}
	}
	check("shard-parse-rejects", bad == 6, fmt.Sprintf("%d", bad))

	type toy struct{ label, stem, cas string }
	toys := []toy{{"b", "s", "c"}, {"a", "s", "c"}, {"c", "s", "c"}, {"a", "s", "b"}}
	key := func(t toy) (string, string) { return t.label, t.stem }
	sh0 := SelectShard(toys, key, 0, 2)
	sh1 := SelectShard(toys, key, 1, 2)
	// Groups sort to a=0,b=1,c=2; shard 0 keeps a,c (3 tasks),
	// shard 1 keeps b (1 task), input order preserved.
	splitOK := len(sh0) == 3 && len(sh1) == 1 && sh1[0] == (toy{"b", "s", "c"})
	keys := map[[2]string]int{}
	for _, t := range sh0 {
		keys[[2]string{t.label, t.stem}]++
	}
	splitOK = splitOK && keys[[2]string{"a", "s"}] == 2 && keys[[2]string{"c", "s"}] == 1
	inS0 := map[toy]bool{}
	for _, t := range sh0 {
		inS0[t] = true
	}
	cohesive := true
	for _, t := range sh1 {
		if inS0[t] {
			cohesive = false
		}
	}
	seenGroups := map[[2]string]int{}
	for _, t := range append(append([]toy{}, sh0...), sh1...) {
		seenGroups[[2]string{t.label, t.stem}]++
	}
	cohesive = cohesive && len(seenGroups) == 3 && len(sh0)+len(sh1) == len(toys)
	check("shard-split", splitOK, fmt.Sprintf("%v %v", sh0, sh1))
	check("shard-cohesion", cohesive, fmt.Sprintf("%v %v", sh0, sh1))

	runs := []RunMetrics{
		{CpuS: pyjson.FloatNum(1.0), MaxRSSKb: 100, WallS: pyjson.FloatNum(1.2), Instructions: intPtr(10), Cycles: intPtr(20)},
		{CpuS: pyjson.FloatNum(2.0), MaxRSSKb: 110, WallS: pyjson.FloatNum(2.2), Instructions: intPtr(12), Cycles: intPtr(22)},
		{CpuS: pyjson.FloatNum(3.0), MaxRSSKb: 105, WallS: pyjson.FloatNum(3.2)},
	}
	s := SummarizeCase(runs)
	check("summarize", s != nil &&
		s.CpuS.Min.Float64() == 1.0 && s.CpuS.Median.Float64() == 2.0 && s.CpuS.Max.Float64() == 3.0 &&
		s.MaxRSSKb.Median.Float64() == 105 &&
		s.Instructions != nil && s.Instructions.Min.Float64() == 10 &&
		s.Instructions.Median.Float64() == 11.0 && s.Instructions.Max.Float64() == 12,
		fmt.Sprintf("%+v", s))
	check("summarize-empty", SummarizeCase(nil) == nil, "")

	errStr := "boom"
	bmf := BuildBMF([]CaseResult{
		{Label: "sort/quick_sort", Stem: "solution", Ok: true, Summary: s},
		{Label: "sort/quick_sort", Stem: "solution", Ok: false, Error: &errStr},
	}, nil)
	entry := bmf["sort/quick_sort/solution"]
	check("bmf", entry.CpuTime != nil && entry.PeakMemory != nil &&
		entry.AllocatedBytes == nil && entry.CallgrindInstructions == nil &&
		entry.ComplexityExponent == nil &&
		entry.CpuTime.Value.Float64() == 2.0 && entry.CpuTime.LowerValue.Float64() == 1.0 &&
		entry.CpuTime.UpperValue.Float64() == 3.0 &&
		entry.PeakMemory.Value.Float64() == 105*1024 &&
		entry.PeakMemory.LowerValue.Float64() == 100*1024 &&
		entry.PeakMemory.UpperValue.Float64() == 110*1024,
		fmt.Sprintf("%+v", entry))

	newLog := "[0.002s][info][gc] Using Epsilon\n" +
		"[1.053s][info][gc] Heap: 2048M reserved, 2048M (100.00%) committed, 103M (5.05%) used\n" +
		"[1.232s][info][gc] Heap: 2048M reserved, 2048M (100.00%) committed, 136M (6.69%) used\n"
	used, err := ParseEpsilonUsed(newLog)
	check("epsilon-parse-new", err == nil && used == 136*1024*1024, fmt.Sprintf("%d %v", used, err))
	used, err = ParseEpsilonUsed("[0.5s][gc] Heap: 2048M reserved, 134M committed, 49824K used\n")
	check("epsilon-parse-old", err == nil && used == 49824*1024, fmt.Sprintf("%d %v", used, err))
	usedG, errG := ParseEpsilonUsed("Heap: 8G reserved, 8G committed, 2G used\n")
	usedG2, errG2 := ParseEpsilonUsed("Heap: 8G reserved, 8G committed, 1.5G (18.75%) used\n")
	check("epsilon-parse-g", errG == nil && errG2 == nil && usedG == 2*1024*1024*1024 &&
		usedG2 == int64(1.5*1024*1024*1024), fmt.Sprintf("%d %d", usedG, usedG2))
	verboseLog := "[1.2s][info][gc] Heap: 2048M reserved, 2048M committed, 100M used\n" +
		"[1.2s][info][gc,metaspace] Metaspace: 1088M reserved, 12480K committed, 12218K used\n" +
		"[1.3s][info][gc] Heap: 2048M reserved, 2048M committed, 137M used\n" +
		"[1.3s][info][gc,exit]    space 2097152K,   6% used [0x0, 0x0, 0x0)\n"
	used, err = ParseEpsilonUsed(verboseLog)
	check("epsilon-parse-last-heap-only", err == nil && used == 137*1024*1024, fmt.Sprintf("%d %v", used, err))
	badLogs := []string{"", "garbage\n", "Heap: broken line\n",
		"Heap: 2048M reserved, 134M committed, 136X used\n",
		"Metaspace: 1088M reserved, 12218K used\n"}
	badCount := 0
	for _, text := range badLogs {
		if _, err := ParseEpsilonUsed(text); err != nil {
			badCount++
		}
	}
	check("epsilon-parse-rejects", badCount == len(badLogs), fmt.Sprintf("%d", badCount))

	det := SummarizeAlloc([]int64{61917 * 1024, 61917 * 1024, 61917 * 1024})
	check("alloc-deterministic", det.Min == 61917*1024 && det.Max == 61917*1024 && det.Deterministic,
		fmt.Sprintf("%+v", det))
	wob := SummarizeAlloc([]int64{61626 * 1024, 61628 * 1024, 61628 * 1024})
	check("alloc-wobble-min", wob.Min == 61626*1024 && wob.Max == 61628*1024 && !wob.Deterministic,
		fmt.Sprintf("%+v", wob))

	s1 := *s
	s1.AllocatedBytes = SummarizeAlloc([]int64{300, 100, 200})
	s2 := *s
	s2.AllocatedBytes = SummarizeAlloc([]int64{400, 400, 400})
	bmf2 := BuildBMF([]CaseResult{
		{Label: "sort/quick_sort", Stem: "solution", Ok: true, Summary: &s1},
		{Label: "sort/quick_sort", Stem: "solution2", Ok: true, Summary: &s2},
	}, nil)
	entry2 := bmf2["sort/quick_sort/solution"]
	check("bmf-alloc", entry2.AllocatedBytes != nil &&
		entry2.AllocatedBytes.Value.Float64() == 100 &&
		entry2.AllocatedBytes.LowerValue.Float64() == 100 &&
		entry2.AllocatedBytes.UpperValue.Float64() == 300 &&
		bmf2["sort/quick_sort/solution2"].AllocatedBytes.Value.Float64() == 400,
		fmt.Sprintf("%+v", entry2))

	s3 := *s
	cg := SummarizeInts([]int64{300, 100, 200})
	s3.CallgrindInstructions = &cg
	s3.CallgrindAllocatedBytes = SummarizeAlloc([]int64{500, 300, 400})
	bmf3 := BuildBMF([]CaseResult{
		{Label: "a", Stem: "s", Case: "c", Ok: true, Summary: &s3},
	}, nil)
	check("bmf-callgrind", bmf3["a/s"].CallgrindInstructions != nil &&
		bmf3["a/s"].CallgrindInstructions.Value.Float64() == 100 &&
		bmf3["a/s"].CallgrindAllocatedBytes != nil &&
		bmf3["a/s"].CallgrindAllocatedBytes.Value.Float64() == 300 &&
		bmf3["a/s"].AllocatedBytes == nil, fmt.Sprintf("%+v", bmf3["a/s"]))

	checkMerge(check)

	cgOut := "# callgrind format\nversion: 1\nsummary: 6615963986\n"
	ir, err := ParseCallgrindSummary(cgOut)
	check("callgrind-parse", err == nil && ir == 6615963986, fmt.Sprintf("%d %v", ir, err))
	_, err = ParseCallgrindSummary("no summary here\n")
	check("callgrind-parse-rejects", err != nil, "no error raised")

	shimOut := "program chatter\nsnip_alloc_total_bytes=123456\n"
	shimTotal, err := ParseAllocShimTotal(shimOut)
	check("allocshim-parse", err == nil && shimTotal == 123456, fmt.Sprintf("%d %v", shimTotal, err))
	_, err = ParseAllocShimTotal("no total here\n")
	check("allocshim-parse-rejects", err != nil, "no error raised")
	_, err = ParseAllocShimTotal("snip_alloc_total_bytes=abc\n")
	check("allocshim-parse-rejects-malformed", err != nil, "no error raised")

	check("num-int-marshal", marshalNum(pyjson.IntNum(107520)) == "107520", marshalNum(pyjson.IntNum(107520)))
	check("num-float-marshal", marshalNum(pyjson.FloatNum(105.0)) == "105.0", marshalNum(pyjson.FloatNum(105.0)))

	emptyRep := Report{Results: []CaseResult{}, Failures: []string{}, Missing: []string{},
		Complexity: map[string]slope.Entry{}}
	emptyRaw, emptyErr := pyjson.MarshalIndent2(emptyRep)
	emptyText := string(emptyRaw)
	check("json-empty-slices", emptyErr == nil &&
		strings.Contains(emptyText, `"results": []`) &&
		strings.Contains(emptyText, `"failures": []`) &&
		strings.Contains(emptyText, `"missing": []`) &&
		strings.Contains(emptyText, `"complexity": {}`), emptyText)

	checkSummary(check)
	checkAOT(check)
	checkSlopeRunner(check)
	return out
}

func intPtr(v int64) *int64 { return &v }

func strPtr(s string) *string { return &s }

func numPtr(v float64) *pyjson.Num {
	n := pyjson.FloatNum(v)
	return &n
}

func equalStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func marshalNum(n pyjson.Num) string {
	raw, err := pyjson.MarshalIndent2(n)
	if err != nil {
		return "ERR:" + err.Error()
	}
	return strings.TrimSpace(string(raw))
}

func checkMerge(check func(string, bool, string)) {
	s := SummarizeCase([]RunMetrics{
		{CpuS: pyjson.FloatNum(1.0), MaxRSSKb: 100, WallS: pyjson.FloatNum(1.2), Instructions: intPtr(10), Cycles: intPtr(20)},
		{CpuS: pyjson.FloatNum(2.0), MaxRSSKb: 110, WallS: pyjson.FloatNum(2.2), Instructions: intPtr(12), Cycles: intPtr(22)},
		{CpuS: pyjson.FloatNum(3.0), MaxRSSKb: 105, WallS: pyjson.FloatNum(3.2)},
	})
	s1 := *s
	s1.AllocatedBytes = SummarizeAlloc([]int64{300, 100, 200})
	s2 := *s
	s2.AllocatedBytes = SummarizeAlloc([]int64{400, 400, 400})
	cxA := map[string]slope.Entry{"a/s": {
		Ok: true, Fit: &slope.Fit{Ok: true, Slope: numPtr(1.0), Lower: numPtr(0.9), Upper: numPtr(1.1)},
	}}
	cxB := map[string]slope.Entry{"b/s": {
		Ok: true, Fit: &slope.Fit{Ok: true, Slope: numPtr(2.0), Lower: numPtr(1.9), Upper: numPtr(2.1)},
	}}
	writeReport := func(results []CaseResult, failures, missing []string, cx map[string]slope.Entry) string {
		rep := Report{Tool: "t", Config: Config{Shard: strPtr("0/2")},
			Results: results, Failures: failures, Missing: missing, Complexity: cx}
		raw, err := pyjson.MarshalIndent2(rep)
		if err != nil {
			panic(err)
		}
		path := filepath.Join(os.TempDir(), fmt.Sprintf("snip-selftest-%p.json", &raw))
		if err := os.WriteFile(path, raw, 0o644); err != nil {
			panic(err)
		}
		return path
	}
	shardA := writeReport([]CaseResult{
		{Label: "a", Stem: "s", Case: "c1", Ok: true, Summary: &s1},
		{Label: "a", Stem: "s", Case: "c2", Ok: true, Summary: &s1},
	}, []string{"x: no resources found"}, []string{"x"}, cxA)
	defer os.Remove(shardA)
	shardB := writeReport([]CaseResult{
		{Label: "b", Stem: "s", Case: "c1", Ok: true, Summary: &s2},
	}, []string{"x: no resources found"}, []string{"x", "y"}, cxB)
	defer os.Remove(shardB)
	merged, mergedBMF, err := MergeReports([]string{shardA, shardB})
	wholeBMF := BuildBMF([]CaseResult{
		{Label: "a", Stem: "s", Case: "c1", Ok: true, Summary: &s1},
		{Label: "a", Stem: "s", Case: "c2", Ok: true, Summary: &s1},
		{Label: "b", Stem: "s", Case: "c1", Ok: true, Summary: &s2},
	}, map[string]slope.Entry{"a/s": cxA["a/s"], "b/s": cxB["b/s"]})
	bmfEqual := err == nil && marshalBMF(mergedBMF) == marshalBMF(wholeBMF)
	orderOK := err == nil && len(merged.Results) == 3 &&
		merged.Results[0].Case == "c1" && merged.Results[1].Case == "c2" &&
		merged.Results[2].Label == "b"
	check("merge", err == nil && bmfEqual && orderOK &&
		merged.Config.Shard != nil && *merged.Config.Shard == "merged" &&
		equalStrings(merged.Failures, []string{"x: no resources found"}) &&
		equalStrings(merged.Missing, []string{"x", "y"}) &&
		len(merged.Complexity) == 2 &&
		mergedBMF["a/s"].ComplexityExponent.Value.Float64() == 1.0 &&
		mergedBMF["b/s"].ComplexityExponent.Value.Float64() == 2.0,
		fmt.Sprintf("%v", err))
	merged2, mergedBMF2, err := MergeReports([]string{shardA, shardA})
	check("merge-dedupe", err == nil && len(merged2.Results) == 2 &&
		len(merged2.Complexity) == 1 &&
		marshalBMF(mergedBMF2) == marshalBMF(BuildBMF([]CaseResult{
			{Label: "a", Stem: "s", Case: "c1", Ok: true, Summary: &s1},
			{Label: "a", Stem: "s", Case: "c2", Ok: true, Summary: &s1},
		}, cxA)), fmt.Sprintf("%v", err))
}

func marshalBMF(bmf BMF) string {
	raw, err := pyjson.MarshalIndent2(bmf)
	if err != nil {
		return "ERR:" + err.Error()
	}
	return string(raw)
}

func checkSummary(check func(string, bool, string)) {
	cpuModel, java := "c", "j"
	rep := Report{TimestampUTC: "t", Host: Host{CpuModel: &cpuModel},
		Runtime: Runtime{Name: "jvm", JavaVersion: &java},
		Config:  Config{Repeat: 1, Jobs: 1},
		Results: []CaseResult{}, Failures: []string{}, Missing: []string{},
		Complexity: map[string]slope.Entry{}}
	withCxp := BMF{
		"a/s": {CpuTime: &Measure{Value: pyjson.FloatNum(1.0)},
			PeakMemory:         &Measure{Value: pyjson.FloatNum(1048576.0)},
			AllocatedBytes:     &Measure{Value: pyjson.FloatNum(2097152.0)},
			ComplexityExponent: &Measure{Value: pyjson.FloatNum(2.0)}},
		"b/s": {CpuTime: &Measure{Value: pyjson.FloatNum(2.0)},
			PeakMemory: &Measure{Value: pyjson.FloatNum(1048576.0),
				LowerValue: pyjson.FloatNum(1.0), UpperValue: pyjson.FloatNum(3.0)}},
	}
	path := filepath.Join(os.TempDir(), "snip-selftest-summary.md")
	defer os.Remove(path)
	os.Remove(path)
	if err := AppendSummary(path, rep, withCxp); err != nil {
		check("summary-cxp-column", false, err.Error())
	} else {
		raw, _ := os.ReadFile(path)
		text := string(raw)
		check("summary-cxp-column", strings.Contains(text, "| cxp (p) |") &&
			strings.Contains(text, "- runtime: jvm, perf counters: no, repeat: 1, jobs: 1, alloc_repeat: n/a") &&
			strings.Contains(text, "| a/s | 0 | 1.000 | 1.0 | 2.0 | n/a | 0.000 | 2.00 |") &&
			strings.Contains(text, "| b/s | 0 | 2.000 | 1.0 | n/a | n/a | 0.000 | n/a |"), text)
	}
	withoutCxp := BMF{
		"a/s": {CpuTime: withCxp["a/s"].CpuTime, PeakMemory: withCxp["a/s"].PeakMemory,
			AllocatedBytes: withCxp["a/s"].AllocatedBytes},
		"b/s": withCxp["b/s"],
	}
	os.Remove(path)
	if err := AppendSummary(path, rep, withoutCxp); err != nil {
		check("summary-no-cxp-column", false, err.Error())
	} else {
		raw, _ := os.ReadFile(path)
		check("summary-no-cxp-column", !strings.Contains(string(raw), "| cxp (p) |"), string(raw))
	}
	withCg := BMF{
		"a/s": {CpuTime: withCxp["a/s"].CpuTime, PeakMemory: withCxp["a/s"].PeakMemory,
			AllocatedBytes:        withCxp["a/s"].AllocatedBytes,
			CallgrindInstructions: &Measure{Value: pyjson.IntNum(6200111758)}},
	}
	os.Remove(path)
	if err := AppendSummary(path, rep, withCg); err != nil {
		check("summary-cg-column", false, err.Error())
	} else {
		raw, _ := os.ReadFile(path)
		text := string(raw)
		check("summary-cg-column", strings.Contains(text, "| cg_insn |") &&
			strings.Contains(text, "| a/s | 0 | 1.000 | 1.0 | 2.0 | n/a | 6200111758 | 0.000 |"), text)
	}
	block := SlopeFileLines(map[string]slope.Entry{
		"a/s": {Ok: true, Fit: &slope.Fit{Ok: true, Slope: numPtr(1.5), Lower: numPtr(1.4),
			Upper: numPtr(1.6), RSquared: numPtr(0.99)}},
		"b/s": {Ok: false, Fit: &slope.Fit{Ok: false}, Error: strPtr("boom")},
	})
	check("summary-slope-block", len(block) == 5 && block[1] == "**Complexity exponents (T20):**" &&
		block[3] == "- a/s: p = 1.500 [1.400, 1.600], R^2 = 0.9900" &&
		block[4] == "- b/s: skipped (boom)" &&
		len(SlopeFileLines(map[string]slope.Entry{})) == 0, fmt.Sprintf("%q", block))
}

func checkAOT(check func(string, bool, string)) {
	dir, err := os.MkdirTemp("", "snip-selftest-aot-")
	if err != nil {
		check("aot-transform-inline", false, err.Error())
		return
	}
	defer os.RemoveAll(dir)
	script := filepath.Join(dir, "solution.clj")
	body := "(defn tokenize [s] (vec s))\n\n(defn -main [input] (println (tokenize input)))\n\n(-main (slurp *in*))\n"
	if err := os.WriteFile(script, []byte(body), 0o644); err != nil {
		check("aot-transform-inline", false, err.Error())
		return
	}
	tr, err := aot.Transform(script, "t/prob", "solution")
	ok := err == nil && tr.NS == "snip_t_prob_solution" &&
		strings.Contains(tr.Source, "(defn script-main [input]") &&
		strings.Contains(tr.Source, "(script-main (slurp *in*))") &&
		strings.Contains(tr.Source, "(defn -main [& _args]\n  (-aot-run))")
	check("aot-transform-inline", ok, fmt.Sprintf("%v %+v", err, tr))
	forms, err := cljread.Split("(def a \"x)y\") ;; c\n[1 2]\n'quoted\n")
	check("cljread-split-inline", err == nil && len(forms) == 3 &&
		forms[0] == `(def a "x)y")` && forms[1] == "[1 2]" && forms[2] == "'quoted",
		fmt.Sprintf("%q %v", forms, err))
}

func checkSlopeRunner(check func(string, bool, string)) {
	// Stubbed measure (no JVM): reports exact quadratic times
	// parsed back out of the case name, after validating the
	// generated IO like the Python stub.
	stubOK := func(req slope.ScaleRequest) slope.ScaleOutcome {
		fail := func(msg string) slope.ScaleOutcome {
			return slope.ScaleOutcome{Ok: false, Error: msg}
		}
		// Mirror the signature-drift guard: slope scales are
		// timing-only probes with alloc_repeat forced to 0.
		if req.AllocRepeat != 0 {
			return fail(fmt.Sprintf("alloc_repeat drift: %d", req.AllocRepeat))
		}
		tail := req.CaseName[strings.LastIndex(req.CaseName, "_")+1:]
		n, err := strconv.Atoi(tail)
		if err != nil {
			return fail("bad case name")
		}
		raw, err := os.ReadFile(req.InPath)
		if err != nil {
			return fail("bad generated io")
		}
		toks := strings.Fields(string(raw))
		if len(toks) == 0 || toks[0] != tail || len(toks) != n+1 {
			return fail("bad generated io")
		}
		if _, err := os.Stat(req.OutPath); err != nil {
			return fail("bad generated io")
		}
		t := 2.0 + 1e-6*float64(n)*float64(n)
		return slope.ScaleOutcome{Ok: true, CpuMin: t, CpuMedian: t, CpuMax: t}
	}
	stubFail := func(req slope.ScaleRequest) slope.ScaleOutcome {
		return slope.ScaleOutcome{Ok: false, Error: "boom"}
	}
	tiny := map[string]slope.Generator{
		"sort/insertion_sort/solution": SlopeAdapter{generators.GENERATORS["sort/insertion_sort/solution"]},
		"sort/quick_sort/solution":     SlopeAdapter{generators.GENERATORS["sort/quick_sort/solution"]},
	}
	scripts := map[slope.Group]string{
		{Label: "sort/insertion_sort", Stem: "solution"}: "/fake/i.clj",
		{Label: "sort/quick_sort", Stem: "solution"}:     "/fake/q.clj",
	}
	groups := []slope.Group{
		{Label: "sort/insertion_sort", Stem: "solution"},
		{Label: "sort/quick_sort", Stem: "solution"},
	}
	got, err := slope.RunSlopeAll(stubOK, tiny, scripts, "", groups, nil, 1, 1, 1, nil)
	bothOK := err == nil && len(got) == 2
	for _, entry := range got {
		bothOK = bothOK && entry.Ok && entry.Fit != nil && entry.Fit.Ok &&
			entry.Fit.Slope.Float64() > 1.95 && entry.Fit.Slope.Float64() < 2.05 &&
			!entry.ScaleFailed
	}
	check("slope-runner-both", bothOK, fmt.Sprintf("%v %+v", err, got))

	got, err = slope.RunSlopeAll(stubOK, tiny, scripts, "quick", groups, nil, 1, 1, 1, nil)
	check("slope-runner-only", err == nil && len(got) == 1 && got["sort/quick_sort/solution"].Ok,
		fmt.Sprintf("%v %v", err, sortedEntryKeys(got)))

	wide := []slope.Group{{Label: "a", Stem: "s"}, groups[0], groups[1], {Label: "z", Stem: "s"}}
	got, err = slope.RunSlopeAll(stubOK, tiny, scripts, "", wide, &slope.Shard{Index: 1, Total: 4}, 1, 1, 1, nil)
	check("slope-runner-shard", err == nil && len(got) == 1 && got["sort/insertion_sort/solution"].Ok,
		fmt.Sprintf("%v %v", err, sortedEntryKeys(got)))

	got, err = slope.RunSlopeAll(stubOK, tiny,
		map[slope.Group]string{{Label: "a", Stem: "s"}: "/fake/a.clj"}, "", groups, nil, 1, 1, 1, nil)
	check("slope-runner-no-scripts", err == nil && len(got) == 0, fmt.Sprintf("%v %v", err, got))

	got, err = slope.RunSlopeAll(stubFail, tiny, scripts, "", groups, nil, 1, 1, 1, nil)
	failOK := err == nil && len(got) == 2
	for _, entry := range got {
		failOK = failOK && !entry.Ok && entry.Error != nil && entry.ScaleFailed
	}
	check("slope-runner-error", failOK, fmt.Sprintf("%v %+v", err, got))

	type progressCall struct {
		index, total int
		name         string
		ok           bool
	}
	var calls []progressCall
	_, err = slope.RunSlopeAll(stubOK, tiny, scripts, "", groups, nil, 1, 1, 1,
		func(index, total int, name string, entry slope.Entry) {
			calls = append(calls, progressCall{index, total, name, entry.Ok})
		})
	progOK := err == nil && len(calls) == 2 &&
		calls[0] == progressCall{1, 2, "sort/insertion_sort/solution", true} &&
		calls[1] == progressCall{2, 2, "sort/quick_sort/solution", true}
	check("slope-runner-progress", progOK, fmt.Sprintf("%v %+v", err, calls))
}
