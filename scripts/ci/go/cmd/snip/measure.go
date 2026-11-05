package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/generators"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/measure"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/slope"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

func sortedComplexityNames(complexity map[string]slope.Entry) []string {
	names := make([]string, 0, len(complexity))
	for name := range complexity {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

const measureUsage = `Load-independent performance measurement for the Clojure snippet repo.

Runs every src/**/*.clj script against the shared test cases and records
cpu_s, maxrss_kb, allocated_bytes, instructions/cycles, plus (under the
new flags) callgrind instruction counts, Pin instruction counts,
and AOT-compiled runs. --repeat 0 skips the timed phase and runs
only the instrument phases (pin-only legs).

Usage: snip measure [--resources DIR] [--repeat N] [--jobs N] [--timeout S]
                   [--only SUBSTR] [--output F] [--bmf-output F]
                   [--summary-file F] [--time-bin BIN] [--no-perf]
                   [--runtime NAME] [--command-template T] [--shard K/N]
                   [--alloc-repeat N | --no-alloc] [--slope]
                   [--callgrind] [--callgrind-repeat N] [--aot]
                   [--aot-cache DIR]
                   [--pin] [--pin-bin BIN] [--pin-tool SO] [--pin-repeat N]
                   [--pin-timeout S]

       snip measure --merge INPUT... [--output F] [--bmf-output F]
                   [--summary-file F]

       snip measure --self-test
`

func runMeasure(argv []string) int {
	root := snipcore.RepoRoot()
	resources := filepath.Join(root, "tests", "resources")
	var cp, only, output, bmfOutput, summaryFile, timeBin, shard, aotCache, pinBin, pinTool string
	runtimeName := "jvm"
	template := measure.DefaultCommandTemplate
	jobs := 1
	timeout := 120
	repeat := 5
	allocRepeat := measure.AllocRepeatDefault
	slopeRepeat := 3
	callgrindRepeat := measure.CallgrindRepeatDefault
	pinRepeat := measure.PinRepeatDefault
	pinTimeout := 600
	perf := true
	alloc := true
	merge := false
	slopeFlag := false
	slopeOnly := false
	selfTest := false
	aotFlag := false
	callgrind := false
	pin := false
	positionals, err := parseFlags("measure", argv, flagSpec{
		Strs: map[string]*string{
			"resources": &resources, "cp": &cp, "only": &only,
			"output": &output, "bmf-output": &bmfOutput,
			"summary-file": &summaryFile, "time-bin": &timeBin,
			"runtime": &runtimeName, "command-template": &template,
			"shard": &shard, "aot-cache": &aotCache,
			"pin-bin": &pinBin, "pin-tool": &pinTool,
		},
		Ints: map[string]*int{
			"jobs": &jobs, "timeout": &timeout, "repeat": &repeat,
			"alloc-repeat": &allocRepeat, "slope-repeat": &slopeRepeat,
			"callgrind-repeat": &callgrindRepeat, "pin-repeat": &pinRepeat,
			"pin-timeout": &pinTimeout,
		},
		Bools: map[string]*bool{
			"perf": &perf, "alloc": &alloc, "merge": &merge,
			"slope": &slopeFlag, "slope-only": &slopeOnly,
			"self-test": &selfTest, "aot": &aotFlag,
			"callgrind": &callgrind, "pin": &pin,
		},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(measureUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if selfTest {
		return measure.SelfTest(os.Stdout)
	}
	if merge {
		return runMerge(positionals, output, bmfOutput, summaryFile)
	}
	if repeat < 0 {
		fmt.Fprintln(os.Stderr, "snip measure: --repeat must be >= 0")
		return 2
	}
	if allocRepeat < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --alloc-repeat must be >= 1")
		return 2
	}
	if jobs < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --jobs must be >= 1")
		return 2
	}
	if slopeOnly {
		slopeFlag = true
	}
	if slopeRepeat < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --slope-repeat must be >= 1")
		return 2
	}
	if callgrindRepeat < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --callgrind-repeat must be >= 1")
		return 2
	}
	if pinRepeat < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --pin-repeat must be >= 1")
		return 2
	}
	if pinTimeout < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --pin-timeout must be >= 1")
		return 2
	}
	var shardParsed *[2]int
	if shard != "" {
		idx, total, err := snipcore.ParseMeasureShard(shard)
		if err != nil {
			fmt.Fprintln(os.Stderr, "snip measure:", err)
			return 2
		}
		shardParsed = &[2]int{idx, total}
	}
	if aotFlag && template != measure.DefaultCommandTemplate {
		fmt.Fprintln(os.Stderr, "snip measure: cannot combine --aot with --command-template")
		return 2
	}

	timeBinPath, err := measure.ResolveTimeBin(timeBin)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	usePerf := perf && measure.PerfUsable()
	if perf && !usePerf {
		fmt.Fprintln(os.Stderr, "warning: perf counters unavailable; instructions/cycles will be null")
	}
	useCallgrind := callgrind && measure.CallgrindUsable()
	if callgrind && !useCallgrind {
		fmt.Fprintln(os.Stderr, "warning: valgrind unavailable; callgrind_instructions will be null")
	}
	usePin := pin && measure.PinUsable(pinBin, pinTool)
	if pin && !usePin {
		fmt.Fprintln(os.Stderr, "warning: pin unavailable (need --pin-bin launcher and --pin-tool inscount .so); pin_instructions will be null")
	}
	var pinVersion *string
	if usePin {
		pinVersion = measure.PinVersion(pinBin)
	}
	if usePin && measure.TemplateHeadIsJava(template) && !measure.PinNoASLRUsable() {
		fmt.Fprintln(os.Stderr, "snip measure: jvm pin legs need setarch (util-linux) to fix the address layout")
		return 2
	}
	classpath, err := snipcore.ResolveClasspath(root, cp)
	if err != nil {
		if !snipcore.TemplateNeedsClasspath(template) {
			fmt.Fprintln(os.Stderr, "warning: no Clojure classpath; continuing without one (template has no {cp} hole)")
			classpath = ""
		} else {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	found, err := snipcore.Discover(root, resources)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	type task struct {
		label, stem, caseName, script, inp, outp string
	}
	var tasks []task
	missing := []string{}
	for _, entry := range found {
		label := entry.Label
		if only != "" && !strings.Contains(label, only) {
			continue
		}
		if len(entry.Cases) == 0 {
			missing = append(missing, label)
			continue
		}
		for _, c := range entry.Cases {
			tasks = append(tasks, task{label, entry.Stem, c.Name, entry.Script, c.Input, c.Output})
		}
	}
	slopeGroups := map[slope.Group]bool{}
	for _, t := range tasks {
		slopeGroups[slope.Group{Label: t.label, Stem: t.stem}] = true
	}
	slopeGroupList := make([]slope.Group, 0, len(slopeGroups))
	for g := range slopeGroups {
		slopeGroupList = append(slopeGroupList, g)
	}
	if shardParsed != nil {
		tasks = measure.SelectShard(tasks, func(t task) (string, string) {
			return t.label, t.stem
		}, shardParsed[0], shardParsed[1])
	}
	if slopeOnly {
		tasks = nil
		missing = nil
	}
	effectiveAlloc := 0
	if alloc {
		effectiveAlloc = allocRepeat
	}
	if effectiveAlloc > 0 && len(tasks) > 0 && !aotFlag {
		t0 := tasks[0]
		if !measure.AllocSupported(template, classpath, t0.script, t0.stem) {
			fmt.Fprintln(os.Stderr, "warning: allocation measurement needs a java command template; skipping (BMF will omit allocated_bytes)")
			effectiveAlloc = 0
		}
	}
	// Callgrind runs for JVM and native legs alike now: the
	// recipe branches on the resolved argv0 (EpsilonGC log vs
	// LD_PRELOAD counting). Only valgrind's absence disables it.
	effectiveCg := 0
	if useCallgrind {
		effectiveCg = callgrindRepeat
	}
	effectivePin := 0
	if usePin {
		effectivePin = pinRepeat
	}
	if repeat == 0 && effectiveAlloc == 0 && effectiveCg == 0 && effectivePin == 0 {
		fmt.Fprintln(os.Stderr, "snip measure: --repeat 0 needs a measurement phase (--alloc, --callgrind, or --pin)")
		return 2
	}
	var aotCacheObj *measure.AOTCache
	if aotFlag {
		java := "unknown"
		if v := measure.JavaVersion(); v != nil {
			java = *v
		}
		aotCacheObj = &measure.AOTCache{
			Root: root, CacheDir: aotCache, Classpath: classpath,
			Java: java, Timeout: timeout,
		}
	}

	results := make([]measure.CaseResult, len(tasks))
	work := make(chan int)
	total := len(tasks)
	var done atomic.Int64
	var printMu sync.Mutex
	var workers sync.WaitGroup
	for w := 0; w < jobs; w++ {
		workers.Add(1)
		go func() {
			defer workers.Done()
			for i := range work {
				t := tasks[i]
				results[i] = measure.MeasureCase(measure.CaseOpts{
					TimeBin: timeBinPath, UsePerf: usePerf, Template: template,
					Classpath: classpath, Label: t.label, Stem: t.stem,
					Script: t.script, Case: t.caseName, InputPath: t.inp,
					OutputPath: t.outp, Repeat: repeat,
					AllocRepeat: effectiveAlloc, CallgrindRepeat: effectiveCg,
					PinBin: pinBin, PinTool: pinTool, PinRepeat: effectivePin,
					PinTimeout: pinTimeout,
					Timeout:    timeout, Root: root, AOT: aotCacheObj,
				})
				n := done.Add(1)
				printMu.Lock()
				fmt.Println(formatCaseProgress(int(n), total, results[i]))
				printMu.Unlock()
			}
		}()
	}
	go func() {
		for i := range tasks {
			work <- i
		}
		close(work)
	}()
	workers.Wait()
	sort.Slice(results, func(i, j int) bool {
		if results[i].Label != results[j].Label {
			return results[i].Label < results[j].Label
		}
		if results[i].Stem != results[j].Stem {
			return results[i].Stem < results[j].Stem
		}
		return results[i].Case < results[j].Case
	})
	failures := []string{}
	for _, r := range results {
		if !r.Ok && r.Error != nil {
			failures = append(failures, *r.Error)
		}
	}
	sort.Strings(failures)
	for _, label := range missing {
		failures = append(failures, label+": no resources found")
	}
	complexity := map[string]slope.Entry{}
	if slopeFlag {
		slopeScripts := map[slope.Group]string{}
		for _, entry := range found {
			label := entry.Label
			if only != "" && !strings.Contains(label, only) {
				continue
			}
			slopeScripts[slope.Group{Label: label, Stem: entry.Stem}] = entry.Script
		}
		var slopeShard *slope.Shard
		if shardParsed != nil {
			slopeShard = &slope.Shard{Index: shardParsed[0], Total: shardParsed[1]}
		}
		registry := map[string]slope.Generator{}
		for name, gen := range generators.GENERATORS {
			registry[name] = measure.SlopeAdapter{Gen: gen}
		}
		measureScale := func(req slope.ScaleRequest) slope.ScaleOutcome {
			res := measure.MeasureCase(measure.CaseOpts{
				TimeBin: timeBinPath, UsePerf: usePerf, Template: template,
				Classpath: classpath, Label: req.Label, Stem: req.Stem,
				Script: req.Script, Case: req.CaseName, InputPath: req.InPath,
				OutputPath: req.OutPath, Repeat: req.Repeat,
				AllocRepeat: req.AllocRepeat, CallgrindRepeat: 0,
				PinBin: pinBin, PinTool: pinTool, PinRepeat: 0,
				PinTimeout: pinTimeout,
				Timeout:    req.Timeout, Root: root, AOT: aotCacheObj,
			})
			out := slope.ScaleOutcome{Ok: res.Ok}
			if !res.Ok {
				if res.Error != nil {
					out.Error = *res.Error
				}
				return out
			}
			out.CpuMin = res.Summary.CpuS.Min.Float64()
			out.CpuMedian = res.Summary.CpuS.Median.Float64()
			out.CpuMax = res.Summary.CpuS.Max.Float64()
			return out
		}
		var slopeErr error
		complexity, slopeErr = slope.RunSlopeAll(measureScale, registry, slopeScripts,
			only, slopeGroupList, slopeShard, slopeRepeat, timeout, jobs,
			func(index, total int, name string, entry slope.Entry) {
				fmt.Println(formatSlopeProgress(index, total, name, entry))
			})
		if slopeErr != nil {
			fmt.Fprintln(os.Stderr, "snip measure:", slopeErr)
			return 1
		}
		for _, name := range sortedComplexityNames(complexity) {
			entry := complexity[name]
			if entry.ScaleFailed && entry.Error != nil {
				failures = append(failures, "slope "+name+": "+*entry.Error)
			}
		}
	}

	javaLeg := aotFlag
	nativeBin := ""
	if !aotFlag && len(tasks) > 0 {
		t0 := tasks[0]
		if inner, err := measure.BuildInnerCommand(template, classpath, t0.script, t0.stem); err == nil && len(inner) > 0 {
			if measure.InnerIsJava(inner) {
				javaLeg = true
			} else {
				nativeBin = inner[0]
			}
		}
	} else if !aotFlag {
		javaLeg = measure.TemplateHeadIsJava(template)
	}
	var javaVersion, runtimeVersion *string
	if javaLeg {
		javaVersion = measure.JavaVersion()
	} else if nativeBin != "" {
		runtimeVersion = measure.RuntimeVersion(nativeBin)
	}
	bmf := measure.BuildBMF(results, complexity)
	report := measure.Report{
		Tool: "snip measure", Version: measure.ToolVersion,
		TimestampUTC: time.Now().UTC().Format("2006-01-02T15:04:05.000000-07:00"),
		Host:         measure.HostInfo(),
		Runtime: measure.Runtime{
			Name: runtimeName, CommandTemplate: template,
			JavaVersion: javaVersion, RuntimeVersion: runtimeVersion,
			Classpath: classpath,
			TimeBin:   timeBinPath, PerfUsed: usePerf, CallgrindUsed: effectiveCg > 0,
			PinUsed: effectivePin > 0, PinVersion: pinVersion,
		},
		Config: measure.Config{
			Repeat: repeat, Jobs: jobs, Timeout: timeout, Only: only,
			Shard: strOrNil(shard), AllocRepeat: &effectiveAlloc,
			Slope: slopeFlag, SlopeRepeat: slopeRepeat,
			CallgrindRepeat: effectiveCg, PinRepeat: effectivePin, PinTimeout: pinTimeout,
			Aot: aotFlag,
		},
		Results: results, Failures: failures, Missing: missing,
		Complexity: complexity,
	}
	if output != "" {
		if err := writeJSON(output, report); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if bmfOutput != "" {
		if err := writeJSON(bmfOutput, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if summaryFile != "" {
		if err := measure.AppendSummary(summaryFile, report, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
		if block := measure.SlopeFileLines(complexity); len(block) > 0 {
			fd, err := os.OpenFile(summaryFile, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				return 1
			}
			fd.WriteString(strings.Join(block, "\n") + "\n")
			fd.Close()
		}
	}
	measure.PrintTable(os.Stdout, results, failures, missing)
	if len(failures) > 0 {
		return 1
	}
	return 0
}

func strOrNil(s string) *string {
	if s == "" {
		return nil
	}
	return &s
}

// formatCaseProgress renders one live progress line for a finished
// case: the [done/total] counter streams the battery's progress to
// the log while the full table still prints at the end.
func formatCaseProgress(done, total int, res measure.CaseResult) string {
	outcome := "ok"
	if !res.Ok {
		outcome = "failed"
		if res.Error != nil {
			outcome = *res.Error
		}
	} else if res.CallgrindSkipped != nil {
		outcome = "ok (callgrind skipped: " + *res.CallgrindSkipped + ")"
	} else if res.PinSkipped != nil {
		outcome = "ok (pin skipped: " + *res.PinSkipped + ")"
	}
	return fmt.Sprintf("measure [%d/%d] %s/%s/%s: %s",
		done, total, res.Label, res.Stem, res.Case, outcome)
}

// formatSlopeProgress renders one live progress line for a finished
// slope benchmark, streamed from the RunSlopeAll callback in
// sorted-name order (same text the end-of-run block used to print,
// plus the [index/total] counter).
func formatSlopeProgress(index, total int, name string, entry slope.Entry) string {
	if entry.Ok && entry.Fit != nil && entry.Fit.Ok {
		var scales []string
		for _, n := range entry.Scales {
			scales = append(scales, string(n))
		}
		return fmt.Sprintf("slope [%d/%d] %s: p=%.3f [%.3f,%.3f] R^2=%.4f floor=%.2fs n=[%s]",
			index, total, name, entry.Fit.Slope.Float64(), entry.Fit.Lower.Float64(),
			entry.Fit.Upper.Float64(), entry.Fit.RSquared.Float64(),
			entry.Fit.FloorC.Float64(), strings.Join(scales, ", "))
	}
	errText := ""
	if entry.Error != nil {
		errText = *entry.Error
	}
	return fmt.Sprintf("slope [%d/%d] %s: SKIPPED %s", index, total, name, errText)
}

func runMerge(inputs []string, output, bmfOutput, summaryFile string) int {
	if len(inputs) == 0 {
		fmt.Fprintln(os.Stderr, "snip measure: --merge needs at least one input report")
		return 2
	}
	report, bmf, err := measure.MergeReports(inputs)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	if output != "" {
		if err := writeJSON(output, report); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if bmfOutput != "" {
		if err := writeJSON(bmfOutput, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if summaryFile != "" {
		if err := measure.AppendSummary(summaryFile, report, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
		if block := measure.SlopeFileLines(report.Complexity); len(block) > 0 {
			fd, err := os.OpenFile(summaryFile, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
			if err != nil {
				fmt.Fprintln(os.Stderr, "snip measure:", err)
				return 1
			}
			fd.WriteString(strings.Join(block, "\n") + "\n")
			fd.Close()
		}
	}
	fmt.Printf("merge: %d results, %d benchmarks, %d failures\n",
		len(report.Results), len(bmf), len(report.Failures))
	if len(report.Failures) > 0 {
		return 1
	}
	return 0
}

func writeJSON(path string, v any) error {
	raw, err := pyjson.MarshalIndent2(v)
	if err != nil {
		return fmt.Errorf("cannot write %s: %v", path, err)
	}
	if err := os.WriteFile(path, raw, 0o644); err != nil {
		return fmt.Errorf("cannot write %s: %v", path, err)
	}
	return nil
}
