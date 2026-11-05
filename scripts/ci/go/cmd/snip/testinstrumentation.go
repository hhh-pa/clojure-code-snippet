package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/measure"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

const testInstrumentationUsage = `Instrumented correctness runner for the Clojure snippet repo.

For every solution script under src/<area>/<problem>/<file>.clj this runs
java -cp <classpath> clojure.main <file>.clj once per shared test case and
diff-compares stdout against the expected output (trailing whitespace per
line ignored). Each case runs exactly once: with --pin-bin/--pin-tool the
run happens under Intel Pin plus GNU time, recording the dynamic
instruction count and peak RSS alongside the verdict; without them the
run is native and checks correctness only.

Usage: snip test-instrumentation [--resources DIR] [--cp CP] [--jobs N]
                 [--timeout S] [--only SUBSTR] [--shard I/N]
                 [--pin-bin BIN] [--pin-tool SO] [--time-bin BIN]
`

type testInstrumentationTask struct {
	label  string
	case_  string
	script string
	inp    string
	outp   string
}

func runTestInstrumentation(argv []string) int {
	root := snipcore.RepoRoot()
	resources := filepath.Join(root, "tests", "resources")
	var cp, only, shard, pinBin, pinTool, timeBin string
	jobs := 2
	timeout := 300
	_, err := parseFlags("test-instrumentation", argv, flagSpec{
		Strs: map[string]*string{
			"resources": &resources, "cp": &cp, "only": &only, "shard": &shard,
			"pin-bin": &pinBin, "pin-tool": &pinTool, "time-bin": &timeBin,
		},
		Ints: map[string]*int{"jobs": &jobs, "timeout": &timeout},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(testInstrumentationUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	// Pin requested means Pin required: a silently native leg
	// would report vacuous (null) instruction counts.
	pinRequested := pinBin != "" || pinTool != ""
	timeBinPath := ""
	pinVersion := ""
	if pinRequested {
		if !measure.PinUsable(pinBin, pinTool) {
			fmt.Fprintln(os.Stderr, "snip test-instrumentation: pin unavailable (need --pin-bin launcher and --pin-tool inscount .so)")
			return 1
		}
		if !measure.PinNoASLRUsable() {
			fmt.Fprintln(os.Stderr, "snip test-instrumentation: jvm pin legs need setarch (util-linux) to fix the address layout")
			return 1
		}
		timeBinPath, err = measure.ResolveTimeBin(timeBin)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
		if v := measure.PinVersion(pinBin); v != nil {
			pinVersion = *v
		}
	}
	shardIndex, shardTotal := 0, 1
	if shard != "" {
		var shardErr error
		shardIndex, shardTotal, shardErr = snipcore.ParseTestShard(shard)
		if shardErr != nil {
			fmt.Fprintln(os.Stderr, shardErr)
			return 1
		}
	}
	classpath, err := snipcore.ResolveClasspath(root, cp)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	found, err := snipcore.Discover(root, resources)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	var tasks []testInstrumentationTask
	var missing []string
	for _, entry := range found {
		label := entry.Area + "/" + entry.Problem
		if only != "" && !strings.Contains(label, only) && !strings.Contains(entry.Script, only) {
			continue
		}
		if len(entry.Cases) == 0 {
			missing = append(missing, label)
			continue
		}
		for _, c := range entry.Cases {
			tasks = append(tasks, testInstrumentationTask{label, c.Name, entry.Script, c.Input, c.Output})
		}
	}
	sort.Slice(tasks, func(i, j int) bool {
		if tasks[i].label != tasks[j].label {
			return tasks[i].label < tasks[j].label
		}
		if tasks[i].case_ != tasks[j].case_ {
			return tasks[i].case_ < tasks[j].case_
		}
		return tasks[i].script < tasks[j].script
	})
	if shardTotal > 1 {
		var kept []testInstrumentationTask
		for index, task := range tasks {
			if index%shardTotal == shardIndex {
				kept = append(kept, task)
			}
		}
		tasks = kept
	}
	total := len(tasks)
	problems := map[string]bool{}
	for _, task := range tasks {
		problems[task.label] = true
	}
	shardTag := ""
	if shardTotal > 1 {
		shardTag = fmt.Sprintf(" [shard %d/%d]", shardIndex+1, shardTotal)
	}
	fmt.Printf("clojure tests: %d cases%s (%d problems)\n", total, shardTag, len(problems))
	if pinRequested {
		fmt.Printf("instrumentation: pin %s (instructions + RSS per case)\n", pinVersion)
	} else {
		fmt.Println("instrumentation: native (no --pin-bin/--pin-tool; correctness only)")
	}

	if jobs < 1 {
		jobs = 1
	}
	type result struct {
		label string
		case_ string
		error string
		insns int64
		rssKb int64
	}
	work := make(chan testInstrumentationTask)
	results := make(chan result)
	var workers sync.WaitGroup
	for w := 0; w < jobs; w++ {
		workers.Add(1)
		go func() {
			defer workers.Done()
			for task := range work {
				if pinRequested {
					stdout, insns, rssKb, runErr := measure.RunInstrumented(
						timeBinPath, pinBin, pinTool,
						[]string{"java", "-cp", classpath, "clojure.main", task.script},
						task.inp, timeout)
					if runErr != "" {
						results <- result{task.label, task.case_, runErr, 0, 0}
						continue
					}
					results <- result{task.label, task.case_, compareOutput(stdout, task.outp), insns, rssKb}
					continue
				}
				results <- result{task.label, task.case_, runTestCase(classpath, task, timeout), 0, 0}
			}
		}()
	}
	go func() {
		for _, task := range tasks {
			work <- task
		}
		close(work)
	}()
	var failures []string
	passed, done := 0, 0
	var totalInsns, peakRSS int64
	for i := 0; i < len(tasks); i++ {
		res := <-results
		done++
		if res.error == "" {
			passed++
			if pinRequested {
				totalInsns += res.insns
				if res.rssKb > peakRSS {
					peakRSS = res.rssKb
				}
				fmt.Printf("PASS %s/%s [%d/%d] insn=%d rss=%dK\n", res.label, res.case_, done, total, res.insns, res.rssKb)
			} else {
				fmt.Printf("PASS %s/%s [%d/%d]\n", res.label, res.case_, done, total)
			}
		} else {
			failures = append(failures, fmt.Sprintf("%s/%s: %s", res.label, res.case_, res.error))
			fmt.Printf("FAIL %s/%s [%d/%d]: %s\n", res.label, res.case_, done, total, res.error)
		}
	}
	workers.Wait()
	close(results)
	for _, label := range missing {
		failures = append(failures, label+": no resources found")
	}
	for _, failure := range sortedStrings(failures) {
		fmt.Println("FAIL", failure)
	}
	fmt.Printf("clojure tests: %d passed, %d failed, %d missing-resource problems\n",
		passed, len(failures)-len(missing), len(missing))
	if pinRequested {
		fmt.Printf("instrumentation: %d cases, total_insn=%d peak_rss=%dK\n", passed, totalInsns, peakRSS)
	}
	if len(failures) > 0 {
		return 1
	}
	return 0
}

func runTestCase(classpath string, task testInstrumentationTask, timeout int) string {
	input, err := os.ReadFile(task.inp)
	if err != nil {
		return "open input: " + err.Error()
	}
	stdout, stderr, err := snipcore.Run(
		[]string{"java", "-cp", classpath, "clojure.main", task.script},
		input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	return compareOutput(stdout, task.outp)
}

func sortedStrings(in []string) []string {
	out := append([]string(nil), in...)
	sort.Strings(out)
	return out
}

func compareOutput(stdout []byte, outp string) string {
	expected, err := os.ReadFile(outp)
	if err != nil {
		return "open expected: " + err.Error()
	}
	normGot := snipcore.Normalize(stdout)
	normWant := snipcore.Normalize(expected)
	if string(normGot) != string(normWant) {
		got := snipcore.TruncateChars(strings.ToValidUTF8(string(normGot), ""), 200)
		want := snipcore.TruncateChars(strings.ToValidUTF8(string(normWant), ""), 200)
		return "output mismatch: expected " + snipcore.PyRepr(want) + " got " + snipcore.PyRepr(got)
	}
	return ""
}
