package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

const testUsage = `Self-contained correctness runner for the Clojure snippet repo.

For every solution script under src/<area>/<problem>/<file>.clj this runs
java -cp <classpath> clojure.main <file>.clj once per shared test case and
diff-compares stdout against the expected output (trailing whitespace per
line ignored).

Usage: snip test [--resources DIR] [--cp CP] [--jobs N] [--timeout S]
                 [--only SUBSTR] [--shard I/N]
`

type testTask struct {
	label  string
	case_  string
	script string
	inp    string
	outp   string
}

func runTest(argv []string) int {
	root := snipcore.RepoRoot()
	resources := filepath.Join(root, "tests", "resources")
	var cp, only, shard string
	jobs := 4
	timeout := 120
	_, err := parseFlags("test", argv, flagSpec{
		Strs: map[string]*string{"resources": &resources, "cp": &cp, "only": &only, "shard": &shard},
		Ints: map[string]*int{"jobs": &jobs, "timeout": &timeout},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(testUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
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
	var tasks []testTask
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
			tasks = append(tasks, testTask{label, c.Name, entry.Script, c.Input, c.Output})
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
		var kept []testTask
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

	if jobs < 1 {
		jobs = 1
	}
	type result struct {
		label string
		case_ string
		error string
	}
	work := make(chan testTask)
	results := make(chan result)
	var workers sync.WaitGroup
	for w := 0; w < jobs; w++ {
		workers.Add(1)
		go func() {
			defer workers.Done()
			for task := range work {
				results <- result{task.label, task.case_, runTestCase(classpath, task, timeout)}
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
	for i := 0; i < len(tasks); i++ {
		res := <-results
		done++
		if res.error == "" {
			passed++
			fmt.Printf("PASS %s/%s [%d/%d]\n", res.label, res.case_, done, total)
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
	if len(failures) > 0 {
		return 1
	}
	return 0
}

func runTestCase(classpath string, task testTask, timeout int) string {
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
		return fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), lastLine(stderr))
	}
	expected, err := os.ReadFile(task.outp)
	if err != nil {
		return "open expected: " + err.Error()
	}
	normGot := snipcore.Normalize(stdout)
	normWant := snipcore.Normalize(expected)
	if string(normGot) != string(normWant) {
		got := truncateChars(strings.ToValidUTF8(string(normGot), ""), 200)
		want := truncateChars(strings.ToValidUTF8(string(normWant), ""), 200)
		return "output mismatch: expected " + pyRepr(want) + " got " + pyRepr(got)
	}
	return ""
}

// lastLine ports the Python `tail[-1] if tail else "no stderr"` idiom.
func lastLine(stderr []byte) string {
	text := strings.TrimSpace(string(stderr))
	if text == "" {
		return "no stderr"
	}
	lines := strings.Split(text, "\n")
	return strings.TrimSpace(lines[len(lines)-1])
}

func truncateChars(text string, max int) string {
	runes := []rune(text)
	if len(runes) > max {
		return string(runes[:max])
	}
	return text
}

func sortedStrings(in []string) []string {
	out := append([]string(nil), in...)
	sort.Strings(out)
	return out
}
