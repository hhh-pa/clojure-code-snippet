package snipcore

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

func TestNormalize(t *testing.T) {
	cases := map[string]string{
		"a\n":                "a",
		"a\n\n":              "a\n",
		"":                   "",
		"a\r\nb\r\n":         "a\nb",
		"  a  \n\tb\t":       "a\nb",
		"a\rb":               "a\nb",
		"a\x0bb":             "a\x0bb",
		"a\x0cb":             "a\x0cb",
		"x\ny\n":             "x\ny",
		"  \n":               "",
		"\n":                 "",
		"a\n\n\n":            "a\n\n",
		"héllo  \n  wörld\t": "héllo\nwörld",
		"a\x1cb":             "a\x1cb",
		"\x85 x \x85":        "\x85 x \x85",
	}
	for in, want := range cases {
		if got := string(Normalize([]byte(in))); got != want {
			t.Fatalf("Normalize(%q) = %q, want %q", in, got, want)
		}
	}
}

func TestParseTestShard(t *testing.T) {
	idx, total, err := ParseTestShard("2/4")
	if err != nil || idx != 1 || total != 4 {
		t.Fatalf("ParseTestShard(2/4) = %d,%d,%v", idx, total, err)
	}
	for _, bad := range []string{"5/4", "0/4", "1/0", "x", "1/2/3", "", "1/"} {
		if _, _, err := ParseTestShard(bad); err == nil {
			t.Fatalf("ParseTestShard(%q) accepted", bad)
		}
	}
	if _, _, err := ParseTestShard("x"); err == nil || err.Error() != "bad --shard 'x': want I/N" {
		t.Fatalf("test shard message: %v", err)
	}
	if _, _, err := ParseTestShard("0/4"); err == nil || err.Error() != "bad --shard '0/4': want 1 <= I <= N" {
		t.Fatalf("test shard range message: %v", err)
	}
}

func TestParseMeasureShard(t *testing.T) {
	idx, total, err := ParseMeasureShard("1/4")
	if err != nil || idx != 1 || total != 4 {
		t.Fatalf("ParseMeasureShard(1/4) = %d,%d,%v", idx, total, err)
	}
	for _, bad := range []string{"4/4", "-1/4", "0/0", "x", "1/2/3", ""} {
		if _, _, err := ParseMeasureShard(bad); err == nil {
			t.Fatalf("ParseMeasureShard(%q) accepted", bad)
		}
	}
	if _, _, err := ParseMeasureShard("x"); err == nil || err.Error() != "bad --shard 'x': want K/N" {
		t.Fatalf("measure shard message: %v", err)
	}
	if _, _, err := ParseMeasureShard("4/4"); err == nil || err.Error() != "bad --shard '4/4': need 0 <= K < N" {
		t.Fatalf("measure shard range message: %v", err)
	}
}

func TestResolveClasspath(t *testing.T) {
	root := t.TempDir()
	jars := filepath.Join(root, ".ci", "jars")
	if err := os.MkdirAll(jars, 0o755); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"clojure.jar", "spec.alpha.jar"} {
		if err := os.WriteFile(filepath.Join(jars, name), []byte("x"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	t.Setenv("CLOJURE_CP", "")
	t.Setenv("CLOJURE_JAR", "")
	got, err := ResolveClasspath(root, "/explicit/cp.jar")
	if err != nil || got != "/explicit/cp.jar" {
		t.Fatalf("explicit: %q %v", got, err)
	}
	t.Setenv("CLOJURE_CP", " /env/cp.jar ")
	got, err = ResolveClasspath(root, "")
	if err != nil || got != "/env/cp.jar" {
		t.Fatalf("env cp: %q %v", got, err)
	}
	t.Setenv("CLOJURE_CP", "")
	got, err = ResolveClasspath(root, "")
	want := strings.Join([]string{filepath.Join(jars, "clojure.jar"), filepath.Join(jars, "spec.alpha.jar")}, ":")
	if err != nil || got != want {
		t.Fatalf("jars dir: %q want %q (%v)", got, want, err)
	}
	other := t.TempDir()
	main := filepath.Join(other, "c.jar")
	os.WriteFile(main, []byte("x"), 0o644)
	os.WriteFile(filepath.Join(other, "spec.alpha-1.jar"), []byte("x"), 0o644)
	os.WriteFile(filepath.Join(other, "core.specs-2.jar"), []byte("x"), 0o644)
	t.Setenv("CLOJURE_JAR", main)
	got, err = ResolveClasspath(t.TempDir(), "")
	want = strings.Join([]string{main, filepath.Join(other, "spec.alpha-1.jar"), filepath.Join(other, "core.specs-2.jar")}, ":")
	if err != nil || got != want {
		t.Fatalf("jar siblings: %q want %q (%v)", got, want, err)
	}
	t.Setenv("CLOJURE_JAR", "")
	if _, err := ResolveClasspath(t.TempDir(), ""); err == nil {
		t.Fatal("expected fatal with no classpath source")
	}
}

func TestTemplateNeedsClasspath(t *testing.T) {
	for _, tc := range []struct {
		template string
		want     bool
	}{
		{"java -cp {cp} clojure.main {script}", true},
		{"clojure-rs run {script}", false},
		{"run {script} {stem}", false},
		{"echo {{cp}}", false},
		{"echo {{cp}} {cp}", true},
		{"{cpx}", false},
		{"{", false},
		{"", false},
	} {
		if got := TemplateNeedsClasspath(tc.template); got != tc.want {
			t.Fatalf("TemplateNeedsClasspath(%q) = %v, want %v", tc.template, got, tc.want)
		}
	}
}

func TestDiscover(t *testing.T) {
	root := t.TempDir()
	write := func(path, body string) {
		full := filepath.Join(root, path)
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(full, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	write("src/sort/quick_sort/solution.clj", "(println 1)")
	write("src/sort/bubble_sort/solution.clj", "(println 2)")
	write("src/compiler/lonely.clj", "(println 3)")
	write("src/not-code.txt", "skip me")
	res := filepath.Join(root, "res")
	write("res/sort/quick_sort/solution/test_case_1/input.txt", "in")
	write("res/sort/quick_sort/solution/test_case_1/output.txt", "out")
	write("res/sort/quick_sort/solution/test_case_2/input.txt", "in")
	write("res/sort/quick_sort/solution/test_case_2/output.txt", "out")
	write("res/sort/quick_sort/solution/stray.txt", "not a case")
	found, err := Discover(root, res)
	if err != nil {
		t.Fatal(err)
	}
	if len(found) != 3 {
		t.Fatalf("found %d scripts, want 3", len(found))
	}
	// Sorted by path: compiler/lonely, bubble_sort, quick_sort.
	if found[0].Label != "compiler/lonely" || found[0].Stem != "lonely" {
		t.Fatalf("lonely: %+v", found[0])
	}
	if len(found[0].Cases) != 0 {
		t.Fatalf("lonely cases: %+v", found[0].Cases)
	}
	if found[1].Label != "sort/bubble_sort" || found[1].Stem != "solution" {
		t.Fatalf("bubble: %+v", found[1])
	}
	q := found[2]
	if q.Label != "sort/quick_sort" || len(q.Cases) != 2 {
		t.Fatalf("quick: %+v", q)
	}
	if q.Cases[0].Name != "test_case_1" || q.Cases[1].Name != "test_case_2" {
		t.Fatalf("quick cases: %+v", q.Cases)
	}
	if !strings.HasSuffix(q.Cases[0].Input, "input.txt") || !strings.HasSuffix(q.Cases[0].Output, "output.txt") {
		t.Fatalf("quick case paths: %+v", q.Cases[0])
	}
}

func TestRun(t *testing.T) {
	stdout, stderr, err := Run([]string{"sh", "-c", "echo out; echo err >&2"}, nil, 30)
	if err != nil || string(stdout) != "out\n" || string(stderr) != "err\n" {
		t.Fatalf("run: %q %q %v", stdout, stderr, err)
	}
	_, _, err = Run([]string{"sh", "-c", "exit 3"}, nil, 30)
	if err == nil || ExitCode(err) != 3 {
		t.Fatalf("exit code: %v", err)
	}
	if ExitCode(nil) != 0 {
		t.Fatal("nil exit code")
	}
	stdout, _, err = Run([]string{"sh", "-c", "cat"}, []byte("hello"), 30)
	if err != nil || string(stdout) != "hello" {
		t.Fatalf("stdin: %q %v", stdout, err)
	}
	_, _, err = Run([]string{"sh", "-c", "sleep 30"}, nil, 1)
	var timeout *TimeoutError
	if err == nil || !errors.As(err, &timeout) || timeout.Seconds != 1 {
		t.Fatalf("timeout: %v", err)
	}
}

func TestRunEnv(t *testing.T) {
	stdout, _, err := RunEnv([]string{"sh", "-c", "echo $SNIP_PROBE"}, nil, 30, []string{"SNIP_PROBE=bar"})
	if err != nil || string(stdout) != "bar\n" {
		t.Fatalf("env passthrough: %q %v", stdout, err)
	}
	stdout, _, err = RunEnv([]string{"sh", "-c", "echo out"}, nil, 30, nil)
	if err != nil || string(stdout) != "out\n" {
		t.Fatalf("empty extra env: %q %v", stdout, err)
	}
}

func TestRunKillsGrandchildren(t *testing.T) {
	if _, err := os.Stat("/proc"); err != nil {
		t.Skip("needs /proc to hunt grandchildren")
	}
	sleepBin, err := exec.LookPath("sleep")
	if err != nil {
		t.Skip("needs sleep")
	}
	// A shell that spawns a background sleeper and waits: the
	// sleeper is a grandchild in the same process group and must
	// die with the timeout (the Python tools leak it). The
	// symlink gives the sleeper a cmdline unique to this test
	// run so stray same-named sleepers cannot pollute the hunt.
	marker := fmt.Sprintf("snip-probe-%d", os.Getpid())
	link := filepath.Join(t.TempDir(), marker)
	if err := os.Symlink(sleepBin, link); err != nil {
		t.Fatal(err)
	}
	_, _, err = Run([]string{"sh", "-c", link + " 61 & wait"}, nil, 1)
	var timeout *TimeoutError
	if err == nil || !errors.As(err, &timeout) {
		t.Fatalf("timeout: %v", err)
	}
	deadline := time.Now().Add(10 * time.Second)
	for {
		if !procHasCmdline(marker) {
			return
		}
		if time.Now().After(deadline) {
			t.Fatal("grandchild sleep survived the timeout")
		}
		time.Sleep(100 * time.Millisecond)
	}
}

func procHasCmdline(needle string) bool {
	entries, err := os.ReadDir("/proc")
	if err != nil {
		return false
	}
	for _, e := range entries {
		if _, err := strconv.Atoi(e.Name()); err != nil {
			continue
		}
		raw, err := os.ReadFile("/proc/" + e.Name() + "/cmdline")
		if err != nil {
			continue
		}
		cmdline := strings.ReplaceAll(string(raw), "\x00", " ")
		if strings.Contains(cmdline, needle) {
			return true
		}
	}
	return false
}

func TestPyRepr(t *testing.T) {
	cases := map[string]string{
		"hi":                      "'hi'",
		"a\nb\tc\rd\\e":           "'a\\nb\\tc\\rd\\\\e'",
		"it's":                    `"it's"`,
		`say "hi"`:                `'say "hi"'`,
		`both'and"`:               `'both\'and"'`,
		"":                        "''",
		"café \x00\x1f\x7f\u0080": "'café \\x00\\x1f\\x7f\\x80'",
		"tab\there":               "'tab\\there'",
		"\x0b\x0c":                "'\\x0b\\x0c'",
		"Ā":                       "'Ā'",
		"\U0001f600":              "'\U0001f600'",
	}
	for in, want := range cases {
		if got := PyRepr(in); got != want {
			t.Fatalf("PyRepr(%q) = %s, want %s", in, got, want)
		}
	}
}
