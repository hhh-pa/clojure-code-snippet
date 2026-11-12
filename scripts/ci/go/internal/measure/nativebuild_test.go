package measure

import (
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// fakeCompiler writes a shell "compiler" that logs each invocation
// to <dir>/calls and writes an executable echoing the script's first
// line to {out}; a script containing FAIL makes it exit 3.
func fakeCompiler(t *testing.T, dir string) string {
	t.Helper()
	path := filepath.Join(dir, "fakec")
	body := "#!/bin/sh\n" +
		"echo \"$1\" >> " + filepath.Join(dir, "calls") + "\n" +
		"if grep -q FAIL \"$1\"; then echo 'boom: unsupported' >&2; exit 3; fi\n" +
		"printf '#!/bin/sh\\nhead -n1 %s\\n' \"$1\" > \"$2\"\n" +
		"chmod +x \"$2\"\n"
	if err := os.WriteFile(path, []byte(body), 0o755); err != nil {
		t.Fatal(err)
	}
	return path
}

func calls(t *testing.T, dir string) int {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(dir, "calls"))
	if os.IsNotExist(err) {
		return 0
	}
	if err != nil {
		t.Fatal(err)
	}
	return strings.Count(string(raw), "\n")
}

func TestExpandBuildTemplate(t *testing.T) {
	argv, err := ExpandBuildTemplate("cc build {script} -o '{out}' --name {stem}", "a b.clj", "s", "o")
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"cc", "build", "a", "b.clj", "-o", "o", "--name", "s"}
	if strings.Join(argv, "|") != strings.Join(want, "|") {
		t.Fatalf("argv = %q", argv)
	}
	for _, bad := range []string{"cc {cp}", "", "cc {", "cc '"} {
		if _, err := ExpandBuildTemplate(bad, "x", "y", "z"); err == nil ||
			!strings.HasPrefix(err.Error(), "bad build template") {
			t.Errorf("%q: err = %v", bad, err)
		}
	}
}

func TestNativeFingerprint(t *testing.T) {
	dir := t.TempDir()
	compiler := fakeCompiler(t, dir)
	path, sum, err := NativeFingerprint(compiler + " {script} -o {out}")
	if err != nil || path != compiler || len(sum) != 64 {
		t.Fatalf("path=%q sum=%q err=%v", path, sum, err)
	}
	if _, _, err := NativeFingerprint("definitely-not-a-compiler-66 {script}"); err == nil {
		t.Fatal("missing compiler must fail")
	}
	if _, _, err := NativeFingerprint("{nope}"); err == nil {
		t.Fatal("bad template must fail")
	}
	if _, _, err := NativeFingerprint(dir + " {script}"); err == nil {
		t.Fatal("a directory is not a compiler")
	}
}

func TestNativeBuildCacheBuildsOnceAndReuses(t *testing.T) {
	dir := t.TempDir()
	compiler := fakeCompiler(t, dir)
	script := filepath.Join(dir, "solution.clj")
	if err := os.WriteFile(script, []byte(";; hello\n(println 1)\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	newCache := func(fp string) *NativeBuildCache {
		return &NativeBuildCache{
			Template: compiler + " {script} {out}", CacheDir: filepath.Join(dir, "cache"),
			Fingerprint: fp, Timeout: 30,
		}
	}
	c := newCache("fp1")
	var wg sync.WaitGroup
	bins := make([]string, 8)
	for i := range bins {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			bin, err := c.Get("sort/x", "solution", script)
			if err != nil {
				t.Error(err)
			}
			bins[i] = bin
		}(i)
	}
	wg.Wait()
	if calls(t, dir) != 1 {
		t.Fatalf("concurrent Gets built %d times", calls(t, dir))
	}
	for _, bin := range bins[1:] {
		if bin != bins[0] || !filepath.IsAbs(bin) {
			t.Fatalf("bins differ or relative: %q", bins)
		}
	}
	out, _, err := runQuiet([]string{bins[0]})
	if err != nil || strings.TrimSpace(out) != ";; hello" {
		t.Fatalf("binary ran %q, %v", out, err)
	}
	// A fresh cache (new process) reuses the slot through its marker.
	if _, err := newCache("fp1").Get("sort/x", "solution", script); err != nil || calls(t, dir) != 1 {
		t.Fatalf("marker hit rebuilt: calls=%d err=%v", calls(t, dir), err)
	}
	// A new compiler fingerprint, label, or stem is a new slot.
	if _, err := newCache("fp2").Get("sort/x", "solution", script); err != nil || calls(t, dir) != 2 {
		t.Fatalf("fingerprint change: calls=%d err=%v", calls(t, dir), err)
	}
	if _, err := newCache("fp2").Get("sort/x", "twin", script); err != nil || calls(t, dir) != 3 {
		t.Fatalf("stem change: calls=%d err=%v", calls(t, dir), err)
	}
	// A slot whose binary vanished behind a valid marker rebuilds.
	if err := os.Remove(bins[0]); err != nil {
		t.Fatal(err)
	}
	if _, err := newCache("fp1").Get("sort/x", "solution", script); err != nil || calls(t, dir) != 4 {
		t.Fatalf("missing binary: calls=%d err=%v", calls(t, dir), err)
	}
}

func TestNativeBuildCacheFailuresAreSticky(t *testing.T) {
	dir := t.TempDir()
	compiler := fakeCompiler(t, dir)
	script := filepath.Join(dir, "bad.clj")
	if err := os.WriteFile(script, []byte("FAIL\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	c := &NativeBuildCache{Template: compiler + " {script} {out}", CacheDir: filepath.Join(dir, "cache"), Timeout: 30}
	_, err := c.Get("a/b", "bad", script)
	if err == nil || err.Error() != "build exit 3: boom: unsupported" {
		t.Fatalf("err = %v", err)
	}
	if _, err2 := c.Get("a/b", "bad", script); err2 == nil || calls(t, dir) != 1 {
		t.Fatalf("failure not cached: calls=%d", calls(t, dir))
	}
	if _, err := c.Get("a/b", "gone", filepath.Join(dir, "missing.clj")); err == nil {
		t.Fatal("unreadable script must fail")
	}
	sleeper := filepath.Join(dir, "sleeper")
	if err := os.WriteFile(sleeper, []byte("#!/bin/sh\nsleep 5\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	slow := &NativeBuildCache{Template: sleeper + " {script} {out}", CacheDir: filepath.Join(dir, "c2"), Timeout: 1}
	if _, err := slow.Get("a/b", "slow", script); err == nil || err.Error() != "build TIMEOUT (>1s)" {
		t.Fatalf("timeout err = %v", err)
	}
}

func TestCaseInnerCommandUsesNativeBinary(t *testing.T) {
	dir := t.TempDir()
	compiler := fakeCompiler(t, dir)
	script := filepath.Join(dir, "s.clj")
	if err := os.WriteFile(script, []byte("x\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	o := CaseOpts{Label: "a/b", Stem: "s", Script: script, Native: &NativeBuildCache{
		Template: compiler + " {script} {out}", CacheDir: filepath.Join(dir, "cache"), Timeout: 30,
	}}
	argv, err := o.innerCommand()
	if err != nil || len(argv) != 1 || filepath.Base(argv[0]) != "s" {
		t.Fatalf("argv=%q err=%v", argv, err)
	}
	if lastLines([]byte("a\n\n b \nc\nd\n"), 2) != "c | d" {
		t.Fatal("lastLines keeps the tail")
	}
}

// runQuiet runs argv and returns stdout.
func runQuiet(argv []string) (string, string, error) {
	out, errb, err := runArgv(argv)
	return string(out), string(errb), err
}

func runArgv(argv []string) ([]byte, []byte, error) {
	return snipcore.Run(argv, nil, 30)
}
