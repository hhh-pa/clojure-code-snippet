package measure

import (
	"os/exec"
	"runtime"
	"testing"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// TestAllocShimLive builds the counting shim and runs a shell under
// it: stdout must pass through intact and the total must parse.
// Linux-only (LD_PRELOAD/glibc); skipped elsewhere like the
// grandchildren test skips without /proc.
func TestAllocShimLive(t *testing.T) {
	if runtime.GOOS != "linux" {
		t.Skip("alloc shim needs Linux (LD_PRELOAD)")
	}
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skip("needs cc to build the shim")
	}
	so, err := EnsureAllocShim()
	if err != nil {
		t.Fatalf("build shim: %v", err)
	}
	stdout, stderr, err := snipcore.RunEnv(
		[]string{"sh", "-c", "echo hi"}, nil, 60,
		[]string{"LD_PRELOAD=" + so})
	if err != nil {
		t.Fatalf("run under shim: %v\n%s", err, stderr)
	}
	if string(stdout) != "hi\n" {
		t.Fatalf("stdout mangled: %q", stdout)
	}
	total, err := ParseAllocShimTotal(string(stderr))
	if err != nil {
		t.Fatalf("parse total: %v\n%s", err, stderr)
	}
	if total <= 0 {
		t.Fatalf("non-positive total: %d", total)
	}
}

func TestInnerIsJava(t *testing.T) {
	if !InnerIsJava([]string{"java", "-cp", "x"}) {
		t.Fatal("bare java not detected")
	}
	if !InnerIsJava([]string{"/usr/lib/jvm/bin/java", "Foo"}) {
		t.Fatal("java path not detected")
	}
	if InnerIsJava([]string{"clojure-rs", "run", "s.clj"}) {
		t.Fatal("native binary misdetected as java")
	}
	if InnerIsJava(nil) || InnerIsJava([]string{}) {
		t.Fatal("empty argv must not be java")
	}
}

func TestTemplateHeadIsJava(t *testing.T) {
	if !TemplateHeadIsJava(DefaultCommandTemplate) {
		t.Fatal("default template must be java")
	}
	if TemplateHeadIsJava("clojure-rs run {script}") {
		t.Fatal("native template misdetected as java")
	}
	if TemplateHeadIsJava("") {
		t.Fatal("empty template must not be java")
	}
}
