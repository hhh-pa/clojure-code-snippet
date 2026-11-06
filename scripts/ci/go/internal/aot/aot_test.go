package aot

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

func repoRoot(t *testing.T) string {
	t.Helper()
	root, err := filepath.Abs(filepath.Join("..", "..", "..", "..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	return root
}

func TestTransformBubbleSort(t *testing.T) {
	root := repoRoot(t)
	tr, err := Transform(
		filepath.Join(root, "src", "sort", "bubble_sort", "solution.clj"),
		"sort/bubble_sort", "solution")
	if err != nil {
		t.Fatal(err)
	}
	if tr.NS != "snip_sort_bubble_sort_solution" {
		t.Fatalf("ns = %q", tr.NS)
	}
	if tr.Class != tr.NS {
		t.Fatalf("class = %q", tr.Class)
	}
	for _, want := range []string{
		"(ns snip_sort_bubble_sort_solution (:gen-class :main true))",
		"(defn script-main",
		"(script-main (slurp *in*))",
		"(defn -aot-run []",
		"(defn -main [& _args]\n  (-aot-run))",
	} {
		if !strings.Contains(tr.Source, want) {
			t.Fatalf("source lacks %q", want)
		}
	}
	if strings.Contains(tr.Source, "(defn -main [input]") {
		t.Fatal("script -main was not renamed")
	}
}

func TestTransformNsFile(t *testing.T) {
	root := repoRoot(t)
	tr, err := Transform(
		filepath.Join(root, "src", "computational_geometry", "convex_hull", "convex_hull.clj"),
		"computational_geometry/convex_hull", "convex_hull")
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(tr.Source, "(:gen-class :main true)") {
		t.Fatal("gen-class not injected")
	}
	if strings.Count(tr.Source, "(ns ") != 1 {
		t.Fatal("ns form duplicated or lost")
	}
	if tr.Class == "" || strings.Contains(tr.Class, "-") {
		t.Fatalf("class not munged: %q", tr.Class)
	}
}

func TestTransformNoMain(t *testing.T) {
	root := repoRoot(t)
	tr, err := Transform(
		filepath.Join(root, "src", "data_structure", "bloom_filter", "solution.clj"),
		"data_structure/bloom_filter", "solution")
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(tr.Source, "script-main") {
		t.Fatal("spurious rename without script -main")
	}
	if !strings.Contains(tr.Source, "(let [toks (tokenize (slurp *in*))]") {
		t.Fatal("driver let not moved into -aot-run")
	}
}

// TestTransformRepo proves every snippet transforms (fast, no JVM).
func TestTransformRepo(t *testing.T) {
	root := repoRoot(t)
	found, err := snipcore.Discover(root, filepath.Join(root, "tests", "resources"), ".clj")
	if err != nil {
		t.Fatal(err)
	}
	if len(found) == 0 {
		t.Fatal("no snippets discovered")
	}
	for _, entry := range found {
		label := entry.Area + "/" + entry.Problem
		if _, err := Transform(entry.Script, label, entry.Stem); err != nil {
			t.Fatalf("%s: %v", label, err)
		}
	}
	// Shared ns names across scripts are fine: every snippet
	// compiles into its own content-hash slot directory.
	t.Logf("transformed %d snippets", len(found))
}

func TestMunge(t *testing.T) {
	if got := Munge("and-or-search"); got != "and_or_search" {
		t.Fatalf("munge = %q", got)
	}
	if got := Munge("foo.bar-baz"); got != "foo.bar_baz" {
		t.Fatalf("dotted munge = %q", got)
	}
}

// TestLiveAOT compiles pilots and proves AOT stdout equals script
// stdout. Slow (JVM boots); runs only with SNIP_LIVE=1.
func TestLiveAOT(t *testing.T) {
	if os.Getenv("SNIP_LIVE") == "" {
		t.Skip("set SNIP_LIVE=1 for JVM tests")
	}
	root := repoRoot(t)
	cp, err := snipcore.ResolveClasspath(root, "")
	if err != nil {
		t.Fatal(err)
	}
	cache := t.TempDir()
	pilots := []struct {
		label, stem, script, stdin string
	}{
		{"sort/bubble_sort", "solution", "src/sort/bubble_sort/solution.clj", "5\n5 4 3 2 1\n"},
		{"computational_geometry/convex_hull", "convex_hull",
			"src/computational_geometry/convex_hull/convex_hull.clj", "4\n0 0\n1 0\n1 1\n0 1\n"},
		{"data_structure/bloom_filter", "solution",
			"src/data_structure/bloom_filter/solution.clj", "10 2 2 2 1 2 1 3"},
	}
	for _, p := range pilots {
		script := filepath.Join(root, p.script)
		want, stderr, err := snipcore.Run(
			[]string{"java", "-cp", cp, "clojure.main", script},
			[]byte(p.stdin), 300)
		if err != nil {
			t.Fatalf("%s script: %v %s", p.label, err, stderr)
		}
		built, err := Compile(root, cache, cp, "live-test", p.label, p.stem, script, 300)
		if err != nil {
			t.Fatalf("%s compile: %v", p.label, err)
		}
		if built.Cached {
			t.Fatalf("%s: unexpected cache hit on first build", p.label)
		}
		got, stderr, err := snipcore.Run(built.Argv(cp), []byte(p.stdin), 300)
		if err != nil {
			t.Fatalf("%s aot run: %v %s", p.label, err, stderr)
		}
		if string(got) != string(want) {
			t.Fatalf("%s: aot %q != script %q", p.label, got, want)
		}
		again, err := Compile(root, cache, cp, "live-test", p.label, p.stem, script, 300)
		if err != nil {
			t.Fatalf("%s recompile: %v", p.label, err)
		}
		if !again.Cached {
			t.Fatalf("%s: expected cache hit on rebuild", p.label)
		}
	}
}
