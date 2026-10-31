package cljread

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestSplitBasic(t *testing.T) {
	src := ";; header\n\n(def x 1) ; trailing\n\n(defn f [a] (+ a 1))\n(f x)\n"
	forms, err := Split(src)
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"(def x 1)", "(defn f [a] (+ a 1))", "(f x)"}
	if len(forms) != len(want) {
		t.Fatalf("forms = %q", forms)
	}
	for i := range want {
		if forms[i] != want[i] {
			t.Fatalf("form %d = %q, want %q", i, forms[i], want[i])
		}
	}
}

func TestSplitTricky(t *testing.T) {
	src := "(def s \"a)b\\\";(c\")\n" +
		"(def ch \\)) ; paren char\n" +
		"(def v [1 {2 (3)}])\n" +
		"#_ (dead code)\n" +
		"(live 1)\n"
	forms, err := Split(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(forms) != 5 {
		t.Fatalf("forms = %q", forms)
	}
	if forms[0] != "(def s \"a)b\\\";(c\")" {
		t.Fatalf("string form = %q", forms[0])
	}
	if forms[1] != "(def ch \\))" {
		t.Fatalf("char form = %q", forms[1])
	}
	if forms[3] != "#_ (dead code)" {
		t.Fatalf("discard form = %q", forms[3])
	}
}

func TestSplitRejects(t *testing.T) {
	for _, src := range []string{
		"(def x 1",           // unbalanced
		"(def s \"abc)",      // unterminated string
		";; only comments\n", // no forms
		"#=(evil)",           // read-eval
	} {
		if _, err := Split(src); err == nil {
			t.Fatalf("Split(%q) accepted", src)
		}
	}
}

func TestHead(t *testing.T) {
	cases := map[string]string{
		"(defn foo [x] x)":     "defn",
		"  (ns bar (:gen))":    "ns",
		"(-main (slurp *in*))": "-main",
		"(let [a 1] a)":        "let",
		"(clojure.set/union)":  "clojure.set/union",
		"\"literal\"":          "",
		"( :keyword-position)": "",
		"42":                   "",
	}
	for form, want := range cases {
		if got := Head(form); got != want {
			t.Fatalf("Head(%q) = %q, want %q", form, got, want)
		}
	}
}

// TestSplitRepo proves the splitter accepts every snippet and keeps
// the driver call last on the bubble_sort pilot.
func TestSplitRepo(t *testing.T) {
	root := filepath.Join("..", "..", "..", "..", "..")
	count := 0
	err := filepath.Walk(filepath.Join(root, "src"), func(path string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() || !strings.HasSuffix(path, ".clj") {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		forms, err := Split(string(raw))
		if err != nil {
			t.Fatalf("%s: %v", path, err)
		}
		if len(forms) == 0 {
			t.Fatalf("%s: no forms", path)
		}
		count++
		if strings.HasSuffix(path, "bubble_sort/solution.clj") {
			last := forms[len(forms)-1]
			if last != "(-main (slurp *in*))" {
				t.Fatalf("bubble last form = %q", last)
			}
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if count == 0 {
		t.Fatal("no snippets found")
	}
	t.Logf("split %d snippets", count)
}
