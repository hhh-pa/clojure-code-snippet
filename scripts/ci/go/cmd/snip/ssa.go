package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

const ssaUsage = `Focused regression test: ssa_dominance stdout contract (issue #28).

Pins the exact idom/phi-block output of src/compiler/ssa_dominance.clj on
loopy graphs, including the reference's order-dependent single-pass DF
quirk. Comparison ignores trailing whitespace per line, same as snip test.

Usage: snip ssa [--cp CLASSPATH] [--timeout S]
`

type ssaCase struct {
	name     string
	stdin    string
	expected string
}

// ssaCases ports the CASES table of tests/ssa_df_regression.py verbatim.
var ssaCases = []ssaCase{
	{"loop-head-def",
		"4 4 1 2 2 3 3 4 4 2 1 2",
		"idom(2)=1 idom(3)=2 idom(4)=3\nNONE\n"},
	{"nested-loops-def",
		"5 6 1 2 2 3 3 4 4 2 4 5 5 3 1 2",
		"idom(2)=1 idom(3)=2 idom(4)=3 idom(5)=4\nNONE\n"},
	{"back-to-entry-def",
		"3 3 1 2 2 3 3 1 1 1",
		"idom(2)=1 idom(3)=2\nNONE\n"},
	{"entry-in-own-frontier",
		"2 2 1 2 2 1 1 1",
		"idom(2)=1\n1\n"},
	{"self-loop-def",
		"2 2 1 2 2 2 1 2",
		"idom(2)=1\n2\n"},
	{"diamond-unreachable-artifact",
		"5 4 1 2 1 3 2 4 3 4 2 2 3",
		"idom(2)=1 idom(3)=1 idom(4)=1 idom(5)=0\n4\n"},
	{"two-back-def",
		"4 5 1 2 2 3 3 4 4 2 3 2 1 3",
		"idom(2)=1 idom(3)=2 idom(4)=3\n2\n"},
}

func runSSA(argv []string) int {
	root := snipcore.RepoRoot()
	var cp string
	timeout := 120
	_, err := parseFlags("ssa", argv, flagSpec{
		Strs: map[string]*string{"cp": &cp},
		Ints: map[string]*int{"timeout": &timeout},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(ssaUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	classpath, err := snipcore.ResolveClasspath(root, cp)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	script := filepath.Join(root, "src", "compiler", "ssa_dominance.clj")
	var failures []string
	for _, c := range ssaCases {
		if caseErr := runSSACase(classpath, script, c, timeout); caseErr == "" {
			fmt.Printf("PASS %s\n", c.name)
		} else {
			failures = append(failures, c.name+": "+caseErr)
		}
	}
	for _, failure := range failures {
		fmt.Println("FAIL", failure)
	}
	fmt.Printf("ssa_df_regression: %d passed, %d failed\n", len(ssaCases)-len(failures), len(failures))
	if len(failures) > 0 {
		return 1
	}
	return 0
}

func runSSACase(classpath, script string, c ssaCase, timeout int) string {
	stdout, stderr, err := snipcore.Run(
		[]string{"java", "-cp", classpath, "clojure.main", script},
		[]byte(c.stdin), timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	if string(snipcore.Normalize(stdout)) != string(snipcore.Normalize([]byte(c.expected))) {
		return "output mismatch: expected " +
			snipcore.PyRepr(snipcore.TruncateChars(c.expected, 160)) + " got " +
			snipcore.PyRepr(snipcore.TruncateChars(strings.ToValidUTF8(string(stdout), ""), 160))
	}
	return ""
}
