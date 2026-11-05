package main

import (
	"errors"
	"testing"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/measure"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/slope"
)

func TestParseFlags(t *testing.T) {
	var s, cp string
	jobs := 4
	perf := true
	pos, err := parseFlags("test-instrumentation", []string{"--resources", "r", "--cp=x.jar", "--jobs", "2", "--no-perf", "a", "b"},
		flagSpec{
			Strs:  map[string]*string{"resources": &s, "cp": &cp},
			Ints:  map[string]*int{"jobs": &jobs},
			Bools: map[string]*bool{"perf": &perf},
		})
	if err != nil || s != "r" || cp != "x.jar" || jobs != 2 || perf {
		t.Fatalf("parse: %q %v", pos, err)
	}
	if len(pos) != 2 || pos[0] != "a" || pos[1] != "b" {
		t.Fatalf("positionals: %q", pos)
	}
	single := ""
	if _, err := parseFlags("t", []string{"-resources", "v"}, flagSpec{Strs: map[string]*string{"resources": &single}}); err != nil || single != "v" {
		t.Fatalf("single dash: %v", err)
	}
	merge := false
	if _, err := parseFlags("m", []string{"--merge"}, flagSpec{Bools: map[string]*bool{"merge": &merge}}); err != nil || !merge {
		t.Fatalf("bool: %v", err)
	}
	if _, err := parseFlags("t", []string{"--bogus"}, flagSpec{}); err == nil {
		t.Fatal("unknown flag accepted")
	}
	n := 0
	if _, err := parseFlags("t", []string{"--jobs", "x"}, flagSpec{Ints: map[string]*int{"jobs": &n}}); err == nil {
		t.Fatal("bad int accepted")
	}
	if _, err := parseFlags("t", []string{"--jobs"}, flagSpec{Ints: map[string]*int{"jobs": &n}}); err == nil {
		t.Fatal("missing value accepted")
	}
	if _, err := parseFlags("t", []string{"--merge=x"}, flagSpec{Bools: map[string]*bool{"merge": &merge}}); err == nil {
		t.Fatal("bool value accepted")
	}
	if _, err := parseFlags("t", []string{"-h"}, flagSpec{}); !errors.Is(err, errHelp) {
		t.Fatalf("help: %v", err)
	}
}

func TestDispatch(t *testing.T) {
	if code := dispatch([]string{"bogus"}); code != 2 {
		t.Fatalf("unknown subcommand: %d", code)
	}
	if code := dispatch(nil); code != 2 {
		t.Fatalf("no args: %d", code)
	}
}

func TestFormatCaseProgress(t *testing.T) {
	ok := measure.CaseResult{Label: "sort/bubble_sort", Stem: "solution", Case: "test_case_1", Ok: true}
	if got := formatCaseProgress(7, 480, ok); got != "measure [7/480] sort/bubble_sort/solution/test_case_1: ok" {
		t.Fatalf("ok line: %q", got)
	}
	msg := "sort/bubble_sort/test_case_2: TIMEOUT (>120s)"
	bad := measure.CaseResult{Label: "sort/bubble_sort", Stem: "solution", Case: "test_case_2", Error: &msg}
	if got := formatCaseProgress(8, 480, bad); got != "measure [8/480] sort/bubble_sort/solution/test_case_2: "+msg {
		t.Fatalf("error line: %q", got)
	}
	skip := "TIMEOUT (>900s)"
	skipped := measure.CaseResult{Label: "sort/bubble_sort", Stem: "solution", Case: "test_case_1", Ok: true, CallgrindSkipped: &skip}
	if got := formatCaseProgress(9, 480, skipped); got != "measure [9/480] sort/bubble_sort/solution/test_case_1: ok (callgrind skipped: TIMEOUT (>900s))" {
		t.Fatalf("skipped line: %q", got)
	}
}

func TestFormatSlopeProgress(t *testing.T) {
	num := func(f float64) *pyjson.Num { n := pyjson.FloatNum(f); return &n }
	fit := &slope.Fit{Ok: true, Slope: num(2.5), Lower: num(2.4), Upper: num(2.6),
		RSquared: num(0.99), FloorC: num(0.11)}
	entry := slope.Entry{Scales: []slope.Digits{"10", "20"}, Ok: true, Fit: fit}
	want := "slope [3/90] sort/bubble_sort/solution: p=2.500 [2.400,2.600] R^2=0.9900 floor=0.11s n=[10, 20]"
	if got := formatSlopeProgress(3, 90, "sort/bubble_sort/solution", entry); got != want {
		t.Fatalf("fit line: %q", got)
	}
	msg := "boom"
	skip := slope.Entry{Error: &msg}
	if got := formatSlopeProgress(4, 90, "a/b", skip); got != "slope [4/90] a/b: SKIPPED boom" {
		t.Fatalf("skipped line: %q", got)
	}
}
