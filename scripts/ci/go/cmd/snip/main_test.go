package main

import (
	"errors"
	"testing"
)

func TestParseFlags(t *testing.T) {
	var s, cp string
	jobs := 4
	perf := true
	pos, err := parseFlags("test", []string{"--resources", "r", "--cp=x.jar", "--jobs", "2", "--no-perf", "a", "b"},
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
