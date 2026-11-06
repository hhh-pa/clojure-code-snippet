// Compare two `snip measure` full-report JSONs case by case.
//
// `snip compare` is the performance-comparison leg of CI: the sweep
// measures a from-tree and a to-tree per shard and runtime, the
// report job merges each side, and compare matches the merged
// reports by (label, stem, case). Correspondence gaps are data,
// never errors: a case missing on one side gets null stats there
// ("stat missing"), since trees legitimately add and drop snippets
// across commits. Report-only: exit 0 whenever the comparison
// itself runs; only flag/IO errors exit nonzero.
package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/measure"
)

const compareUsage = `Compare two snip measure full-report JSONs.

Usage: snip compare --from FROM.json --to TO.json [--output F]
                   [--summary-file F] [--top N]
                   [--from-label L] [--to-label L]

Matches cases by (label, stem, case) and reports to/from ratios on
the timed signals (cpu_s min, wall_s median, maxrss_kb max). Cases
missing on a side keep null stats there. --top caps each summary
list (default 20); --from-label/--to-label name the sides in the
summary header (default "from"/"to").
`

func runCompare(argv []string) int {
	var fromPath, toPath, output, summaryFile, fromLabel, toLabel string
	fromLabel = "from"
	toLabel = "to"
	top := 20
	_, err := parseFlags("compare", argv, flagSpec{
		Strs: map[string]*string{
			"from": &fromPath, "to": &toPath,
			"output": &output, "summary-file": &summaryFile,
			"from-label": &fromLabel, "to-label": &toLabel,
		},
		Ints: map[string]*int{"top": &top},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(compareUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if fromPath == "" || toPath == "" {
		fmt.Fprintln(os.Stderr, "snip compare: --from and --to are required")
		return 2
	}
	if top < 1 {
		fmt.Fprintln(os.Stderr, "snip compare: --top must be >= 1")
		return 2
	}
	fromRep, err := readReport(fromPath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	toRep, err := readReport(toPath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	c := measure.CompareReports(fromRep, toRep, fromLabel, toLabel)
	if output != "" {
		if err := writeJSON(output, c); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if summaryFile != "" {
		if err := measure.AppendCompareSummary(summaryFile, c, top); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	fmt.Printf("compare %s -> %s: matched %d, only in from %d, only in to %d\n",
		fromLabel, toLabel, c.Matched, c.FromOnly, c.ToOnly)
	return 0
}

func readReport(path string) (measure.Report, error) {
	var rep measure.Report
	raw, err := os.ReadFile(path)
	if err != nil {
		return rep, fmt.Errorf("cannot read report %s: %v", path, err)
	}
	if err := json.Unmarshal(raw, &rep); err != nil {
		return rep, fmt.Errorf("cannot read report %s: %v", path, err)
	}
	return rep, nil
}
