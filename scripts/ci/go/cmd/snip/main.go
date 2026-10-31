// Command snip replaces the repo's Python and shell tooling with one
// statically built binary (profile-runner fashion: built once per
// source hash, cached, never rebuilt redundantly):
//
//	snip test     correctness runner (tests/run_tests.py)
//	snip ssa      SSA DF regression test (tests/ssa_df_regression.py)
//	snip measure  load-independent perf measurement (tools/measure.py)
//	snip bundle   encrypted resource-bundle builder (tools/build_bundle.sh)
//
// Every subcommand keeps the flags, outputs, and exit codes of the
// entry point it replaces. Run from the repository root.
package main

import (
	"fmt"
	"os"
)

func main() {
	os.Exit(dispatch(os.Args[1:]))
}

func dispatch(argv []string) int {
	if len(argv) == 0 {
		fmt.Print(usage)
		return 2
	}
	switch argv[0] {
	case "-h", "--help", "help":
		fmt.Print(usage)
		return 0
	case "test":
		return runTest(argv[1:])
	case "ssa":
		return runSSA(argv[1:])
	case "measure":
		return runMeasure(argv[1:])
	case "bundle":
		return runBundle(argv[1:])
	default:
		fmt.Fprintf(os.Stderr, "snip: unknown subcommand %q\n%s", argv[0], usage)
		return 2
	}
}

const usage = `Usage: snip <subcommand> [flags]

Subcommands:
  test      run every src/**/*.clj script against the shared test cases
  ssa       focused ssa_dominance stdout regression test
  measure   load-independent performance measurement
  bundle    build the encrypted test-resource bundle

Run from the repository root. Each subcommand accepts -h/--help.
`
