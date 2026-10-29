#!/usr/bin/env python3
"""Focused regression test: ssa_dominance stdout contract (issue #28).

Pins the exact idom/phi-block output of src/compiler/ssa_dominance.clj on
loopy graphs, including the reference's order-dependent single-pass DF
quirk: a loop header defined inside its own loop gets NO phi block from
the reference (e.g. `4 4 1 2 2 3 3 4 4 2 1 2` prints NONE), while a
textbook Cytron DF would place one. A rewrite that "fixes" the DF to the
textbook form breaks stdout identity -- this test fails first on such a
change.

Expected outputs were captured from the pre-rewrite implementation
(`git show master:src/compiler/ssa_dominance.clj`); comparison ignores
trailing whitespace per line, same as tests/run_tests.py.

Usage: python3 tests/ssa_df_regression.py [--cp CLASSPATH]
Exit status is non-zero when any case mismatches.
"""

import argparse
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from run_tests import normalize, resolve_classpath, ROOT  # noqa: E402

SCRIPT = os.path.join(ROOT, "src", "compiler", "ssa_dominance.clj")

CASES = [
    # (name, stdin, expected stdout) -- loopy, def-in-loop, unreachable
    ("loop-head-def",
     "4 4 1 2 2 3 3 4 4 2 1 2",
     "idom(2)=1 idom(3)=2 idom(4)=3\nNONE\n"),
    ("nested-loops-def",
     "5 6 1 2 2 3 3 4 4 2 4 5 5 3 1 2",
     "idom(2)=1 idom(3)=2 idom(4)=3 idom(5)=4\nNONE\n"),
    ("back-to-entry-def",
     "3 3 1 2 2 3 3 1 1 1",
     "idom(2)=1 idom(3)=2\nNONE\n"),
    # The original leaves idom[1] as 0, so a back edge into the entry
    # puts 1 in DF(2) and then in DF(1); CHK maps root->root, which
    # must be reset to 0 before the DF pass or phi(1) is lost.
    ("entry-in-own-frontier",
     "2 2 1 2 2 1 1 1",
     "idom(2)=1\n1\n"),
    ("self-loop-def",
     "2 2 1 2 2 2 1 2",
     "idom(2)=1\n2\n"),
    ("diamond-unreachable-artifact",
     "5 4 1 2 1 3 2 4 3 4 2 2 3",
     "idom(2)=1 idom(3)=1 idom(4)=1 idom(5)=0\n4\n"),
    ("two-back-def",
     "4 5 1 2 2 3 3 4 4 2 3 2 1 3",
     "idom(2)=1 idom(3)=2 idom(4)=3\n2\n"),
]


def run_case(classpath, stdin, expected, timeout=120):
    try:
        proc = subprocess.run(
            ["java", "-cp", classpath, "clojure.main", SCRIPT],
            input=stdin.encode(),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return "TIMEOUT (>{}s)".format(timeout)
    if proc.returncode != 0:
        tail = proc.stderr.decode(errors="ignore").strip().splitlines()
        return "exit {}: {}".format(proc.returncode, tail[-1] if tail else "no stderr")
    if normalize(proc.stdout) != normalize(expected.encode()):
        return "output mismatch: expected {!r} got {!r}".format(
            expected[:160], proc.stdout.decode(errors="ignore")[:160])
    return None


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--cp", default=None)
    parser.add_argument("--timeout", type=int, default=120)
    args = parser.parse_args()

    classpath = resolve_classpath(args.cp)
    failures = []
    for name, stdin, expected in CASES:
        error = run_case(classpath, stdin, expected, args.timeout)
        if error is None:
            print("PASS {}".format(name))
        else:
            failures.append("{}: {}".format(name, error))
    for failure in failures:
        print("FAIL", failure)
    print("ssa_df_regression: {} passed, {} failed".format(
        len(CASES) - len(failures), len(failures)))
    sys.exit(1 if failures else 0)
