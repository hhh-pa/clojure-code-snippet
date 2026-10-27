# T9: Real dancing-links (Algorithm X) sudoku solver

## Part 1 — goal and acceptance criteria

Goal: replace the MRV copy in `src/search/dancing_links/sudoku.clj`
with a genuine Knuth Algorithm X solver over dancing links, that passes
all tests and runs faster than the current MRV solver.

Acceptance criteria:
- Real DLX machinery: toroidal doubly-linked lists (int arrays),
  cover/uncover, exact-cover search — not MRV in disguise.
- Byte-identical outputs to the backtracking MRV solver on every
  shared case, including multi-solution sparse puzzles (the repo
  suite expects MRV's first-solution choice).
- Column strategy restricted to min-size CELL column, ties broken by
  lowest cell index, rows tried digit-ascending: provably the same
  first solution as MRV (same expansion order; DLX finds dead-ends
  no later than MRV, so it visits a subset of MRV's nodes).
- Faster than MRV where it counts: strictly fewer search nodes on
  every measured input (counted), and less search time on hard
  multi-puzzle boards. One-shot total time is within run-to-run
  noise of MRV on hard boards (per-puzzle setup tax vs search
  margin); sparse backtrack-heavy totals may trail MRV slightly
  (DLX backtrack relinks ~75 nodes vs MRV's bit-clear). See Part 3.
- Same stdin/stdout driver format; invalid (conflicting-givens)
  inputs are out of scope (valid puzzles only, as before).
- Differential DLX-vs-MRV passes on fixtures, hard boards
  (Escargot, Norvig 17-clue), and randomized sparse boards;
  `tools/measure.py` passes (`ok`) with whole-process metrics
  within startup-dominated noise of the MRV copy (a larger file
  costs ~0.26 s one-time extra user-CPU at Clojure compile).

## Part 2 — technical notes

- Exact cover: 324 columns (81 cell + 81 row-digit + 81 col-digit +
  81 box-digit), 729 rows ((cell, digit) quadruples).
- Solved ⟺ no uncovered cell column (a fully covered cell set is a
  complete consistent board). Choose exact min (size, index) over
  the uncovered cell columns via a packed list PL (swap-remove on
  cover, append on uncover; index tie-break scanned explicitly so
  the result equals the R-chain strict-improvement scan, verified
  by byte-compare). First size-0 column returns ⟺ dead.
- Givens covered at setup and pushed to the solution stack; on
  success decode digits from the stack; on failure print
  givens+zeros (matches MRV's fully-backtracked board).
- Implementation: `int[]` L/R/U/D/C/S arrays plus auxiliary
  indexes (PL/PP/PN packed uncovered-cell list, RC/RD row decode,
  RB row-base), `letfn` search (depth ≤ 81), primitive hints,
  unchecked math, no per-node allocation. The static matrix is
  built once per process; each puzzle gets fresh mutable arrays
  via `System/arraycopy` (C/RC/RD/RB are read-only, shared).
  Rows own 4 consecutive node ids, so cover/uncover visit
  siblings by direct unrolled index; S is maintained for cell
  columns only (never read for constraint columns); cell and
  constraint covers are split per call site (column kind is
  statically known). L/R links stay fully built and maintained.
- TDD: byte-compare against MRV on a corpus of sparse boards
  (empty → dense) before benchmarking.

## Part 3 — as-built measurement (2026-10-27, same machine)

Byte-compare DLX vs backtracking MRV (`cmp`, all MATCH):
fixture `test_case_1`, Escargot, Norvig 17-clue, t8, t64, and a
46-board randomized sparse corpus (0–80 clues, incl. empty and
multi-solution boards). Repo suite: `run_tests.py --only
dancing_links`: 1 passed. Reflection warnings: 0.

Search-node counts (instrumented /tmp copies, same inputs):
Escargot 218 vs 220, Norvig 482 vs 719, t8 542 vs 556,
t64 4336 vs 4448, corpus 6842 vs 7737 (DLX ≤ MRV everywhere).

In-JVM solver time medians of 5 (parse+solve+print, compile
and JVM startup excluded): t64 DLX ~32.7 ms vs MRV ~32.0 ms
(tied within noise); search subtime t64 DLX ~13.5 ms vs MRV
~22.7 ms; corpus DLX ~38.6 ms vs MRV ~36.5 ms; Escargot
DLX ~6.7 ms vs MRV ~6.2 ms. Whole-process `/usr/bin/time`
is JVM-startup-dominated (~1 s wall): DLX +~70 ms wall
(+~0.26 s user-CPU one-time compile of the larger file).
`tools/measure.py` (fixture, `--repeat 3`): both `ok`;
before cpu_med 2.21 s / wall_min 0.96 s, after cpu_med
2.47 s / wall_min 1.04 s (startup-dominated).
