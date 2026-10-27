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
  first solution as MRV (same expansion order; DLX's extra pruning
  only removes dead subtrees).
- Faster than MRV, measured two ways: fewer search nodes (counted)
  and less solver time on hard + sparse boards.
- Same stdin/stdout driver format; invalid (conflicting-givens)
  inputs are out of scope (valid puzzles only, as before).
- Differential DLX-vs-MRV passes on fixtures, hard boards
  (Escargot, Norvig 17-clue), and randomized sparse boards;
  `tools/measure.py` shows no regression.

## Part 2 — technical notes

- Exact cover: 324 columns (81 cell + 81 row-digit + 81 col-digit +
  81 box-digit), 729 rows ((cell, digit) quadruples).
- Solved ⟺ no uncovered cell column (a fully covered cell set is a
  complete consistent board). Choose min (size, index) over R-chain
  cell columns; size-0 chosen column ⟺ dead (MRV finds the same
  dead-ends, possibly a node later — outputs still identical).
- Givens covered at setup and pushed to the solution stack; on
  success decode digits from the stack; on failure print
  givens+zeros (matches MRV's fully-backtracked board).
- Implementation: `int[]` L/R/U/D/C/S arrays, `letfn` search
  (depth ≤ 81), primitive hints, unchecked math, no per-node
  allocation. Per-puzzle fresh arrays (setup is O(729)).
- TDD: byte-compare against MRV on a corpus of sparse boards
  (empty → dense) before benchmarking.
