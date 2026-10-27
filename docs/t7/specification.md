# T7: Round-2 snippet optimizations

## Part 1 — goal and acceptance criteria

Goal: optimize the remaining style/complexity hot spots identified by
audit, with byte-identical outputs.

Targets and acceptance criteria:
- kruskalmstrsub: edge order sorted by a primitive index quicksort
  (replacing boxed `sort-by` over a lazy range); Union-Find loop walks
  the index array. Same MST weight output on all inputs.
- backtracking + dancing_links sudoku (identical MRV solvers): per-node
  4-int array replaced by packed-long MRV state; precomputed box table;
  direct int output. Same solution bytes on all puzzles.
- count_strings mmult: modular reduction every 8 inner adds instead of
  every add (overflow-safe: 8*(MOD-1)^2 < Long/MAX_VALUE). Identical
  counts.
- Each target: differential old-vs-new passes on randomized inputs;
  before/after numbers from tools/measure.py show no regression.

## Part 2 — technical notes

- Outputs must be byte-identical (line-stripped): keep traversal orders,
  tie-breaking, and formatting exactly.
- Sudoku early-exit (stop scan at first count-1 cell) must be preserved
  exactly, including its skip-rest behavior.
- Kruskal quicksort need not be stable: equal-weight edge order can
  differ as long as the MST *weight* is identical. Differential tests
  compare the printed weight (the program's only output).
