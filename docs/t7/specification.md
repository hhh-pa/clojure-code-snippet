# T7: Round-2 snippet optimizations

## Part 1 — goal and acceptance criteria

Goal: optimize the remaining style/complexity hot spots identified by
audit, with byte-identical outputs. Ship only changes with measured
evidence; revert the rest and record why.

Targets and acceptance criteria:
- kruskalmstrsub (SHIPPED): edge order sorted by a primitive index
  quicksort (replacing boxed `sort-by` over a lazy range); Union-Find
  loop walks the index array; unchecked arithmetic in hot loops. Same
  MST weight output on all inputs. Measured: peak RSS 263MB -> 163MB
  (-38%), CPU/instructions neutral.
- backtracking + dancing_links sudoku (REVERTED after measurement):
  packed-long MRV state was implemented and output-verified (MATCH on
  fixtures), then reverted: the solver is sub-millisecond even on hard
  puzzles (AI Escargot, Norvig 17-clue) while JVM startup is ~2s, so no
  solver change is measurable; the rewrite only added compile bloat
  (+6% instructions). Files restored byte-identical to baseline.
- count_strings mmult (REVERTED after measurement): reduction-chunking
  (8 adds per rem, overflow-safe) was implemented and output-verified
  (MATCH), then reverted: repeatably +8% slower, and a no-rem timing
  probe proved the loop is not idiv-bound (removal saves ~0s). The
  nested-block shape only added loop overhead. File restored
  byte-identical to baseline.
- Each target: differential old-vs-new passes; before/after numbers
  from tools/measure.py decide ship vs revert.

## Part 2 — technical notes

- Outputs must be byte-identical (line-stripped): keep traversal orders,
  tie-breaking, and formatting exactly.
- Sudoku early-exit (stop scan at first count-1 cell) must be preserved
  exactly, including its skip-rest behavior.
- Kruskal quicksort need not be stable: equal-weight edge order can
  differ as long as the MST *weight* is identical. Differential tests
  compare the printed weight (the program's only output).
