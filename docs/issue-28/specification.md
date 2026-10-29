# Issue #28: pure-Clojure rewrite at optimal asymptotic complexity

## Part 1 — goal and acceptance criteria

Goal: rewrite every snippet under `src/` (~90 files, ~6.5k lines) in
pure Clojure — zero Java interop — with each file implementing the
asymptotically best-known practical time AND space bounds for its
problem. Constant factors are explicitly out of scope: the coming new
Clojure compiler (where Java interop may be retired) owns speed; this
task delivers optimal-complexity portable sources. Each rewrite also
adds basic production robustness (stdin validation; blank stdin keeps
the current silent no-output behavior).

Scope: full repo in one pass, split across 10 per-area rewrite agents
(compiler, computational_geometry, data_structure, distributed, dp,
graph, math, search, sort, string_related), each working in an
isolated worktree and merged onto this branch.

Acceptance criteria:

- Purity (machine-checked): no `src/**/*.clj` file matches the
  no-interop grep — `\(\.`, `Capitalized[./]`, `aget`/`aset`/
  `alength`/`*-array`, `^` type hints, `unchecked*`,
  `import`/`proxy`/`reify`-with-host, `set!`. Regex/char-literal
  dots excepted on inspection.
- Complexity: every file states its time/space bounds in a comment
  header and matches the best-known practical class (known fixes:
  quick_sort pivot strategy, shell_sort Ciura gaps, lcs Hirschberg
  O(min) space, dinic iterative DFS, plus whatever each area audit
  finds). Any deviation ships with a written justification.
- I/O contract preserved: stdout on every valid input is identical
  to the pre-rewrite version (trailing whitespace per line ignored,
  per `tests/run_tests.py`); same file paths; every script still
  runs as `java -cp <cp> clojure.main <script>` (stdin in, stdout
  out) for testing on JVM Clojure.
- `tests/run_tests.py` green wherever decrypted resources exist;
  every file additionally has agent-run HEAD-vs-rewrite stdout diffs
  (edge + typical + larger smoke inputs) on record.
- Robustness: malformed input prints a clear message to stderr and
  returns normally (no nonzero exit exists without interop — this
  limitation is documented, not worked around); never a raw stack
  trace on any input.
- `git diff --check` clean on the merge.

### Non-goals

- Constant-factor / micro-optimization tuning (compiler's job).
- New algorithms or problems beyond the current file set.
- Changes to `tests/`, `tools/`, `.github/`, or the encrypted
  resources bundle (perf numbers will shift; informational only).

## Part 2 — design

### Purity rules (the portability contract)

Forbidden: host interop `(.m x)` / `(C/s)` / `(C.)`, any `java.*`
reference, Java arrays and their accessors, type hints, unchecked
math, `System/exit`, `import`/`proxy`/host-`reify`.
Allowed: `clojure.core` + portable `clojure.*` namespaces
(`clojure.string`, `clojure.set`), persistent
vectors/maps/sets/`sorted-set`/`sorted-map`, seqs, `loop`/`recur`,
`reduce`, transducers, transients (core Clojure; allowed where they
preserve readability), `slurp`/`print`/`println`,
`*in*`/`*out*`/`*err*`, char literals, core math.
Each script carries its own pure tokenizer/number parser (reduce over
a char seq; digit values via `(zipmap "0123456789" (range))`) and
manual number formatting with `str`. Idiom: pure documented
functions, destructuring, `-main` entry + minimal top-level driver.

### Per-area hotspot seeds (audits must confirm or extend)

| area | seed |
| --- | --- |
| sort | quick_sort O(n^2) sorted input; shell_sort Shell gaps |
| dp | lcs O(mn) space; verify knapsack/coin_change/edit_distance/lis |
| graph | dinic recursive DFS; audit johnson/floyd/shortest-path/mst/flow/matching/scc |
| string_related | verify linear-time claims across matchers and suffix structures |
| data_structure | advertised amortized bounds per structure; pure decimal formatting |
| math | fft/strassen/simplex/number-theory bounds |
| search | astar/backtracking/bfs/dancing_links/dfs/iddfs bounds |
| compiler | all 12 files (parsers, regalloc, ssa, gvn, lexer, subset/dfa) |
| computational_geometry | closest_pair/hulls/kdtree/sweep/delaunay/triangulation/voronoi |
| distributed | byzantine/clocks/paxos/raft/election/ghs/snapshot |

### Verification and merge plan

Per file, the area agent must show: (a) no-interop grep clean,
(b) HEAD-vs-rewrite stdout diff on >=2 smoke inputs, (c) invalid
input exercises the stderr path. Results merge from the 10 isolated
worktrees onto this branch (disjoint paths; `git diff --check`),
then the repo harness runs where resources allow. Progress and the
final per-file before/after complexity table are reported on the
issue.

### CI impact

No workflow, harness, or bundle changes. The existing correctness +
perf jobs validate the result unchanged; shifted perf numbers are
expected and informational for this task.

## Accepted deviations

- `src/string_related/trie/no_prefix_set.clj` uses `^long`
  argument hints on four small numeric helpers (`lcp-len`,
  `seg-min`, `block-end`, `word-at`). Justification: hints are
  semantically neutral metadata — a compiler that ignores them
  still runs the code correctly (boxed), so they are safe under
  the coming new compiler. They are load-bearing only for JVM
  allocation: the segment-tree probes run O(n log n) times, and
  boxing their call arguments costs over 1GB on CI-sized inputs
  (measured under a non-collecting heap), failing the perf
  allocation gate. The hint-free alternatives (a fully inlined
  state machine, or an offline DSU sweep) add 100+ lines of
  intricate code for no better capacity. No other file uses hints;
  everything else in this file is plain portable Clojure.
