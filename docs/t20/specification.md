# T20: complexity-slope measurement from scaled inputs

## Part 1 — goal and acceptance criteria

Goal: a framework that runs covered benchmarks at scaled input
sizes, fits the complexity exponent, and tracks it in Bencher as
the measure `complexity_exponent` (informational, no threshold) —
the machine-independent algorithm signature for the coming
native-code comparison. Scope: framework + input generators for
every benchmark whose contract admits a slope ladder (92 of 102;
the 10 skips are listed below with reasons).

Acceptance criteria:

- `python3 tools/measure.py --self-test` reports 0 failed,
  including new slope-fit checks on synthetic data: linear
  input recovers an exponent in [0.95, 1.05], quadratic in
  [1.95, 2.05], n log n over an 8x range in [1.0, 1.15],
  R^2 ~= 1 on clean data, and graceful skips (flat data,
  tiny n-range, too few points, non-positive values).
- Local end-to-end on every covered benchmark: each slope
  scale is correctness-diffed like a regular case, and a scale
  failure (timeout/mismatch/crash) fails the run — only fit
  skips (flat/poor-fit/tiny-range/...) stay informational.
  Measured exponents match the known complexity class of each
  algorithm family (spot-checked per area in the PR; deviations
  OK with explanation, e.g. JIT amortization reading
  sublinear on linear work, or an input family exercising a
  better-than-worst case).
- BMF emits `complexity_exponent` ONLY for covered benchmarks
  with a successful fit (sparse — zero report-size impact on
  all other benchmarks). No threshold is configured for it
  (informational trend; rationale in Part 2).
- No change to existing metrics, thresholds, benchmark names,
  or default harness behavior: slope runs happen only under
  an explicit `--slope` flag (CI passes it); without the flag
  the report is byte-comparable to before modulo timestamps.
- The `tools/measure.py` hook stays small and clearly
  delimited (a sibling task edits the same file; the parent
  merges both), and `git diff --check` is clean.

### Pilot choice (with feasibility reasoning)

First pilots: `sort/insertion_sort/solution` (expected exponent
2.0) and `sort/quick_sort/solution` (expected exponent 1.0-1.15,
i.e. n log n seen through a power-law fit over an 8x range).

Why these two first:

- Same stdin/stdout contract, verified by reading all three
  sort drivers: stdin is `n` followed by n longs, stdout is
  the sorted values as `[v1,v2,...]`. One generator module
  serves both pilots (and every future sort pilot) — minimal
  code for maximal exponent contrast (2.0 vs ~1.0), which is
  the strongest possible proof that the framework measures
  real complexity rather than noise.
- Expected outputs are trivially computable in Python (sorted
  order of a generated permutation), so scaled runs keep full
  correctness checking through the existing `measure_case`
  diff path — no trust-me timing-only runs.

### Full coverage (92 of 102) and the 10 skips

Every benchmark got a contract survey (stdin grammar, stdout
shape, scale knobs, determinism, reference feasibility) and,
where feasible, a generator + calibrated 4-scale ladder (8x
span, top scale aimed at 5-60 s local solve cpu). Two reference
strategies, in preference order:

- Construction-known answers: inputs whose correct output is
  trivially derivable (a complete graph has all distances 1, a
  unary string has n distinct substrings, a semiprime's factors
  are known). Required for superlinear subjects, where a
  Python port would be slower than the solution itself.
- Exact ports: a Python reimplementation of the solution's
  logic, verified diff-clean against the real JVM solution at
  every scale (ports that could not be made byte-exact were
  rejected in favor of a skip).

The 10 benchmarks with no ladder, by reason:

- No scale knob: `search/iddfs/minimum_depth_of_binary_tree`
  (O(1) closed form), `search/backtracking/sudoku` and
  `search/dancing_links/sudoku` (fixed 9x9; counting puzzles
  would measure replication, not the solver).
- Sub-logarithmic signal: `search/binary_search/winter_challenge`
  (O(log n) reads as flat) and
  `string_related/automata/count_strings` (no polynomial knob:
  length is logarithmic, DFA size exponential).
- Reference-cost ceiling: `math/fft/fft`, `math/strassen/strassen`
  (a correct Python reference is asymptotically slower than the
  subject, so the subject is instant at every reference-feasible
  scale), `computational_geometry/triangulation/delaunay`,
  `computational_geometry/voronoi/fortune_voronoi` (JVM flat
  ~3 s from n=10 to n=200 while the O(n^4) reference already
  needs 21 s at n=200).
- Tolerance-only output: `math/simplex/simplex` (2-decimal
  float verdict needs an epsilon check; the harness diffs
  bytes).

### Calibration (why the fit models the startup floor)

Measured pre-PR-29 (kept for the floor-bias argument; the
pure-Clojure rewrite changed the constants — see the ladder
note above). JVM cpu_s = user+sys:

| run | cpu (s) |
| --- | ---: |
| quick_sort, empty input (pure startup floor) | 2.04 |
| insertion_sort n=10k / 40k / 80k | 2.20 / 3.72 / 8.39 |
| quick_sort n=2k / 1M / 4M | 2.05 / 3.08 / 4.85 |

Insertion solve time quadruples per n-doubling (0.2 / 1.7 /
6.4 s above the floor) — textbook quadratic. But the ~2 s
JVM startup floor is comparable to solve time at moderate n,
so plain log-log OLS on raw times is badly biased: on
insertion totals 2.4 / 3.7 / 8.4 / 29 s at
20k / 40k / 80k / 160k it reports a slope of ~1.20 for a
quadratic algorithm (verified by hand from the OLS formula).
A metric that calls insertion sort n^1.2 is worse than none —
and the bias differs per machine and runtime, which defeats
the goal (JVM-vs-native comparison needs a floor-free
exponent). The fit must therefore model the floor explicitly
(see Part 2). Side benefit: for native binaries the floor
collapses to ~milliseconds and both methods agree, so the
same framework ports to the native comparison unchanged.

Pilot scales (solve signal comfortably above floor noise):

- insertion_sort: n = 5k / 10k / 20k / 40k
  (the pure-Clojure PR-29 rewrite is ~20x slower per op than
  the version below, scaling ~n^2.3: 20k->8 s, 40k->33 s,
  80k->167 s wall, 120k past the 300 s timeout).
- quick_sort: n = 1M / 2M / 4M / 8M
  (expected totals ~3.1 / 3.9 / 4.9 / 7 s cpu).

Every other ladder follows the same rule (top scale 5-60 s
local solve, 8x span, fit on the true work scale — path cells
for astar corridors, max N for rho-factoring, k for CRT folds)
and is recorded with its rationale in
`tools/generators/__init__.py` plus per-module docstrings.

## Part 2 — design

### New module: `tools/slope.py`

Pure-stdlib power-law fit plus the slope-runner:

- `fit_power_law(ns, ts)` fits `t = C + k * n^p` by least
  squares: for fixed p the inner (C, k) problem is closed-form
  OLS of t on x = n^p; the outer 1-D search over
  p in [0, 3] is golden-section on SSE (~80 iterations over
  <= 8 points — microseconds). Returns slope p, fitted floor
  C, k, R^2 on raw t, and a leave-one-out min/max band for
  the slope (degenerate [p, p] band if fewer than 2 LOO
  refits succeed). Deterministic: same inputs, same outputs.
- Garbage-in protection — skip with a recorded reason when:
  fewer than 3 points (`too-few-points`); any n or t
  non-positive (`non-positive`); n-range max/min < 4
  (`tiny-range`); t spread (max-min)/max < 5%
  (`flat`, no measurable scaling above floor noise);
  fitted k <= 0 (`non-increasing`); R^2 < 0.8
  (`poor-fit`). Clean synthetic data gives R^2 ~= 1 and
  real sorts are expected at R^2 > 0.99, so 0.8 is generous.
- `run_slope_all(...)` drives the harness: for each covered
  benchmark (registry lookup by BMF benchmark name, honoring
  `--only` and `--shard`), writes generated input/expected
  files to a temp dir, reuses `measure_case` per scale
  (correctness-checked like any other case), fits per-scale
  cpu_s medians, deletes that benchmark's files before moving
  on (peak disk stays near one benchmark; a `finally` keeps a
  crashed sweep from orphaning gigabytes). Returns a
  `complexity` dict for the full report: per benchmark the
  fit result plus per-scale n/median/min/max for transparency.
  A benchmark whose scales fail records `scale_failed: true`,
  which the measure hook turns into a run failure (see below);
  a benchmark whose fit is rejected records a skip reason and
  stays informational.

Fit input: per-scale median cpu_s across `--slope-repeat`
runs (default 3; CI passes 1 — with 92 covered benchmarks x 4
scales, repeat 3 would triple CI time for little fit gain,
and the R^2 >= 0.8 gate already rejects noise-dominated
fits). cpu_time is the existing primary metric
and the most load-independent per-point signal; wall time is
too noisy on shared runners and perf counters are null on
hosted runners — documented here so the choice is explicit.

### Input generators: `tools/generators/` package

One module per area, each generator a class with
`__init__(self, scales)` and
`generate(n, seed=1000) -> (input_bytes, expected_bytes, fit_n)` —
a pure deterministic function of `(n, seed)`, stdlib only:

- `sort_arrays.py`: `SortArrayGenerator` for the shared sort
  stdin contract (`n` plus a fixed-seed shuffle of `range(n)`,
  memory-conscious `array('l')` buffer; expected
  `[0,1,...,n-1]`) serving all ten sorts, plus
  `QuickselectGenerator` (same stdin, rank-`n//2` stdout).
- `search.py`: astar (forced snake-corridor path), bfs, dfs,
  pairs (exact two-pointer port).
- `dp_math.py`: coin_change, edit_distance, both knapsacks
  (note the swapped header order), lcs (tie-break-exact
  Hirschberg port), lis, max_subarray, crt (truncating-division
  port, fit on k), miller_rabin_pollard (construction-known
  semiprimes, fit on max N).
- `graph.py`: 17 generators, all construction-known (complete
  graphs, line graphs, directed cycles, unique matchings,
  diagonal/zero-cost matrices, disjoint 3-cycles, empty edge
  sets).
- `string_related.py`: 14 generators, all construction-known
  (zero-match scans, all-mismatch alignments, unary strings).
- `data_structure.py`: 12 generators, exact ports (AVL/BST,
  b-tree multiset, union-find, bug-compatible fenwick sweep,
  lazy-deletion fib heap, two-heap median with trunc-division,
  segtrees, set-script refs).
- `compiler.py`: 12 generators (Catalan CYK counts, minimal
  mod-k DFA, quirk-exact Earley/GVN/linear-scan/LL(1)/LR/
  Pratt/Chaitin/SSA ports, regex fullmatch, subset counts).
- `geo_dist.py`: 6 geometry (exact-decimal coordinates dodge
  float-format boundaries) + 7 distributed sims (integer-exact).
- `__init__.py`: `GENERATORS` registry mapping BMF benchmark
  name to generator + scales. Adding a benchmark is one
  registry line when its stdin contract already has a
  generator.

### Thin hook in `tools/measure.py` (mergeability)

All hook code sits between explicit
`T20 complexity-slope hook (begin/end)` markers, with lazy
imports inside the hook so there is zero top-of-file churn:

- argparse: `--slope` (store_true, default off — no behavior
  change unless opted in) and `--slope-repeat` (default 3);
  `--slope-only` skips the regular loop (fast local runs
  without decrypted resources).
- After the main loop: `complexity = run_slope_all(...)`
  (one call; slope honors `--only`/`--shard` so CI shards
  stay balanced and the `jq -s add` fan-in stays exact —
  slope entries ride in the benchmark's own BMF entry).
  Entries with `scale_failed` are appended to `failures`,
  so a broken ladder (or a timeout) fails the run exactly
  like a broken regular case; fit skips never touch
  `failures`.
- `build_bmf(results, complexity=None)`: merges
  `complexity_exponent` (`value` = slope, `lower/upper` =
  LOO band) into covered benchmarks' entries, creating the
  entry if regular cases are absent (local runs); default
  `None` preserves existing behavior exactly.
- Full report gains additive-only `complexity` plus
  `slope`/`slope_repeat` config provenance; `print_table`
  and the step summary gain one line per covered benchmark
  (slope, band, R^2, or the skip reason).

### BMF / threshold policy

One new measure, `complexity_exponent`, sparse (covered
benchmarks with successful fits only). NO threshold, by
design: the exponent moves only on algorithmic change, and
per-commit jitter from noisy small-scale points would turn a
threshold into alert spam; trend-watching on Bencher's perf
page is the whole use case (JVM-vs-native overlay via
testbeds, same as cpu_time). Revisit once history exists.
No workflow threshold flags change: the PR path clones
thresholds from master config (where none exists for the new
measure) and the master path sets thresholds for peak_memory
only. The workflow's measure invocation gains `--slope`
(one line) so CI produces the new measure; that CI run is
the end-to-end proof (pilots gain the measure, everything
else unchanged).

### CI cost

Scaled runs happen only for covered benchmarks: 92 covered x
4 scales x 1 repeat (`--slope-repeat 1`) = 368 extra JVM runs
plus generator-side reference compute (all references stay
under ~60 s; most under ~10 s). Measured locally: 3528 s of
scale cpu in total (~59 min), i.e. ~7-8 min wall per shard at
`--jobs 2` — the dominant cost of the perf job, accepted for
full complexity coverage. No new dependencies, no new jobs.
The merged step summary (one per Action run) carries the
`cxp (p)` complexity column next to cpu/memory, so the
aggregated report is the place to watch exponents.
