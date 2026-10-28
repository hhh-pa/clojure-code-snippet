# T20: complexity-slope measurement from scaled inputs

## Part 1 — goal and acceptance criteria

Goal: a framework that runs covered benchmarks at scaled input
sizes, fits the complexity exponent, and tracks it in Bencher as
the measure `complexity_exponent` (informational, no threshold) —
the machine-independent algorithm signature for the coming
native-code comparison. Scope: framework + 2 pilot benchmarks
with input generators (prove the pattern, don't boil the ocean).

Acceptance criteria:

- `python3 tools/measure.py --self-test` reports 0 failed,
  including new slope-fit checks on synthetic data: linear
  input recovers an exponent in [0.95, 1.05], quadratic in
  [1.95, 2.05], n log n over an 8x range in [1.0, 1.15],
  R^2 ~= 1 on clean data, and graceful skips (flat data,
  tiny n-range, too few points, non-positive values).
- Local end-to-end on both pilots: measured exponents match
  the known complexity class of each pilot algorithm
  (expected vs measured stated in the PR; small deviations OK
  with explanation).
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

Pilots: `sort/insertion_sort/solution` (expected exponent 2.0)
and `sort/quick_sort/solution` (expected exponent 1.0-1.15,
i.e. n log n seen through a power-law fit over an 8x range).

Why these two:

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
- Rejected `string_function_calculation`: its stdin contract
  (single token string) is simple, but producing expected
  outputs at scale in Python needs a suffix automaton of its
  own — too much machinery for a pilot. Rejected
  `sort/merge_sort` as the second pilot: same class as
  quick_sort, so the pair would prove less.

### Calibration (why the fit models the startup floor)

Measured on this machine (JVM cpu_s = user+sys):

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

Pilot scales (solve signal comfortably above floor noise,
total cost ~3 min CPU per CI run — see Part 2):

- insertion_sort: n = 20k / 40k / 80k / 160k
  (expected totals ~2.4 / 3.7 / 8.4 / 29 s cpu).
- quick_sort: n = 1M / 2M / 4M / 8M
  (expected totals ~3.1 / 3.9 / 4.9 / 7 s cpu).

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
  cpu_s medians, cleans up. Returns a `complexity` dict for
  the full report: per benchmark the fit result plus
  per-scale n/median/min/max for transparency.

Fit input: per-scale median cpu_s across `--slope-repeat`
runs (default 3). cpu_time is the existing primary metric
and the most load-independent per-point signal; wall time is
too noisy on shared runners and perf counters are null on
hosted runners — documented here so the choice is explicit.

### Input generators: `tools/generators/` package

- `tools/generators/sort_arrays.py`: `SortArrayGenerator`
  for the shared sort stdin contract. `generate(n, seed)`
  returns `(input_bytes, expected_bytes, n)`: input is
  `n` plus a fixed-seed shuffle of `range(n)`
  (memory-conscious `array('l')` buffer — 8M-element inputs
  stay under ~100 MB); expected is `[0,1,...,n-1]`.
  Average-case inputs: random permutation gives insertion
  sort Theta(n^2) and quicksort Theta(n log n).
- `tools/generators/__init__.py`: `GENERATORS` registry
  mapping benchmark name to generator + scales:
  `sort/insertion_sort/solution` -> 20k/40k/80k/160k,
  `sort/quick_sort/solution` -> 1M/2M/4M/8M.
  Adding a benchmark is one registry line when its stdin
  contract already has a generator.

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
  slope entries ride in the pilot's own BMF entry).
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

Scaled runs happen only for covered benchmarks: 2 pilots x
4 scales x 3 repeats = 24 extra JVM runs, ~3 min CPU total
(~2 min wall at `--jobs 2`, spread over the shards owning
the pilots). No new dependencies, no new jobs.
