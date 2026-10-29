# T19: Track allocated-bytes (EpsilonGC) as load-independent algorithmic metric

## Part 1 — goal and acceptance criteria

Goal: measure allocated bytes per benchmark — a load-independent,
machine-independent gauge of algorithmic work for the coming
native-code comparison (Valgrind rejected for JVM CI: 20–50x
slowdown; `perf` counters unavailable on hosted runners) — and
track it in Bencher as `allocated_bytes` (informational trend, no
threshold), without breaking the report-size budget.

Acceptance criteria:
- Every case gets dedicated EpsilonGC allocation runs (3 per case,
  min-of-3; see Part 2) alongside — never replacing — the existing
  cpu/memory repeats, whose invocation is byte-identical (no
  baseline break).
- `build_bmf` emits `allocated_bytes` per benchmark; `bencher run`
  moves into each perf-shard job (per-shard reports); the fan-in
  job no longer runs Bencher on a merged file (that would
  re-truncate at 108 rows — the exact T11 failure).
- No new threshold: `peak_memory` (`t_test`) only, by design;
  `allocated_bytes` is an informational trend like `cpu_time`
  (workflow comment + this spec say so).
- `python3 tools/measure.py --self-test` reports 0 failed, with new
  coverage: Epsilon stderr parsing (K/M/G units + malformed lines),
  determinism property on toy data, BMF shape with the new measure.
- Local end-to-end on the 4-snippet fixture set: full + sharded
  runs green, per-shard BMFs contain `allocated_bytes`,
  merged/per-shard keys correct. `git diff --check` clean.
- README metrics list includes `allocated_bytes`; CI paragraph
  describes per-shard reports.
- End-to-end proof is the PR's own CI: per-shard checks
  intelligible, no truncation, benchmark history continuous.

## Part 2 — technical notes

- Epsilon flags: `-XX:+UnlockExperimentalVMOptions -XX:+UseEpsilonGC
  -Xms2g -Xmx2g` (Xms=Xmx avoids resize noise; 2g fits runners; no
  GC ever runs, so heap-used-at-exit equals total allocation).
  Capture with `-Xlog:gc:<tmpfile>` (file, not stderr): program
  stdout then stays clean for the correctness diff, and file
  capture sidesteps a routing surprise — on Oracle JDK 27,
  `-Xlog:gc` with no output target wrote to stdout, not stderr.
- Parse the LAST `Heap:` line's used value (earlier lines are
  mid-run states, e.g. `103M` before the final `136M`). Handle
  both format generations — old (`49824K used`, no percentages)
  and new (`136M (6.69%) used`, `2048M (100.00%) committed`) —
  with K/M/G (/B/T) units.
- Determinism was assumed but FAILED verification (local aarch64,
  Oracle JDK 27 + Temurin 21.0.13 — CI's JDK family):
  kruskal/27: 141,137,137,137,136,136M then 136,136,136M (M
  granularity; sub-M wobble visible in the % field every run);
  sudoku/21: 61626K,61628K,61628K (K granularity exposes KB
  wobble even on Temurin 21); count_strings/21: 57532K x3;
  sudoku/27: 61917K x3; kruskal/21: 193M x3. Verdict: NOT
  byte-deterministic (concurrent-allocation/TLAB waste; worse
  under load, which is exactly CI's environment) → min-of-3 per
  case. The noise is one-sided (waste only inflates used-at-exit),
  so the BMF value is the sum of per-case MINIMUMS (lower bound =
  same; upper = sum of per-case maximums), and the full report
  records all runs plus a per-case `deterministic` flag for audit.
- New full-report fields: `results[].alloc_runs` (bytes),
  `results[].summary.allocated_bytes` ({min,median,max,
  deterministic}); `TOOL_VERSION` 1 → 2. Non-`java` templates
  (future native runtimes) skip alloc runs: summary None, BMF
  omits the measure. Flags: `--alloc/--no-alloc` (default on),
  `--alloc-repeat` (default 3). Epsilon failures (nonzero exit,
  OOM past 2g, missing `Heap:` line) fail the case loudly — a
  silent null in a tracked metric is worse than a red run.
- Report-size math (measured 55,510 chars / 72 rows ≈ 771
  chars/row; 37 snippets now → largest shard 10 benchmarks):
  per-shard report ≤ 10 x 3 = 30 rows ≈ 23KB (~36% of 64KB);
  with a 4th measure later: 40 rows ≈ 31KB (~48%). Both fit with
  headroom. History stays continuous: benchmark names, project,
  testbed, and branch are unchanged; each shard reports a disjoint
  benchmark subset every commit, so every benchmark's series is
  unbroken. The push-path `--threshold-measure peak_memory ...`
  upsert runs per shard (idempotent); PR runs keep
  `--start-point-clone-thresholds`.
- Fan-in report job: downloads all shard artifacts, merges BMFs
  (`jq -s add`, exact by snippet-cohesive sharding) into a merged
  artifact for analysis, and appends a COMBINED step-summary table
  via a new `measure.py --merge` mode (concatenates full reports,
  rebuilds the BMF with `build_bmf`, reuses `append_summary`);
  per-shard job summaries are unchanged. Permissions follow the
  reporting: perf jobs gain `checks:write` + `pull-requests:write`,
  report job drops to `contents:read`.
