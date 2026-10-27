# T4: Faster CI via caches and parallel perf shards

## Part 1 — goal and acceptance criteria

Goal: cut CI wall time by caching install steps and sharding perf
measurement across parallel jobs, without changing what is measured.

Acceptance criteria:
- Clojure jars are cached; repeat runs skip the Maven downloads.
- apt packages (`time`, `linux-tools-*`) are cached; warm runs skip apt.
- `tools/measure.py` supports `--shard K/N` (deterministic split by
  snippet); the union of all shards equals the unsharded task set.
- The `perf` job runs 4 shards; a fan-in `report` job merges the per-shard
  BMF files (`jq -s add`) and posts exactly one Bencher report per runtime.
  Merged BMF equals the unsharded BMF exactly (snippet cohesion: a
  snippet's cases never split, so per-shard aggregates are complete).
- Benchmark names, measures, thresholds, and the Bencher check behavior
  are unchanged; branch CI goes green.

## Part 2 — technical notes

- Shard by snippet (label/stem) over the deterministically sorted
  snippet list, after any `--only` filtering; record shard/total in
  the report config. Case-level splitting would leave each shard
  with partial per-snippet aggregates that `jq -s add` silently
  overwrites (cpu_time undercount) — hence snippet cohesion.
- Shard artifacts are named per shard (`perf-results-<runtime>-shard-<k>`);
  the report job downloads by pattern (no merge-multiple, so same-named
  files don't collide) and merges with `jq -s add` (snippet keys from
  different shards are disjoint by construction).
- The `report` job duplicates the tiny runtime matrix; both matrices must
  be extended together when a new compiler lands (noted in a comment).
- `linux-tools` install stays best-effort in spirit: if apt fails the
  step fails loudly, but missing perf counters at *runtime* still degrade
  gracefully to null (existing measure.py behavior, unchanged).
