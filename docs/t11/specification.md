# T11: Bencher report hygiene — trim BMF payload, document threshold policy

(Note: issue #11's plan text names the spec path `docs/t10/`; the task
id is T11, so the spec lives at `docs/t11/` with T11 used everywhere.)

## Part 1 — goal and acceptance criteria

Goal: fix the truncated Bencher Report check and document the
threshold policy next to the residual No-Threshold boilerplate.

Background: the Bencher Report check on the head commit is truncated —
36 benchmarks x 3 measures (`cpu_time`, `peak_memory`, `wall_time`)
with long per-row URLs exceed GitHub's ~64KB check-summary limit, so
only warnings + alert count are visible. The report also carries
"No Threshold found" boilerplate for measures intentionally left
un-thresholded.

Acceptance criteria:
- `build_bmf` in `tools/measure.py` emits EXACTLY the measures
  `cpu_time` and `peak_memory` per benchmark: `wall_time` is dropped
  (load-dependent on shared runners; `cpu_time` is the stable proxy)
  and `instructions`/`cycles` are never emitted even when perf data
  is present (always null on hosted runners; re-adding rows
  re-truncates the report).
- The full `perf-results.json` report is untouched: all metrics
  (`wall_s`, `instructions`, `cycles`, faults, per-run detail) stay
  in the uploaded artifact for analysis.
- The bencher `run` step in `.github/workflows/test.yml` carries a
  short comment documenting the policy: thresholds on `peak_memory`
  (`t_test`) only, by design — `cpu_time` is too noisy on shared
  runners, so Bencher's residual No-Threshold boilerplate for
  `cpu_time` is expected and not actionable.
- Every other consumer of BMF `wall_time` (step summary, README) is
  adjusted so nothing breaks or misleads.
- `python3 tools/measure.py --self-test` reports 0 failed; a BMF
  key-assertion check confirms every benchmark's measures equal
  exactly `{cpu_time, peak_memory}`; `git diff --check` is clean.
- End-to-end proof is the PR's own CI Bencher comment (no truncation
  warning), confirmed by the reviewer.

## Part 2 — technical notes

- Root cause of truncation: 36 benchmarks x 3 measures with long
  per-row Bencher URLs exceed GitHub's ~64KB check limit.
- Row estimate after the trim: 36 benchmarks x 2 measures = 72 rows,
  which fits in 64KB (each row section shrinks by a third and the
  longest per-row URLs drop with the third measure).
- `wall_time` stays measured and recorded per run/case in
  `perf-results.json`; only the Bencher-bound BMF payload drops it.
- Thresholds are set where Bencher runs on branch pushes
  (`--threshold-measure peak_memory --threshold-test t_test ...`);
  PR runs clone thresholds from the start point
  (`--start-point-clone-thresholds`), so the policy comment belongs
  at the `run` step covering both branches.
- `docs/t2/specification.md` keeps its historical `wall_time`
  mention (T2-era record); only live consumers are updated.
