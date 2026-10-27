# T13: Stop small-sample Bencher false positives

## Part 1 — goal and acceptance criteria

Goal: the `peak_memory` t_test must not alert until it has enough
history to calibrate variance. Both master false positives so far
(iddfs, then `dp/knapsack/solution` on 472b10a — 3 bimodal repeat
runs straddling baseline) fired on tiny sample history.

Acceptance criteria:
- Push-path `bencher run` passes `--threshold-min-sample-size 10`.
- Manual `workflow_dispatch` on the branch exercises the push path
  end-to-end (branch run green, threshold upsert accepted).
- No other threshold/measurement behavior changes; PR CI green.

## Part 2 — technical notes

- t_test with a handful of tightly-clustered samples treats any
  GC-timing mode flip as significant; 10 samples is the smallest
  history that calibrates maxrss variance (lands are frequent,
  so the blind window closes in days; trends still record).
- PR branches inherit via `--start-point-clone-thresholds`.
- The red Bencher check on 472b10a is immutable history (noise
  alert); this change prevents recurrence, it cannot rewrite it.
