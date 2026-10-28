# T24: memory alert stability — allocated-bytes percentage threshold

## Problem

The master-branch `bencher run` in `.github/workflows/test.yml` sets a
`t_test` threshold on `peak_memory` (peak RSS). The t-test
variance-tightens to ~1.5% on stable history, then trips on normal
+/-4% JVM RSS jitter on shared runners — flaky master alerts with no
real regression behind them (see issue #24).

## Evidence

Flaky failure run:
https://github.com/m-hu/clojure-code-snippet/runs/122767526019

## Bencher ops (already done by parent, before this workflow change)

- Master `peak_memory` threshold `01a17ebb`: model removed.
- Master `allocated_bytes` threshold `01a18265`: active, test
  `percentage`, upper boundary `0.10` (10%).

Rationale: a percentage threshold stays fixed at 10% — far above the
~3% worst observed allocation wobble, and far below a real memory
regression. `allocated_bytes` (EpsilonGC, T19) is also far less noisy
than peak RSS.

## This workflow change

In `.github/workflows/test.yml`, the push-run (non-PR, else-branch)
`bencher run` invocation now passes:

```
--threshold-measure allocated_bytes --threshold-test percentage \
--threshold-min-sample-size 10 --threshold-max-sample-size 64 \
--threshold-upper-boundary 0.10
```

instead of the old `peak_memory` / `t_test` / `0.99` flags. The PR-branch
invocation (`--start-point-clone-thresholds` path) is untouched, so PRs
keep cloning the branch threshold from the base.

Without this change, every master push would re-create the flaky
peak-memory threshold model that the parent just removed.

## Acceptance

- The next master push keeps the `allocated_bytes` percentage threshold
  and does not restore a `peak_memory` threshold model.
- `python3 -c yaml.safe_load` passes on the workflow; `git diff --check`
  is clean.
