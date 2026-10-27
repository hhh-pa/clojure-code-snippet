# T2: Bencher peak_memory regression threshold (mini-spec)

Goal: Bencher alerts on peak_memory regressions for master on the
ubuntu-latest-jvm testbed.

Acceptance criteria:
- The push-path `bencher run` sets a t_test threshold on peak_memory.
- PR branches inherit it via start-point-clone-thresholds (no change needed there).
- No threshold on cpu_time/wall_time yet (shared-runner noise); no --error-on-alert.
- Branch CI run goes green; threshold visible in Bencher after the master run.
