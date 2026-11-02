# T1211: callgrind-only perf CI with JVM vs clojure-rs comparison

## Part 1 — goal and acceptance criteria

Goal: consolidate the perf CI signal onto the deterministic callgrind
sweep, halve its wall time with finer sharding, and measure the Rust
port (`clojure-rs`, https://github.com/hhh-pa/clojure) on the same
snippets and cases — so `callgrind_instructions` and
`callgrind_allocated_bytes` for JVM Clojure vs clojure-rs overlay per
benchmark on Bencher's perf page (same benchmark names, distinct
testbeds).

Acceptance criteria:

- `test.yml` has no `perf`/`report` jobs; `perf-callgrind` runs a
  shard(8) x runtime{jvm, clojure-rs} matrix; `report-callgrind`
  merges per runtime; `test` and `perf-pilot` are unchanged.
- `snip measure --runtime clojure-rs --command-template 'clojure-rs
  run {script}' --callgrind` reports both gungraun-run measures per
  case (valgrind Ir + allocator-shim total); the JVM recipe behaves
  exactly as before (same flags, same parsers, same BMF shape).
- `snip measure --self-test` reports 0 failed, including new
  alloc-shim parser checks; `go test ./...` is green;
  `python3 -c yaml.safe_load` passes on the workflow;
  `git diff --check` is clean.
- The pushed branch shows green `test` + all 16 sweep legs +
  both `report-callgrind` merges, with per-shard Bencher reports
  for both runtimes and merged tables in the report summaries.
- Per-shard sweep wall time roughly halves vs the 4-shard
  baseline on the same hardware class; every per-shard Bencher
  check stays under GitHub's ~64KB summary limit.

BDD scenarios (user-observable):

- Given a push to the branch, when CI runs, then correctness
  gates in `test`, and 16 sweep legs measure deterministic
  instruction and allocation counts for both runtimes.
- Given Bencher's perf page, when the `ubuntu-latest-jvm` and
  `ubuntu-latest-clojure-rs` testbeds are overlaid, then each
  snippet shows JVM vs clojure-rs Ir and allocated bytes side
  by side.

Non-goals:

- No `snip test` runner flag: the recipes-drop-in `--runner`
  experiment stays local-only. The sweep's per-case stdout gate
  (first callgrind run, both runtimes) is the correctness check;
  output mismatches fail the leg.
- No timed `allocated_bytes` for the native runtime: it stays
  null, exactly like the JVM sweep under `--no-alloc`. Native
  allocation is reported only as `callgrind_allocated_bytes`.
- No `--slope` in the sweep (unchanged: slope fits timed runs;
  the sweep stays lean) and no AOT leg for clojure-rs
  (`clojure-rs build` comparison is future work).
- No Bencher-side cleanup of the retired `allocated_bytes`
  threshold and no branch-protection edit on this branch —
  both are merge-time admin steps (below).

Merge prerequisites (repo admin, at merge time — not on this
branch):

- Master branch protection currently requires `test`,
  `perf (0..3)`, `report (jvm)`. At merge time, replace the
  `perf`/`report` contexts with the new `perf-callgrind (...)`
  and `report-callgrind (...)` check names (exact names taken
  from this PR's checks).
- Optionally retire the master `allocated_bytes` Bencher
  threshold once reporting stops (history is kept; no action
  is required for correctness).

## Part 2 — design

### Workflow (`test.yml`)

- Delete the `perf` and `report` jobs entirely (Bencher
  `allocated_bytes` threshold wiring goes with them).
- `perf-callgrind` matrix: `shard: [0..7]` x runtime legs
  `jvm` (testbed `ubuntu-latest-jvm`, unchanged) and
  `clojure-rs` (testbed `ubuntu-latest-clojure-rs`).
  - JDK/JDK-jar steps run only on the `jvm` leg
    (`if: matrix.runtime == 'jvm'`).
  - New `clojure-rs` leg steps: install stable Rust
    (`dtolnay/rust-toolchain`), checkout `hhh-pa/clojure` at
    the pinned `$CLOJURE_RS_SHA`, `cargo build --release
    --locked --no-default-features` in `rust/` (interpreter
    only — `run` needs no LLVM, so CI avoids the ~GB LLVM 22
    toolchain), cache with `Swatinem/rust-cache`, copy the
    binary to `$GITHUB_WORKSPACE/cljrs-bin` and prepend it
    to `PATH` via `$GITHUB_PATH`.
  - The measure step passes `--runtime ${{ matrix.runtime }}`
    and `--command-template 'clojure-rs run {script}'` on the
    native leg (default JVM template otherwise), keeping the
    lean `--repeat 1 --no-alloc --callgrind
    --callgrind-repeat 1` recipe and `--shard N/8`.
  - The Bencher step is unchanged, including the 1%
  `callgrind_instructions` threshold flags: thresholds are
  per testbed, so the new testbed starts with empty history
  and arms after 10 samples. The `jq` strip and artifact
  upload are runtime-agnostic already (names carry
  runtime+shard).
- `report-callgrind` matrix gains the `clojure-rs` runtime
  (merge + `jq -s add` verify + upload, unchanged shape).
- `perf-pilot` stays: it is a cheap non-blocking AOT stdout-gate
  canary, not a perf signal.

Shard math: 1754 cases x ~21 s all-in / 2 jobs / 8 shards ~= 38
min per JVM shard (vs ~1.2 h at 4 shards). The native leg costs
one callgrind run (same order as JVM `-Xint` under valgrind)
plus one near-native-speed shim run, so native shards land in
the same band. 8 — not 16: 16 sweep jobs + 4 test + pilot + 2
reports fit GitHub's 20-job concurrency, and per-shard BMFs
(~50 benchmarks x 2 measures) sit far below the check-summary
limit. 16 shards stay available as a future lever.

### `snip measure`: native callgrind recipe

Today `CallgrindOnce` assumes the JVM: it splices `-Xint` +
EpsilonGC flags into `inner` and reads allocation from the
JVM's GC log, and `measure.go` disables callgrind outright for
non-`java` templates. The change branches on
`filepath.Base(inner[0]) == "java"`:

- JVM leg: byte-identical recipe (flags, parsers, skip rules).
- Native leg: `valgrind --tool=callgrind --cache-sim=no
  --branch-sim=no --collect-jumps=no --callgrind-out-file=X
  <inner...>` (argv0 resolved as today), stdout-gated on the
  first repeat exactly like the JVM path (mismatch fails the
  case; timeout degrades to `callgrind_skipped` with a
  warning).
- Native allocation comes from a second run under an
  `LD_PRELOAD` counting shim, not from DHAT/Massif: DHAT
  instruments every memory access (~25x slowdown, which would
  triple native per-case cost), while Massif reports snapshots,
  not cumulative totals. The shim interposes
  `malloc/calloc/realloc/reallocarray/posix_memalign/aligned_alloc`,
  sums requested bytes with atomics, and a destructor writes
  `snip_alloc_total_bytes=N` to fd 2. The C source is embedded
  in Go, compiled on demand with `cc`, and cached per process;
  it is Linux-only (Rust links libc dynamically, so
  interposition applies).
  - Semantic note (documented in README): the JVM number is
    EpsilonGC heap-used-at-exit, the native number is
    malloc-request bytes. Both are "total requested
    allocation" for the same program run — ratio-meaningful
    per benchmark, not bit-comparable absolutes.
  - Failure policy: a missing `cc`, a non-Linux host, or a
    shim-run timeout degrades the case to `callgrind_skipped`
    with a reason (visible in JSON, excluded from BMF) —
    the same treatment as callgrind timeouts. Wrong numbers
    never silently replace missing numbers.
- `CallgrindSkipReason`'s EpsilonGC-exempt skip applies to the
  JVM leg only; the native leg has no fixed heap, so it never
  skips for size reasons.
- Timed allocation (`MeasureAllocOnce`) stays java-only; native
  timed `allocated_bytes` stays null (symmetric with the JVM
  sweep under `--no-alloc`).
- Report provenance: `JavaVersion` is recorded for JVM legs
  only (nil otherwise); a new best-effort `RuntimeVersion`
  carries the first line of `<argv0> --version` (nil on
  failure). `ToolVersion` bumps 4 -> 5 (new field + native
  recipe).

### clojure-rs pin

- `CLOJURE_RS_SHA` workflow env pins `hhh-pa/clojure`
  (spec-time HEAD `0dbb78a`, the "all 1754 snippet cases
  green" drop-in commit; re-verified at implementation).
- Bump process (README): update the SHA, re-run; the sweep's
  stdout gate catches regressions, Bencher shows the delta.
- `clojure-rs run <script>` (stdin in, stdout out, exit
  0/1/3) is the invocation; exit codes and `*in*` behavior
  were proven by the drop-in over all 1754 cases.

### TDD plan

- New parser `ParseAllocShimTotal` with self-test checks first
  (valid line amid noise, missing line -> error, malformed
  line -> error), following the `ParseCallgrindSummary`
  pattern (self-test stays subprocess-free, so the live
  compile-and-run check against `sh` lives in
  `allocshim_test.go` with `t.Skip` where `cc` is absent or
  the host is not Linux).
- Workflow validated by `python3 -c yaml.safe_load` and a
  matrix/condition review (no local Actions runner).
- Local end-to-end is partial by construction: this host has
  no valgrind, no GNU time, and no decrypted resources, so
  `go build`, `go vet`, `go test ./...`, and `snip measure
  --self-test` are the local gates; Linux CI on the PR is
  the proof for the valgrind/shim paths.

### CI impact

- This change rewrites `.github/workflows/test.yml` (drop
  `perf`/`report`, re-shard `perf-callgrind`, add the
  clojure-rs leg) and Bencher reporting; no
  Jenkinsfile/.arclint changes needed (this repo uses GitHub
  Actions, not Jenkins).
- New CI dependencies on the native leg only: Rust stable
  toolchain + `Swatinem/rust-cache` (no LLVM).

### Risks

- Branch protection still names the old checks, so this PR
  shows them as forever-pending until the merge-time admin
  update above.
- clojure-rs nondeterminism (e.g. hash seeds) would surface
  as stdout-gate failures or Bencher wobble, never as silent
  drift; the 1% threshold only arms after 10 samples.
- If native callgrind runs slower than estimated, the lever
  is shard count (8 -> 16), not recipe changes.
- `required_signatures` is on: every commit on the branch
  must be signed (ssh), as here.
