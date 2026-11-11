# Issue #66: AOT legs in the perf-instrumentation sweep

## Part 1 — goal and acceptance criteria

Goal: the `perf-instrumentation` sweep measures every snippet
ahead-of-time compiled, both with JVM Clojure and with clojure-rs,
on every push and PR. These legs sit next to the existing
interpreted legs, so all four runtimes are comparable per shard.

| leg | command measured | compile step (not measured) |
|---|---|---|
| `jvm` | `java -cp <cp> clojure.main <script>` | — |
| `jvm-aot` | `java -cp <classes>:<cp> <Class>` | gen-class transform + `compile` (`--aot`) |
| `clojure-rs` | `clojure-rs run <script>` | — |
| `clojure-rs-aot` | the built binary | `clojure-rs build <script> -o <bin>` (`--build-template`) |

Acceptance criteria:

- **Every leg is a correctness gate.** Each of the four legs
  diff-gates stdout like `snip test` (same normalization, same 300 s
  timeout), so every case must pass under all four runtimes.
- **Compile time is never measured.** Each script is built once,
  before its first measured run, into a content-keyed cache.
- **Build failures fail loudly.** A failed build fails the script's
  cases; nothing is silently skipped.
- **Every leg measures the same cases.** The verify step checks that
  each leg measured exactly the same `(label, stem, case)` set and
  missing set as `jvm`, in both single-run and comparison mode.
- **Reporting.** `report-instrumentation` merges per runtime over
  `[jvm, jvm-aot, clojure-rs, clojure-rs-aot]`. Artifacts follow the
  existing `sweep-results-<runtime>-*` scheme, and existing artifact
  names (consumed by the hhh-pa/clojure h2h gate) are unchanged.
- **The `clojure-rs` run leg keeps its interpreter-only binary**, so
  its series stays comparable across this change.
- **`perf-pilot` is retired.** Its single-case `--aot` canary is
  subsumed by the full `jvm-aot` leg.

Non-goals: Bencher thresholds (the sweep stays report-only), and
native-coverage targets for clojure-rs `build` (tracked in
hhh-pa/clojure#256).

### BDD scenarios

- **Given** a push, **when** shard K runs,
  **then** it uploads `sweep-results-{jvm,jvm-aot,clojure-rs,clojure-rs-aot}-shard-K`
  with identical case keys.
- **Given** a snippet that `clojure-rs build` cannot compile,
  **when** the `clojure-rs-aot` leg runs,
  **then** its cases fail with `aot: build exit N: <stderr tail>`,
  and the shard fails.
- **Given** a snippet that ends with `print` and no newline,
  **when** the `jvm-aot` leg runs,
  **then** the output is flushed, because the generated `-main`
  flushes `*out*` like `clojure.main`.
- **Given** two byte-identical scripts in one problem dir,
  **when** both are JVM-AOT compiled concurrently,
  **then** they use separate cache slots. The key now includes the
  label and stem; before, both raced on one slot.

## Part 2 — design

### `snip measure --build-template T [--build-cache DIR]`

- **Template holes.** `T` has `{script}`, `{stem}` and `{out}` holes,
  with the same escapes and errors as `--command-template`.
- **Incompatible flags.** It cannot be combined with `--aot` or a
  non-default `--command-template`, because the measured command is
  the built binary. Allocation runs are skipped with a warning, since
  there is no JVM.
- **`NativeBuildCache`** (`internal/measure/nativebuild.go`):
  - singleflight per script;
  - slot key: sha256 over the compiler fingerprint (sha256 of the
    resolved argv0's bytes), `T`, label, stem and the script bytes;
  - each build writes `<slot>/<stem>.tmp`, renames it into place, and
    writes the `ok` marker last;
  - failures are sticky per run, and the error reports the last 3
    stderr lines.
- **Report.** It records `config.build_template` (omitted otherwise,
  so older reports and parity fixtures are unchanged) and the
  compiler's `runtime_version`.
- **Instrumentation.** Instrumented dispatches treat the binary as a
  native leg (plain callgrind + `LD_PRELOAD` counting, plain Pin).

### JVM AOT fixes (`internal/aot`)

- The generated `-main` is
  `(try (-aot-run) (finally (flush)))`.
- The cache key covers label + stem + script bytes, and the marker is
  bumped to `v2` so old slots recompile.

### Workflow (`.github/workflows/test.yml`)

- **Builds, on cache miss.** The clojure-rs repo's own
  `setup-llvm` action (from the clojure-rs checkout) installs LLVM 22.
  Then two builds run: `--no-default-features` (into `cljrs-bin/`, for
  `run`) and `-p clojure-rs -p clj-rt` with the `aot` feature (into
  `cljrs-aot/`, with `libcljrt.a` beside the compiler). Both are
  cached under `cljrs-bin-aot-<os>-<sha>`.
- **Legs.** Single-run mode measures 4 legs per shard, and comparison
  mode 8 (from/to × 4).
- **Verify pairs.** `jvm` against each other leg, per tree.
- **Uploads and report.** Upload steps cover the new legs, and the
  report matrix covers all four runtimes.

### Verification (local, 2026-11-11)

- **JVM AOT.** Every one of the 394 scripts compiles, and all 1754
  cases pass under the CI locale (en-US). A comma-decimal macOS
  locale makes `format "%.6f"` print commas, equally in `jvm` and
  `jvm-aot`.
- **clojure-rs AOT** (hhh-pa/clojure#257). All 394 scripts build,
  and all 1754 cases pass.
- **Go tests.** `go test ./...` is green, including the new
  nativebuild tests and the self-test.

### Landing order

This PR needs hhh-pa/clojure#257 (`build` compiles every program).
Until #257 is on master, validate by dispatching with
`clojure_rs_sha` set to the #257 head; land this PR after #257.

### CI impact

- Sweep shards run two more timed legs. Expected shard time goes from
  about 5 min to about 12 min, plus a one-time LLVM install and
  double cargo build on a clojure-rs revision cache miss.
- No Jenkinsfile/.arclint (GitHub Actions only).
