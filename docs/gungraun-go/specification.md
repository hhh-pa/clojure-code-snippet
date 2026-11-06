# Gungraun-style instruction+memory measurement, AOT mode, and Go port of all Python/shell tooling

Tracking fallback (no id): `arc` is absent from this machine and the
stored `gh` token is invalid, so neither a Phabricator task nor a
GitHub issue could be created. Work proceeds on branch
`gungraun-aot-go-measure` with this spec at
`docs/gungraun-go/specification.md`, following the issue-28
`docs/<id>/specification.md` convention.

## Part 1 — goal and acceptance criteria

Goal: (1) integrate gungraun's measurement methodology — Valgrind
callgrind total instruction counts plus whole-process memory — so
every snippet run on the JVM is measured from start to finish;
(2) measure the same signals for AOT-compiled snippets, not just
`clojure.main` script runs; (3) replace every Python program and
shell script in the repo with Go, built and cached like the existing
`profile-runner` Go tool (build once per source hash, reuse the
cached binary on every later use, never rebuild redundantly).

Scope (the full file inventory; `git ls-files`):

- `tests/run_tests.py` (182 lines), `tests/ssa_df_regression.py` (97)
- `tools/measure.py` (1350), `tools/slope.py` (410)
- `tools/generators/*.py` (9 files, ~5100 lines)
- `tools/build_bundle.sh` (59)
- `.github/workflows/test.yml` rewiring, `README.md` updates
- New: callgrind backend, AOT pipeline, composite action(s)

Acceptance criteria:

- One Go module `scripts/ci/go` (zero non-stdlib deps) builds a
  single binary `snip` with subcommands `test`, `ssa`, `measure`,
  `bundle` whose flags are identical to the Python/sh entry points
  they replace (same names, same defaults, same `--shard` semantics,
  same `--no-*` negations).
- `snip measure` full-report JSON, BMF JSON, and step-summary table
  are field-identical to the Python output for identical runs
  (timestamps/host excluded); verified by running both harnesses on
  synthetic resources and diffing, plus `jq -S` BMF equality in CI.
- `snip measure --self-test` preserves every existing check name
  and passes with no JVM/time/perf/valgrind present.
- `snip measure` streams per-case `measure [done/total]` progress
  lines as cases complete and per-benchmark `slope [index/total]`
  lines as slope fits finish (in sorted-name order); the full
  results table, JSON/BMF outputs, and summary file are unchanged.
- Every slope generator port is byte-verified: Go
  `generate(n, seed)` output equals Python `generate(n, seed)` at
  probed scales (exact Mersenne Twister replication, proven by
  diff, not by review).
- `snip measure --callgrind` records deterministic whole-process
  instruction counts (`callgrind_instructions` min/median/max) via
  the recipe `-Xint + EpsilonGC + fixed heap + -XX:-UsePerfData`
  (probed: 6,201,082,869 vs 6,201,117,587 across runs, 0.0006%
  wobble, ~90 s per small case on this machine); missing valgrind
  degrades to null with a warning, exactly like `perf` today.
  A case exceeding the callgrind timeout degrades to
  `callgrind_skipped` (Ok, timed data valid, warning on stderr)
  instead of failing, as do EpsilonGC-exempt workloads (which
  exceed the fixed callgrind heap by definition); crashes and
  output mismatches still fail.
  Verified end to end: `snip measure --callgrind` on bubble_sort
  reports 6,204,377,129 / 6,203,575,053 Ir with BMF
  `callgrind_instructions` and the `cg_insn` summary column.
- The callgrind run also reports EpsilonGC total allocation
  (`summary.callgrind_allocated_bytes`, BMF
  `callgrind_allocated_bytes`, `cg_alloc` summary column): the
  memory metric of the deterministic run, read from the JVM's GC
  log. Each `perf-instrumentation` shard reports both gungraun-run
  measures to Bencher (other measures stripped to avoid polluting
  the perf shards' histories), with a 1% percentage threshold on
  `callgrind_instructions`; `callgrind_allocated_bytes` is tracked
  threshold-free until its wobble is characterized.
- Memory stays on JVM-native signals: whole-process peak RSS
  (start-to-finish) plus EpsilonGC total allocation. Valgrind
  DHAT/Massif are explicitly NOT used for Java heap: HotSpot
  bulk-allocates its heap via mmap, so C-level heap profilers see
  one giant mapping, not Java objects. The spec records this as a
  deliberate non-use with rationale.
- `snip measure --aot` compiles each snippet to JVM bytecode
  ahead of time (generated `gen-class` namespace, per-script
  content-hash cache under `.ci/aot-cache/`, recompile only on
  cache miss) and measures the `java <MainClass>` run with the
  identical harness (time/perf/alloc/callgrind) and identical
  stdout-diff correctness gate. AOT stdout must equal script-mode
  stdout on every measured case. Verified end to end: `--aot`
  populates `.ci/aot-cache/<hash>/classes`, records `config.aot`,
  and reports cpu/insn/alloc below the JIT run on bubble_sort.
- New composite action `.github/actions/snip-tools` mirrors
  `profile-runner` exactly (actions/cache on the binary dir keyed
  by OS + source hash, setup-go + `CGO_ENABLED=0 go build` on miss
  only, no `working-directory` key); CI jobs use the cached `snip`
  binary instead of `python3`.
- CI runs a full-matrix `perf-instrumentation` sweep per push (4 shards,
  lean `--repeat 1 --no-alloc` recipe, ~1h per shard parallel with
  `perf`): the pilot-only constraint was calibrated on a slow dev
  box (~90s/case); CI hardware measures ~21s/case all-in, so the
  full matrix is affordable. `--aot` stays a one-benchmark
  non-blocking pilot (stdout-gate canary).
- All tracked `.py`/`.sh` files deleted; `__pycache__/` dirs
  removed; no `python3`/`bash tools/` invocation remains in CI,
  README, or docs (except this spec's historical notes).
- `go vet`, `gofmt -l` (empty), `go test ./...`, and
  `git diff --check` are all clean; workflow YAML still parses.

### Non-goals

- A Rust crate depending on the gungraun library: gungraun is a
  Rust-only `cargo bench` harness and cannot drive this repo's
  394-script stdin/stdout JVM harness without reimplementing it;
  this task integrates gungraun's instrument methodology
  (callgrind `Ir`, one-shot runs, CI-comparable counts) with
  gungraun-compatible tool flags, which is the capability asked for.
- DHAT/Massif memory numbers (see rationale above).
- New snippets, new test resources, bundle rotation, Bencher
  threshold changes (the new `callgrind_instructions` BMF measure
  ships threshold-free like `cpu_time`).
- Pushing, PRs, landing: leave the branch unpushed for review
  (no credentials for either remote helper on this machine).

## Part 2 — design

### Go module layout (all under `scripts/ci/go`)

- `cmd/snip/main.go`: subcommand dispatcher only
  (`test`, `ssa`, `measure`, `bundle`).
- `internal/snipcore`: discovery (`src/**/*.clj` + resources
  layout), classpath resolution (`--cp`/`CLOJURE_CP`/`CLOJURE_JAR`/
  `.ci/jars`), `normalize` (strip trailing whitespace per line),
  subprocess run with timeout, `--shard` selection.
- `internal/measure`: GNU-time/perf/epsilon/callgrind parsers,
  `measure_case` loop, summaries, BMF builder, merge, step-summary
  table, main/merge/self-test drivers. Flag-compatible with
  `tools/measure.py`, plus `--callgrind`, `--callgrind-repeat`,
  `--aot`, `--aot-cache`.
- `internal/slope`: `t = C + k*n**p` golden-section fit with the
  same guards/reasons, LOO band, `run_slope_all` driver.
- `internal/generators`: one file per Python module, same type
  names and `GENERATORS` registry with identical scales.
- `internal/mtrand`: exact CPython `random.Random` replication
  (MT19937 + `random`/`getrandbits`/`randbelow`/`randint`/
  `randrange`/`choice`/`shuffle`); the ONLY randomness source for
  generators, so outputs are byte-identical to Python.
- `internal/cljread`: Clojure top-level form splitter (bracket
  depth + strings + char literals + `;` comments + `#_` forms).
- `internal/aot`: script-to-namespace transform, `compile`
  driver, content-hash class cache, AOT run argv builder.
- `internal/bundle`: `build_bundle.sh` port (openssl/tar via
  exec, `gpg` required; the Python-`pgpy` fallback dies with the
  port — `gpg` exists on runners and here).
- `internal/executil`, `cmd/runner-profile`: untouched.

### CLI and output compatibility contract

- Every flag keeps its Python spelling, default, and meaning;
  `--perf/--no-perf` and `--alloc/--no-alloc` negations are
  reimplemented (Go's `flag` package has no `BooleanOptionalAction`).
- Report JSON: same keys in the same order (`tool`, `version`,
  `timestamp_utc`, `host`, `runtime`, `config`, `results`,
  `failures`, `missing`, `complexity`); `TOOL_VERSION` bumps 2→3
  (new optional fields only: `summary.callgrind_instructions`,
  `runtime.callgrind_used`, `config.callgrind_repeat`,
  `config.aot`, BMF `callgrind_instructions` when measured).
- BMF: adds the 4th measure `callgrind_instructions`
  (value/lower/upper = summed per-case min/min/max, one-sided
  noise like `allocated_bytes`); per-shard reports keep the
  headroom the T11 comment reserves for a 4th measure.
- Summary table: adds a `cg_insn` column ONLY when some
  benchmark reports callgrind data (same conditional-column
  pattern as `cxp (p)`); without the new flags the table is
  byte-identical to Python's.
- Exit codes preserved: nonzero on any case failure or missing
  resources; outputs still written for partial upload.

### AOT transform rules (survey: 394 files, 115 with `(ns)`, ~308 with `(defn -main`)

1. Split top-level forms with `internal/cljread`.
2. Keep the original `(ns ...)` form when present, injecting
   `(:gen-class :main true)`; else emit
   `(ns <path-derived> (:gen-class :main true))`.
3. Keep `def`/`defn`/`defn-`/`defmacro`/`defmulti`/`defmethod`/
   `defprotocol`/`defrecord`/`deftype`/`declare`/`require`/`use`/
   `import`/`comment` forms verbatim, in order.
4. Move every other top-level (action) form into
   `(defn -aot-run [] ...)`; append
   `(defn -main [& _args] (-aot-run))`.
5. If the script defines `-main`, rename that defn to
   `script-main` and rewrite the symbol `-main`→`script-main`
   inside the moved action forms only (token-aware, never inside
   strings/comments). 31 scripts define variadic `-main` and
   need this rule.
6. Compile with `clojure.main -e "(compile '<ns>)"` into
   `.ci/aot-cache/<sha256(script)>/classes`; skip the compile
   when the marker (script hash + classpath fingerprint) hits.
7. Run `java -cp <classes>:<cp> <MungedNs>`; stdout must diff
   clean against the same expected output as script mode.

### Callgrind backend (gungraun methodology)

- One-shot per case (plus `--callgrind-repeat`, default 1,
  min kept): `valgrind --tool=callgrind --cache-sim=no
  --branch-sim=no --collect-jumps=no --callgrind-out-file=<tmp>
  <real-java> -Xint <EPSILON_FLAGS> -XX:-UsePerfData -cp ...`.
- Parse the `summary: N` line for total `Ir`
  (start-to-finish, JVM boot included, as requested).
- Invoke the resolved real `java` path: under valgrind the
  `/bin/java` alternatives symlink loses `libjli.so`
  (exit 127, probed); the tool resolves symlinks first.
- Perf `instructions` (JIT, hardware counters, null in CI)
  and `callgrind_instructions` (`-Xint`, simulated,
  works everywhere) are reported side by side, never merged.

### CI and caching

- `.github/actions/snip-tools/action.yml`: composite, mirrors
  `profile-runner` line by line (cache `snip-bin/`, key
  `snip-<os>-<hash(action, go.mod, cmd/snip/**,
  internal/** except executil-independent?)>` — final key
  covers exactly the sources the build reads; setup-go +
  build on miss only; no `working-directory` key anywhere).
- `test.yml`: each job gains the `snip-tools` step; `python3
  tests/run_tests.py` → `snip test`, `ssa_df_regression.py` →
  `snip ssa`, `tools/measure.py` → `snip measure` (same flags);
  `Install measurement tools` gains `valgrind` (apt cache
  action) so `--callgrind` pilots can run.
- CI cost policy: `perf` keeps equivalent flags (no callgrind);
  a parallel 4-shard `perf-instrumentation` sweep measures the full
  matrix with the lean recipe and reports stripped per-shard
  BMFs to Bencher; `--aot` stays a one-benchmark non-blocking
  `perf-pilot` job (own job with the cached binary, allowed to
  fail without blocking).
- Locally `go build ./cmd/snip` uses Go's build cache
  (no redundant rebuilds); AOT classes persist in
  `.ci/aot-cache/` (already gitignored via `.ci/`).

### Verification (TDD where machine-checkable)

- `go test ./...`: parser/fit/shard/BMF/merge/table unit
  tests ported from `--self-test` (same names where sensible).
- Generator byte-diffs: for every generator, Python
  `generate(n, seed)` vs Go at small + one full scale;
  all bytes equal (this is the port's proof).
- Harness parity: synthetic `resources/` (2 snippets × 1
  case, no passphrase needed) run under both Python and Go
  `measure`/`test`; JSON diff clean modulo timestamps/host.
- Live AOT: pilot snippets compile, stdout-identical,
  measured end to end; live callgrind: `Ir ≈ 6.2B` on the
  bubble_sort probe with summary + BMF entries present.
- `python3 -c yaml.safe_load` on the workflow (pre-deletion
  sanity), `git diff --check`, `gofmt -l` empty, `go vet`
  clean. Deletion of `.py`/`.sh` happens only after the Go
  tools pass parity.
