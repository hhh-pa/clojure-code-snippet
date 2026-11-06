# clojure-code-snippet

## Introduction

This piece of code is a curated collection of commonly used algorithms, aimed at reinforcing a software engineer's general algorithmic knowledge. Each snippet is implemented in a minimal and accessible manner, focusing on clarity of logic rather than production-readiness.

This repository is the Clojure port of the collection (see [cpp-code-snippets](https://github.com/m-hu/cpp-code-snippets)). Solutions mirror the other snippet repos: one script per problem at `src/<topic>/<problem>.clj`, executed as `java -cp <clojure.jar> clojure.main <script>` (stdin in, stdout out).

### Scope

The repository intentionally excludes production-level concerns such as:

- Memory management
- Error handling
- Vulnerability mitigation

These aspects, while important in real-world applications, are intentionally left out to keep the focus on core algorithmic logic. The assumption is that the intended viewers are primarily interested in the conceptual content itself, rather than scrutinizing the code for potential attack surfaces or production-level robustness.

### Privacy & Exposure

Some code examples have been moved to a **private repository** as a precautionary measure to limit exposure and reduce potential risk, especially when the repository is accessed in contexts where the intent of the viewer is uncertain.

### Test Cases

To respect the rights of original content creators, **test cases are not included** in this repository in plain form. Instead, they can be found on the websites where the referenced problems originate.

For automated validation, the shared test cases from the recipes superproject (`resource/shared`) ship as an encrypted bundle, `tests/resources.tar.xz.asc` (tar.xz, AES-256, ASCII-armored). `snip test` runs every `src/**/*.clj` script against the decrypted cases and diff-compares stdout against the expected output (trailing whitespace per line ignored).

### CI/CD Pipeline

`.github/workflows/test.yml` validates the snippets on push to `master`, pull requests, and manual dispatch. It downloads the Clojure jars, decrypts the bundle with the `RESOURCES_PASSPHRASE` organization secret (shared with the python/ocaml snippet repos), builds the `snip` tool once (cached per source hash), and runs:

```sh
snip test --resources tests/resources --jobs 4 --timeout 300
```

To test locally, build `snip` (`go build -o snip ./cmd/snip` in
`scripts/ci/go`; stdlib-only, no dependencies), decrypt the bundle
into `tests/resources/` (passphrase holders only) and run the same
command. The Clojure classpath comes from `--cp` or `$CLOJURE_CP`;
when neither is set, sibling `spec.alpha` / `core.specs.alpha` jars
next to a jar named by `$CLOJURE_JAR` (or `.ci/jars/*.jar`) are picked
up automatically.

Rebuild the bundle after resource changes with:

```sh
snip bundle <resource/shared dir> <passphrase-file>
```

See `snip bundle --help` for the rotation instructions when using a fresh passphrase.

## Performance tracking

`snip measure` runs the same scripts and cases but records
load-independent metrics per case (Linux-only, needs GNU `time`,
optionally `perf`):

- `cpu_s` (user+sys CPU time) — primary time signal; min-of-repeats
- `maxrss_kb` (peak RSS) — primary memory signal; max-of-repeats
- `allocated_bytes` (heap-used-at-exit under EpsilonGC, min of 3) —
  total allocation; load-independent gauge of algorithmic work
- `instructions`/`cycles` (`perf stat`, user-space) — most stable signal
- `callgrind_instructions` (`--callgrind` only, needs Valgrind) —
  deterministic whole-process Ir count (JVM: under `-Xint` +
  EpsilonGC; native: plain callgrind, no JVM flags)
- `callgrind_allocated_bytes` (`--callgrind` only) — total
  allocation of that same deterministic run: EpsilonGC total from
  the JVM's GC log on JVM legs (valgrind-side heap profilers
  cannot see Java objects), or the `LD_PRELOAD` counting shim's
  malloc-request total on native legs
- `wall_s` — secondary; not load-independent

Each case runs `--repeat` times (default 5) plus dedicated
EpsilonGC allocation runs unless `--no-alloc`; summaries keep
min/median/max. Compare same-machine ratios only (e.g. Clojure vs
native back-to-back), never absolutes across machines. JVM
allocated bytes are comparable within one JDK build only
(absolutes shift between JDK versions); native-leg bytes are
malloc-request totals — ratio-meaningful against the JVM number
per benchmark, not bit-comparable absolutes. `--aot` measures the
AOT-compiled classes instead of `clojure.main` interpretation
(gen-class transform, cached per source hash under
`.ci/aot-cache/`); `--callgrind` adds the one-shot deterministic
instrumentation (~21s per case all-in on CI hardware, measured
for all snippets by the sweep job). Cases exceeding the callgrind
budget, EpsilonGC-exempt workloads (JVM leg only), and native-leg
counter problems degrade to `callgrind_skipped` with a warning
instead of failing (the timed data stays valid); crashes and
output mismatches still fail.

```sh
snip measure --resources tests/resources --repeat 5 \
  --output perf.json --bmf-output bencher.json
snip measure --self-test  # parser checks, no JVM needed
```

CI runs correctness in the `test` job, then measures every snippet
with `--callgrind --pin` in a sharded `perf-instrumentation`
sweep job (`needs: test`, 8 shard jobs via `snip measure
--shard K/N`, lean `--repeat 1 --no-alloc` recipe, ~80min per job
callgrind-only baseline). Each shard job runs both runtime legs
sequentially on the same runner — same hardware, same checkout,
same resources — and a step verifies both legs measured exactly
the same cases before anything reports. Each
shard reports one BMF subset per runtime to Bencher (`bencher run --adapter
json --file sweep-bench-<runtime>.json`, carrying only the two gungraun-run
measures: `callgrind_instructions` thresholded at 1% plus
unthresholded `callgrind_allocated_bytes`) — per-shard reports keep
every check under GitHub's check-summary limit, where one merged
report would re-truncate it — and uploads per-shard artifacts; a
fan-in `report-instrumentation` job merges the full reports per runtime
(`snip measure --merge`), uploads the merged bundles, and appends
a combined results table to its step summary. A non-blocking
`perf-pilot` job keeps one `--aot` measurement per push as a
stdout-gate canary. Bencher plots each snippet's
`callgrind_instructions`/`callgrind_allocated_bytes` per commit
for both runtimes and posts a check per shard on the head commit
(full metrics stay in the uploaded `sweep-results-*.json`
artifacts). The Bencher project API key lives in the
`BENCHER_API_KEY` repository secret. Clojure jars, apt packages,
the cargo build, and the Pin kit + built inscount tool are cached
between runs.

## Comparing compilers

Each compiler is a Bencher testbed (`ubuntu-latest-jvm` for JVM
Clojure, `ubuntu-latest-clojure-rs` for the Rust port);
benchmark names stay identical across compilers, so overlay the
testbeds on Bencher's perf page to compare them visually. The
native leg always builds clojure-rs master HEAD (resolved once
per run so all 8 shard jobs share one revision; interpreter-only
`cargo build --release --locked --no-default-features` — `run`
needs no LLVM), since the sweep diff-gates every case and
Bencher shows the delta; a manual dispatch may pin
`clojure_rs_sha` to bisect or repro. To add a compiler:

1. Add a leg in `.github/workflows/test.yml` (another `snip
   measure` invocation plus upload step in perf-instrumentation,
   extend its verify/Bencher `jvm clojure-rs` loops, and a
   `report-instrumentation` matrix entry, testbed
   `ubuntu-latest-<runtime>`) with whatever build steps it needs.
2. Point `snip measure` at its invocation via `--runtime <label>
   --command-template '...'` (`{cp}`, `{script}`, `{stem}` holes);
   the same GNU `time` + `perf` harness measures every runtime,
   and non-`java` commands automatically take the native
   callgrind recipe (plain callgrind + `LD_PRELOAD` counting),
   keeping numbers comparable.
