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

For automated validation, the shared test cases from the recipes superproject (`resource/shared`) ship as an encrypted bundle, `tests/resources.tar.xz.asc` (tar.xz, AES-256, ASCII-armored). `tests/run_tests.py` runs every `src/**/*.clj` script against the decrypted cases and diff-compares stdout against the expected output (trailing whitespace per line ignored).

### CI/CD Pipeline

`.github/workflows/test.yml` validates the snippets on push to `master`, pull requests, and manual dispatch. It downloads the Clojure jars, decrypts the bundle with the `RESOURCES_PASSPHRASE` repository secret, and runs:

```sh
python3 tests/run_tests.py --resources tests/resources --jobs 4 --timeout 300
```

To test locally, decrypt the bundle into `tests/resources/` (passphrase holders only) and run the same command. The Clojure classpath comes from `--cp` or `$CLOJURE_CP`; when neither is set, sibling `spec.alpha` / `core.specs.alpha` jars next to a jar named by `$CLOJURE_JAR` (or `.ci/jars/*.jar`) are picked up automatically.

Rebuild the bundle after resource changes with:

```sh
tools/build_bundle.sh <resource/shared dir> <passphrase-file>
```

See `tools/build_bundle.sh` for the rotation instructions when using a fresh passphrase.

## Performance tracking

`tools/measure.py` runs the same scripts and cases but records
load-independent metrics per case (Linux-only, needs GNU `time`,
optionally `perf`):

- `cpu_s` (user+sys CPU time) — primary time signal; min-of-repeats
- `maxrss_kb` (peak RSS) — primary memory signal; max-of-repeats
- `instructions`/`cycles` (`perf stat`, user-space) — most stable signal
- `wall_s` — secondary; not load-independent

Each case runs `--repeat` times (default 5, CI uses 3); summaries keep
min/median/max. Compare same-machine ratios only (e.g. Clojure vs native
back-to-back), never absolutes across machines.

```sh
python3 tools/measure.py --resources tests/resources --repeat 5 \
  --output perf.json --bmf-output bencher.json
python3 tools/measure.py --self-test  # parser checks, no JVM needed
```

CI runs correctness in the `test` job, then measures each configured
compiler in a sharded matrix `perf` job (`needs: test`, 4 shards via
`tools/measure.py --shard K/N`), uploads per-shard artifacts, and a
fan-in `report` job merges the BMF files, appends a results table to
the step summary, and reports once per runtime to Bencher
(`bencher run --adapter json --file bencher-bmf.json`), which plots each
snippet's `cpu_time`/`peak_memory` per commit and posts a check on the
head commit (the BMF payload carries only those two measures to fit
GitHub's check-summary limit; full metrics stay in the uploaded
`perf-results.json` artifact). The Bencher project API key lives
in the `BENCHER_API_KEY` repository secret. Clojure jars and apt
packages are cached between runs.

## Comparing compilers

Each compiler is a Bencher testbed (`ubuntu-latest-jvm` today); benchmark
names stay identical across compilers, so overlay the testbeds on
Bencher's perf page to compare them visually. To add a compiler:

1. Add a matrix entry in `.github/workflows/test.yml` (`runtime` label +
   `testbed` name) with whatever build steps it needs.
2. Point `tools/measure.py` at its invocation via `--runtime <label>
   --command-template '...'` (`{cp}`, `{script}`, `{stem}` holes); the
   same GNU `time` + `perf` harness measures every runtime, keeping
   numbers comparable.
