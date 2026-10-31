#!/usr/bin/env python3
"""Load-independent performance measurement for the Clojure snippet repo.

Runs every src/**/*.clj script against the shared test cases (same layout
as tests/run_tests.py) and records metrics chosen to stay comparable across
runs, machine load, and machines:

  * cpu_s (user+sys from GNU time) -- counts only time the process spent
    on CPU, so competing load does not inflate it directly.
  * instructions / cycles (perf stat, user-space only) -- instruction
    count is nearly load-invariant for the same binary and input.
  * maxrss_kb (peak RSS from GNU time) -- competing CPU load does not
    change how much RAM a process uses.
  * allocated_bytes (heap-used-at-exit under EpsilonGC, min of
    --alloc-repeat runs) -- total allocation, the load-independent
    gauge of algorithmic work. Epsilon never collects, so nothing
    is ever freed and used-at-exit equals total bytes allocated.

Wall time is recorded too but is NOT load-independent; treat it as
secondary. Each case runs --repeat times; the summary keeps min/median/max
so the least-disturbed run (min CPU) and worst-case footprint (max RSS)
stay visible. Absolute numbers must only be compared as same-machine
ratios (e.g. Clojure vs native back-to-back); never compare absolutes
across different machines or runners. Allocated bytes are comparable
within one JDK build only -- absolutes shift between JDK versions
(e.g. the same case reports 193M on Temurin 21 vs 136M on Oracle 27)
-- so Bencher tracks them per testbed like every other metric.

Outputs:

  * --output: full JSON (per-run metrics, per-case summaries, failures).
  * --bmf-output: Bencher Metric Format JSON (one Benchmark per snippet,
    measures cpu_time/peak_memory/allocated_bytes -- wall_time and perf
    counters stay in the full report to keep Bencher's check summary
    under GitHub's size limit), consumable by
    `bencher run --adapter json --file ...` to track evolution in Bencher.
    CI reports each --shard's BMF separately: one merged report at 3
    measures would re-truncate the check summary (T11), while per-shard
    reports (~9 benchmarks x 3 measures) fit with headroom.
    Under --slope, covered benchmarks gain a sparse `complexity_exponent`
    measure (fitted power-law exponent; no threshold, informational
    trend) and the full report gains a "complexity" section.
  * --summary-file: Markdown table appended for GitHub step summaries.

Compiler comparison: benchmark names are compiler-agnostic on purpose.
Report each compiler as its own Bencher testbed (e.g. ubuntu-latest-jvm
vs ubuntu-latest-native) with identical benchmark names, then overlay the
testbeds on Bencher's perf page for visual comparison. Use --runtime to
label which compiler produced a report and --command-template to change
how snippets are invoked (default runs them on the JVM):

    python3 tools/measure.py --resources tests/resources --runtime native \
        --command-template './build/{stem}' ...

Available template placeholders: {cp}, {script}, {stem}. stdin still
comes from the test-case input file and stdout is still diffed, so any
runtime that honors the snippet stdin/stdout contract can be measured
with the exact same harness (GNU time + perf), keeping numbers comparable.

Usage:
    python3 tools/measure.py --resources tests/resources \
        [--repeat 5] [--jobs 1] [--timeout 300] [--only substring] \
        [--output perf.json] [--bmf-output bencher.json] \
        [--summary-file "$GITHUB_STEP_SUMMARY"] [--no-perf] \
        [--runtime jvm] [--command-template '...'] [--shard K/N] \
        [--alloc-repeat 3 | --no-alloc] [--slope]

    python3 tools/measure.py --merge shard-*.json --output merged.json \
        [--bmf-output bencher.json] [--summary-file summary.md]

    python3 tools/measure.py --self-test   # durable parser checks, no JVM needed

Linux-only: requires GNU time and (optionally) perf. perf counters degrade
gracefully to null when unavailable. Exit status is non-zero when any case
fails correctness or resources are missing (numbers for wrong output would
be meaningless); --output/--bmf-output are still written so CI can upload
partial results.
"""

import argparse
import datetime
import json
import os
import platform
import re
import shlex
import shutil
import socket
import statistics
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "tests"))
import run_tests

TIME_FORMAT = (
    "wall=%e user=%U sys=%S maxrss_kb=%M cpu_pct=%P "
    "major_faults=%F minor_faults=%R"
)
PERF_EVENTS = "instructions:u,cycles:u"
TOOL_VERSION = 2
DEFAULT_COMMAND_TEMPLATE = "java -cp {cp} clojure.main {script}"
# Allocation runs: EpsilonGC never collects, so heap-used-at-exit equals
# total allocation. Xms=Xmx avoids resize noise; 2g fits CI runners.
EPSILON_FLAGS = [
    "-XX:+UnlockExperimentalVMOptions",
    "-XX:+UseEpsilonGC",
    "-Xms2g",
    "-Xmx2g",
]
ALLOC_REPEAT_DEFAULT = 3
_UNIT_MULTIPLIERS = {"B": 1, "K": 1024, "M": 1024 ** 2,
                     "G": 1024 ** 3, "T": 1024 ** 4}
# E.g. "49824K used" (old format) or "136M (6.69%) used" (new format).
_EPSILON_USED_RE = re.compile(
    r"(\d+(?:\.\d+)?)\s*([BKMGT])(?:\s*\([^)]*\))?\s+used\b")


def parse_time_text(text):
    """Parse one GNU time -f TIME_FORMAT line into a metrics dict."""
    fields = {}
    for token in text.split():
        if "=" not in token:
            continue
        key, _, value = token.partition("=")
        fields[key] = value
    try:
        wall = float(fields["wall"])
        user = float(fields["user"])
        sys_s = float(fields["sys"])
        maxrss_kb = int(fields["maxrss_kb"])
        cpu_pct = float(fields["cpu_pct"].rstrip("%"))
        major = int(fields["major_faults"])
        minor = int(fields["minor_faults"])
    except (KeyError, ValueError) as exc:
        raise ValueError(
            "unparseable GNU time output: {!r}".format(text.strip())
        ) from exc
    return {
        "wall_s": wall,
        "user_s": user,
        "sys_s": sys_s,
        "cpu_s": user + sys_s,
        "maxrss_kb": maxrss_kb,
        "cpu_pct": cpu_pct,
        "major_faults": major,
        "minor_faults": minor,
    }


def parse_perf_text(text):
    """Parse perf stat -x, output; unsupported counters become None."""
    counters = {"instructions": None, "cycles": None}
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split(",")
        if len(parts) < 3:
            continue
        raw_value = parts[0].strip()
        base = parts[2].strip().split(":")[0]
        if base not in counters:
            continue
        if raw_value in ("", "not counted", "<not supported>", "<not counted>"):
            counters[base] = None
        else:
            try:
                counters[base] = int(raw_value)
            except ValueError:
                counters[base] = None
    return counters


def parse_epsilon_used(text):
    """Parse Epsilon -Xlog:gc output; return heap-used-at-exit in bytes.

    Takes the used value from the LAST `Heap:` line (earlier lines are
    mid-run states) and converts its K/M/G unit to bytes. Lines without
    `Heap:` (metaspace, gc,exit) never match, so verbose logs are safe.
    """
    found = None
    for line in text.splitlines():
        if "Heap:" not in line:
            continue
        for match in _EPSILON_USED_RE.finditer(line):
            found = match
    if found is None:
        raise ValueError(
            "no Epsilon Heap used value in: {!r}".format(text.strip()[-200:])
        )
    return int(float(found.group(1)) * _UNIT_MULTIPLIERS[found.group(2)])


def summarize_numbers(values):
    values = sorted(values)
    return {"min": values[0], "median": statistics.median(values),
            "max": values[-1]}


def parse_shard(text):
    """Parse K/N shard selector into (index, total), 0-based index."""
    try:
        k, _, n = text.partition("/")
        index, total = int(k), int(n)
    except (TypeError, ValueError) as exc:
        raise ValueError("bad --shard {!r}: want K/N".format(text)) from exc
    if total < 1 or not 0 <= index < total:
        raise ValueError(
            "bad --shard {!r}: need 0 <= K < N".format(text))
    return index, total


def select_shard(tasks, index, total):
    """Deterministically keep every total-th snippet starting at index.

    Sharding is by SNIPPET (label/stem), never by case: each snippet's
    cases stay together in one shard, so per-shard BMF aggregates are
    complete per snippet and the fan-in `jq -s add` merge is exact.
    (Splitting cases of one snippet across shards would leave each
    shard with a partial per-snippet aggregate that the merge would
    silently overwrite — undercounting cpu_time.)
    """
    groups = {}
    for t in tasks:
        groups.setdefault((t[0], t[1]), []).append(t)
    keep = {g for i, g in enumerate(sorted(groups)) if i % total == index}
    return [t for t in tasks if (t[0], t[1]) in keep]


def summarize_case(runs):
    """Aggregate per-run metric dicts into min/median/max summary."""
    if not runs:
        return None
    summary = {
        "cpu_s": summarize_numbers([r["cpu_s"] for r in runs]),
        "maxrss_kb": summarize_numbers([r["maxrss_kb"] for r in runs]),
        "wall_s": summarize_numbers([r["wall_s"] for r in runs]),
        "runs": len(runs),
    }
    insn = [r["instructions"] for r in runs if r.get("instructions") is not None]
    cyc = [r["cycles"] for r in runs if r.get("cycles") is not None]
    summary["instructions"] = summarize_numbers(insn) if insn else None
    summary["cycles"] = summarize_numbers(cyc) if cyc else None
    return summary


def summarize_alloc(byte_values):
    """Aggregate per-run allocated-byte counts into a summary dict.

    Besides min/median/max this records a `deterministic` flag (all
    runs identical) so allocation stability stays auditable in the
    full report. The measurement noise is one-sided -- TLAB waste
    and allocation interleaving only inflate used-at-exit -- so BMF
    aggregation uses the MINIMUM as the point estimate of true
    total allocation.
    """
    values = sorted(byte_values)
    return {"min": values[0], "median": statistics.median(values),
            "max": values[-1],
            "deterministic": len(set(values)) == 1}


def resolve_time_bin(explicit):
    if explicit:
        path = explicit
    elif os.path.exists("/usr/bin/time"):
        path = "/usr/bin/time"
    else:
        path = shutil.which("time")
    if not path:
        raise SystemExit(
            "GNU time not found: install it (Debian/Ubuntu: apt install time) "
            "or pass --time-bin"
        )
    try:
        proc = subprocess.run(
            [path, "--version"],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=10,
        )
    except OSError as exc:
        raise SystemExit("cannot execute time binary {!r}: {}".format(path, exc))
    if b"GNU" not in proc.stdout:
        raise SystemExit(
            "{!r} is not GNU time (need -f/-o support); install GNU time "
            "or pass --time-bin".format(path)
        )
    return path


def perf_usable():
    """True when perf can count user-space instructions/cycles here."""
    if shutil.which("perf") is None:
        return False
    fd, tmp = tempfile.mkstemp(prefix="perf-probe-")
    os.close(fd)
    try:
        proc = subprocess.run(
            ["perf", "stat", "-e", PERF_EVENTS, "-x,", "-o", tmp, "--", "true"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=30,
        )
        if proc.returncode != 0:
            return False
        with open(tmp) as handle:
            counters = parse_perf_text(handle.read())
        return counters["instructions"] is not None or counters["cycles"] is not None
    except OSError:
        return False
    finally:
        try:
            os.unlink(tmp)
        except OSError:
            pass


def host_info():
    info = {
        "hostname": socket.gethostname(),
        "os": platform.platform(),
        "nproc": os.cpu_count(),
        "cpu_model": None,
        "mem_total_kb": None,
    }
    try:
        with open("/proc/cpuinfo") as handle:
            for line in handle:
                for key in ("model name", "Model", "Hardware"):
                    if line.startswith(key):
                        info["cpu_model"] = line.split(":", 1)[1].strip()
                        break
                if info["cpu_model"]:
                    break
    except OSError:
        pass
    try:
        with open("/proc/meminfo") as handle:
            for line in handle:
                if line.startswith("MemTotal:"):
                    info["mem_total_kb"] = int(line.split()[1])
                    break
    except (OSError, ValueError, IndexError):
        pass
    return info


def java_version():
    try:
        proc = subprocess.run(
            ["java", "-version"],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=30,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    out = proc.stdout.decode(errors="ignore").strip().splitlines()
    return out[0] if out else None


def build_inner_command(template, classpath, script, stem):
    """Expand a --command-template into the snippet argv (no shell)."""
    return shlex.split(template.format(cp=classpath, script=script, stem=stem))


def build_command(time_bin, time_file, use_perf, perf_file, inner):
    # GNU time must wrap the snippet directly: its max-RSS covers only the
    # direct child, so perf goes outside and time inside.
    timed = [time_bin, "-f", TIME_FORMAT, "-o", time_file] + inner
    if use_perf:
        return ["perf", "stat", "-e", PERF_EVENTS, "-x,", "-o", perf_file,
                "--"] + timed
    return timed


def measure_once(time_bin, use_perf, template, classpath, script, stem,
                 input_path, timeout):
    """Run script once under GNU time (+perf). Return (stdout, metrics, err)."""
    try:
        inner = build_inner_command(template, classpath, script, stem)
    except (KeyError, ValueError) as exc:
        return None, None, "bad command template: {}".format(exc)
    if not inner:
        return None, None, "bad command template: expands to empty argv"
    time_fd, time_file = tempfile.mkstemp(prefix="snip-time-")
    os.close(time_fd)
    perf_file = None
    if use_perf:
        perf_fd, perf_file = tempfile.mkstemp(prefix="snip-perf-")
        os.close(perf_fd)
    try:
        cmd = build_command(time_bin, time_file, use_perf, perf_file, inner)
        with open(input_path, "rb") as stdin_fd:
            try:
                proc = subprocess.run(
                    cmd, stdin=stdin_fd,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                    timeout=timeout,
                )
            except subprocess.TimeoutExpired:
                return None, None, "TIMEOUT (>{}s)".format(timeout)
        if proc.returncode != 0:
            tail = proc.stderr.decode(errors="ignore").strip().splitlines()
            return None, None, "exit {}: {}".format(
                proc.returncode, tail[-1] if tail else "no stderr")
        try:
            with open(time_file) as handle:
                metrics = parse_time_text(handle.read())
        except (OSError, ValueError) as exc:
            return None, None, "time output unreadable: {}".format(exc)
        if use_perf:
            try:
                with open(perf_file) as handle:
                    metrics.update(parse_perf_text(handle.read()))
            except OSError as exc:
                return None, None, "perf output unreadable: {}".format(exc)
        else:
            metrics["instructions"] = None
            metrics["cycles"] = None
        return proc.stdout, metrics, None
    finally:
        for path in (time_file, perf_file):
            if path:
                try:
                    os.unlink(path)
                except OSError:
                    pass


def alloc_supported(template, classpath, script, stem):
    """True when the template invokes a JVM (Epsilon flags apply).

    Non-JVM runtimes (a future native template) skip allocation
    measurement: their summaries record None and the BMF omits the
    measure for those snippets.
    """
    try:
        inner = build_inner_command(template, classpath, script, stem)
    except (KeyError, ValueError):
        return False
    return bool(inner) and os.path.basename(inner[0]) == "java"


def measure_alloc_once(template, classpath, script, stem,
                       input_path, timeout):
    """Run script once under EpsilonGC. Return (stdout, bytes, err).

    The gc log goes to a file (-Xlog:gc:<file>) so program stdout
    stays clean for the correctness diff on every JDK (unified
    logging's default stream differs: stderr on some, stdout on
    Oracle 27).
    """
    try:
        inner = build_inner_command(template, classpath, script, stem)
    except (KeyError, ValueError) as exc:
        return None, None, "bad command template: {}".format(exc)
    if not inner:
        return None, None, "bad command template: expands to empty argv"
    gc_fd, gc_file = tempfile.mkstemp(prefix="snip-gc-")
    os.close(gc_fd)
    cmd = [inner[0]] + EPSILON_FLAGS + ["-Xlog:gc:" + gc_file] + inner[1:]
    try:
        with open(input_path, "rb") as stdin_fd:
            try:
                proc = subprocess.run(
                    cmd, stdin=stdin_fd,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                    timeout=timeout,
                )
            except subprocess.TimeoutExpired:
                return None, None, "TIMEOUT (>{}s)".format(timeout)
        if proc.returncode != 0:
            err_text = proc.stderr.decode(errors="ignore")
            tail = err_text.strip().splitlines()
            msg = "exit {}: {}".format(
                proc.returncode, tail[-1] if tail else "no stderr")
            if "OutOfMemoryError" in err_text:
                msg += " (Epsilon heap exhausted; raise -Xmx in EPSILON_FLAGS)"
            return None, None, msg
        try:
            with open(gc_file) as handle:
                used = parse_epsilon_used(handle.read())
        except (OSError, ValueError) as exc:
            return None, None, "epsilon log unreadable: {}".format(exc)
        return proc.stdout, used, None
    finally:
        try:
            os.unlink(gc_file)
        except OSError:
            pass


def measure_case(time_bin, use_perf, template, classpath, label, stem, script,
                 case_name, input_path, output_path, repeat, alloc_repeat,
                 timeout):
    result = {
        "label": label,
        "stem": stem,
        "script": os.path.relpath(script, ROOT),
        "case": case_name,
        "ok": False,
        "error": None,
        "runs": [],
        "alloc_runs": [],
        "summary": None,
    }
    with open(output_path, "rb") as fd:
        expected = run_tests.normalize(fd.read())
    for _ in range(repeat):
        stdout, metrics, error = measure_once(
            time_bin, use_perf, template, classpath, script, stem,
            input_path, timeout)
        if error is not None:
            result["error"] = "{}/{}: {}".format(label, case_name, error)
            return result
        if not result["runs"]:
            if run_tests.normalize(stdout) != expected:
                got = run_tests.normalize(stdout).decode(errors="ignore")
                want = expected.decode(errors="ignore")
                result["error"] = "{}/{}: output mismatch: expected {!r} got {!r}".format(
                    label, case_name, want[:200], got[:200])
                return result
        result["runs"].append(metrics)
    result["summary"] = summarize_case(result["runs"])
    # Dedicated allocation runs: separate JVM invocations alongside --
    # never replacing -- the timed repeats above, so cpu/memory
    # baselines are untouched.
    if alloc_repeat and alloc_supported(template, classpath, script, stem):
        for i in range(alloc_repeat):
            stdout, used, error = measure_alloc_once(
                template, classpath, script, stem, input_path, timeout)
            if error is not None:
                result["error"] = "{}/{}: alloc: {}".format(
                    label, case_name, error)
                return result
            if i == 0 and run_tests.normalize(stdout) != expected:
                got = run_tests.normalize(stdout).decode(errors="ignore")
                want = expected.decode(errors="ignore")
                result["error"] = "{}/{}: alloc output mismatch: expected {!r} got {!r}".format(
                    label, case_name, want[:200], got[:200])
                return result
            result["alloc_runs"].append(used)
        result["summary"]["allocated_bytes"] = summarize_alloc(
            result["alloc_runs"])
    else:
        result["summary"]["allocated_bytes"] = None
    result["ok"] = True
    return result


def build_bmf(results, complexity=None):
    """Aggregate per-case summaries into Bencher Metric Format JSON.

    One Benchmark per snippet (label/stem) with EXACTLY the measures
    cpu_time, peak_memory, and allocated_bytes: wall_time is dropped
    (load-dependent on shared runners; cpu_time is the stable proxy)
    and instructions/cycles are never emitted even when perf data is
    present (always null on hosted runners, and extra rows re-truncate
    Bencher's GitHub check summary). cpu_time/peak_memory use the
    median as value with min/max as lower/upper bounds so Bencher
    tracks the robust middle while keeping the spread visible;
    allocated_bytes instead uses the MINIMUM as value (and lower
    bound) because its noise is one-sided (see summarize_alloc).
    cpu_time and allocated_bytes are summed over a snippet's cases;
    peak_memory takes the worst case (max). Memory measures are in
    bytes. Snippets without allocation data (non-JVM runtimes, or
    pre-T19 reports) omit allocated_bytes. When complexity (T20
    slope results) is given, covered benchmarks with a successful
    fit gain a sparse fourth measure, complexity_exponent (slope
    with its leave-one-out band).
    """
    per_snippet = {}
    for res in results:
        if not res["ok"] or not res["summary"]:
            continue
        key = "{}/{}".format(res["label"], res["stem"])
        per_snippet.setdefault(key, []).append(res["summary"])
    bmf = {}
    for name in sorted(per_snippet):
        summaries = per_snippet[name]
        measures = {
            "cpu_time": {
                "value": sum(s["cpu_s"]["median"] for s in summaries),
                "lower_value": sum(s["cpu_s"]["min"] for s in summaries),
                "upper_value": sum(s["cpu_s"]["max"] for s in summaries),
            },
            "peak_memory": {
                "value": max(s["maxrss_kb"]["median"] for s in summaries) * 1024,
                "lower_value": max(s["maxrss_kb"]["min"] for s in summaries) * 1024,
                "upper_value": max(s["maxrss_kb"]["max"] for s in summaries) * 1024,
            },
        }
        alloc = [s.get("allocated_bytes") for s in summaries
                 if s.get("allocated_bytes")]
        if alloc and len(alloc) == len(summaries):
            measures["allocated_bytes"] = {
                "value": sum(a["min"] for a in alloc),
                "lower_value": sum(a["min"] for a in alloc),
                "upper_value": sum(a["max"] for a in alloc),
            }
        bmf[name] = measures
    # ---- T20 hook: sparse complexity_exponent for covered benchmarks. ----
    for name in sorted(complexity or {}):
        entry = (complexity or {})[name]
        fit = entry.get("fit") or {}
        if not entry.get("ok") or not fit.get("ok"):
            continue
        bmf.setdefault(name, {})["complexity_exponent"] = {
            "value": fit["slope"],
            "lower_value": fit["lower"],
            "upper_value": fit["upper"],
        }
    return bmf


def merge_reports(paths):
    """Concatenate full perf-report JSONs into one (report, bmf) pair.

    Used by the CI fan-in job to reunite per-shard reports into a
    single merged artifact and combined step summary. Shards are
    disjoint by snippet, so concatenation is exact; duplicate
    (label/stem/case) entries (e.g. the same file passed twice) keep
    the first copy. Header fields (host/runtime/config) come from
    the first report with the shard tag replaced by "merged".
    Complexity (T20 slope) sections union the same way (first wins),
    so the merged BMF keeps every shard's complexity_exponent and
    still equals the per-shard BMF union.
    """
    merged = []
    seen = set()
    failures = []
    missing = set()
    complexity = {}
    base = None
    for path in paths:
        try:
            with open(path) as fd:
                rep = json.load(fd)
        except (OSError, ValueError) as exc:
            raise SystemExit("cannot read report {}: {}".format(path, exc))
        if base is None:
            base = rep
        for res in rep.get("results", []):
            key = (res.get("label"), res.get("stem"), res.get("case"))
            if key in seen:
                continue
            seen.add(key)
            merged.append(res)
        failures.extend(rep.get("failures", []))
        missing.update(rep.get("missing", []))
        for name, entry in rep.get("complexity", {}).items():
            complexity.setdefault(name, entry)
    merged.sort(key=lambda r: (r.get("label"), r.get("stem"), r.get("case")))
    report = dict(base) if base else {}
    report["results"] = merged
    report["failures"] = sorted(set(failures))
    report["missing"] = sorted(missing)
    report["complexity"] = complexity
    config = dict(report.get("config", {}))
    config["shard"] = "merged"
    report["config"] = config
    return report, build_bmf(merged, complexity)


def write_json(path, payload):
    try:
        with open(path, "w") as fd:
            json.dump(payload, fd, indent=2)
            fd.write("\n")
    except OSError as exc:
        raise SystemExit("cannot write {}: {}".format(path, exc))


def print_table(results, failures, missing):
    for res in results:
        tag = "{}/{}/{}".format(res["label"], res["stem"], res["case"])
        if not res["ok"]:
            print("FAIL", res["error"])
            continue
        s = res["summary"]
        insn = s["instructions"]["median"] if s["instructions"] else None
        ab = s.get("allocated_bytes")
        alloc = "{}B".format(ab["min"]) if ab else "n/a"
        print("{}: ok cpu_min={:.3f}s cpu_med={:.3f}s rss_max={}K wall_min={:.3f}s insn_med={} alloc_min={}".format(
            tag, s["cpu_s"]["min"], s["cpu_s"]["median"],
            s["maxrss_kb"]["max"], s["wall_s"]["min"], insn, alloc))
    for label in missing:
        print("FAIL {}: no resources found".format(label))
    ok = sum(1 for r in results if r["ok"])
    print("measure: {} ok, {} failed, {} missing-resource problems".format(
        ok, len(failures) - len(missing), len(missing)))


def slope_file_lines(complexity):
    """Markdown bullet lines for a complexity dict (T20 follow-up).

    Shared by the measure and merge paths so the slope detail block
    renders identically in per-shard and merged step summaries.
    Returns [] when there is nothing to report (keeps no-slope
    output byte-comparable).
    """
    lines = []
    if not complexity:
        return lines
    lines.extend(["", "**Complexity exponents (T20):**", ""])
    for name in sorted(complexity):
        entry = complexity[name]
        fit = entry.get("fit") or {}
        if entry.get("ok") and fit.get("ok"):
            lines.append("- {}: p = {:.3f} [{:.3f}, {:.3f}], "
                         "R^2 = {:.4f}".format(
                             name, fit["slope"], fit["lower"],
                             fit["upper"], fit["r_squared"]))
        else:
            lines.append("- {}: skipped ({})".format(
                name, entry.get("error")))
    return lines


def append_summary(path, report, bmf):
    host = report["host"]
    runtime = report["runtime"]
    results = report["results"]
    ok = sum(1 for r in results if r["ok"])
    # Complexity column only when some benchmark actually reports
    # an exponent: without --slope (or with all fits skipped) the
    # table stays byte-identical to before.
    show_cxp = any("complexity_exponent" in (m or {})
                   for m in (bmf or {}).values())
    header = ("| snippet | cases | cpu total (s) | peak RSS (MB) | "
              "alloc total (MB) | instructions | wall total (s) |")
    sep = "| --- | ---: | ---: | ---: | ---: | ---: | ---: |"
    if show_cxp:
        header = header[:-2] + " | cxp (p) |"
        sep = sep[:-2] + " | ---: |"
    lines = [
        "## Perf measurement (`tools/measure.py`)",
        "",
        "- {} ({}), java: {}".format(
            report["timestamp_utc"], host.get("cpu_model") or "unknown cpu",
            runtime["java_version"] or "unknown"),
        "- runtime: {}, perf counters: {}, repeat: {}, jobs: {}, alloc_repeat: {}".format(
            runtime["name"], "yes" if runtime["perf_used"] else "no",
            report["config"]["repeat"], report["config"]["jobs"],
            report["config"].get("alloc_repeat", "n/a")),
        "- {} ok, {} failed, {} missing-resource problems".format(
            ok, len(report["failures"]) - len(report["missing"]),
            len(report["missing"])),
        "- Primary signals: min CPU time, max peak RSS, min allocated "
        "bytes, median instructions (load-independent); wall time is "
        "secondary. Compare same-machine ratios only.",
        "",
        header,
        sep,
    ]
    per_snippet_cases = {}
    per_snippet_wall = {}
    per_snippet_insn = {}
    for res in results:
        if res["ok"]:
            key = "{}/{}".format(res["label"], res["stem"])
            per_snippet_cases[key] = per_snippet_cases.get(key, 0) + 1
            # Wall time and instructions are not in the BMF payload
            # (T11: Bencher report hygiene), so total them here from
            # the full per-case summaries to keep this table complete.
            per_snippet_wall[key] = (per_snippet_wall.get(key, 0.0)
                                     + res["summary"]["wall_s"]["median"])
            ins = res["summary"]["instructions"]
            per_snippet_insn.setdefault(key, []).append(
                ins["median"] if ins else None)
    for name in sorted(bmf):
        m = bmf[name]
        parts = per_snippet_insn.get(name, [])
        insn = (sum(parts) if parts and all(p is not None for p in parts)
                else "n/a")
        alloc = m.get("allocated_bytes")
        alloc_mb = ("{:.1f}".format(alloc["value"] / 1048576)
                    if alloc else "n/a")
        row = ("| {} | {} | {:.3f} | {:.1f} | {} | {} | {:.3f} |".format(
            name, per_snippet_cases.get(name, 0),
            m["cpu_time"]["value"], m["peak_memory"]["value"] / 1048576,
            alloc_mb, insn, per_snippet_wall.get(name, 0.0)))
        if show_cxp:
            cxp = (m.get("complexity_exponent") or {}).get("value")
            row = row[:-2] + " | {} |".format(
                "{:.2f}".format(cxp) if cxp is not None else "n/a")
        lines.append(row)
    lines.append("")
    try:
        with open(path, "a") as fd:
            fd.write("\n".join(lines))
    except OSError as exc:
        raise SystemExit("cannot append summary to {}: {}".format(path, exc))


def self_test():
    """Durable parser/aggregator checks; needs no JVM, time, or perf."""
    failures = []

    def check(name, cond, detail=""):
        print("{:4s} {}".format("PASS" if cond else "FAIL", name))
        if not cond:
            failures.append(name)
            if detail:
                print("     {}".format(detail))

    m = parse_time_text(
        "wall=1.23 user=0.90 sys=0.10 maxrss_kb=123456 cpu_pct=81% "
        "major_faults=2 minor_faults=300\n")
    check("time-parse",
          m["wall_s"] == 1.23 and m["cpu_s"] == 1.0
          and m["maxrss_kb"] == 123456 and m["cpu_pct"] == 81.0
          and m["major_faults"] == 2 and m["minor_faults"] == 300, repr(m))
    try:
        parse_time_text("garbage line\n")
        check("time-parse-rejects", False, "no ValueError raised")
    except ValueError:
        check("time-parse-rejects", True)

    p = parse_perf_text(
        "134041,,instructions:u,525440,100.00,0.50,insn per cycle\n"
        "266896,,cycles:u,525440,100.00,,\n")
    check("perf-parse", p == {"instructions": 134041, "cycles": 266896}, repr(p))
    p2 = parse_perf_text(
        "<not supported>,,instructions:u,,,\nnot counted,,cycles:u,,,\n")
    check("perf-parse-unsupported",
          p2 == {"instructions": None, "cycles": None}, repr(p2))

    inner = build_inner_command(DEFAULT_COMMAND_TEMPLATE, "/a/b.jar",
                                "/s/x.clj", "x")
    check("template-default",
          inner == ["java", "-cp", "/a/b.jar", "clojure.main", "/s/x.clj"],
          repr(inner))
    inner2 = build_inner_command("./build/{stem}", "/a", "/s/y.clj", "y")
    check("template-stem", inner2 == ["./build/y"], repr(inner2))

    check("shard-parse", parse_shard("1/4") == (1, 4))
    bad = 0
    for text in ("4/4", "-1/4", "0/0", "x", "1/2/3", ""):
        try:
            parse_shard(text)
        except ValueError:
            bad += 1
    check("shard-parse-rejects", bad == 6, str(bad))
    toy = [("b", "s", "c", "x", "i", "o"), ("a", "s", "c", "x", "i", "o"),
           ("c", "s", "c", "x", "i", "o"), ("a", "s", "b", "x", "i", "o")]
    s0 = select_shard(toy, 0, 2)
    s1 = select_shard(toy, 1, 2)
    check("shard-split",
          sorted(s0 + s1) == sorted(toy) and not set(s0) & set(s1)
          and sorted((t[0], t[1]) for t in s0) == [("a", "s"), ("a", "s"), ("c", "s")]
          and [(t[0], t[1]) for t in s1] == [("b", "s")], repr((s0, s1)))
    # Snippet cohesion: a snippet's cases never split across shards,
    # so per-shard BMF aggregates merge exactly with jq -s add.
    def snippets(tasks):
        return {(t[0], t[1]) for t in tasks}
    check("shard-cohesion",
          not snippets(s0) & snippets(s1)
          and snippets(s0) | snippets(s1) == snippets(toy),
          repr((snippets(s0), snippets(s1))))

    runs = [
        {"cpu_s": 1.0, "maxrss_kb": 100, "wall_s": 1.2,
         "instructions": 10, "cycles": 20},
        {"cpu_s": 2.0, "maxrss_kb": 110, "wall_s": 2.2,
         "instructions": 12, "cycles": 22},
        {"cpu_s": 3.0, "maxrss_kb": 105, "wall_s": 3.2,
         "instructions": None, "cycles": None},
    ]
    s = summarize_case(runs)
    check("summarize",
          s["cpu_s"] == {"min": 1.0, "median": 2.0, "max": 3.0}
          and s["maxrss_kb"]["median"] == 105
          and s["instructions"] == {"min": 10, "median": 11.0, "max": 12},
          repr(s))
    check("summarize-empty", summarize_case([]) is None)

    bmf = build_bmf([
        {"ok": True, "label": "sort/quick_sort", "stem": "solution",
         "summary": s},
        {"ok": False, "label": "sort/quick_sort", "stem": "solution",
         "summary": None},
    ])
    entry = bmf.get("sort/quick_sort/solution", {})
    # The fixture summary carries instructions data, so the exact key
    # set also proves perf counters are never emitted (T11); it lacks
    # allocated_bytes, which proves graceful omission for pre-T19 and
    # non-JVM summaries.
    check("bmf",
          set(entry) == {"cpu_time", "peak_memory"}
          and entry.get("cpu_time") == {"value": 2.0, "lower_value": 1.0,
                                        "upper_value": 3.0}
          and entry.get("peak_memory") == {"value": 105 * 1024,
                                           "lower_value": 100 * 1024,
                                           "upper_value": 110 * 1024},
          repr(entry))

    new_log = (
        "[0.002s][info][gc] Using Epsilon\n"
        "[1.053s][info][gc] Heap: 2048M reserved, 2048M (100.00%) "
        "committed, 103M (5.05%) used\n"
        "[1.232s][info][gc] Heap: 2048M reserved, 2048M (100.00%) "
        "committed, 136M (6.69%) used\n")
    check("epsilon-parse-new",
          parse_epsilon_used(new_log) == 136 * 1024 ** 2,
          repr(parse_epsilon_used(new_log)))
    old_log = ("[0.5s][gc] Heap: 2048M reserved, 134M committed, "
               "49824K used\n")
    check("epsilon-parse-old",
          parse_epsilon_used(old_log) == 49824 * 1024)
    check("epsilon-parse-g",
          parse_epsilon_used("Heap: 8G reserved, 8G committed, 2G used\n")
          == 2 * 1024 ** 3
          and parse_epsilon_used("Heap: 8G reserved, 8G committed, "
                                 "1.5G (18.75%) used\n")
          == int(1.5 * 1024 ** 3))
    # Verbose (gc*) logs interleave metaspace/exit "used" lines; only
    # Heap: lines count, and the last one wins.
    verbose_log = (
        "[1.2s][info][gc] Heap: 2048M reserved, 2048M committed, "
        "100M used\n"
        "[1.2s][info][gc,metaspace] Metaspace: 1088M reserved, 12480K "
        "committed, 12218K used\n"
        "[1.3s][info][gc] Heap: 2048M reserved, 2048M committed, "
        "137M used\n"
        "[1.3s][info][gc,exit]    space 2097152K,   6% used "
        "[0x0000000080000000, 0x00000000889b98b8, 0x0000000100000000)\n")
    check("epsilon-parse-last-heap-only",
          parse_epsilon_used(verbose_log) == 137 * 1024 ** 2)
    bad_logs = ["", "garbage\n", "Heap: broken line\n",
                "Heap: 2048M reserved, 134M committed, 136X used\n",
                "Metaspace: 1088M reserved, 12218K used\n"]
    bad = 0
    for text in bad_logs:
        try:
            parse_epsilon_used(text)
        except ValueError:
            bad += 1
    check("epsilon-parse-rejects", bad == len(bad_logs), str(bad))

    det = summarize_alloc([61917 * 1024] * 3)
    wob = summarize_alloc([61626 * 1024, 61628 * 1024, 61628 * 1024])
    check("alloc-deterministic",
          det == {"min": 61917 * 1024, "median": 61917 * 1024,
                  "max": 61917 * 1024, "deterministic": True}, repr(det))
    check("alloc-wobble-min",
          wob["min"] == 61626 * 1024 and wob["max"] == 61628 * 1024
          and wob["deterministic"] is False, repr(wob))

    s1 = dict(s, allocated_bytes=summarize_alloc([300, 100, 200]))
    s2 = dict(s, allocated_bytes=summarize_alloc([400, 400, 400]))
    bmf2 = build_bmf([
        {"ok": True, "label": "sort/quick_sort", "stem": "solution",
         "summary": s1},
        {"ok": True, "label": "sort/quick_sort", "stem": "solution2",
         "summary": s2},
    ])
    entry2 = bmf2.get("sort/quick_sort/solution", {})
    # Value is the sum of per-case MINIMUMS (one-sided noise); two
    # snippets prove per-snippet aggregation still splits by stem.
    check("bmf-alloc",
          set(entry2) == {"cpu_time", "peak_memory", "allocated_bytes"}
          and entry2.get("allocated_bytes") == {"value": 100,
                                                "lower_value": 100,
                                                "upper_value": 300}
          and bmf2.get("sort/quick_sort/solution2", {}).get(
              "allocated_bytes", {}).get("value") == 400,
          repr(entry2))

    fd, shard_a = tempfile.mkstemp(prefix="bmf-shard-a-", suffix=".json")
    os.close(fd)
    fd, shard_b = tempfile.mkstemp(prefix="bmf-shard-b-", suffix=".json")
    os.close(fd)
    # T20 slope entries, one per shard: the merge must union them so
    # the merged BMF keeps every shard's complexity_exponent.
    cx_a = {"a/s": {"ok": True,
                    "fit": {"ok": True, "slope": 1.0,
                            "lower": 0.9, "upper": 1.1}}}
    cx_b = {"b/s": {"ok": True,
                    "fit": {"ok": True, "slope": 2.0,
                            "lower": 1.9, "upper": 2.1}}}
    try:
        with open(shard_a, "w") as handle:
            json.dump({"tool": "t", "config": {"shard": "0/2"},
                       "results": [
                           {"ok": True, "label": "a", "stem": "s",
                            "case": "c1", "summary": s1},
                           {"ok": True, "label": "a", "stem": "s",
                            "case": "c2", "summary": s1}],
                       "failures": ["x: no resources found"],
                       "missing": ["x"], "complexity": cx_a}, handle)
        with open(shard_b, "w") as handle:
            json.dump({"tool": "t", "config": {"shard": "1/2"},
                       "results": [
                           {"ok": True, "label": "b", "stem": "s",
                            "case": "c1", "summary": s2}],
                       "failures": ["x: no resources found"],
                       "missing": ["x", "y"], "complexity": cx_b}, handle)
        merged, merged_bmf = merge_reports([shard_a, shard_b])
        whole_bmf = build_bmf(
            [{"ok": True, "label": "a", "stem": "s", "case": "c1",
              "summary": s1},
             {"ok": True, "label": "a", "stem": "s", "case": "c2",
              "summary": s1},
             {"ok": True, "label": "b", "stem": "s", "case": "c1",
              "summary": s2}],
            {"a/s": cx_a["a/s"], "b/s": cx_b["b/s"]})
        check("merge",
              merged_bmf == whole_bmf
              and merged["config"]["shard"] == "merged"
              and merged["failures"] == ["x: no resources found"]
              and merged["missing"] == ["x", "y"]
              and merged["complexity"]
              == {"a/s": cx_a["a/s"], "b/s": cx_b["b/s"]}
              and merged_bmf["a/s"]["complexity_exponent"]["value"] == 1.0
              and merged_bmf["b/s"]["complexity_exponent"]["value"] == 2.0
              and [(r["label"], r["case"]) for r in merged["results"]]
              == [("a", "c1"), ("a", "c2"), ("b", "c1")],
              repr(sorted(merged_bmf)))
        # Merging the same shard twice must not double-count.
        merged2, merged_bmf2 = merge_reports([shard_a, shard_a])
        check("merge-dedupe",
              merged_bmf2 == build_bmf(
                  [{"ok": True, "label": "a", "stem": "s",
                    "case": "c1", "summary": s1},
                   {"ok": True, "label": "a", "stem": "s",
                    "case": "c2", "summary": s1}],
                  cx_a)
              and len(merged2["results"]) == 2
              and merged2["complexity"] == cx_a)
    finally:
        for path in (shard_a, shard_b):
            try:
                os.unlink(path)
            except OSError:
                pass
    # ---- Summary table complexity column + shared slope block. ----
    cx_bmf = {
        "a/s": {"cpu_time": {"value": 1.0},
                "peak_memory": {"value": 1048576.0},
                "allocated_bytes": {"value": 2097152.0},
                "complexity_exponent": {"value": 2.0}},
        "b/s": {"cpu_time": {"value": 2.0},
                "peak_memory": {"value": 1048576.0,
                                "lower_value": 1.0, "upper_value": 3.0}},
    }
    cx_rep = {"timestamp_utc": "t", "host": {"cpu_model": "c"},
              "runtime": {"name": "jvm", "perf_used": False,
                          "java_version": "j"},
              "config": {"repeat": 1, "jobs": 1},
              "results": [], "failures": [], "missing": []}
    fd, cx_path = tempfile.mkstemp(prefix="cxp-summary-")
    os.close(fd)
    try:
        append_summary(cx_path, cx_rep, cx_bmf)
        with open(cx_path) as handle:
            cx_text = handle.read()
        check("summary-cxp-column",
              "| cxp (p) |" in cx_text
              and "| a/s | 0 | 1.000 | 1.0 | 2.0 | n/a | 0.000 | 2.00 |"
              in cx_text
              and "| b/s | 0 | 2.000 | 1.0 | n/a | n/a | 0.000 | n/a |"
              in cx_text,
              cx_text)
        os.unlink(cx_path)
        append_summary(cx_path, cx_rep,
                       {k: {m: v for m, v in meas.items()
                             if m != "complexity_exponent"}
                        for k, meas in cx_bmf.items()})
        with open(cx_path) as handle:
            cx_text = handle.read()
        check("summary-no-cxp-column", "| cxp (p) |" not in cx_text,
              cx_text)
    finally:
        try:
            os.unlink(cx_path)
        except OSError:
            pass
    cx_block = slope_file_lines({
        "a/s": {"ok": True,
                "fit": {"ok": True, "slope": 1.5, "lower": 1.4,
                        "upper": 1.6, "r_squared": 0.99}},
        "b/s": {"ok": False, "fit": {"ok": False}, "error": "boom"}})
    check("summary-slope-block",
          cx_block[1] == "**Complexity exponents (T20):**"
          and cx_block[3] == "- a/s: p = 1.500 [1.400, 1.600], "
          "R^2 = 0.9900"
          and cx_block[4] == "- b/s: skipped (boom)"
          and slope_file_lines({}) == [],
          repr(cx_block))
    # ---- T20 hook: slope-fit checks (no JVM/time/perf needed). ----
    sys.path.insert(0, os.path.join(ROOT, "tools"))
    import slope as slope_mod
    for _name, _ok, _detail in slope_mod.run_checks():
        check(_name, _ok, _detail)

    print("self-test: {} failed".format(len(failures)))
    return 1 if failures else 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--resources",
                        default=os.path.join(ROOT, "tests", "resources"))
    parser.add_argument("--cp", default=None)
    parser.add_argument("--jobs", type=int, default=1,
                        help="parallelism; keep 1 for cleanest numbers")
    parser.add_argument("--timeout", type=int, default=120)
    parser.add_argument("--repeat", type=int, default=5)
    parser.add_argument("--only", default="",
                        help="substring filter on area/problem")
    parser.add_argument("--output", default=None,
                        help="full JSON report path")
    parser.add_argument("--bmf-output", default=None,
                        help="Bencher Metric Format JSON path")
    parser.add_argument("--summary-file", default=None,
                        help="append Markdown table (e.g. $GITHUB_STEP_SUMMARY)")
    parser.add_argument("--time-bin", default=None)
    parser.add_argument("--perf", action=argparse.BooleanOptionalAction,
                        default=True, help="use perf counters when available")
    parser.add_argument("--runtime", default="jvm",
                        help="compiler/runtime label recorded in the report")
    parser.add_argument("--command-template", default=DEFAULT_COMMAND_TEMPLATE,
                        help="snippet argv with {cp}, {script}, {stem} holes")
    parser.add_argument("--shard", default=None, metavar="K/N",
                        help="measure only shard K of N (0-based, deterministic;"
                             " split by snippet so per-shard BMFs merge exactly)")
    parser.add_argument("--alloc", action=argparse.BooleanOptionalAction,
                        default=True,
                        help="run EpsilonGC allocation measurement per case")
    parser.add_argument("--alloc-repeat", type=int,
                        default=ALLOC_REPEAT_DEFAULT, metavar="N",
                        help="allocation runs per case; the minimum is kept"
                             " (default %(default)s)")
    parser.add_argument("--merge", action="store_true",
                        help="concatenate full-report JSONs given as INPUTS"
                             " into one merged report instead of measuring")
    parser.add_argument("inputs", nargs="*",
                        help="full-report JSONs for --merge")
    # ---- T20 hook: complexity-slope flags (default off: opt-in only). ----
    parser.add_argument("--slope", action="store_true",
                        help="run scaled slope measurements for covered benchmarks")
    parser.add_argument("--slope-repeat", type=int, default=3,
                        help="repeats per slope scale (the median is fitted)")
    parser.add_argument("--slope-only", action="store_true",
                        help="skip the regular loop; run only slope measurements")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv)

    if args.self_test:
        return self_test()
    if args.merge:
        if not args.inputs:
            parser.error("--merge needs at least one input report")
        report, bmf = merge_reports(args.inputs)
        if args.output:
            write_json(args.output, report)
        if args.bmf_output:
            write_json(args.bmf_output, bmf)
        if args.summary_file:
            append_summary(args.summary_file, report, bmf)
            block = slope_file_lines(report.get("complexity") or {})
            if block:
                try:
                    with open(args.summary_file, "a") as fd:
                        fd.write("\n".join(block) + "\n")
                except OSError as exc:
                    raise SystemExit(
                        "cannot append summary to {}: {}".format(
                            args.summary_file, exc))
        print("merge: {} results, {} benchmarks, {} failures".format(
            len(report["results"]), len(bmf), len(report["failures"])))
        return 1 if report["failures"] else 0
    if args.repeat < 1:
        parser.error("--repeat must be >= 1")
    if args.alloc_repeat < 1:
        parser.error("--alloc-repeat must be >= 1")
    if args.jobs < 1:
        parser.error("--jobs must be >= 1")
    # ---- T20 hook: --slope-only implies --slope. ----
    if args.slope_only:
        args.slope = True
    if args.slope_repeat < 1:
        parser.error("--slope-repeat must be >= 1")
    shard = None
    if args.shard is not None:
        try:
            shard = parse_shard(args.shard)
        except ValueError as exc:
            parser.error(str(exc))

    time_bin = resolve_time_bin(args.time_bin)
    use_perf = args.perf and perf_usable()
    if args.perf and not use_perf:
        print("warning: perf counters unavailable; instructions/cycles "
              "will be null", file=sys.stderr)
    classpath = run_tests.resolve_classpath(args.cp)

    tasks = []
    missing = []
    for area, problem, script, cases in run_tests.discover(args.resources):
        label = "{}/{}".format(area, problem).replace(os.sep, "/")
        if args.only and args.only not in label:
            continue
        stem = os.path.basename(script)[:-len(".clj")]
        if not cases:
            missing.append(label)
            continue
        for case_name, inp, outp in cases:
            tasks.append((label, stem, case_name, script, inp, outp))
    # ---- T20 hook: capture full snippet groups BEFORE shard filtering,
    # so slope entries land in the same shard as their regular entry. ----
    slope_groups = sorted({(t[0], t[1]) for t in tasks})
    if shard is not None:
        tasks = select_shard(tasks, *shard)
    # ---- T20 hook: slope-only mode skips the regular loop entirely. ----
    if args.slope_only:
        tasks = []
        missing = []

    alloc_repeat = args.alloc_repeat if args.alloc else 0
    if alloc_repeat and tasks:
        stem0, script0 = tasks[0][1], tasks[0][3]
        if not alloc_supported(args.command_template, classpath,
                               script0, stem0):
            print("warning: allocation measurement needs a java "
                  "command template; skipping (BMF will omit "
                  "allocated_bytes)", file=sys.stderr)
            alloc_repeat = 0

    results = []
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {
            pool.submit(measure_case, time_bin, use_perf,
                        args.command_template, classpath,
                        label, stem, script, case, inp, outp,
                        args.repeat, alloc_repeat,
                        args.timeout): (label, stem, case)
            for label, stem, case, script, inp, outp in tasks
        }
        for future in as_completed(futures):
            results.append(future.result())
    results.sort(key=lambda r: (r["label"], r["stem"], r["case"]))
    failures = sorted(r["error"] for r in results if not r["ok"])
    for label in missing:
        failures.append("{}: no resources found".format(label))

    # ---- T20 complexity-slope hook (begin; keep small for mergeability) ----
    complexity = {}
    if args.slope:
        sys.path.insert(0, os.path.join(ROOT, "tools"))
        import slope as slope_mod
        slope_scripts = {}
        for area, problem, script, cases in run_tests.discover(args.resources):
            slope_label = "{}/{}".format(area, problem).replace(os.sep, "/")
            if args.only and args.only not in slope_label:
                continue
            # No missing-cases exclusion: slope runs use generated
            # inputs, so they work without decrypted resources.
            stem = os.path.basename(script)[:-len(".clj")]
            slope_scripts[(slope_label, stem)] = script
        complexity = slope_mod.run_slope_all(
            measure_case, time_bin, use_perf, args.command_template,
            classpath, slope_scripts, args.only, slope_groups, shard,
            args.slope_repeat, args.timeout, args.jobs)
        # Scale failures (timeout/mismatch/crash) fail the run like
        # regular case failures; fit skips (flat/poor-fit/...)
        # stay informational, recorded in the entry only.
        for name in sorted(complexity):
            entry = complexity[name]
            if entry.get("scale_failed"):
                failures.append("slope {}: {}".format(
                    name, entry.get("error")))
        for name in sorted(complexity):
            entry = complexity[name]
            fit = entry.get("fit") or {}
            if entry.get("ok") and fit.get("ok"):
                print("slope {}: p={:.3f} [{:.3f},{:.3f}] R^2={:.4f} "
                      "floor={:.2f}s n={}".format(
                          name, fit["slope"], fit["lower"], fit["upper"],
                          fit["r_squared"], fit["floor_c"],
                          entry["scales"]))
            else:
                print("slope {}: SKIPPED {}".format(
                    name, entry.get("error")))
    # ---- T20 complexity-slope hook (end) ----

    bmf = build_bmf(results, complexity)
    report = {
        "tool": "tools/measure.py",
        "version": TOOL_VERSION,
        "timestamp_utc": datetime.datetime.now(
            datetime.timezone.utc).isoformat(),
        "host": host_info(),
        "runtime": {
            "name": args.runtime,
            "command_template": args.command_template,
            "java_version": java_version(),
            "classpath": classpath,
            "time_bin": time_bin,
            "perf_used": use_perf,
        },
        "config": {
            "repeat": args.repeat,
            "jobs": args.jobs,
            "timeout": args.timeout,
            "only": args.only,
            "shard": args.shard,
            "alloc_repeat": alloc_repeat,
            "slope": args.slope,  # T20 hook: slope-run provenance
            "slope_repeat": args.slope_repeat,  # T20 hook
        },
        "results": results,
        "failures": failures,
        "missing": sorted(missing),
        "complexity": complexity,  # T20 hook: {} unless --slope
    }
    if args.output:
        write_json(args.output, report)
    if args.bmf_output:
        write_json(args.bmf_output, bmf)
    if args.summary_file:
        append_summary(args.summary_file, report, bmf)
    # ---- T20 hook: slope lines in the step summary (own block;
    # shared helper also serves the merge path). ----
    if args.summary_file:
        block = slope_file_lines(complexity)
        if block:
            try:
                with open(args.summary_file, "a") as fd:
                    fd.write("\n".join(block) + "\n")
            except OSError as exc:
                raise SystemExit("cannot append summary to {}: {}".format(
                    args.summary_file, exc))
    print_table(results, failures, missing)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())

