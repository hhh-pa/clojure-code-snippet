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

Wall time is recorded too but is NOT load-independent; treat it as
secondary. Each case runs --repeat times; the summary keeps min/median/max
so the least-disturbed run (min CPU) and worst-case footprint (max RSS)
stay visible. Absolute numbers must only be compared as same-machine
ratios (e.g. Clojure vs native back-to-back); never compare absolutes
across different machines or runners.

Outputs:

  * --output: full JSON (per-run metrics, per-case summaries, failures).
  * --bmf-output: Bencher Metric Format JSON (one Benchmark per snippet,
    measures cpu_time/peak_memory/wall_time/instructions), consumable by
    `bencher run --adapter json --file ...` to track evolution in Bencher.
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
        [--runtime jvm] [--command-template '...']

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
TOOL_VERSION = 1
DEFAULT_COMMAND_TEMPLATE = "java -cp {cp} clojure.main {script}"


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


def summarize_numbers(values):
    values = sorted(values)
    return {"min": values[0], "median": statistics.median(values),
            "max": values[-1]}


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


def measure_case(time_bin, use_perf, template, classpath, label, stem, script,
                 case_name, input_path, output_path, repeat, timeout):
    result = {
        "label": label,
        "stem": stem,
        "script": os.path.relpath(script, ROOT),
        "case": case_name,
        "ok": False,
        "error": None,
        "runs": [],
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
    result["ok"] = True
    return result


def build_bmf(results):
    """Aggregate per-case summaries into Bencher Metric Format JSON.

    One Benchmark per snippet (label/stem); each measure uses the median
    as value with min/max as lower/upper bounds so Bencher tracks the
    robust middle while keeping the spread visible. cpu_time, wall_time
    and instructions are summed over a snippet's cases; peak_memory takes
    the worst case (max), in bytes.
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
            "wall_time": {
                "value": sum(s["wall_s"]["median"] for s in summaries),
                "lower_value": sum(s["wall_s"]["min"] for s in summaries),
                "upper_value": sum(s["wall_s"]["max"] for s in summaries),
            },
        }
        insn = [s["instructions"] for s in summaries if s.get("instructions")]
        if insn and len(insn) == len(summaries):
            measures["instructions"] = {
                "value": sum(s["median"] for s in insn),
                "lower_value": sum(s["min"] for s in insn),
                "upper_value": sum(s["max"] for s in insn),
            }
        bmf[name] = measures
    return bmf


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
        print("{}: ok cpu_min={:.3f}s cpu_med={:.3f}s rss_max={}K wall_min={:.3f}s insn_med={}".format(
            tag, s["cpu_s"]["min"], s["cpu_s"]["median"],
            s["maxrss_kb"]["max"], s["wall_s"]["min"], insn))
    for label in missing:
        print("FAIL {}: no resources found".format(label))
    ok = sum(1 for r in results if r["ok"])
    print("measure: {} ok, {} failed, {} missing-resource problems".format(
        ok, len(failures) - len(missing), len(missing)))


def append_summary(path, report, bmf):
    host = report["host"]
    runtime = report["runtime"]
    results = report["results"]
    ok = sum(1 for r in results if r["ok"])
    lines = [
        "## Perf measurement (`tools/measure.py`)",
        "",
        "- {} ({}), java: {}".format(
            report["timestamp_utc"], host.get("cpu_model") or "unknown cpu",
            runtime["java_version"] or "unknown"),
        "- runtime: {}, perf counters: {}, repeat: {}, jobs: {}".format(
            runtime["name"], "yes" if runtime["perf_used"] else "no",
            report["config"]["repeat"], report["config"]["jobs"]),
        "- {} ok, {} failed, {} missing-resource problems".format(
            ok, len(report["failures"]) - len(report["missing"]),
            len(report["missing"])),
        "- Primary signals: min CPU time, max peak RSS, median instructions "
        "(load-independent); wall time is secondary. Compare same-machine "
        "ratios only.",
        "",
        "| snippet | cases | cpu total (s) | peak RSS (MB) | instructions | wall total (s) |",
        "| --- | ---: | ---: | ---: | ---: | ---: |",
    ]
    per_snippet_cases = {}
    for res in results:
        if res["ok"]:
            key = "{}/{}".format(res["label"], res["stem"])
            per_snippet_cases[key] = per_snippet_cases.get(key, 0) + 1
    for name in sorted(bmf):
        m = bmf[name]
        insn = m.get("instructions", {}).get("value", "n/a")
        lines.append("| {} | {} | {:.3f} | {:.1f} | {} | {:.3f} |".format(
            name, per_snippet_cases.get(name, 0),
            m["cpu_time"]["value"], m["peak_memory"]["value"] / 1048576,
            insn, m["wall_time"]["value"]))
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
    check("bmf",
          entry.get("cpu_time") == {"value": 2.0, "lower_value": 1.0,
                                    "upper_value": 3.0}
          and entry.get("peak_memory") == {"value": 105 * 1024,
                                           "lower_value": 100 * 1024,
                                           "upper_value": 110 * 1024}
          and entry.get("instructions") == {"value": 11.0, "lower_value": 10,
                                            "upper_value": 12}
          and "wall_time" in entry, repr(entry))

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
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv)

    if args.self_test:
        return self_test()
    if args.repeat < 1:
        parser.error("--repeat must be >= 1")
    if args.jobs < 1:
        parser.error("--jobs must be >= 1")

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

    results = []
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {
            pool.submit(measure_case, time_bin, use_perf,
                        args.command_template, classpath,
                        label, stem, script, case, inp, outp,
                        args.repeat, args.timeout): (label, stem, case)
            for label, stem, case, script, inp, outp in tasks
        }
        for future in as_completed(futures):
            results.append(future.result())
    results.sort(key=lambda r: (r["label"], r["stem"], r["case"]))
    failures = sorted(r["error"] for r in results if not r["ok"])
    for label in missing:
        failures.append("{}: no resources found".format(label))

    bmf = build_bmf(results)
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
        },
        "results": results,
        "failures": failures,
        "missing": sorted(missing),
    }
    if args.output:
        write_json(args.output, report)
    if args.bmf_output:
        write_json(args.bmf_output, bmf)
    if args.summary_file:
        append_summary(args.summary_file, report, bmf)
    print_table(results, failures, missing)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())

