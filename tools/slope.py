"""Complexity-exponent measurement from scaled inputs (T20).

Fits t = C + k * n**p by least squares: for fixed p the inner
(C, k) problem is closed-form OLS of t on x = n**p, and the
outer 1-D search over p is golden-section minimization of SSE.
The explicit floor term C absorbs JVM startup (~2 s cpu here),
which would otherwise flatten a plain log-log slope (it reports
~1.2 for quadratic insertion sort on typical scales).

Public surface:
  fit_power_law(ns, ts) -> dict with slope/band/r_squared or skipped reason
  run_slope_all(...)    -> drives scaled runs through the measure harness
  run_checks()          -> (name, ok, detail) list for --self-test
"""

import math
import os
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed

# Fit search range for the exponent. Covers sublinear through
# cubic; widen if a future pilot needs more.
P_MIN = 0.0
P_MAX = 3.0
# Golden-section iterations: far more than enough for 1-D SSE(p).
SEARCH_ITERS = 80
# Garbage-in guards (each skip records its reason; see spec).
MIN_POINTS = 3
MIN_N_RANGE = 4.0  # max(n)/min(n)
MIN_T_SPREAD = 0.05  # (max(t)-min(t))/max(t)
MIN_R_SQUARED = 0.8


def _ols(x, y):
    """Closed-form OLS of y on x. Returns (intercept, slope, sse)."""
    n = len(x)
    mx = sum(x) / n
    my = sum(y) / n
    sxx = sum((v - mx) ** 2 for v in x)
    if sxx <= 0.0:
        return None
    sxy = sum((xi - mx) * (yi - my) for xi, yi in zip(x, y))
    slope = sxy / sxx
    intercept = my - slope * mx
    sse = sum((yi - (intercept + slope * xi)) ** 2 for xi, yi in zip(x, y))
    return intercept, slope, sse


def _sse_at_p(ns, ts, p):
    """SSE of the best (C, k) fit of t = C + k*n**p at fixed p."""
    try:
        xs = [float(n) ** p for n in ns]
    except OverflowError:
        return None
    if any(not math.isfinite(v) for v in xs):
        return None
    return _ols(xs, ts)


def _search(ns, ts):
    """Golden-section search for argmin SSE(p) over [P_MIN, P_MAX]."""
    inv_phi = (math.sqrt(5.0) - 1.0) / 2.0
    lo, hi = P_MIN, P_MAX
    c = hi - inv_phi * (hi - lo)
    d = lo + inv_phi * (hi - lo)
    fc = _sse_at_p(ns, ts, c)
    fd = _sse_at_p(ns, ts, d)
    fc = fc[2] if fc else float("inf")
    fd = fd[2] if fd else float("inf")
    for _ in range(SEARCH_ITERS):
        if fc < fd:
            hi, d, fd = d, c, fc
            c = hi - inv_phi * (hi - lo)
            fc = _sse_at_p(ns, ts, c)
            fc = fc[2] if fc else float("inf")
        else:
            lo, c, fc = c, d, fd
            d = lo + inv_phi * (hi - lo)
            fd = _sse_at_p(ns, ts, d)
            fd = fd[2] if fd else float("inf")
    p = (lo + hi) / 2.0
    best = _sse_at_p(ns, ts, p)
    if best is None:
        return None
    return p, best[0], best[1], best[2]


def _r_squared(ts, sse):
    mean = sum(ts) / len(ts)
    sst = sum((t - mean) ** 2 for t in ts)
    if sst <= 0.0:
        return 0.0
    return max(0.0, 1.0 - sse / sst)


def _band(ns, ts, slope):
    """Leave-one-out min/max slope band; degenerate when <2 refits work."""
    slopes = []
    for drop in range(len(ns)):
        sub_n = [v for i, v in enumerate(ns) if i != drop]
        sub_t = [v for i, v in enumerate(ts) if i != drop]
        if len(sub_n) < MIN_POINTS:
            continue
        if max(sub_n) / min(sub_n) < MIN_N_RANGE:
            continue
        try:
            found = _search(sub_n, sub_t)
        except (ArithmeticError, ValueError):
            continue
        if found is not None and found[2] > 0:
            slopes.append(found[0])
    if len(slopes) < 2:
        return slope, slope
    # Include the full-fit slope so float dust can't push the band
    # off the point estimate on near-exact data.
    slopes.append(slope)
    return min(slopes), max(slopes)


def fit_power_law(ns, ts):
    """Fit t = C + k*n**p. Returns result dict (see module docstring).

    On success: {"ok": True, "slope": p, "lower": lo, "upper": hi,
    "floor_c": C, "k": k, "r_squared": r2}. On garbage input:
    {"ok": False, "reason": "<code>", "detail": "..."}.
    """
    ns = [float(v) for v in ns]
    ts = [float(v) for v in ts]
    if len(ns) != len(ts) or len(ns) < MIN_POINTS:
        return {"ok": False, "reason": "too-few-points",
                "detail": "need >= {} points, got {}".format(MIN_POINTS, len(ns))}
    if any(v <= 0 for v in ns) or any(v <= 0 for v in ts):
        return {"ok": False, "reason": "non-positive",
                "detail": "n and t must all be > 0"}
    if max(ns) / min(ns) < MIN_N_RANGE:
        return {"ok": False, "reason": "tiny-range",
                "detail": "max(n)/min(n) = {:.2f} < {}".format(
                    max(ns) / min(ns), MIN_N_RANGE)}
    if (max(ts) - min(ts)) / max(ts) < MIN_T_SPREAD:
        return {"ok": False, "reason": "flat",
                "detail": "t spread < {:.0%} of max: no measurable scaling".format(
                    MIN_T_SPREAD)}
    try:
        found = _search(ns, ts)
    except (ArithmeticError, ValueError) as exc:
        return {"ok": False, "reason": "search-failed", "detail": str(exc)}
    if found is None:
        return {"ok": False, "reason": "search-failed",
                "detail": "degenerate design matrix over p range"}
    p, floor_c, k, sse = found
    if not all(math.isfinite(v) for v in (p, floor_c, k, sse)):
        return {"ok": False, "reason": "search-failed",
                "detail": "non-finite fit parameters"}
    if k <= 0:
        return {"ok": False, "reason": "non-increasing",
                "detail": "fitted k <= 0: t does not grow with n"}
    r2 = _r_squared(ts, sse)
    if r2 < MIN_R_SQUARED:
        return {"ok": False, "reason": "poor-fit",
                "detail": "R^2 = {:.3f} < {}".format(r2, MIN_R_SQUARED)}
    lo, hi = _band(ns, ts, p)
    return {"ok": True, "slope": p, "lower": lo, "upper": hi,
            "floor_c": floor_c, "k": k, "r_squared": r2}


def run_slope_all(measure_case, time_bin, use_perf, template, classpath,
                  scripts, only, all_groups, shard, repeat, timeout, jobs):
    """Run scaled slope measurements for covered benchmarks.

    measure_case: harness fn (same signature as in tools/measure.py).
    scripts: {(label, stem): script_path} for discovered snippets.
    only: same substring filter as the main loop. all_groups/shard:
    the main loop's full sorted (label, stem) groups plus its parsed
    (index, total) shard (or None) — slope entries must land in the
    SAME shard as their benchmark's regular entry, because the CI
    fan-in `jq -s add` overwrites (not deep-merges) duplicate keys.
    Returns the report-ready "complexity" dict {name: {...}}.
    """
    import generators  # tools-local; lazy so measure.py has no new top imports

    tasks = []  # (name, label, stem, script, gen)
    keep = None
    if shard is not None:
        index, total = shard
        keep = {g for i, g in enumerate(sorted(all_groups))
                if i % total == index}
    for name, gen in sorted(generators.GENERATORS.items()):
        label, _, stem = name.rpartition("/")
        if only and only not in label:
            continue
        if (label, stem) not in scripts:
            continue
        if keep is not None and (label, stem) not in keep:
            continue
        tasks.append((name, label, stem, scripts[(label, stem)], gen))
    complexity = {}
    if not tasks:
        return complexity
    def _clean(tmpdir):
        try:
            for fname in os.listdir(tmpdir):
                try:
                    os.unlink(os.path.join(tmpdir, fname))
                except OSError:
                    pass
        except OSError:
            return
        try:
            os.rmdir(tmpdir)
        except OSError:
            pass

    # One benchmark at a time: generate its scales, measure them,
    # fit, then delete its files before moving on. Peak disk stays
    # near one benchmark (~4GB for a full 90-benchmark sweep would
    # otherwise sit on disk until the very end).
    for name, label, stem, script, gen in tasks:
        tmpdir = tempfile.mkdtemp(prefix="slope-")
        try:
            scale_jobs = []  # (fit_n, input_path, output_path)
            for idx, n in enumerate(gen.scales):
                data_in, data_out, fit_n = gen.generate(n, seed=1000 + idx)
                in_path = os.path.join(tmpdir, "{}-{}.in".format(stem, idx))
                out_path = os.path.join(tmpdir, "{}-{}.expected".format(
                    stem, idx))
                with open(in_path, "wb") as fd:
                    fd.write(data_in)
                with open(out_path, "wb") as fd:
                    fd.write(data_out)
                scale_jobs.append((fit_n, in_path, out_path))
            per_scale = []
            with ThreadPoolExecutor(max_workers=max(1, jobs)) as pool:
                futures = {
                    # alloc_repeat=0: slope scales are timing-only
                    # probes; the fit reads cpu medians, so skip
                    # EpsilonGC alloc runs.
                    pool.submit(measure_case, time_bin, use_perf,
                                template, classpath, label, stem, script,
                                "slope_n_{}".format(fit_n), in_path,
                                out_path, repeat, 0, timeout): fit_n
                    for fit_n, in_path, out_path in scale_jobs
                }
                for future in as_completed(futures):
                    per_scale.append((futures[future], future.result()))
            scales = sorted(per_scale, key=lambda pair: pair[0])
            entry = {"scales": [n for n, _ in scales], "points": [],
                     "ok": False, "fit": None, "error": None,
                     "scale_failed": False}
            failed = [res for _, res in scales if not res["ok"]]
            if failed:
                entry["error"] = failed[0]["error"]
                entry["scale_failed"] = True
            else:
                medians = []
                for fit_n, res in scales:
                    cpu = res["summary"]["cpu_s"]
                    entry["points"].append(
                        {"n": fit_n, "cpu_median": cpu["median"],
                         "cpu_min": cpu["min"], "cpu_max": cpu["max"]})
                    medians.append(cpu["median"])
                fit = fit_power_law([n for n, _ in scales], medians)
                entry["fit"] = fit
                entry["ok"] = fit.get("ok", False)
                if not fit.get("ok"):
                    entry["error"] = "slope fit skipped: {} ({})".format(
                        fit["reason"], fit["detail"])
            complexity[name] = entry
        finally:
            # Crash safety: a failed sweep must not orphan gigabytes.
            _clean(tmpdir)
    return complexity


def run_checks():
    """Self-test checks. Returns [(name, ok, detail)]; no JVM/time/perf."""
    out = []

    def check(name, cond, detail=""):
        out.append((name, bool(cond), detail))

    lin_n = [100000.0, 200000.0, 400000.0, 800000.0]
    lin_t = [2.0 + 1e-6 * n for n in lin_n]
    r = fit_power_law(lin_n, lin_t)
    check("slope-linear",
          r.get("ok") and 0.95 <= r["slope"] <= 1.05
          and r["r_squared"] > 0.999, repr(r))

    quad_n = [20000.0, 40000.0, 80000.0, 160000.0]
    quad_t = [2.0 + 1e-9 * n * n for n in quad_n]
    r = fit_power_law(quad_n, quad_t)
    check("slope-quadratic",
          r.get("ok") and 1.95 <= r["slope"] <= 2.05
          and r["r_squared"] > 0.999, repr(r))

    nlogn_n = [1000000.0, 2000000.0, 4000000.0, 8000000.0]
    nlogn_t = [2.0 + 1e-7 * n * math.log(n) for n in nlogn_n]
    r = fit_power_law(nlogn_n, nlogn_t)
    check("slope-nlogn",
          r.get("ok") and 1.0 <= r["slope"] <= 1.15
          and r["r_squared"] > 0.99, repr(r))

    # Floor recovery: the fitted C must track a known startup floor.
    r = fit_power_law(quad_n, [1.5 + 1e-9 * n * n for n in quad_n])
    check("slope-floor",
          r.get("ok") and abs(r["floor_c"] - 1.5) < 0.05, repr(r))

    # Determinism: identical inputs give identical outputs.
    a = fit_power_law(quad_n, quad_t)
    b = fit_power_law(quad_n, quad_t)
    check("slope-deterministic", a == b, repr((a, b)))

    # Band sanity on clean data: tight around the slope.
    check("slope-band",
          a.get("ok") and a["lower"] <= a["slope"] <= a["upper"]
          and a["upper"] - a["lower"] < 0.05, repr(a))

    flat = fit_power_law(quad_n, [2.0, 2.001, 1.999, 2.0])
    check("slope-skip-flat",
          not flat.get("ok") and flat["reason"] == "flat", repr(flat))

    tiny = fit_power_law([100.0, 110.0, 120.0, 130.0], [1.0, 2.0, 3.0, 4.0])
    check("slope-skip-tiny-range",
          not tiny.get("ok") and tiny["reason"] == "tiny-range", repr(tiny))

    few = fit_power_law([1.0, 2.0], [1.0, 4.0])
    check("slope-skip-too-few",
          not few.get("ok") and few["reason"] == "too-few-points", repr(few))

    nonpos = fit_power_law(quad_n, [2.0, 0.0, 3.0, 4.0])
    check("slope-skip-non-positive",
          not nonpos.get("ok") and nonpos["reason"] == "non-positive",
          repr(nonpos))

    dec = fit_power_law(quad_n, [8.0, 4.0, 2.0, 1.0])
    check("slope-skip-non-increasing",
          not dec.get("ok") and dec["reason"] == "non-increasing", repr(dec))

    # Runner filtering, with a stubbed measure_case (no JVM): the stub
    # reports exact quadratic times parsed back out of the case name.
    import generators
    from generators.sort_arrays import SortArrayGenerator

    def stub_ok(time_bin, use_perf, template, classpath, label, stem,
               script, case_name, input_path, output_path, repeat,
               alloc_repeat, timeout):
        # Mirror measure_case's real signature: a drift here fails the
        # self-test loudly instead of crashing only on CI shards.
        assert alloc_repeat == 0, alloc_repeat
        n = int(case_name.rsplit("_", 1)[-1])
        t = 2.0 + 1e-6 * n * n
        with open(input_path, "rb") as fd:
            toks = fd.read().split()
        ok = (toks and int(toks[0]) == n and len(toks) == n + 1
              and os.path.exists(output_path))
        if not ok:
            return {"ok": False, "error": "bad generated io", "summary": None}
        return {"ok": True, "error": None,
                "summary": {"cpu_s": {"min": t, "median": t, "max": t}}}

    def stub_fail(*args, **kwargs):
        return {"ok": False, "error": "boom", "summary": None}

    tiny = {"sort/insertion_sort/solution": SortArrayGenerator(
                scales=[100, 200, 400, 800]),
            "sort/quick_sort/solution": SortArrayGenerator(
                scales=[100, 200, 400, 800])}
    real_registry = generators.GENERATORS
    generators.GENERATORS = tiny
    try:
        scripts = {("sort/insertion_sort", "solution"): "/fake/i.clj",
                   ("sort/quick_sort", "solution"): "/fake/q.clj"}
        groups = sorted(scripts)
        got = run_slope_all(stub_ok, None, False, "", "", scripts, "",
                            groups, None, 1, 0, 1)
        check("slope-runner-both",
              sorted(got) == sorted(tiny) and all(
                  got[name]["ok"]
                  and abs(got[name]["fit"]["slope"] - 2.0) < 0.05
                  and got[name]["scale_failed"] is False
                  for name in got),
              repr({k: (v["ok"], (v["fit"] or {}).get("slope"))
                    for k, v in got.items()}))
        got = run_slope_all(stub_ok, None, False, "", "", scripts,
                            "quick", groups, None, 1, 0, 1)
        check("slope-runner-only",
              sorted(got) == ["sort/quick_sort/solution"], repr(sorted(got)))
        wide = [("a", "s"), ("sort/insertion_sort", "solution"),
                ("sort/quick_sort", "solution"), ("z", "s")]
        # Sorted ranks: a=0, insertion=1, quick=2, z=3.
        got = run_slope_all(stub_ok, None, False, "", "", scripts, "",
                            wide, (1, 4), 1, 0, 1)
        check("slope-runner-shard",
              sorted(got) == ["sort/insertion_sort/solution"],
              repr(sorted(got)))
        got = run_slope_all(stub_ok, None, False, "", "",
                            {("a", "s"): "/fake/a.clj"}, "", groups,
                            None, 1, 0, 1)
        check("slope-runner-no-scripts", got == {}, repr(got))
        got = run_slope_all(stub_fail, None, False, "", "", scripts, "",
                            groups, None, 1, 0, 1)
        check("slope-runner-error",
              sorted(got) == sorted(tiny) and all(
                  not v["ok"] and v["error"]
                  and v["scale_failed"] is True
                  for v in got.values()),
              repr(got))
    finally:
        generators.GENERATORS = real_registry

    return out
