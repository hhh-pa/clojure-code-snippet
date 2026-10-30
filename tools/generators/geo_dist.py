"""Slope input generators for computational_geometry + distributed (T20).

Each generator produces (input_bytes, expected_bytes, fit_n) for one
benchmark's stdin contract at requested scales. Geometry inputs use
integer or half-decimal coordinates so printed values are exact, and
expected outputs mirror the solutions' halves-up renderers bit for
bit. Distributed sims use unbounded ints with zero float I/O, so
their references are trivially byte-exact. All generation is a pure
function of (n, seed) using only the standard library.
"""

import bisect
import math
import random


_EPS = 1e-9


def _newton_sqrt(x):
    """Newton square root mirroring the Clojure solutions (30 iters)."""
    if x == 0.0:
        return x
    if not x == x:
        return x
    if x == x * 2.0:
        return x
    if x < 0.0:
        return float("nan")
    m = x
    adj = 1.0
    while m > 4.0:
        m /= 4.0
        adj *= 2.0
    while m < 0.25:
        m *= 4.0
        adj /= 2.0
    g = m if m > 1.0 else 1.0
    for _ in range(30):
        g = (g + m / g) / 2.0
    return g * adj


def _double_to_bigint_str(v):
    """Exact integer value of an integral double beyond long range."""
    q = v
    k = 0
    while q >= 9007199254740992.0:
        q /= 2.0
        k += 1
    return str(int(q) * (2 ** k))


def _format_fixed(x, k):
    """Render x with exactly k decimals, halves up (mirrors Clojure)."""
    if not x == x:
        return "NaN"
    if x != 0.0 and x == x * 2.0:
        return "-Infinity" if x < 0.0 else "Infinity"
    neg = math.copysign(1.0, x) < 0.0
    a = -x if neg else x
    if a >= 9.0e18:
        return ("-" if neg else "") + _double_to_bigint_str(a) + "." + "0" * k
    pow10k = 1.0
    for _ in range(k):
        pow10k *= 10.0
    ip = int(a)
    frac = a - float(ip)
    r = int(frac * pow10k + 0.5)
    if r >= int(pow10k):
        ip += 1
        r = 0
    fs = str(r)
    return ("-" if neg else "") + str(ip) + "." + "0" * (k - len(fs)) + fs


def _format_fixed_even_1(x):
    """Render x with exactly one decimal, ties to even (mirrors Clojure)."""
    if not x == x:
        return "NaN"
    if x != 0.0 and x == x * 2.0:
        return "-Infinity" if x < 0.0 else "Infinity"
    neg = math.copysign(1.0, x) < 0.0
    a = -x if neg else x
    if a >= 9.0e18:
        return ("-" if neg else "") + _double_to_bigint_str(a) + ".0"
    ip = int(a)
    frac = a - float(ip)
    scaled = frac * 10.0
    fl = int(scaled)
    f = scaled - float(fl)
    if f > 0.5:
        r = fl + 1
    elif f < 0.5:
        r = fl
    elif fl % 2 == 0:
        r = fl
    else:
        r = fl + 1
    if r == 10:
        ip += 1
        r = 0
    return ("-" if neg else "") + str(ip) + "." + str(r)


def _cross(o, a, b):
    """Twice the signed area of triangle o-a-b (exact integer math)."""
    return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])


def _monotone_chain(pts):
    """Andrew chain mirroring graham-scan: CCW, no collinear middles."""
    uniq = sorted(set(pts))
    if len(uniq) <= 2:
        return list(uniq)
    lower = []
    for p in uniq:
        while len(lower) >= 2 and _cross(lower[-2], lower[-1], p) <= 0:
            lower.pop()
        lower.append(p)
    upper = []
    for p in reversed(uniq):
        while len(upper) >= 2 and _cross(upper[-2], upper[-1], p) <= 0:
            upper.pop()
        upper.append(p)
    return lower[:-1] + upper[:-1]


def _hull_perimeter(hull):
    """Cyclic hull perimeter mirroring the convex-hull solution."""
    acc = 0.0
    m = len(hull)
    for i in range(m):
        x1, y1 = hull[i]
        x2, y2 = hull[(i + 1) % m]
        dx = float(x1 - x2)
        dy = float(y1 - y2)
        acc = acc + _newton_sqrt(dx * dx + dy * dy)
    return acc


def _circumcircle(a, b, c):
    """Circumcenter + squared radius, or None when |d| <= EPS."""
    ax = float(a[0])
    ay = float(a[1])
    bx = float(b[0])
    by = float(b[1])
    cx = float(c[0])
    cy = float(c[1])
    d = 2.0 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
    if abs(d) <= _EPS:
        return None
    ux = ((ax * ax + ay * ay) * (by - cy)
          + (bx * bx + by * by) * (cy - ay)
          + (cx * cx + cy * cy) * (ay - by)) / d
    uy = ((ax * ax + ay * ay) * (cx - bx)
          + (bx * bx + by * by) * (ax - cx)
          + (cx * cx + cy * cy) * (bx - ax)) / d
    rsq = (ax - ux) * (ax - ux) + (ay - uy) * (ay - uy)
    return (ux, uy, rsq)


def _empty_circle(raw, i, j, k):
    """True when triple i<j<k has an empty circumcircle over all points."""
    cc = _circumcircle(raw[i], raw[j], raw[k])
    if cc is None:
        return False
    ux, uy, rsq = cc
    for m in range(len(raw)):
        if m == i or m == j or m == k:
            continue
        dx = float(raw[m][0]) - ux
        dy = float(raw[m][1]) - uy
        if dx * dx + dy * dy < rsq - _EPS:
            return False
    return True


def _point_cmp(p1, p2):
    """Order points by x then y, within tolerance (mirrors Clojure)."""
    if abs(p1[0] - p2[0]) > _EPS:
        return -1 if p1[0] < p2[0] else 1
    if abs(p1[1] - p2[1]) > _EPS:
        return -1 if p1[1] < p2[1] else 1
    return 0


class ClosestPairGenerator(object):
    """Collinear unit-spaced points; min distance exactly 1.0."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        toks = [str(n)]
        for i in range(n):
            toks.append(str(i))
            toks.append("0")
        if n < 2:
            expected = "1000000000000000000.000000\n"
        else:
            expected = "1.000000\n"
        return (" ".join(toks).encode("ascii"),
                expected.encode("ascii"), n)


class ChanHullGenerator(object):
    """Random int64 points; monotone chain + rotation to lex smallest."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        pts = [(rng.randint(-1000000, 1000000),
                rng.randint(-1000000, 1000000)) for _ in range(n)]
        toks = [str(n)]
        for (x, y) in pts:
            toks.append(str(x))
            toks.append(str(y))
        hull = _monotone_chain(pts)
        if hull:
            mi = 0
            for i in range(1, len(hull)):
                if hull[i] < hull[mi]:
                    mi = i
            hull = hull[mi:] + hull[:mi]
        lines = [str(len(hull))]
        for (x, y) in hull:
            lines.append("{0} {1}".format(x, y))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(lines) + "\n").encode("ascii"), n)


class ConvexHullGenerator(object):
    """Axis-aligned power-of-two rectangle + strict-interior points."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        w = 1024
        h = 2048
        corners = [(0, 0), (w, 0), (w, h), (0, h)]
        if n <= 4:
            pts = corners[:n]
        else:
            pts = list(corners)
            for _ in range(n - 4):
                pts.append((rng.randint(1, w - 1), rng.randint(1, h - 1)))
        toks = [str(n)]
        for (x, y) in pts:
            toks.append(str(x))
            toks.append(str(y))
        uniq = sorted(set(pts))
        if len(uniq) < 2:
            expected = "0.0\n"
        else:
            expected = _format_fixed_even_1(
                _hull_perimeter(_monotone_chain(uniq))) + "\n"
        return (" ".join(toks).encode("ascii"),
                expected.encode("ascii"), n)


class KdtreeGenerator(object):
    """Points (10i, 0); RANGE boxes on the lattice, unit-offset nearest."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        q = 0 if n == 0 else max(1, n // 10)
        toks = [str(n)]
        for i in range(n):
            toks.append(str(10 * i))
            toks.append("0")
        toks.append(str(q))
        out = []
        for _ in range(q):
            if n > 0 and rng.random() < 0.5:
                i = rng.randint(0, n - 1)
                toks.append("NEAREST")
                toks.append(str(10 * i + 1))
                toks.append("0")
                out.append("{0}.00 0.00 1.00".format(10 * i))
            else:
                if n > 0:
                    a = rng.randint(0, n - 1)
                    b = rng.randint(0, n - 1)
                    lo = min(a, b)
                    hi = max(a, b)
                else:
                    lo = 0
                    hi = -1
                r = rng.random()
                if r < 0.6:
                    y1 = -rng.randint(0, 3)
                    y2 = rng.randint(0, 3)
                elif r < 0.8:
                    y1 = rng.randint(1, 4)
                    y2 = y1 + rng.randint(0, 4)
                else:
                    y2 = -rng.randint(1, 4)
                    y1 = y2 - rng.randint(0, 4)
                toks.append("RANGE")
                toks.append(str(10 * lo))
                toks.append(str(y1))
                toks.append(str(10 * hi))
                toks.append(str(y2))
                if n > 0 and y1 <= 0 and 0 <= y2:
                    out.append(str(hi - lo + 1))
                else:
                    out.append("0")
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class BentleyOttmannGenerator(object):
    """Axis-aligned grid: v verticals x horizontals, crossings at integers."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        v = n // 2
        h = n - v
        toks = [str(n)]
        if v >= 2 and h >= 2:
            for i in range(v):
                toks.append(str(i))
                toks.append("0")
                toks.append(str(i))
                toks.append(str(h - 1))
            for j in range(h):
                toks.append("0")
                toks.append(str(j))
                toks.append(str(v - 1))
                toks.append(str(j))
            out = [str(v * h)]
            for i in range(v):
                for j in range(h):
                    out.append("{0}.00 {1}.00".format(i, j))
        else:
            for i in range(n):
                toks.append(str(10 * i))
                toks.append(str(10 * i))
                toks.append(str(10 * i + 1))
                toks.append(str(10 * i + 1))
            out = ["0"]
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class DelaunayGenerator(object):
    """Random distinct integer points; O(n^4) brute empty-circle triples."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        pts = []
        seen = set()
        while len(pts) < n:
            p = (rng.randint(0, 199), rng.randint(0, 199))
            if p not in seen:
                seen.add(p)
                pts.append(p)
        toks = [str(n)]
        for (x, y) in pts:
            toks.append(str(x))
            toks.append(str(y))
        raw = [(float(x), float(y)) for (x, y) in pts]
        tris = []
        for i in range(n):
            for j in range(i + 1, n):
                for k in range(j + 1, n):
                    if _empty_circle(raw, i, j, k):
                        tris.append((i, j, k))
        tris.sort()
        out = [str(len(tris))]
        for (i, j, k) in tris:
            out.append("{0} {1} {2}".format(i, j, k))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class PolygonTriangulationGenerator(object):
    """Convex parabola polygon (i, i^2); ear clip yields a fan at n-1."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        toks = [str(n)]
        for i in range(n):
            toks.append(str(i))
            toks.append(str(i * i))
        if n < 3:
            tris = []
        elif n == 3:
            tris = [(0, 1, 2)]
        else:
            tris = [(k, k + 1, n - 1) for k in range(n - 2)]
        tris = sorted(tris)
        out = [str(len(tris))]
        for (i, j, k) in tris:
            out.append("{0} {1} {2}".format(i, j, k))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class VoronoiGenerator(object):
    """Two-row integer grid; brute Delaunay circumcenters, EPS-deduped."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        cols = max(2, (n + 1) // 2) if n > 1 else 1
        pts = [(i % cols, i // cols) for i in range(n)]
        toks = [str(n)]
        for (x, y) in pts:
            toks.append(str(x))
            toks.append(str(y))
        raw = [(float(x), float(y)) for (x, y) in pts]
        centers = []
        for i in range(n):
            for j in range(i + 1, n):
                for k in range(j + 1, n):
                    if _empty_circle(raw, i, j, k):
                        cc = _circumcircle(raw[i], raw[j], raw[k])
                        centers.append((cc[0], cc[1]))
        centers.sort()
        uniq = []
        for pt in centers:
            if not uniq or _point_cmp(uniq[-1], pt) != 0:
                uniq.append(pt)
        out = [str(len(uniq))]
        for (x, y) in uniq:
            out.append("{0} {1}".format(_format_fixed(x, 2),
                                       _format_fixed(y, 2)))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class ByzantineEigGenerator(object):
    """Random loyal/faulty split; decision is 0 iff commander faulty."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        nf = rng.randint(0, 3)
        cmd = rng.randint(-1000000000000000000, 1000000000000000000)
        ids = []
        if n > 0 and nf > 0:
            pool = list(range(n))
            rng.shuffle(pool)
            ids = sorted(pool[:min(nf, n)])
        faulty = set(ids)
        decision = 0 if 0 in faulty else cmd
        toks = [str(n), str(nf), str(cmd), str(len(ids))]
        toks.extend(str(i) for i in ids)
        out = ["DECISION: {0}".format(decision)]
        for i in range(1, n):
            if i not in faulty:
                out.append("P{0}: {1}".format(i, decision))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class VectorClocksGenerator(object):
    """Random LOCAL/SEND/RECV ops plus causality queries over events."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        procs = max(2, min(64, n // 500)) if n > 0 else 2
        nq = max(2, n // 10) if n > 0 else 0
        ops = []
        labels = []
        sent = []
        mctr = 0
        for i in range(n):
            p = rng.randint(0, procs - 1)
            r = rng.random()
            if r < 0.4:
                ops.append(("LOCAL", p, "L{0}".format(i)))
                labels.append("L{0}".format(i))
            elif r < 0.7:
                mctr += 1
                ops.append(("SEND", p, "M{0}".format(mctr)))
                sent.append("M{0}".format(mctr))
                labels.append("M{0}_SEND".format(mctr))
            else:
                if sent and rng.random() < 0.8:
                    msg = sent[rng.randint(0, len(sent) - 1)]
                else:
                    mctr += 1
                    msg = "M{0}".format(mctr)
                ops.append(("RECV", p, msg))
                labels.append("{0}_RECV".format(msg))
        queries = []
        for t in range(nq):
            if labels and rng.random() < 0.85:
                e1 = labels[rng.randint(0, len(labels) - 1)]
            else:
                e1 = "Z{0}".format(t)
            if labels and rng.random() < 0.85:
                e2 = labels[rng.randint(0, len(labels) - 1)]
            else:
                e2 = "Z{0}b".format(t)
            queries.append((e1, e2))
        toks = [str(procs), str(n)]
        for (op, p, name) in ops:
            toks.append(op)
            toks.append(str(p))
            toks.append(name)
        toks.append(str(nq))
        for (e1, e2) in queries:
            toks.append(e1)
            toks.append(e2)
        vc = [[0] * procs for _ in range(procs)]
        events = {}
        inflight = {}
        for (op, p, name) in ops:
            if op == "LOCAL":
                vc[p][p] += 1
                events[name] = list(vc[p])
            elif op == "SEND":
                vc[p][p] += 1
                inflight[name] = list(vc[p])
                events[name + "_SEND"] = list(vc[p])
            else:
                cur = vc[p]
                got = inflight.get(name)
                if got is not None:
                    merged = [a if a >= b else b
                              for (a, b) in zip(cur, got)]
                else:
                    merged = list(cur)
                merged[p] += 1
                vc[p] = merged
                events[name + "_RECV"] = list(merged)
        zero = [0] * procs
        out = []
        for (e1, e2) in queries:
            v1 = events.get(e1, zero)
            v2 = events.get(e2, zero)
            aleb = all(a <= b for (a, b) in zip(v1, v2))
            blea = all(a <= b for (a, b) in zip(v2, v1))
            if aleb and blea:
                out.append("{0} == {1}".format(e1, e2))
            elif aleb:
                out.append("{0} -> {1}".format(e1, e2))
            elif blea:
                out.append("{0} -> {1}".format(e2, e1))
            else:
                out.append("{0} || {1}".format(e1, e2))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class PaxosGenerator(object):
    """Random PREPARE/PROPOSE stream over A acceptors; per-acceptor sim."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        acc = max(3, min(256, n // 200)) if n > 0 else 3
        ops = []
        cur = 0
        for i in range(n):
            if rng.random() < 0.5:
                cur += rng.randint(1, 3)
                ops.append(("PREPARE", cur, None))
            else:
                if rng.random() < 0.7:
                    cur += rng.randint(1, 3)
                    num = cur
                else:
                    num = rng.randint(0, cur)
                ops.append(("PROPOSE", num, "v{0}".format(i)))
        toks = [str(acc), str(n)]
        for (op, num, val) in ops:
            toks.append(op)
            toks.append(str(num))
            if val is not None:
                toks.append(val)
        majority = 1 + acc // 2
        promised = [0] * acc
        accepted = [0] * acc
        aval = [""] * acc
        out = []
        for (op, num, val) in ops:
            if op == "PREPARE":
                cnt = 0
                top_n = 0
                top_v = ""
                for i in range(acc):
                    if promised[i] < num:
                        promised[i] = num
                        cnt += 1
                        if accepted[i] > top_n:
                            top_n = accepted[i]
                            top_v = aval[i]
                if cnt >= majority:
                    if top_n > 0:
                        out.append("PROMISED {0} PREV {1} {2}".format(
                            num, top_n, top_v))
                    else:
                        out.append("PROMISED {0} NONE".format(num))
                else:
                    out.append("PREPARE REJECTED {0}".format(num))
            else:
                cnt = 0
                for i in range(acc):
                    if promised[i] <= num:
                        promised[i] = num
                        accepted[i] = num
                        aval[i] = val
                        cnt += 1
                if cnt >= majority:
                    out.append("CHOSEN {0} {1}".format(num, val))
                else:
                    out.append("PROPOSE REJECTED {0}".format(num))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class RaftGenerator(object):
    """Election/append/print event mix; full cluster-state sim."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        nodes = max(3, min(128, n // 200)) if n > 0 else 3
        events = []
        for i in range(n):
            r = rng.random()
            if r < 0.3:
                events.append(("START_ELECTION",
                               rng.randint(0, nodes - 1), None))
            elif r < 0.7:
                events.append(("APPEND_ENTRY",
                               rng.randint(0, nodes - 1),
                               "c{0}".format(i)))
            else:
                events.append(("PRINT_STATE",
                               rng.randint(0, nodes - 1), None))
        toks = [str(nodes), str(n)]
        for (ev, a, b) in events:
            toks.append(ev)
            toks.append(str(a))
            if b is not None:
                toks.append(b)
        majority = 1 + nodes // 2
        roles = ["FOLLOWER"] * nodes
        terms = [0] * nodes
        voted = [-1] * nodes
        commits = [-1] * nodes
        logsz = 0
        out = []
        for (ev, a, b) in events:
            if ev == "START_ELECTION":
                c = a
                t = terms[c] + 1
                roles[c] = "CANDIDATE"
                terms[c] = t
                voted[c] = c
                votes = 1
                for i in range(nodes):
                    if i != c and terms[i] < t:
                        roles[i] = "FOLLOWER"
                        terms[i] = t
                        voted[i] = c
                        votes += 1
                if votes >= majority:
                    roles[c] = "LEADER"
                    out.append("LEADER ELECTED: {0} TERM: {1}".format(c, t))
                else:
                    roles[c] = "FOLLOWER"
                    out.append("ELECTION FAILED: {0}".format(c))
            elif ev == "APPEND_ENTRY":
                l = a
                cmd = b
                if roles[l] != "LEADER":
                    out.append("REJECT NOT LEADER")
                else:
                    t = terms[l]
                    sz = logsz + 1
                    for i in range(nodes):
                        if i != l:
                            roles[i] = "FOLLOWER"
                            terms[i] = t
                    logsz = sz
                    if nodes >= majority:
                        commits = [sz - 1] * nodes
                        out.append("COMMITTED {0} AT INDEX {1}".format(
                            cmd, sz - 1))
            else:
                p = a
                out.append("NODE {0} ROLE {1} TERM {2} COMMITTED {3}".format(
                    p, roles[p], terms[p], commits[p]))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


def _next_greater_dists(ring):
    """Circular distance to the next strictly greater element (or n)."""
    n = len(ring)
    d = [n] * n
    stack = []
    for j in range(2 * n - 1, -1, -1):
        val = ring[j % n]
        while stack and ring[stack[-1] % n] <= val:
            stack.pop()
        if j < n and stack:
            d[j] = stack[-1] - j
        stack.append(j)
    return d


class LeaderElectionGenerator(object):
    """Random bignum ring ids; LCR sum + HS closed form over phases."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        ids = []
        for i in range(n):
            if ids and rng.random() < 0.1:
                ids.append(ids[rng.randint(0, len(ids) - 1)])
            else:
                ids.append(rng.randint(-1000000000000000000,
                                       1000000000000000000))
        toks = [str(n)]
        toks.extend(str(v) for v in ids)
        d_next = _next_greater_dists(ids)
        fwd = _next_greater_dists(list(reversed(ids)))
        d_prev = [fwd[n - 1 - i] for i in range(n)] if n else []
        lcr = sum(d_next)
        if n <= 1:
            hs = 0 if n == 0 else 1
        else:
            lims = []
            lim = 1
            while True:
                lims.append(lim)
                if lim >= n // 2:
                    break
                lim *= 2
            dall = sorted(d_next + d_prev)
            prefix = [0]
            for dd in dall:
                prefix.append(prefix[-1] + dd)
            total_n = len(dall)
            hs = 0
            for lim in lims:
                k = bisect.bisect_right(dall, lim)
                hs += prefix[k] + 2 * lim * (total_n - k)
        leader = -1
        for v in ids:
            if v > leader:
                leader = v
        out = ["LEADER: {0}".format(leader),
               "LCR MESSAGES: {0}".format(lcr),
               "HS MESSAGES: {0}".format(hs)]
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class GhsMstGenerator(object):
    """Random graph with distinct weights; Kruskal with union by size."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        nodes = max(2, n)
        m = 3 * nodes
        weights = list(range(m))
        rng.shuffle(weights)
        edges = []
        for i in range(m):
            u = rng.randint(0, nodes - 1)
            v = rng.randint(0, nodes - 2)
            if v >= u:
                v += 1
            edges.append((u, v, weights[i] - m // 2))
        toks = [str(nodes), str(m)]
        for (u, v, w) in edges:
            toks.append(str(u))
            toks.append(str(v))
            toks.append(str(w))
        parent = list(range(nodes))
        size = [1] * nodes

        def find(x):
            path = []
            while parent[x] != x:
                path.append(x)
                x = parent[x]
            for nd in path:
                parent[nd] = x
            return x

        weight = 0
        pairs = []
        for (u, v, w) in sorted(edges, key=lambda e: e[2]):
            ra = find(u)
            rb = find(v)
            if ra != rb:
                if size[ra] >= size[rb]:
                    parent[rb] = ra
                    size[ra] += size[rb]
                else:
                    parent[ra] = rb
                    size[rb] += size[ra]
                weight += w
                pairs.append((min(u, v), max(u, v)))
        pairs.sort()
        out = ["GHS MST Weight: {0}".format(weight),
               "Edges: {0}".format(len(pairs))]
        for (a, b) in pairs:
            out.append("{0} {1}".format(a, b))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)


class ChandyLamportGenerator(object):
    """Transfer/receive/snapshot mix; FIFO-channel snapshot sim."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        procs = max(2, min(64, n // 500)) if n > 0 else 2
        bals = [rng.randint(0, 1000) for _ in range(procs)]
        events = []
        for i in range(n):
            if i % 10 == 9:
                events.append(("SNAPSHOT_INIT",
                               rng.randint(0, procs - 1), None, None))
                continue
            r = rng.random()
            if r < 0.55:
                events.append(("TRANSFER", rng.randint(0, procs - 1),
                               rng.randint(0, procs - 1),
                               rng.randint(1, 100)))
            else:
                events.append(("RECEIVE", rng.randint(0, procs - 1),
                               rng.randint(0, procs - 1), None))
        toks = [str(procs), str(n)]
        toks.extend(str(b) for b in bals)
        for (ev, a, b, c) in events:
            toks.append(ev)
            toks.append(str(a))
            if ev == "TRANSFER":
                toks.append(str(b))
                toks.append(str(c))
            elif ev == "RECEIVE":
                toks.append(str(b))
        balance = list(bals)
        recorded = set()
        recbal = [0] * procs
        chans = {}
        recchans = {}
        recording = set()

        def push(chanmap, key, msg):
            if key in chanmap:
                chanmap[key].append(msg)
            else:
                chanmap[key] = [msg]

        def on_marker(u, v):
            if v in recorded:
                recording.discard((u, v))
                return
            recorded.add(v)
            recbal[v] = balance[v]
            for k in range(procs):
                if k == v:
                    continue
                if k != u:
                    recording.add((k, v))
                push(chans, (v, k), "MARKER")

        def on_snapshot_init(p):
            if p in recorded:
                return
            recorded.add(p)
            recbal[p] = balance[p]
            for k in range(procs):
                if k == p:
                    continue
                recording.add((k, p))
                push(chans, (p, k), "MARKER")

        chan_off = {}
        for (ev, a, b, c) in events:
            if ev == "TRANSFER":
                balance[a] -= c
                push(chans, (a, b), str(c))
            elif ev == "RECEIVE":
                key = (a, b)
                q = chans.get(key)
                off = chan_off.get(key, 0)
                if q is None or off >= len(q):
                    continue
                msg = q[off]
                chan_off[key] = off + 1
                if msg == "MARKER":
                    on_marker(a, b)
                else:
                    balance[b] += int(msg)
                    if key in recording:
                        push(recchans, key, msg)
            else:
                on_snapshot_init(a)
        out = ["P{0}: {1}".format(i, recbal[i]) for i in range(procs)]
        total = sum(recbal)
        for u in range(procs):
            for v in range(procs):
                for msg in recchans.get((u, v), []):
                    out.append("CHANNEL {0}->{1}: {2}".format(u, v, msg))
                    total += int(msg)
        out.append("TOTAL: {0}".format(total))
        return (" ".join(toks).encode("ascii"),
                ("\n".join(out) + "\n").encode("ascii"), n)
