"""Slope input generators for nine dp/math benchmarks.

Each class implements the shared generator interface:

    __init__(self, scales) stores list(scales)
    generate(self, n, seed=1000) -> (input_bytes, expected_bytes, fit_n)

generate is a pure function of (n, seed): stdlib only, deterministic
via random.Random(seed). fit_n is the work-scale number used for slope
fitting (usually n). Expected outputs replicate the real JVM solutions
exactly (verified diff-clean against `java -cp CP clojure.main
src/<path>.clj` under the harness normalize: per-line strip).

Benchmark key (area/problem/stem) -> class:

    dp/coin_change/solution -> CoinChangeGenerator
    dp/edit_distance/solution -> EditDistanceGenerator
    dp/knapsack/solution -> Knapsack01Generator
    dp/knapsack/unbounded_knapsack -> UnboundedKnapsackGenerator
    dp/lcs/solution -> LcsGenerator
    dp/lis/longest_increasing_subsequent -> LisGenerator
    dp/max_subarray/maxsubarray -> MaxSubarrayGenerator
    math/number_theory/crt -> CrtGenerator
    math/number_theory/miller_rabin_pollard -> MillerRabinPollardGenerator

SCALES proposes exactly 4 calibrated scales per benchmark (max/min >= 4,
top scale aimed at 5-60s local solve cpu).
"""

import random
import sys
from array import array
from bisect import bisect_left

# Python 3.11+ caps int<->str conversion at 4300 digits; CRT merged
# moduli legitimately reach 100k+ digits, so raise the cap when the
# interpreter supports it. No-op on older versions.
if hasattr(sys, "set_int_max_str_digits"):
    try:
        sys.set_int_max_str_digits(10000000)
    except (ValueError, AttributeError):
        pass

# String-building block size: bounds transient memory when joining
# multi-million-token inputs (same pattern as sort_arrays.py).
_BLOCK = 100000


def _lev(a, b):
    """Levenshtein distance of strings a and b, two-row DP."""
    if len(a) > len(b):
        a, b = b, a
    m = len(a)
    prev = list(range(m + 1))
    for i in range(1, len(b) + 1):
        bi = b[i - 1]
        cur = [i] + [0] * m
        for j in range(1, m + 1):
            if a[j - 1] == bi:
                cur[j] = prev[j - 1]
            else:
                x = prev[j]
                y = cur[j - 1]
                z = prev[j - 1]
                if y < x:
                    x = y
                if z < x:
                    x = z
                cur[j] = x + 1
        prev = cur
    return prev[m]


def _lcs_scores(x, y):
    """Row of LCS lengths of x against each prefix of y.

    Exact port of lcs-scores in src/dp/lcs/solution.clj.
    """
    m = len(y)
    prev = [0] * (m + 1)
    for xi in x:
        cur = [0] * (m + 1)
        for j in range(1, m + 1):
            if xi == y[j - 1]:
                cur[j] = prev[j - 1] + 1
            else:
                a = prev[j]
                b = cur[j - 1]
                cur[j] = a if a >= b else b
        prev = cur
    return prev


def _hirschberg(x, y):
    """Hirschberg LCS of lists x and y.

    Exact port of hirschberg in src/dp/lcs/solution.clj, including
    tie-breaks: first-max-k split (strict >), len-1 base cases via
    membership scan, mid = len(x) // 2. Callers must pass (b, a)
    (second run first), exactly as the solution does.
    """
    if not x or not y:
        return []
    if len(x) == 1:
        return [x[0]] if x[0] in y else []
    if len(y) == 1:
        return [y[0]] if y[0] in x else []
    mid = len(x) // 2
    x1 = x[:mid]
    x2 = x[mid:]
    fwd = _lcs_scores(x1, y)
    back = _lcs_scores(x2[::-1], y[::-1])
    m = len(y)
    best_val = -1
    best_k = 0
    for k in range(0, m + 1):
        v = fwd[k] + back[m - k]
        if v > best_val:
            best_val = v
            best_k = k
    return _hirschberg(x1, y[:best_k]) + _hirschberg(x2, y[best_k:])


def _cdivmod(a, b):
    """Truncating divmod matching Clojure quot/rem (b nonzero)."""
    q = abs(a) // abs(b)
    if (a < 0) != (b < 0):
        q = -q
    return q, a - q * b


def _ext_gcd(a, b):
    """Exact port of ext-gcd in src/math/number_theory/crt.clj."""
    old_r, r = a, b
    old_s, s = 1, 0
    old_t, t = 0, 1
    while r != 0:
        q, _ = _cdivmod(old_r, r)
        old_r, r = r, old_r - q * r
        old_s, s = s, old_s - q * s
        old_t, t = t, old_t - q * t
    return old_r, old_s, old_t


def _crt_merge(r1, m1, r2, m2):
    """Exact port of merge-two in src/math/number_theory/crt.clj."""
    g, p, _q = _ext_gcd(m1, m2)
    diff = r2 - r1
    _, rem = _cdivmod(diff, g)
    if rem != 0:
        return None
    m2r, _ = _cdivmod(m2, g)
    dq, _ = _cdivmod(diff, g)
    _, factor = _cdivmod(dq, m2r)
    _, pm = _cdivmod(p, m2r)
    _, k1 = _cdivmod(pm * factor, m2r)
    if k1 < 0:
        k1 += m2r
    new_m = m1 * m2r
    _, new_r = _cdivmod(r1 + m1 * k1, new_m)
    if new_r < 0:
        new_r += new_m
    return new_r, new_m


# Deterministic Miller-Rabin bases for n < 2**64.
_MR64_BASES = (2, 325, 9375, 28178, 450775, 9780504, 1795265022)
_TRIAL_PRIMES = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
                 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)


def _is_prime(n):
    """Deterministic primality for n < 2**64 (trial + Miller-Rabin)."""
    if n < 2:
        return False
    for p in _TRIAL_PRIMES:
        if n % p == 0:
            return n == p
    d = n - 1
    s = 0
    while d % 2 == 0:
        d //= 2
        s += 1
    for a in _MR64_BASES:
        if a % n == 0:
            continue
        x = pow(a, d, n)
        if x == 1 or x == n - 1:
            continue
        for _ in range(s - 1):
            x = (x * x) % n
            if x == n - 1:
                break
        else:
            return False
    return True


def _next_prime(start):
    """Smallest prime >= start (start odd, > 97 in our use)."""
    cand = start | 1
    while not _is_prime(cand):
        cand += 2
    return cand


class CoinChangeGenerator(object):
    """Stdin `amount m c1..cm`, stdout one integer via print (no newline).

    Coins are fixed 1..100 (worst-case inner scans); n is the amount so
    work (amount*m) scales linearly with n. Expected output replicates
    the 1-D coin-loop DP.
    """

    COINS = 100

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        m = self.COINS
        row = [0] * (n + 1)
        row[0] = 1
        for c in range(1, m + 1):
            r = row
            for j in range(c, n + 1):
                r[j] += r[j - c]
        parts = [str(n), str(m)]
        parts.extend(str(c) for c in range(1, m + 1))
        data_in = " ".join(parts).encode("ascii")
        return data_in, str(row[n]).encode("ascii"), n


class EditDistanceGenerator(object):
    """Stdin `t a1 b1 ...`, stdout t Levenshtein lines.

    CASES pairs of random length-n strings over a 4-letter alphabet
    (many matches/ties, exercises the match path). Work is
    CASES*n^2; fit_n = n.
    """

    CASES = 2
    ALPHABET = "abcd"

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        alpha = self.ALPHABET
        toks = [str(self.CASES)]
        out = []
        for _ in range(self.CASES):
            a = "".join([alpha[rng.randrange(4)] for _ in range(n)])
            b = "".join([alpha[rng.randrange(4)] for _ in range(n)])
            toks.append(a)
            toks.append(b)
            out.append(str(_lev(a, b)))
        data_in = " ".join(toks).encode("ascii")
        return data_in, ("\n".join(out) + "\n").encode("ascii"), n


class Knapsack01Generator(object):
    """Stdin `t` then per case `s n (w v)*n` (cap FIRST).

    CASES cases with cap = items = n, weights in [0, cap//2], values in
    [-50, 200] (negatives exercise the take-only-if-better rule).
    Expected output replicates the 1-D downward DP. Work ~ CASES*n^2.
    """

    CASES = 2

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        cap = n
        nitems = n
        wmax = max(1, cap // 2)
        toks = [str(self.CASES)]
        out = []
        for _ in range(self.CASES):
            toks.append(str(cap))
            toks.append(str(nitems))
            row = [0] * (cap + 1)
            for _ in range(nitems):
                w = rng.randrange(0, wmax + 1)
                v = rng.randrange(-50, 201)
                toks.append(str(w))
                toks.append(str(v))
                r = row
                for c in range(cap, w - 1, -1):
                    nv = r[c - w] + v
                    if nv > r[c]:
                        r[c] = nv
            out.append(str(row[cap]))
        data_in = " ".join(toks).encode("ascii")
        return data_in, ("\n".join(out) + "\n").encode("ascii"), n


class UnboundedKnapsackGenerator(object):
    """Stdin `t` then per case `n s w1..wn` (count FIRST, cap second).

    Note the header order is SWAPPED vs the 0/1 knapsack; value==weight.
    CASES cases with items = cap = n, weights in [1, 100]. Expected
    output replicates the unbounded upward DP. Work = CASES*n*(n+1).
    """

    CASES = 2

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        cap = n
        nitems = n
        toks = [str(self.CASES)]
        out = []
        for _ in range(self.CASES):
            vals = [rng.randrange(1, 101) for _ in range(nitems)]
            toks.append(str(nitems))
            toks.append(str(cap))
            toks.extend(str(w) for w in vals)
            row = [0] * (cap + 1)
            for c in range(cap + 1):
                best = row[c]
                for w in vals:
                    if w <= c:
                        nv = row[c - w] + w
                        if nv > best:
                            best = nv
                row[c] = best
            out.append(str(row[cap]))
        data_in = " ".join(toks).encode("ascii")
        return data_in, ("\n".join(out) + "\n").encode("ascii"), n


class LcsGenerator(object):
    """Stdin `n m a1..an b1..bm`, stdout one println of the LCS.

    Both runs have length n over an 8-value alphabet (many ties, which
    stresses tie-break replication). Expected output is an exact port of
    the solution's Hirschberg (called as (hirschberg b a), strict->
    first-max-k split, membership-scan len-1 bases). Empty LCS gives a
    blank line. Work ~ 2*n^2.
    """

    ALPHABET = 8

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        alpha = self.ALPHABET
        a = [rng.randrange(alpha) for _ in range(n)]
        b = [rng.randrange(alpha) for _ in range(n)]
        ans = _hirschberg(b, a)
        toks = [str(n), str(n)]
        toks.extend(str(v) for v in a)
        toks.extend(str(v) for v in b)
        data_in = " ".join(toks).encode("ascii")
        expected = (" ".join(str(v) for v in ans) + "\n").encode("ascii")
        return data_in, expected, n


class LisGenerator(object):
    """Stdin `n x1..xn`, stdout length of strictly increasing run.

    Input is a fixed-seed shuffle of range(n) (average case for the
    patience piles). Expected output replicates patience + bisect_left
    (matching the solution's lower-bound). Work O(n log n); fit_n = n.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        vals = array("l", range(n))
        rng.shuffle(vals)
        tails = []
        append = tails.append
        bl = bisect_left
        for v in vals:
            i = bl(tails, v)
            if i == len(tails):
                append(v)
            else:
                tails[i] = v
        buf = bytearray(str(n).encode("ascii"))
        for start in range(0, n, _BLOCK):
            buf += b" " + " ".join(
                str(v) for v in vals[start:start + _BLOCK]
            ).encode("ascii")
        return bytes(buf), (str(len(tails)) + "\n").encode("ascii"), n


class MaxSubarrayGenerator(object):
    """Stdin `t` then per case `n v1..vn` (n>=1).

    CASES cases totalling n values uniform in [-1e6, 1e6]. Expected
    output replicates the one-pass Kadane (nonempty) plus sum-of-
    positives-else-max. Work O(n); fit_n = n.
    """

    CASES = 4
    LO = -1000000
    HI = 1000001

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        base = n // self.CASES
        buf = bytearray(str(self.CASES).encode("ascii"))
        out = []
        for ci in range(self.CASES):
            if ci < self.CASES - 1:
                m = base
            else:
                m = n - base * (self.CASES - 1)
            if m < 1:
                m = 1
            buf += b" " + str(m).encode("ascii")
            cur = None
            best = None
            maxe = None
            pos = 0
            for start in range(0, m, _BLOCK):
                sz = min(_BLOCK, m - start)
                blk = [rng.randrange(self.LO, self.HI) for _ in range(sz)]
                for v in blk:
                    if cur is None:
                        cur = v
                        best = v
                        maxe = v
                        if v > 0:
                            pos = v
                    else:
                        nv = cur + v
                        cur = v if v > nv else nv
                        if cur > best:
                            best = cur
                        if v > maxe:
                            maxe = v
                        if v > 0:
                            pos += v
                buf += b" " + " ".join(
                    str(v) for v in blk
                ).encode("ascii")
            noncontig = pos if pos > 0 else maxe
            out.append("{0} {1}".format(best, noncontig))
        return bytes(buf), ("\n".join(out) + "\n").encode("ascii"), n


class CrtGenerator(object):
    """Stdin `k (r m)*k`, stdout `NO SOLUTION` or `<R> <M>`.

    k congruences with random 512-bit odd moduli and residues of one
    secret X (always consistent, so full-fold work, no short-circuit).
    Expected output is an exact port of the generalized-CRT fold
    (truncating quot/rem included). Work ~ k^2 * MOD_BITS in bigint
    limb ops; fit_n = k.
    """

    MOD_BITS = 512

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        k = int(n)
        rng = random.Random(seed)
        bits = self.MOD_BITS
        top = 1 << (bits - 1)
        mods = [(rng.getrandbits(bits) | top | 1) for _ in range(k)]
        secret = rng.getrandbits(max(256, k))
        pairs = [(secret % m, m) for m in mods]
        r, m = pairs[0]
        for r2, m2 in pairs[1:]:
            merged = _crt_merge(r, m, r2, m2)
            if merged is None:  # unreachable for consistent residues
                return self._encode(pairs), b"NO SOLUTION\n", k
            r, m = merged
        return self._encode(pairs), "{0} {1}\n".format(r, m).encode(
            "ascii"), k

    @staticmethod
    def _encode(pairs):
        toks = [str(len(pairs))]
        for r, m in pairs:
            toks.append(str(r))
            toks.append(str(m))
        return " ".join(toks).encode("ascii")


class MillerRabinPollardGenerator(object):
    """Stdin `t v1..vt`, stdout per value PRIME or COMPOSITE + factors.

    COUNT construction-known semiprimes n=p*q with p, q distinct random
    (bits//2)-bit primes (deterministic trial division + deterministic
    Miller-Rabin for <2**64). Expected factors are the known [p, q]
    (sorted), verified by multiplication. All n < 2**78, under the
    solution's 12-base MR determinism ceiling (318665857834059776441
    ~ 2**78.2), so JVM verdicts agree. Pollard-rho work ~ N**1/4 per
    value; fit_n = max N.
    """

    COUNT = 16

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        bits = int(n)
        rng = random.Random(seed)
        half = bits // 2
        top = 1 << (half - 1)
        vals = []
        facs = []
        for _ in range(self.COUNT):
            p = _next_prime(rng.getrandbits(half) | top | 1)
            q = _next_prime(rng.getrandbits(half) | top | 1)
            if q == p:
                q = _next_prime(q + 2)
            ordered = sorted([p, q])
            if ordered[0] * ordered[1] != p * q:
                raise ValueError("factor check failed")
            vals.append(p * q)
            facs.append(ordered)
        toks = [str(self.COUNT)]
        toks.extend(str(v) for v in vals)
        lines = ["COMPOSITE {0} {1}".format(f[0], f[1]) for f in facs]
        data_in = " ".join(toks).encode("ascii")
        return data_in, ("\n".join(lines) + "\n").encode("ascii"), max(vals)


# Calibrated scale proposals: benchmark key -> exactly 4 scales.
# (max/min >= 4; top scale targets 5-60s local solve cpu.)
SCALES = {
    "dp/coin_change/solution": [80000, 160000, 320000, 640000],
    "dp/edit_distance/solution": [750, 1500, 3000, 6000],
    "dp/knapsack/solution": [1250, 2500, 5000, 10000],
    "dp/knapsack/unbounded_knapsack": [1250, 2500, 5000, 10000],
    "dp/lcs/solution": [1000, 2000, 4000, 8000],
    "dp/lis/longest_increasing_subsequent": [500000, 1000000, 2000000,
                                             4000000],
    "dp/max_subarray/maxsubarray": [1000000, 2000000, 4000000, 8000000],
    "math/number_theory/crt": [250, 500, 1000, 2000],
    "math/number_theory/miller_rabin_pollard": [54, 62, 70, 78],
}
