"""Integer-array generator for the shared sort stdin contract (T20).

Stdin: `n` followed by n longs. Stdout: sorted values printed as
`[v1,v2,...]`. Serves every sort pilot with this driver shape
(verified identical in all ten sort solutions).

Also hosts QuickselectGenerator: same stdin shape (strictly
validated) but stdout is the rank-quot(n,2) order statistic.
"""

import random
from array import array

# String-building block size: keeps 8M-element generation peak
# memory near ~100 MB instead of ~500 MB of transient strs.
_BLOCK = 100000


class SortArrayGenerator(object):
    """Generates shuffled-range inputs at fixed scales."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n.

        Input is a fixed-seed shuffle of range(n): average-case for
        both insertion sort (Theta(n^2)) and quicksort
        (Theta(n log n)). Expected output is the sorted identity
        `[0,1,...,n-1]`, matching the driver's exact print format
        (comma-separated, no spaces, no trailing newline).
        """
        n = int(n)
        rng = random.Random(seed)
        # array('l') keeps 8M-element inputs near ~100 MB peak.
        vals = array("l", range(n))
        rng.shuffle(vals)
        buf = bytearray(str(n).encode("ascii"))
        for start in range(0, n, _BLOCK):
            buf += b" " + " ".join(
                str(v) for v in vals[start:start + _BLOCK]
            ).encode("ascii")
        data_in = bytes(buf)
        out = bytearray(b"[")
        for start in range(0, n, _BLOCK):
            if start:
                out += b","
            out += ",".join(
                str(v) for v in range(start, min(start + _BLOCK, n))
            ).encode("ascii")
        out += b"]"
        return data_in, bytes(out), n


class QuickselectGenerator(object):
    """Generates shuffled-range inputs for the quickselect driver."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n.

        Input is a fixed-seed shuffle of range(n): average-case
        linear for randomized quickselect. Expected output is the
        rank-quot(n,2) order statistic (`n//2` for a permutation
        of range(n)) plus the driver's trailing newline. The
        driver strictly requires n >= 1 with exactly n values.
        """
        n = int(n)
        if n < 1:
            raise ValueError("quickselect needs n >= 1, got %d" % n)
        rng = random.Random(seed)
        vals = array("l", range(n))
        rng.shuffle(vals)
        buf = bytearray(str(n).encode("ascii"))
        for start in range(0, n, _BLOCK):
            buf += b" " + " ".join(
                str(v) for v in vals[start:start + _BLOCK]
            ).encode("ascii")
        return (bytes(buf),
                "{}\n".format(n // 2).encode("ascii"), n)
