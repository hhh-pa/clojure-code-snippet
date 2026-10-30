"""Slope input generators for the string_related benchmarks (T20).

Each generator produces (input_bytes, expected_bytes, fit_n) for one
benchmark's stdin contract at requested scales. Every forcing input
is construction-known: the expected output is derived from the
construction (usually a constant), never by porting the solver, and
was verified diff-clean against the real JVM solutions during
calibration (see the generator report; harness diff ignores trailing
whitespace per line).

Proposed scales per benchmark live in SCALES below (benchmark key as
in the BMF registry -> exactly 4 scales, 8x max/min).
"""

import random

# Chunk size for building multi-megabyte uniform runs without large
# transient copies.
_BLOCK = 1000000


def _repeat_into(buf, byte, n):
    """Append byte * n to bytearray buf in bounded chunks."""
    while n > 0:
        take = _BLOCK if n > _BLOCK else n
        buf += byte * take
        n -= take


class DnaHealthGenerator(object):
    """Aho-Corasick DNA health: genes over {a}, strands of b's only.

    Zero matches by construction, so every strand scores 0 and stdout
    is `0 0`. The automaton still scans every strand char. n sets the
    gene count, the strand count and (with the fixed strand length)
    the total scan work.
    """

    STRAND_LEN = 2000

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        rng = random.Random(seed)
        genes = ["a" * (5 + (i % 16)) for i in range(n)]
        healths = [str(rng.randrange(-1000000, 1000000))
                   for _ in range(n)]
        trip = "0 {0} {1}".format(n - 1, "b" * self.STRAND_LEN)
        parts = [str(n)] + genes + healths + [str(n)] + [trip] * n
        return " ".join(parts).encode("ascii"), b"0 0\n", n


class SherlockAnagramsGenerator(object):
    """Sherlock and anagrams: q=n all-distinct-char strings.

    Every string is a shuffled 26-letter alphabet, so no two
    substrings share a multiset and every count is 0. Distinctness
    is asserted at generation time.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        rng = random.Random(seed)
        alpha = list("abcdefghijklmnopqrstuvwxyz")
        words = []
        for _ in range(n):
            rng.shuffle(alpha)
            word = "".join(alpha)
            assert len(set(word)) == 26
            words.append(word)
        data_in = ("{0} {1}".format(n, " ".join(words))).encode("ascii")
        return data_in, ("0\n" * n).encode("ascii"), n


class BoyerMooreSubstringGenerator(object):
    """Boyer-Moore substring search: text `a`*n, pattern `b`+`a`*999.

    The leading `b` never matches, so every answer is NO, while each
    alignment still compares the full `a` tail before shifting.
    """

    PAIRS = 2
    PATTERN_LEN = 1000

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        pat = b"b" + b"a" * (self.PATTERN_LEN - 1)
        buf = bytearray(str(self.PAIRS).encode("ascii"))
        for _ in range(self.PAIRS):
            buf += b" "
            _repeat_into(buf, b"a", n)
            buf += b" " + pat
        return bytes(buf), b"NO\n" * self.PAIRS, n


class KmpSubstringGenerator(object):
    """KMP substring search: text `a`*n, pattern `a`*999+`b`.

    The trailing `b` never matches, so every answer is NO, while the
    scan walks the full text with maximal partial matches.
    """

    PAIRS = 2
    PATTERN_LEN = 1000

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        pat = b"a" * (self.PATTERN_LEN - 1) + b"b"
        buf = bytearray(str(self.PAIRS).encode("ascii"))
        for _ in range(self.PAIRS):
            buf += b" "
            _repeat_into(buf, b"a", n)
            buf += b" " + pat
        return bytes(buf), b"NO\n" * self.PAIRS, n


class ZFunctionSubstringGenerator(object):
    """Z-function substring search: same forcing shape as the KMP one."""

    PAIRS = 4
    PATTERN_LEN = 1000

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        pat = b"a" * (self.PATTERN_LEN - 1) + b"b"
        buf = bytearray(str(self.PAIRS).encode("ascii"))
        for _ in range(self.PAIRS):
            buf += b" "
            _repeat_into(buf, b"a", n)
            buf += b" " + pat
        return bytes(buf), b"NO\n" * self.PAIRS, n


class CommonChildGenerator(object):
    """Common child (line-based LCS length): `a`*n vs `b`*n.

    No common chars, so the LCS length is 0 while the DP still fills
    the full n*m table.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        return b"a" * n + b"\n" + b"b" * n + b"\n", b"0\n", n


class HirschbergGenerator(object):
    """Hirschberg alignment: `a`*n vs `b`*n (n == m, all mismatch).

    Gaps (-2) are strictly worse than mismatches (-1), so the unique
    optimum is all-diagonal: score -n, aligned lines equal the inputs
    (verified against the real solution, whose tie-breaks keep every
    split centered on this input).
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        data_in = b"a" * n + b" " + b"b" * n
        data_out = (str(-n) + "\n").encode("ascii") + b"a" * n + b"\n" \
            + b"b" * n + b"\n"
        return data_in, data_out, n


class SequenceAlignmentGenerator(object):
    """Global sequence alignment: `a`*n vs `b`*n, all mismatch.

    Same optimum as the Hirschberg forcing input (score -n, originals
    reprinted) under the identical +2/-1/-2 scoring; the diagonal-first
    traceback takes the diagonal at every step here.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        data_in = b"GLOBAL " + b"a" * n + b" " + b"b" * n
        data_out = (str(-n) + "\n").encode("ascii") + b"a" * n + b"\n" \
            + b"b" * n + b"\n"
        return data_in, data_out, n


class ManacherGenerator(object):
    """Manacher palindromes: `a`*n.

    Every substring is a palindrome: max length n, longest the whole
    string, total count n*(n+1)/2.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        data_in = b"a" * n
        data_out = ("{0}\n{1}\n{2}\n".format(
            n, "a" * n, n * (n + 1) // 2)).encode("ascii")
        return data_in, data_out, n


class SuffixArrayFunctionGenerator(object):
    """String function calculation (suffix array): `a`*n.

    Distinct substrings are `a`^k for k in 1..n with n-k+1
    occurrences, so the value is max_k k*(n-k+1) = (n+1)^2//4.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        data_out = ("{0}\n".format((n + 1) * (n + 1) // 4)).encode("ascii")
        return b"a" * n, data_out, n


class SuffixTreeFunctionGenerator(object):
    """String function calculation (suffix tree): `a`*n, same value."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        data_out = ("{0}\n".format((n + 1) * (n + 1) // 4)).encode("ascii")
        return b"a" * n, data_out, n


class SuffixAutomatonGenerator(object):
    """Suffix automaton queries over `a`*n.

    A unary string needs no clones: n+1 states, n transitions, n
    distinct substrings (verified). Queries mix YES (`a` runs) and NO
    (`b`, `ab`) answers.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        queries = ["a" * max(1, n // 4), "a" * n, "b", "ab"]
        data_in = ("{0} 4 {1}".format(
            "a" * n, " ".join(queries))).encode("ascii")
        data_out = ("States: {0}\nTransitions: {1}\n"
                    "Distinct substrings: {1}\n"
                    "YES\nYES\nNO\nNO\n".format(n + 1, n)).encode("ascii")
        return data_in, data_out, n


class NoPrefixSetGenerator(object):
    """No-prefix-set: n fixed-width distinct words -> GOOD SET.

    Words are zero-padded `w` + number at one shared width, so all
    are distinct and same-length: no word can prefix another.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        rng = random.Random(seed)
        width = max(6, len(str(n - 1)))
        words = ["w{0:0{1}d}".format(i, width) for i in range(n)]
        rng.shuffle(words)
        data_in = ("{0} {1}".format(n, " ".join(words))).encode("ascii")
        return data_in, b"GOOD SET\n", n


class UkkonenGenerator(object):
    """Ukkonen suffix tree queries over `a`*n.

    A unary string builds just root + one leaf edge (Nodes: 2) with n
    distinct substrings (verified). Queries mirror the automaton one.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        """Return (input_bytes, expected_bytes, fit_n) for size n."""
        n = max(1, int(n))
        queries = ["a" * max(1, n // 4), "a" * n, "b", "ab"]
        data_in = ("{0} 4 {1}".format(
            "a" * n, " ".join(queries))).encode("ascii")
        data_out = ("Nodes: 2\nDistinct substrings: {0}\n"
                    "YES\nYES\nNO\nNO\n".format(n)).encode("ascii")
        return data_in, data_out, n


# Benchmark key (area/problem/stem) -> proposed slope scales (8x).
SCALES = {
    "string_related/aho_corasick/determining_dna_health":
        [2000, 4000, 8000, 16000],
    "string_related/anagrams/sherlock_and_anagrams":
        [10000, 20000, 40000, 80000],
    "string_related/boyer_moore/substring_searching":
        [10000000, 20000000, 40000000, 80000000],
    "string_related/kmp/substring_searching":
        [2000000, 4000000, 8000000, 16000000],
    "string_related/z_function/substring_searching":
        [500000, 1000000, 2000000, 4000000],
    "string_related/common_child/common_child":
        [2000, 4000, 8000, 16000],
    "string_related/hirschberg/hirschberg":
        [1000, 2000, 4000, 8000],
    "string_related/sequence_alignment/sequence_alignment":
        [1000, 2000, 4000, 8000],
    "string_related/manacher/manacher":
        [500000, 1000000, 2000000, 4000000],
    "string_related/suffix_array/string_function_calculation":
        [200000, 400000, 800000, 1600000],
    "string_related/suffix_tree/string_function_calculation":
        [625000, 1250000, 2500000, 5000000],
    "string_related/suffix_automaton/suffix_automaton":
        [200000, 400000, 800000, 1600000],
    "string_related/trie/no_prefix_set":
        [250000, 500000, 1000000, 2000000],
    "string_related/ukkonen/ukkonen":
        [2500000, 5000000, 10000000, 20000000],
}
