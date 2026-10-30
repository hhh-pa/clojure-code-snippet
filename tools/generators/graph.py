"""Graph-benchmark input generators with construction-known answers.

Each generator emits inputs whose correct output is trivially derivable
(a complete graph, a line graph, a directed cycle, ...), so the expected
bytes cost O(1)/O(n) to build even when the subject solution does
superlinear work. Every solution below shares the repo-wide stdin
contract: signed-64-bit-int + whitespace-only tokens, else an stderr
message with exit 0; blank stdin stays silent; extra tokens ignored.

All generators share one interface: ``__init__(self, scales)`` stores
``list(scales)``, and ``generate(self, n, seed=1000)`` returns
``(input_bytes, expected_bytes, fit_n)`` as a pure function of
``(n, seed)`` (stdlib only, ``random.Random(seed)``, deterministic).
``fit_n`` is the work-scale number the slope fit should use.
"""

import random

# Tokens per string-building block; keeps million-edge generation
# peak memory near one input copy instead of many transient strs.
_BLOCK = 100000

# Benchmark id (graph/<problem>/<file-stem>) -> solution source path
# relative to the repo src/ directory.
SOLUTION_PATHS = {
    "graph/all_pairs_shortest_path/floyd_city_of_blinding_lights":
        "graph/all_pairs_shortest_path/floyd_city_of_blinding_lights.clj",
    "graph/all_pairs_shortest_path/johnson":
        "graph/all_pairs_shortest_path/johnson.clj",
    "graph/bridges/even_tree":
        "graph/bridges/even_tree.clj",
    "graph/eulerian/hierholzer":
        "graph/eulerian/hierholzer.clj",
    "graph/flow/dinic":
        "graph/flow/dinic.clj",
    "graph/flow/push_relabel":
        "graph/flow/push_relabel.clj",
    "graph/matching/hopcroft_karp":
        "graph/matching/hopcroft_karp.clj",
    "graph/matching/hungarian":
        "graph/matching/hungarian.clj",
    "graph/minimum_spanning_tree/boruvka":
        "graph/minimum_spanning_tree/boruvka.clj",
    "graph/minimum_spanning_tree/kruskalmstrsub":
        "graph/minimum_spanning_tree/kruskalmstrsub.clj",
    "graph/minimum_spanning_tree/primsmstsub":
        "graph/minimum_spanning_tree/primsmstsub.clj",
    "graph/shortest_path/bellman_ford":
        "graph/shortest_path/bellman_ford.clj",
    "graph/shortest_path/bfsshortreach":
        "graph/shortest_path/bfsshortreach.clj",
    "graph/shortest_path/dijkstrashortreach":
        "graph/shortest_path/dijkstrashortreach.clj",
    "graph/strongly_connected_component/kosaraju":
        "graph/strongly_connected_component/kosaraju.clj",
    "graph/strongly_connected_component/tarjan_scc":
        "graph/strongly_connected_component/tarjan_scc.clj",
    "graph/topological_sort/making_make":
        "graph/topological_sort/making_make.clj",
}


def _pack(blocks):
    """Join str blocks into ascii input bytes."""
    return " ".join(blocks).encode("ascii")


class FloydGenerator(object):
    """Complete digraph (weight 1) + random queries; every answer is 1."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 2:
            n = 2
        rng = random.Random(seed)
        blocks = ["{0} {1}".format(n, n * (n - 1))]
        for u in range(1, n + 1):
            blocks.append(" ".join("{0} {1} 1".format(u, v)
                                   for v in range(1, n + 1) if v != u))
        q = n
        queries = []
        for _ in range(q):
            u = rng.randint(1, n)
            v = rng.randint(1, n - 1)
            if v >= u:
                v += 1
            queries.append("{0} {1}".format(u, v))
        blocks.append("{0}".format(q))
        blocks.append(" ".join(queries))
        return _pack(blocks), ("1\n" * q).encode("ascii"), n


class JohnsonGenerator(object):
    """Bidirected line graph (weight 1); dist(i, j) = |i - j|."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {1}".format(n, 2 * (n - 1))]
        for start in range(0, n - 1, _BLOCK):
            blocks.append(" ".join("{0} {1} 1 {1} {0} 1".format(i, i + 1)
                                   for i in range(start, min(start + _BLOCK,
                                                             n - 1))))
        rows = []
        for i in range(n):
            rows.append(" ".join(str(abs(i - j)) for j in range(n)))
        return _pack(blocks), ("\n".join(rows) + "\n").encode("ascii"), n


class EvenTreeGenerator(object):
    """1-2-...-n path; root-1 even-subtree edge count is (n-1)//2."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {1}".format(n, n - 1)]
        for start in range(1, n, _BLOCK):
            blocks.append(" ".join("{0} {1}".format(i, i + 1)
                                   for i in range(start, min(start + _BLOCK,
                                                             n))))
        return _pack(blocks), ("{0}\n".format((n - 1) // 2)).encode("ascii"), n


class HierholzerGenerator(object):
    """Directed n-cycle; the trail 0 1 ... n-1 0 is a CIRCUIT."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {1}".format(n, n)]
        for start in range(0, n, _BLOCK):
            blocks.append(" ".join("{0} {1}".format(i, (i + 1) % n)
                                   for i in range(start, min(start + _BLOCK,
                                                             n))))
        trail = " ".join([str(i) for i in range(n)] + ["0"])
        out = "CIRCUIT\n{0}\n".format(trail)
        return _pack(blocks), out.encode("ascii"), n


class _FlowBipartite(object):
    """Shared s -> layer -> t unit-cap forcing; max flow = layer size."""

    def _generate(self, n):
        n = int(n)
        if n < 1:
            n = 1
        t = n + 1
        blocks = ["{0} {1} 0 {2}".format(n + 2, 2 * n, t)]
        for start in range(1, n + 1, _BLOCK):
            blocks.append(" ".join("0 {0} 1 {0} {1} 1".format(i, t)
                                   for i in range(start, min(start + _BLOCK,
                                                             n + 1))))
        return _pack(blocks), ("{0}\n".format(n)).encode("ascii"), n


class DinicGenerator(_FlowBipartite):
    """s -> layer -> t unit caps (s != t); max flow = layer size."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        return self._generate(n)


class PushRelabelGenerator(_FlowBipartite):
    """Same forcing as Dinic (s != t, nonnegative caps only)."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        return self._generate(n)


class HopcroftKarpGenerator(object):
    """Diagonal-only edges (i -> i); the matching is unique and forced."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {0} {0}".format(n)]
        for start in range(1, n + 1, _BLOCK):
            blocks.append(" ".join("{0} {0}".format(i)
                                   for i in range(start, min(start + _BLOCK,
                                                             n + 1))))
        lines = ["{0}".format(n)]
        for start in range(1, n + 1, _BLOCK):
            lines.append("\n".join("{0} {0}".format(i)
                                   for i in range(start, min(start + _BLOCK,
                                                             n + 1))))
        return _pack(blocks), ("\n".join(lines) + "\n").encode("ascii"), n


class HungarianGenerator(object):
    """All-zero costs; optimum is total 0 with the identity assignment.

    Every row scan finds all-zero reduced costs, so row i walks i dual
    steps to the first free column (identity, verified diff-clean):
    the subject still pays full O(n^2 m) work. A 0-diagonal/BIG matrix
    instead converges in one step per row (O(n m), instant at n = 400),
    so it cannot feed a slope fit; hence this construction.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {0}".format(n)]
        row = " ".join(["0"] * n)
        for _ in range(n):
            blocks.append(row)
        lines = ["0"]
        for i in range(1, n + 1):
            lines.append("{0} {0}".format(i))
        return _pack(blocks), ("\n".join(lines) + "\n").encode("ascii"), n


class BoruvkaGenerator(object):
    """0-based line graph (weight 1); MST weight is n - 1."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {1}".format(n, n - 1)]
        for start in range(0, n - 1, _BLOCK):
            blocks.append(" ".join("{0} {1} 1".format(i, i + 1)
                                   for i in range(start, min(start + _BLOCK,
                                                             n - 1))))
        return _pack(blocks), ("{0}\n".format(n - 1)).encode("ascii"), n


class KruskalMstGenerator(object):
    """1-based line graph (weight 1); forest weight is n - 1."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {1}".format(n, n - 1)]
        for start in range(1, n, _BLOCK):
            blocks.append(" ".join("{0} {1} 1".format(i, i + 1)
                                   for i in range(start, min(start + _BLOCK,
                                                             n))))
        return _pack(blocks), ("{0}\n".format(n - 1)).encode("ascii"), n


class PrimsMstGenerator(object):
    """1-based line graph (weight 1), start = 1; MST weight is n - 1."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {1}".format(n, n - 1)]
        for start in range(1, n, _BLOCK):
            blocks.append(" ".join("{0} {1} 1".format(i, i + 1)
                                   for i in range(start, min(start + _BLOCK,
                                                             n))))
        blocks.append("1")
        return _pack(blocks), ("{0}\n".format(n - 1)).encode("ascii"), n


class BellmanFordGenerator(object):
    """0-based directed path (weight 1) from s = 0, edges in REVERSE.

    Distances stay dist(i) = i, but reverse listing order forces one
    newly settled vertex per relaxation pass, so the subject (which
    scans edges in input order with early exit) pays full O(V E) work.
    Forward order converges in 2 passes (instant even at V = 100000).
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["{0} {1} 0".format(n, n - 1)]
        for start in range(n - 2, -1, -_BLOCK):
            blocks.append(" ".join("{0} {1} 1".format(i, i + 1)
                                   for i in range(start, max(start - _BLOCK,
                                                             -1), -1)))
        lines = []
        for start in range(0, n, _BLOCK):
            lines.append("\n".join(str(i)
                                   for i in range(start, min(start + _BLOCK,
                                                             n))))
        return _pack(blocks), ("\n".join(lines) + "\n").encode("ascii"), n


class BfsShortReachGenerator(object):
    """One 1-based path case from src 1; dists are 6, 12, ... ."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["1 {0} {1}".format(n, n - 1)]
        for start in range(1, n, _BLOCK):
            blocks.append(" ".join("{0} {1}".format(i, i + 1)
                                   for i in range(start, min(start + _BLOCK,
                                                             n))))
        blocks.append("1")
        out = " ".join(str(6 * i) for i in range(1, n)) + "\n"
        return _pack(blocks), out.encode("ascii"), n


class DijkstraShortReachGenerator(object):
    """One 1-based unit-weight path case; dists 1..n-1 plus trail space."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 1:
            n = 1
        blocks = ["1 {0} {1}".format(n, n - 1)]
        for start in range(1, n, _BLOCK):
            blocks.append(" ".join("{0} {1} 1".format(i, i + 1)
                                   for i in range(start, min(start + _BLOCK,
                                                             n))))
        blocks.append("1")
        out = "".join("{0} ".format(i) for i in range(1, n)) + "\n"
        return _pack(blocks), out.encode("ascii"), n


def _cycle_cover(n, size=3):
    """Largest vertex count <= n covered by disjoint size-cycles."""
    blocks = max(1, int(n) // size)
    return size * blocks, blocks


class KosarajuGenerator(object):
    """Disjoint directed 3-cycles (1-based); odd-size sum is +nverts."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        nverts, nblocks = _cycle_cover(n)
        blocks = ["{0} 0".format(nverts)]
        for start in range(0, nblocks, _BLOCK):
            parts = []
            for b in range(start, min(start + _BLOCK, nblocks)):
                a = 3 * b + 1
                parts.append("{0} {1} {1} {2} {2} {0}".format(a, a + 1,
                                                              a + 2))
            blocks.append(" ".join(parts))
        return _pack(blocks), ("{0}\n".format(nverts)).encode("ascii"), nverts


class TarjanSccGenerator(object):
    """Disjoint directed 3-cycles (0-based); components print in order."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        nverts, nblocks = _cycle_cover(n)
        blocks = ["{0} {0}".format(nverts)]
        lines = ["{0}".format(nblocks)]
        for start in range(0, nblocks, _BLOCK):
            edge_parts = []
            line_parts = []
            for b in range(start, min(start + _BLOCK, nblocks)):
                a = 3 * b
                edge_parts.append("{0} {1} {1} {2} {2} {0}".format(a, a + 1,
                                                                   a + 2))
                line_parts.append("{0} {1} {2}".format(a, a + 1, a + 2))
            blocks.append(" ".join(edge_parts))
            lines.append("\n".join(line_parts))
        return _pack(blocks), ("\n".join(lines) + "\n").encode("ascii"), nverts


class MakingMakeGenerator(object):
    """Empty edge set; MAX-FIRST Kahn order is n-1 .. 0 plus trail space."""

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        if n < 0:
            n = 0
        out = "".join("{0} ".format(i) for i in range(n - 1, -1, -1)) + "\n"
        return ("{0} 0".format(n)).encode("ascii"), out.encode("ascii"), n


# Benchmark id (graph/<problem>/<file-stem>) -> generator class.
GENERATOR_CLASSES = {
    "graph/all_pairs_shortest_path/floyd_city_of_blinding_lights":
        FloydGenerator,
    "graph/all_pairs_shortest_path/johnson": JohnsonGenerator,
    "graph/bridges/even_tree": EvenTreeGenerator,
    "graph/eulerian/hierholzer": HierholzerGenerator,
    "graph/flow/dinic": DinicGenerator,
    "graph/flow/push_relabel": PushRelabelGenerator,
    "graph/matching/hopcroft_karp": HopcroftKarpGenerator,
    "graph/matching/hungarian": HungarianGenerator,
    "graph/minimum_spanning_tree/boruvka": BoruvkaGenerator,
    "graph/minimum_spanning_tree/kruskalmstrsub": KruskalMstGenerator,
    "graph/minimum_spanning_tree/primsmstsub": PrimsMstGenerator,
    "graph/shortest_path/bellman_ford": BellmanFordGenerator,
    "graph/shortest_path/bfsshortreach": BfsShortReachGenerator,
    "graph/shortest_path/dijkstrashortreach": DijkstraShortReachGenerator,
    "graph/strongly_connected_component/kosaraju": KosarajuGenerator,
    "graph/strongly_connected_component/tarjan_scc": TarjanSccGenerator,
    "graph/topological_sort/making_make": MakingMakeGenerator,
}

# Proposed slope scales per benchmark (4 each, max/min >= 4).
# Calibrated against the real JVM solutions; see delegation report.
DEFAULT_SCALES = {
    "graph/all_pairs_shortest_path/floyd_city_of_blinding_lights":
        [50, 100, 200, 400],
    "graph/all_pairs_shortest_path/johnson":
        [400, 800, 1600, 3200],
    "graph/bridges/even_tree":
        [125000, 250000, 500000, 1000000],
    "graph/eulerian/hierholzer":
        [125000, 250000, 500000, 1000000],
    "graph/flow/dinic":
        [80000, 160000, 320000, 640000],
    "graph/flow/push_relabel":
        [80000, 160000, 320000, 640000],
    "graph/matching/hopcroft_karp":
        [125000, 250000, 500000, 1000000],
    "graph/matching/hungarian":
        [50, 100, 200, 400],
    "graph/minimum_spanning_tree/boruvka":
        [125000, 250000, 500000, 1000000],
    "graph/minimum_spanning_tree/kruskalmstrsub":
        [125000, 250000, 500000, 1000000],
    "graph/minimum_spanning_tree/primsmstsub":
        [125000, 250000, 500000, 1000000],
    "graph/shortest_path/bellman_ford":
        [2000, 4000, 8000, 16000],
    "graph/shortest_path/bfsshortreach":
        [125000, 250000, 500000, 1000000],
    "graph/shortest_path/dijkstrashortreach":
        [125000, 250000, 500000, 1000000],
    "graph/strongly_connected_component/kosaraju":
        [124998, 249999, 499998, 999999],
    "graph/strongly_connected_component/tarjan_scc":
        [124998, 249999, 499998, 999999],
    "graph/topological_sort/making_make":
        [2500, 5000, 10000, 20000],
}
