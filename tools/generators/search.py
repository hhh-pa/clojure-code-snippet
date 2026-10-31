"""Slope input generators for four search benchmarks.

Contracts (all int-only I/O; blank input is silent; malformed input goes to
stderr with exit 0; extra tokens are ignored):

* PacmanAstarGenerator: stdin is a 6-int header ``pr pc fr fc n m`` then
  grid rows as raw tokens concatenated as chars (``%`` is a wall, every
  other char is open; at least n*m chars required). Stdout, when the food
  is reachable, is the move count (path cells minus one) followed by one
  ``r c`` line per path cell from start to food; unreachable food yields
  EMPTY stdout.
* BfsShortestReachGenerator: stdin is ``q`` then per query
  ``n m (u v)*m s`` (1-based, undirected). Stdout per query is the BFS
  distances times 6 (-1 when unreachable) for nodes 1..n except s,
  space-joined on one line.
* DfsRegionGenerator: stdin is ``n m`` plus n*m cells (==1 is land, else
  water). Stdout is the largest 8-connected region of 1s, or -1 when no
  land exists.
* PairsGenerator: stdin is ``n k v1..vn``. Stdout is the two-pointer
  match count over the SORTED values with the exact driver advance rules
  (i=0, j=1, both advance on a match).

Every generator stores ``list(scales)`` and exposes
``generate(n, seed=1000) -> (input_bytes, expected_bytes, fit_n)``, a
pure deterministic function of (n, seed) using only the stdlib.
"""

import random
from array import array
from collections import deque

# String-building block size: bounds transient memory when joining
# multi-million-element inputs (same pattern as sort_arrays.py).
_BLOCK = 100000


def _join_ints(vals):
    """Encode an int sequence as b"<v0> <v1> ..." in bounded blocks."""
    buf = bytearray()
    for start in range(0, len(vals), _BLOCK):
        if start:
            buf += b" "
        buf += " ".join(
            str(v) for v in vals[start:start + _BLOCK]
        ).encode("ascii")
    return bytes(buf)


class PacmanAstarGenerator(object):
    """Snake-corridor mazes with a forced, known-by-construction path.

    Even rows are fully open and odd rows hold a single open cell, so
    the open cells form one unbranched corridor from (0, 0) to the far
    end; the driver must output that whole corridor whatever its
    relaxation order is. ``n`` is the (odd) side length; ``fit_n`` is
    the corridor length in cells, the true work scale.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        s = max(3, int(n))
        if s % 2 == 0:
            s += 1
        grid = []
        for r in range(s):
            if r % 2 == 0:
                grid.append(["-"] * s)
            else:
                row = ["%"] * s
                row[s - 1 if (r // 2) % 2 == 0 else 0] = "-"
                grid.append(row)
        # Walk the corridor from (0, 0); the path is forced.
        path = [(0, 0)]
        prev = None
        cur = (0, 0)
        while True:
            r, c = cur
            nxt = None
            for dr, dc in ((-1, 0), (0, -1), (0, 1), (1, 0)):
                nr, nc = r + dr, c + dc
                if (0 <= nr < s and 0 <= nc < s
                        and grid[nr][nc] == "-" and (nr, nc) != prev):
                    nxt = (nr, nc)
                    break
            if nxt is None:
                break
            path.append(nxt)
            prev = cur
            cur = nxt
        gr, gc = path[-1]
        lines = ["0 0 {} {} {} {}".format(gr, gc, s, s)]
        for row in grid:
            lines.append("".join(row))
        data_in = ("\n".join(lines) + "\n").encode("ascii")
        out = [str(len(path) - 1)]
        for r, c in path:
            out.append("{} {}".format(r, c))
        data_out = ("\n".join(out) + "\n").encode("ascii")
        return data_in, data_out, len(path)


def _bfs_line(n, edges, m, start):
    """Space-joined BFS x6 distances for nodes 1..n except start+1."""
    adj = [[] for _ in range(n)]
    for i in range(m):
        u = edges[2 * i] - 1
        v = edges[2 * i + 1] - 1
        adj[u].append(v)
        adj[v].append(u)
    dist = [-1] * n
    dist[start] = 0
    queue = deque([start])
    while queue:
        cur = queue.popleft()
        for nxt in adj[cur]:
            if dist[nxt] == -1:
                dist[nxt] = dist[cur] + 6
                queue.append(nxt)
    return " ".join(str(dist[i]) for i in range(n) if i != start)


class BfsShortestReachGenerator(object):
    """Two queries per input: a line graph plus a denser random graph.

    Query 1 is the Hamiltonian path 1-2-...-n with a random start;
    query 2 adds random extra edges up to 4n total (capped at the
    complete-graph edge count) with a random start. ``n`` is the node
    count of each query; ``fit_n`` is n.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = max(2, int(n))
        rng = random.Random(seed)
        e1 = [0] * (2 * (n - 1))
        for i in range(1, n):
            e1[2 * (i - 1)] = i
            e1[2 * (i - 1) + 1] = i + 1
        s1 = rng.randrange(1, n + 1)
        m2 = min(4 * n, n * (n - 1) // 2)
        e2 = list(e1)
        seen = set()
        for i in range(1, n):
            seen.add(i * n + i + 1)
        while len(e2) // 2 < m2:
            u = rng.randrange(1, n + 1)
            v = rng.randrange(1, n + 1)
            if u == v:
                continue
            if u > v:
                u, v = v, u
            key = u * n + v
            if key in seen:
                continue
            seen.add(key)
            e2.append(u)
            e2.append(v)
        s2 = rng.randrange(1, n + 1)
        buf = bytearray(b"2\n")
        buf += ("{} {}\n".format(n, n - 1)).encode("ascii")
        buf += _join_ints(e1) + ("\n{}\n".format(s1)).encode("ascii")
        buf += ("{} {}\n".format(n, len(e2) // 2)).encode("ascii")
        buf += _join_ints(e2) + ("\n{}\n".format(s2)).encode("ascii")
        data_out = ("{}\n{}\n".format(
            _bfs_line(n, e1, n - 1, s1 - 1),
            _bfs_line(n, e2, len(e2) // 2, s2 - 1),
        )).encode("ascii")
        return bytes(buf), data_out, n


def _flood_best(cells, n, m):
    """Largest 8-connected region of 1s, -1 when no land exists."""
    nm = n * m
    seen = bytearray(nm)
    best = -1
    for idx in range(nm):
        if cells[idx] != 1 or seen[idx]:
            continue
        size = 0
        stack = [idx]
        seen[idx] = 1
        while stack:
            cur = stack.pop()
            size += 1
            cr = cur // m
            cc = cur % m
            for dr in (-1, 0, 1):
                nr = cr + dr
                if nr < 0 or nr >= n:
                    continue
                for dc in (-1, 0, 1):
                    if dr == 0 and dc == 0:
                        continue
                    nc = cc + dc
                    if nc < 0 or nc >= m:
                        continue
                    ni = nr * m + nc
                    if cells[ni] == 1 and not seen[ni]:
                        seen[ni] = 1
                        stack.append(ni)
        if size > best:
            best = size
    return best


class DfsRegionGenerator(object):
    """Square grids: all-land blocks and random-density grids.

    Even seeds yield all-land grids (answer n*m by construction);
    odd seeds yield density-0.5 grids answered by a flood-fill port.
    ``n`` is the side length; ``fit_n`` is the cell count n*n.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        s = max(2, int(n))
        rng = random.Random(seed)
        if seed % 2 == 0:
            cells = array("B", [1]) * (s * s)
            ans = s * s
        else:
            cells = array("B", (1 if rng.random() < 0.5 else 0
                                for _ in range(s * s)))
            ans = _flood_best(cells, s, s)
        buf = bytearray(("{} {}\n".format(s, s)).encode("ascii"))
        buf += _join_ints(cells) + b"\n"
        data_out = ("{}\n".format(ans)).encode("ascii")
        return bytes(buf), data_out, s * s


def _count_pairs(vals, k):
    """Exact port of the driver scan over sorted values.

    i=0, j=1; both advance on a match. Counter-based counting would
    disagree on duplicates and k=0, so the loop is replicated verbatim.
    """
    s = sorted(vals)
    n = len(s)
    i, j, ans = 0, 1, 0
    while i < n and j < n:
        if j <= i:
            j = i + 1
            continue
        diff = s[j] - s[i]
        if diff == k:
            i += 1
            j += 1
            ans += 1
        elif diff < k:
            j += 1
        else:
            i += 1
    return ans


class PairsGenerator(object):
    """Random values with heavy duplicates and small/zero/negative k.

    Values are uniform over roughly n/8 distinct values so duplicates
    are common; k spans -2..6 across (seed, n), covering k=0 and
    negative-k semantics. ``n`` is the value count; ``fit_n`` is n.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = max(0, int(n))
        rng = random.Random(seed)
        k = (rng.randrange(0, 9) + n) % 9 - 2
        hi = max(2, n // 8 + 1)
        vals = [rng.randrange(0, hi) for _ in range(n)]
        buf = bytearray(("{} {}\n".format(n, k)).encode("ascii"))
        buf += _join_ints(vals) + b"\n"
        data_out = ("{}\n".format(_count_pairs(vals, k))).encode("ascii")
        return bytes(buf), data_out, n
