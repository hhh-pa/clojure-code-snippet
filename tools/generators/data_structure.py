"""Slope input generators for data-structure benchmarks (T20).

Each generator below produces ``(input_bytes, expected_bytes, fit_n)``
for one benchmark's stdin contract at a requested scale. Every
``generate(n, seed)`` is a pure function of ``(n, seed)`` (stdlib
only, ``random.Random(seed)``), calibrated diff-clean against the real
JVM solution under ``tests/run_tests.py`` ``normalize()`` (per-line
strip, so trailing spaces/newlines are ignored).

Benchmark -> generator mapping::

    data_structure/avl_tree/self_balancing_tree -> AvlTreeGenerator
    data_structure/bst_lca/binary_search_tree_lowest_common_ancestor
        -> BstLcaGenerator
    data_structure/b_tree/geek_for_geeks -> BTreeGenerator
    data_structure/disjoint_set/connected_component
        -> DisjointSetGenerator
    data_structure/fenwick_tree/direct_connections
        -> DirectConnectionsGenerator
    data_structure/heap/fibonacci_heap -> FibonacciHeapGenerator
    data_structure/heap/find_the_running_median -> RunningMedianGenerator
    data_structure/segment_tree/range_minimum_query
        -> RangeMinimumQueryGenerator
    data_structure/segment_tree/segment_tree_lazy
        -> LazySegmentTreeGenerator
    data_structure/skip_list/skip_list -> SkipListGenerator
    data_structure/splay_tree/splay_tree -> SplayTreeGenerator
    data_structure/treap/treap -> TreapGenerator

Each class carries ``DEFAULT_SCALES``: exactly 4 calibrated scales
with max/min >= 4, top scale at local solve cpu 5-60s.
"""

import heapq
import random
from bisect import bisect_left

_MOD = 1000000007
_INT32_MAX = 2147483647


def _emit_lines(lines):
    """Join lines with newline plus trailing newline, or nothing."""
    if not lines:
        return b""
    return ("\n".join(lines) + "\n").encode("ascii")


def _println(text):
    """Single println line (bare println format, not emit-lines)."""
    return (text + "\n").encode("ascii")


class _AvlNode(object):
    __slots__ = ("val", "left", "right", "ht")

    def __init__(self, val):
        self.val = val
        self.left = None
        self.right = None
        self.ht = 0


def _avl_height(node):
    return node.ht if node is not None else -1


def _avl_refresh(node):
    node.ht = 1 + max(_avl_height(node.left), _avl_height(node.right))
    return node


def _avl_bf(node):
    if node is None:
        return 0
    return _avl_height(node.left) - _avl_height(node.right)


def _avl_rotate_right(y):
    x = y.left
    y.left = x.right
    _avl_refresh(y)
    x.right = y
    return _avl_refresh(x)


def _avl_rotate_left(x):
    y = x.right
    x.right = y.left
    _avl_refresh(x)
    y.left = x
    return _avl_refresh(y)


def _avl_insert(node, val):
    """Mirror of the Clojure avl-insert: dups right, val-based double
    rotation choice (compare val against the child root value)."""
    if node is None:
        return _AvlNode(val)
    if val < node.val:
        node.left = _avl_insert(node.left, val)
    else:
        node.right = _avl_insert(node.right, val)
    grown = _avl_refresh(node)
    b = _avl_bf(grown)
    if b > 1:
        if val > grown.left.val:
            grown.left = _avl_rotate_left(grown.left)
        return _avl_rotate_right(grown)
    if b < -1:
        if val < grown.right.val:
            grown.right = _avl_rotate_right(grown.right)
        return _avl_rotate_left(grown)
    return grown


def _avl_inorder(root):
    out = []
    stack = []
    cur = root
    while stack or cur is not None:
        while cur is not None:
            stack.append(cur)
            cur = cur.left
        cur = stack.pop()
        out.append(cur)
        cur = cur.right
    return out


def _avl_preorder(root):
    out = []
    stack = [root] if root is not None else []
    while stack:
        cur = stack.pop()
        out.append(cur)
        if cur.right is not None:
            stack.append(cur.right)
        if cur.left is not None:
            stack.append(cur.left)
    return out


class AvlTreeGenerator(object):
    """Self-balancing AVL tree: n, n values, 1 extra value."""

    DEFAULT_SCALES = [37500, 75000, 150000, 300000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        vals = [rng.randint(-n, n) for _ in range(n)]
        extra = rng.randint(-n, n)
        parts = [str(n)] + [str(v) for v in vals] + [str(extra)]
        data_in = " ".join(parts).encode("ascii")
        root = None
        for v in vals + [extra]:
            root = _avl_insert(root, v)
        line1 = "".join("{0}(BF={1}) ".format(nd.val, _avl_bf(nd))
                        for nd in _avl_inorder(root))
        line2 = "".join("{0}(BF={1}) ".format(nd.val, _avl_bf(nd))
                        for nd in _avl_preorder(root))
        expected = (line1 + "\n" + line2 + "\n").encode("ascii")
        return data_in, expected, n


class _BstNode(object):
    __slots__ = ("val", "left", "right")

    def __init__(self, val):
        self.val = val
        self.left = None
        self.right = None


def _bst_insert(root, v):
    """Iterative BST insert, duplicates go right."""
    node = _BstNode(v)
    if root is None:
        return node
    cur = root
    while True:
        if v < cur.val:
            if cur.left is None:
                cur.left = node
                return root
            cur = cur.left
        else:
            if cur.right is None:
                cur.right = node
                return root
            cur = cur.right


def _bst_lca(root, lo, hi):
    """Split-point walk; returns None when the walk falls off."""
    cur = root
    while cur is not None:
        if hi < cur.val:
            cur = cur.left
        elif lo > cur.val:
            cur = cur.right
        else:
            return cur.val
    return None


class BstLcaGenerator(object):
    """BST lowest common ancestor: n, n values, v1 v2 queries."""

    DEFAULT_SCALES = [187500, 375000, 750000, 1500000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        vals = [rng.randint(-n, n) for _ in range(n)]
        v1 = rng.choice(vals)
        if rng.random() < 0.7:
            v2 = rng.choice(vals)
        else:
            v2 = rng.randint(-3 * n, 3 * n)
        parts = [str(n)] + [str(v) for v in vals] + [str(v1), str(v2)]
        data_in = " ".join(parts).encode("ascii")
        root = None
        for v in vals:
            root = _bst_insert(root, v)
        found = _bst_lca(root, min(v1, v2), max(v1, v2))
        if found is None:
            expected = b""
        else:
            expected = _println(str(found))
        return data_in, expected, n


class BTreeGenerator(object):
    """Geek-for-geeks B-tree op stream: dummy token[0], then ops to EOF.

    ``n`` is the op count; ``fit_n`` is the op count.
    """

    DEFAULT_SCALES = [37500, 75000, 150000, 300000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        toks = ["0"]
        counts = {}
        vals = []
        sval = []
        total = 0
        lines = []
        live = []
        for _ in range(n):
            r = rng.random()
            if r < 0.47:
                v = rng.randint(-1000000, 1000000)
                toks.append("i")
                toks.append(str(v))
                counts[v] = counts.get(v, 0) + 1
                i = bisect_left(vals, v)
                vals.insert(i, v)
                sval.insert(i, str(v))
                total += 1
                live.append(v)
            elif r < 0.94:
                if live and rng.random() < 0.97:
                    j = rng.randrange(len(live))
                    v = live[j]
                    live[j] = live[-1]
                    live.pop()
                    hit = True
                else:
                    v = rng.randint(-1000000, 1000000)
                    hit = counts.get(v, 0) > 0
                    if hit:
                        k = live.index(v)
                        live[k] = live[-1]
                        live.pop()
                toks.append("r")
                toks.append(str(v))
                if total == 0:
                    lines.append("The tree is empty")
                elif not hit:
                    lines.append("The key {0} is does not exist"
                                 " in the tree".format(v))
                else:
                    if counts[v] == 1:
                        del counts[v]
                    else:
                        counts[v] -= 1
                    i = bisect_left(vals, v)
                    vals.pop(i)
                    sval.pop(i)
                    total -= 1
            elif r < 0.945:
                toks.append("t")
                lines.append(" ".join(sval))
            else:
                toks.append("z")
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), n


class DisjointSetGenerator(object):
    """Connected components: n, then pairs to EOF (2*n pairs)."""

    DEFAULT_SCALES = [50000, 100000, 200000, 400000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        npairs = 2 * n
        parent = {}
        size = {}

        def find(x):
            while parent.get(x, x) != x:
                parent[x] = parent.get(parent[x], parent[x])
                x = parent[x]
            return x

        toks = [str(n)]
        lines = []
        comps = n
        for _ in range(npairs):
            r = rng.random()
            if r < 0.80:
                u = rng.randint(1, n)
                v = rng.randint(1, n)
            elif r < 0.90:
                u = rng.randint(1, n)
                v = u
            else:
                u = rng.randint(1, n)
                v = (u % n) + 1
            toks.append(str(u))
            toks.append(str(v))
            ru = find(u)
            rv = find(v)
            if ru == rv:
                lines.append("{0} CYCLE FORMED!".format(comps))
            else:
                su = size.get(ru, 1)
                sv = size.get(rv, 1)
                if su < sv:
                    ru, rv = rv, ru
                    su, sv = sv, su
                parent[rv] = ru
                size[ru] = su + sv
                comps -= 1
                lines.append(str(comps))
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), npairs


def _crem(a, m):
    """Clojure (rem a m) for positive m: truncated remainder."""
    if a >= 0:
        return a % m
    return -((-a) % m)


def _direct_case(xs, ps):
    """Wiring cost emulating the solution op-for-op.

    The sweep runs in stable population order (Python's sorted is
    stable, like sort-by). Every modular reduction applies Clojure
    rem semantics: the solution adds the modulus only once before
    rem, so large-magnitude intermediates keep a negative remainder
    and the printed answer itself can be negative.
    """
    n = len(xs)
    order = sorted(range(n), key=lambda i: ps[i])
    sx = sorted(xs)
    bit_c = [0] * (n + 2)
    bit_s = [0] * (n + 2)

    def bit_add(bit, i, v):
        while i <= n:
            bit[i] = _crem(bit[i] + v, _MOD)
            i += i & (-i)

    def bit_sum(bit, i):
        s = 0
        while i > 0:
            s = _crem(s + bit[i], _MOD)
            i -= i & (-i)
        return s

    def cbsearch(v, target):
        lo = 0
        hi = len(v) - 1
        while lo <= hi:
            mid = lo + (hi - lo) // 2
            got = v[mid]
            if got == target:
                return mid
            elif got < target:
                lo = mid + 1
            else:
                hi = mid - 1
        return -1

    ranks = {}
    ans = 0
    for idx in order:
        xi = xs[idx]
        pi = _crem(ps[idx], _MOD)
        r = ranks.get(xi)
        if r is None:
            r = cbsearch(sx, xi) + 1
            ranks[xi] = r
        cl = bit_sum(bit_c, r)
        sl = bit_sum(bit_s, r)
        tc = bit_sum(bit_c, n)
        ts = bit_sum(bit_s, n)
        cr = _crem(tc - cl + _MOD, _MOD)
        sr = _crem(ts - sl + _MOD, _MOD)
        xm = _crem(xi, _MOD)
        left_part = _crem(cl * xm - sl + _MOD, _MOD)
        right_part = _crem(sr - cr * xm + _MOD, _MOD)
        step_ans = _crem(pi * _crem(left_part + right_part, _MOD), _MOD)
        bit_add(bit_c, r, 1)
        bit_add(bit_s, r, xi)
        ans = _crem(ans + step_ans, _MOD)
    return ans


class DirectConnectionsGenerator(object):
    """Direct connections: t=3 cases of n xs and n pops each."""

    DEFAULT_SCALES = [20000, 40000, 80000, 160000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        t = 3
        toks = [str(t)]
        lines = []
        for _ in range(t):
            xs = [rng.randint(0, 2 * n) for _ in range(n)]
            ps = [rng.randint(0, 1000000000) for _ in range(n)]
            toks.append(str(n))
            toks.extend(str(v) for v in xs)
            toks.extend(str(v) for v in ps)
            lines.append(str(_direct_case(xs, ps)))
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), t * n


class FibonacciHeapGenerator(object):
    """Fibonacci heap script: q ops; MIN/EXTRACT_MIN produce output.

    Ids are unique (sequential) and live keys are kept distinct so no
    tie-break can differ between the reference and the solution.
    """

    DEFAULT_SCALES = [93750, 187500, 375000, 750000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        toks = [str(n)]
        lines = []
        heap = []
        id2key = {}
        live = set()
        all_ids = []
        next_id = 1
        seq = 0

        def clean():
            while heap:
                k, s, i = heap[0]
                if i in id2key and id2key[i] == k:
                    break
                heapq.heappop(heap)

        for _ in range(n):
            r = rng.random()
            if r < 0.50:
                k = rng.randint(-1000000000, 1000000000)
                while k in live:
                    k += 1
                i = next_id
                next_id += 1
                toks.extend(("INSERT", str(i), str(k)))
                id2key[i] = k
                live.add(k)
                heapq.heappush(heap, (k, seq, i))
                seq += 1
                all_ids.append(i)
            elif r < 0.65:
                toks.append("MIN")
                clean()
                if heap:
                    k, s, i = heap[0]
                    lines.append("{0} {1}".format(i, k))
                else:
                    lines.append("EMPTY")
            elif r < 0.85:
                toks.append("EXTRACT_MIN")
                clean()
                if heap:
                    k, s, i = heapq.heappop(heap)
                    del id2key[i]
                    live.discard(k)
                    lines.append("{0} {1}".format(i, k))
                else:
                    lines.append("EMPTY")
            elif r < 0.98:
                if all_ids and rng.random() < 0.80:
                    i = rng.choice(all_ids)
                else:
                    i = next_id + rng.randint(1, 1000)
                if i in id2key and rng.random() < 0.90:
                    new = id2key[i] - rng.randint(1, 1000)
                    while new in live:
                        new -= 1
                elif i in id2key:
                    new = id2key[i] + rng.randint(1, 100)
                    while new in live:
                        new += 1
                else:
                    new = rng.randint(-1000000000, 1000000000)
                toks.extend(("DECREASE_KEY", str(i), str(new)))
                if i in id2key and new <= id2key[i]:
                    live.discard(id2key[i])
                    id2key[i] = new
                    live.add(new)
                    heapq.heappush(heap, (new, seq, i))
                    seq += 1
            else:
                toks.append("BOGUS")
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), n


def _median_line(a, b, odd):
    if odd:
        return "{0}.0".format(a)
    s = a + b
    if s >= 0:
        q = s // 2
    else:
        q = -((-s) // 2)
    if s % 2 == 0:
        return "{0}.0".format(q)
    if s < 0 and q == 0:
        return "-0.5"
    return "{0}.5".format(q)


class RunningMedianGenerator(object):
    """Running median: n, then n values (negatives included)."""

    DEFAULT_SCALES = [125000, 250000, 500000, 1000000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        vals = [rng.randint(-1000000, 1000000) for _ in range(n)]
        data_in = " ".join([str(n)] + [str(v) for v in vals])
        data_in = data_in.encode("ascii")
        lo = []
        hi = []
        lines = []
        for i, v in enumerate(vals):
            if not lo or v <= -lo[0]:
                heapq.heappush(lo, -v)
            else:
                heapq.heappush(hi, v)
            if len(lo) > len(hi) + 1:
                heapq.heappush(hi, -heapq.heappop(lo))
            elif len(hi) > len(lo):
                heapq.heappush(lo, -heapq.heappop(hi))
            if (i + 1) % 2 == 1:
                lines.append(_median_line(-lo[0], -lo[0], True))
            else:
                lines.append(_median_line(-lo[0], hi[0], False))
        return data_in, _emit_lines(lines), n


class _MinSegTree(object):
    """Iterative min segment tree with point assignment."""

    def __init__(self, a):
        n = len(a)
        size = 1
        while size < n:
            size *= 2
        self.size = size
        inf = 1 << 62
        self.inf = inf
        t = [inf] * (2 * size)
        for i, v in enumerate(a):
            t[size + i] = v
        for i in range(size - 1, 0, -1):
            x = t[2 * i]
            y = t[2 * i + 1]
            t[i] = x if x < y else y
        self.t = t

    def query(self, l, r):
        t = self.t
        res = self.inf
        l += self.size
        r += self.size
        while l <= r:
            if l & 1:
                if t[l] < res:
                    res = t[l]
                l += 1
            if not (r & 1):
                if t[r] < res:
                    res = t[r]
                r -= 1
            l //= 2
            r //= 2
        return res

    def update(self, i, v):
        t = self.t
        p = self.size + i
        t[p] = v
        p //= 2
        while p:
            x = t[2 * p]
            y = t[2 * p + 1]
            t[p] = x if x < y else y
            p //= 2


class RangeMinimumQueryGenerator(object):
    """Range minimum query: n q, n values, q ops (q=n).

    Queries use ``q x y`` with 1<=x<=y<=n; updates use ``u``. All
    values stay within [0, INT32_MAX] so the solution's disjoint
    sentinel (2147483647) can never leak into an answer.
    """

    DEFAULT_SCALES = [50000, 100000, 200000, 400000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        q = n
        a = [rng.randint(0, 1000000000) for _ in range(n)]
        toks = [str(n), str(q)] + [str(v) for v in a]
        tree = _MinSegTree(a)
        lines = []
        for _ in range(q):
            if rng.random() < 0.50:
                x = rng.randint(1, n)
                y = rng.randint(x, n)
                toks.extend(("q", str(x), str(y)))
                lines.append(str(tree.query(x - 1, y - 1)))
            else:
                x = rng.randint(1, n)
                v = rng.randint(0, 1000000000)
                toks.extend(("u", str(x), str(v)))
                tree.update(x - 1, v)
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), n


class _RangeAddRangeSum(object):
    """Two-BIT structure for range add / range sum (exact ints)."""

    def __init__(self, a):
        self.n = len(a)
        self.b1 = [0] * (len(a) + 2)
        self.b2 = [0] * (len(a) + 2)
        for i, v in enumerate(a):
            self.range_add(i + 1, i + 1, v)

    def _add(self, bit, i, v):
        n = self.n
        while i <= n:
            bit[i] += v
            i += i & (-i)

    def _sum(self, bit, i):
        s = 0
        while i > 0:
            s += bit[i]
            i -= i & (-i)
        return s

    def range_add(self, l, r, v):
        self._add(self.b1, l, v)
        self._add(self.b1, r + 1, -v)
        self._add(self.b2, l, v * (l - 1))
        self._add(self.b2, r + 1, -v * r)

    def _prefix(self, p):
        return self._sum(self.b1, p) * p - self._sum(self.b2, p)

    def range_sum(self, l, r):
        return self._prefix(r) - self._prefix(l - 1)


class LazySegmentTreeGenerator(object):
    """Lazy segment tree: n q, values, UPDATE/QUERY ops (q=n)."""

    DEFAULT_SCALES = [20000, 40000, 80000, 160000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        q = n
        a = [rng.randint(-1000000, 1000000) for _ in range(n)]
        toks = [str(n), str(q)] + [str(v) for v in a]
        bit = _RangeAddRangeSum(a)
        lines = []
        for _ in range(q):
            if rng.random() < 0.50:
                l = rng.randint(1, n)
                r = rng.randint(l, n)
                v = rng.randint(-1000, 1000)
                toks.extend(("UPDATE", str(l), str(r), str(v)))
                bit.range_add(l, r, v)
            else:
                l = rng.randint(1, n)
                r = rng.randint(l, n)
                toks.extend(("QUERY", str(l), str(r)))
                lines.append(str(bit.range_sum(l, r)))
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), n


class _SortedSetRef(object):
    """Set semantics with a sorted parallel string list for PRINT."""

    def __init__(self):
        self.present = set()
        self.vals = []
        self.sval = []

    def insert(self, v):
        if v in self.present:
            return
        self.present.add(v)
        i = bisect_left(self.vals, v)
        self.vals.insert(i, v)
        self.sval.insert(i, str(v))

    def discard(self, v):
        if v not in self.present:
            return
        self.present.discard(v)
        i = bisect_left(self.vals, v)
        self.vals.pop(i)
        self.sval.pop(i)

    def line(self):
        return " ".join(self.sval)


def _set_script(n, seed, find_op, erase_op):
    """Shared op-script driver for skip list / splay / treap."""
    rng = random.Random(seed)
    toks = [str(n)]
    ref = _SortedSetRef()
    lines = []
    pool = []
    for _ in range(n):
        r = rng.random()
        if r < 0.50:
            v = rng.randint(0, 2 * n)
            toks.append("INSERT")
            toks.append(str(v))
            ref.insert(v)
            pool.append(v)
        elif r < 0.80:
            v = rng.randint(0, 2 * n)
            toks.append(find_op)
            toks.append(str(v))
            if v in ref.present:
                lines.append("FOUND")
            else:
                lines.append("NOT_FOUND")
        elif r < 0.999:
            if pool and rng.random() < 0.5:
                v = rng.choice(pool)
            else:
                v = rng.randint(0, 2 * n)
            toks.append(erase_op)
            toks.append(str(v))
            ref.discard(v)
        else:
            toks.append("PRINT")
            lines.append(ref.line())
    return toks, lines


class SkipListGenerator(object):
    """Skip list script: INSERT/SEARCH/DELETE/PRINT ops."""

    DEFAULT_SCALES = [5000, 10000, 20000, 40000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        toks, lines = _set_script(n, seed, "SEARCH", "DELETE")
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), n


class SplayTreeGenerator(object):
    """Splay tree script: INSERT/FIND/DELETE/PRINT ops."""

    DEFAULT_SCALES = [12500, 25000, 50000, 100000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        toks, lines = _set_script(n, seed, "FIND", "DELETE")
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), n


class TreapGenerator(object):
    """Treap script: INSERT/FIND/ERASE/PRINT ops."""

    DEFAULT_SCALES = [15000, 30000, 60000, 120000]

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        toks, lines = _set_script(n, seed, "FIND", "ERASE")
        data_in = " ".join(toks).encode("ascii")
        return data_in, _emit_lines(lines), n
