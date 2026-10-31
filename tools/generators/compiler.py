"""Slope input generators for the compiler benchmarks.

One generator per solution under src/compiler/*.clj. Each class exposes
``__init__(self, scales)`` storing ``list(scales)`` and
``generate(self, n, seed=1000) -> (input_bytes, expected_bytes, fit_n)``,
a pure function of ``(n, seed)`` (stdlib only, ``random.Random(seed)``).

Expected outputs replicate each driver's exact stdout contract
(normalize() in tests/run_tests.py strips trailing whitespace per line,
so a missing final newline still diffs clean). Where the reference
output is implementation-defined (chart-item counts, hash-cons
numbering, spill choices, dominance frontiers), the generator ports the
solution's algorithm exactly; calibration diffs every scale against the
real JVM solution before the scales below ship.
"""

import random
import re
from bisect import bisect_right
from collections import deque
from heapq import heapify
from heapq import heappop
from heapq import heappush

MOD = 1000000007


def _b(text):
    """Encode an ASCII string to bytes."""
    return text.encode("ascii")


class CykParserGenerator(object):
    """Grammar S -> SS | a with query a^n (ways = Catalan(n-1)).

    Stdin: ``2 S S -> SS S -> a 1 <a^n>``. CYK still runs its full
    O(n^3) table fill, so fit_n = n tracks cubic work while the
    expected count is closed-form O(n).
    """

    def __init__(self, scales):
        self.scales = list(scales)

    @staticmethod
    def _catalan(k):
        """Exact Catalan(k) mod MOD via the O(k) recurrence."""
        acc = 1
        for i in range(k):
            acc = acc * 2 * (2 * i + 1) // (i + 2)
        return acc % MOD

    def generate(self, n, seed=1000):
        n = int(n)
        _ = random.Random(seed)
        query = "a" * n
        data_in = _b("2 S S -> SS S -> a 1 " + query)
        data_out = _b("YES {0}".format(self._catalan(n - 1)))
        return data_in, data_out, n


class DfaMinimizationGenerator(object):
    """Minimal-by-construction mod-k counter over 'a'.

    States 0..k-1 with i -> i+1 mod k on 'a' and accept {0}; state i is
    distinguished by a^(k-i), so Hopcroft must report k minimal states
    and 1 accept state. Queries a^j read ACCEPT iff j % k == 0
    (the empty query is spelled EPS per the driver contract).
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        k = int(n)
        _ = random.Random(seed)
        toks = [str(k), "0", "1", "0", "1", "a", str(k)]
        for i in range(k):
            toks.append(str(i))
            toks.append(str((i + 1) % k))
            toks.append("a")
        lengths = [0, 1, k, 2 * k, 2 * k + 1]
        queries = ["EPS" if j == 0 else "a" * j for j in lengths]
        toks.append(str(len(queries)))
        toks.extend(queries)
        lines = ["Minimal DFA states: {0}".format(k),
                 "Accept states: 1"]
        for j in lengths:
            lines.append("ACCEPT" if j % k == 0 else "REJECT")
        return _b(" ".join(toks)), _b("\n".join(lines)), k


def _earley_is_nonterm(c):
    """Mirror earley_parser's predictor guard (uppercase or #)."""
    return c == "#" or ("A" <= c <= "Z")


def _earley_run(word, prods, rules_by_lhs):
    """Exact port of run-earley. Returns (accepted, total_items)."""
    n = len(word)
    charts = [[] for _ in range(n + 1)]
    present = [set() for _ in range(n + 1)]
    charts[0].append((0, 0, 0))
    present[0].add((0, 0, 0))
    tot = 0
    for k in range(n + 1):
        i = 0
        # NOTE: len(charts[k]) is re-read like the Clojure loop, so
        # items added at k are processed in this same pass.
        while i < len(charts[k]):
            rule_id, dot, origin = charts[k][i]
            _lhs, rhs = prods[rule_id]
            if dot < len(rhs):
                nxt = rhs[dot]
                if _earley_is_nonterm(nxt):
                    for rid in rules_by_lhs.get(nxt, ()):
                        item = (rid, 0, k)
                        if item not in present[k]:
                            charts[k].append(item)
                            present[k].add(item)
                elif k < n and word[k] == nxt:
                    item = (rule_id, dot + 1, origin)
                    if item not in present[k + 1]:
                        charts[k + 1].append(item)
                        present[k + 1].add(item)
            else:
                completed = prods[rule_id][0]
                # Snapshot: items appended to charts[origin] while
                # completing (possible when origin == k) are not seen.
                for p_id, p_dot, p_origin in list(charts[origin]):
                    prev_rhs = prods[p_id][1]
                    if p_dot < len(prev_rhs) and prev_rhs[p_dot] == completed:
                        item = (p_id, p_dot + 1, p_origin)
                        if item not in present[k]:
                            charts[k].append(item)
                            present[k].add(item)
            i += 1
        tot += len(charts[k])
    return (0, 1, 0) in present[n], tot


class EarleyParserGenerator(object):
    """Grammar S -> SS | a with query a^n (tot = chart items).

    The item total is implementation-defined (LHS-index predictor,
    persistent-chart dedup, snapshot completer), so the generator ports
    run-earley exactly instead of closed-forming it.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        _ = random.Random(seed)
        query = "a" * n
        data_in = _b("2 S S -> SS S -> a 1 " + query)
        prods = [("@", "S"), ("S", "SS"), ("S", "a")]
        rules_by_lhs = {"@": [0], "S": [1, 2]}
        accepted, tot = _earley_run(query, prods, rules_by_lhs)
        verdict = "ACCEPT" if accepted else "REJECT"
        return data_in, _b("{0} {1}".format(verdict, tot)), n


class GvnGenerator(object):
    """Randomized straight-line programs with an exact hash-cons port.

    Opcodes cover CONST / COPY / PHI / ADD / SUB / MUL. Operands reuse
    recent destinations (frequent redundancies) or a small pool of
    fresh names. Output replicates every per-instruction line shape
    (CONST / COPY-always-redundant / REDUNDANT PHI vs REDUNDANT /
    ADD+MUL canonicalize-swap / PHI min+max) plus both summary lines.
    """

    _OPS = ("ADD", "SUB", "MUL", "PHI")

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        insts = []
        dests = []
        toks = [str(n)]
        for i in range(n):
            dest = "t{0}".format(i)
            draw = rng.random()
            if i == 0 or draw < 0.15:
                arg1 = str(rng.randint(0, 9))
                toks += [dest, "=", "CONST", arg1]
                insts.append((dest, "CONST", arg1, ""))
            elif draw < 0.25:
                if dests and rng.random() < 0.8:
                    arg1 = rng.choice(dests)
                else:
                    arg1 = "x{0}".format(rng.randint(0, 7))
                toks += [dest, "=", "COPY", arg1]
                insts.append((dest, "COPY", arg1, ""))
            else:
                opcode = self._OPS[rng.randint(0, len(self._OPS) - 1)]
                args = []
                for _ in range(2):
                    if dests and rng.random() < 0.85:
                        args.append(rng.choice(dests))
                    else:
                        args.append("x{0}".format(rng.randint(0, 7)))
                toks += [dest, "=", opcode, args[0], args[1]]
                insts.append((dest, opcode, args[0], args[1]))
            dests.append(dest)
        out = []
        var_vn = {}
        vn_rep = {}
        expr_vn = {}
        const_vn = {}
        nxt = [1]
        redundant = [0]

        def get_vn(v):
            vn = var_vn.get(v)
            if vn is None:
                vn = nxt[0]
                nxt[0] += 1
                var_vn[v] = vn
                vn_rep[vn] = v
            return vn

        for dest, opcode, arg1, arg2 in insts:
            if opcode == "CONST":
                val = int(arg1)
                if val in const_vn:
                    vn = const_vn[val]
                    out.append("{0} = {1} (VN{2} REDUNDANT)".format(
                        dest, vn_rep[vn], vn))
                    var_vn[dest] = vn
                    redundant[0] += 1
                else:
                    vn = nxt[0]
                    nxt[0] += 1
                    out.append("{0} = CONST {1} (VN{2})".format(
                        dest, val, vn))
                    const_vn[val] = vn
                    var_vn[dest] = vn
                    vn_rep[vn] = dest
            elif opcode == "COPY":
                vn = get_vn(arg1)
                out.append("{0} = {1} (VN{2} REDUNDANT)".format(
                    dest, vn_rep[vn], vn))
                var_vn[dest] = vn
                redundant[0] += 1
            elif opcode == "PHI":
                vn1 = get_vn(arg1)
                vn2 = get_vn(arg2)
                if vn1 == vn2:
                    out.append("{0} = {1} (VN{2} REDUNDANT PHI)".format(
                        dest, vn_rep[vn1], vn1))
                    var_vn[dest] = vn1
                    redundant[0] += 1
                else:
                    lo = min(vn1, vn2)
                    hi = max(vn1, vn2)
                    key = ("PHI", lo, hi)
                    if key in expr_vn:
                        vn = expr_vn[key]
                        out.append("{0} = {1} (VN{2} REDUNDANT)".format(
                            dest, vn_rep[vn], vn))
                        var_vn[dest] = vn
                        redundant[0] += 1
                    else:
                        vn = nxt[0]
                        nxt[0] += 1
                        out.append("{0} = PHI VN{1} VN{2} (VN{3})".format(
                            dest, lo, hi, vn))
                        expr_vn[key] = vn
                        var_vn[dest] = vn
                        vn_rep[vn] = dest
            else:
                vn1 = get_vn(arg1)
                vn2 = get_vn(arg2)
                if opcode in ("ADD", "MUL") and vn1 > vn2:
                    vn1, vn2 = vn2, vn1
                key = (opcode, vn1, vn2)
                if key in expr_vn:
                    vn = expr_vn[key]
                    out.append("{0} = {1} (VN{2} REDUNDANT)".format(
                        dest, vn_rep[vn], vn))
                    var_vn[dest] = vn
                    redundant[0] += 1
                else:
                    vn = nxt[0]
                    nxt[0] += 1
                    out.append("{0} = {1} VN{2} VN{3} (VN{4})".format(
                        dest, opcode, vn1, vn2, vn))
                    expr_vn[key] = vn
                    var_vn[dest] = vn
                    vn_rep[vn] = dest
        out.append("Total unique VNs: {0}".format(nxt[0] - 1))
        out.append("Redundant instructions eliminated: {0}".format(
            redundant[0]))
        return _b(" ".join(toks)), _b("\n".join(out)), n


class LexerRegexNfaGenerator(object):
    """Pattern (a|b)*a(a|b)^k with random match/no-match cases.

    Patterns are generated from a well-formed grammar so they are valid
    in both the Thompson-NFA driver and Python re (whose fullmatch
    agrees on this operator subset); case strings use a/b plus
    occasional c's (guaranteed NO_MATCH in both engines).
    """

    _CASES = 10

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        k = max(1, n // 100)
        pattern = "(a|b)*a" + "(a|b)" * k
        matcher = re.compile("(?:{0})\\Z".format(pattern))
        cases = []
        for _ in range(self._CASES):
            draw = rng.random()
            if draw < 0.80:
                case = "".join(rng.choice("ab") for _ in range(n))
            elif draw < 0.90:
                case = "".join(rng.choice("ab")
                               for _ in range(rng.randint(1, 5)))
            else:
                chars = [rng.choice("ab") for _ in range(n)]
                chars[rng.randint(0, n - 1)] = "c"
                case = "".join(chars)
            cases.append(case)
        toks = [pattern, str(len(cases))] + cases
        out = ["MATCH" if matcher.match(c) else "NO_MATCH"
               for c in cases]
        return _b(" ".join(toks)), _b("\n".join(out)), n


class LinearScanRegallocGenerator(object):
    """Randomized live intervals with an exact linear-scan port.

    Sort by (start, end) (stable), end-INCLUSIVE expiry, spill the
    active interval with the latest end (last-in-order among ties,
    per max-key), stable by-end insertion after equals, smallest
    free register.
    Register count k = max(1, n // 16) so scan work is O(n*k).
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        k = max(1, n // 16)
        ids = list(range(n))
        rng.shuffle(ids)
        triples = []
        for iid in ids:
            start = rng.randint(0, 3 * n)
            end = start + rng.randint(0, n // 2)
            triples.append((iid, start, end))
        toks = [str(n), str(k)]
        for iid, start, end in triples:
            toks += [str(iid), str(start), str(end)]
        order = sorted(triples, key=lambda t: (t[1], t[2]))
        free = [True] * k
        active = []
        assigned = [-1] * n
        spilled = set()
        for iid, start, end in order:
            unexpired = []
            for entry in active:
                if entry[0] < start:
                    free[entry[2]] = True
                else:
                    unexpired.append(entry)
            if len(unexpired) == k:
                if k == 0:
                    spilled.add(iid)
                else:
                    # Clojure max-key keeps the LAST maximal element on
                    # ties (verified: equal keys replace the
                    # accumulator), so scan from the back.
                    latest = max(reversed(unexpired),
                                 key=lambda t: t[0])
                    if latest[0] > end:
                        reg = latest[2]
                        kept = [t for t in unexpired if t[1] != latest[1]]
                        pos = bisect_right([t[0] for t in kept], end)
                        kept.insert(pos, (end, iid, reg))
                        unexpired = kept
                        assigned[latest[1]] = -1
                        assigned[iid] = reg
                        spilled.add(latest[1])
                    else:
                        spilled.add(iid)
                active = unexpired
            else:
                reg = free.index(True)
                free[reg] = False
                pos = bisect_right([t[0] for t in unexpired], end)
                unexpired.insert(pos, (end, iid, reg))
                assigned[iid] = reg
                active = unexpired
        out = []
        allocated = 0
        spilled_count = 0
        for i in range(n):
            if i in spilled:
                out.append("v{0}: SPILL".format(i))
                spilled_count += 1
            else:
                out.append("v{0}: R{1}".format(i, assigned[i]))
                allocated += 1
        out.append("Allocated: {0} Spilled: {1}".format(
            allocated, spilled_count))
        return _b(" ".join(toks)), _b("\n".join(out)), n


def _ll1_upper(c):
    return "A" <= c <= "Z"


def _ll1_terminal(c):
    return not _ll1_upper(c) and c != "#"


def _ll1_first_of_string(s, first_sets):
    if s == "#" or s == "":
        return set(["#"])
    acc = set()
    for sym in s:
        sym_first = first_sets.get(sym, set())
        acc |= sym_first - set(["#"])
        if "#" not in sym_first:
            return acc
    acc.add("#")
    return acc


def _ll1_compute_first(prods, first_init):
    by_sym = {}
    for idx, (_lhs, rhs) in enumerate(prods):
        if rhs == "#":
            continue
        for sym in sorted(set(rhs)):
            by_sym.setdefault(sym, []).append(idx)
    first = dict((k, set(v)) for k, v in first_init.items())
    changed = set()
    for lhs, rhs in prods:
        old = first.get(lhs, set())
        add = set(["#"]) if rhs == "#" else _ll1_first_of_string(rhs, first)
        new = old | add
        if new != old:
            first[lhs] = new
            changed.add(lhs)
    queue = sorted(changed)
    in_queue = set(changed)
    while queue:
        sym = queue.pop(0)
        in_queue.discard(sym)
        for idx in by_sym.get(sym, ()):
            lhs, rhs = prods[idx]
            old = first.get(lhs, set())
            add = set(["#"]) if rhs == "#" else _ll1_first_of_string(
                rhs, first)
            new = old | add
            if new != old:
                first[lhs] = new
                if lhs not in in_queue:
                    queue.append(lhs)
                    in_queue.add(lhs)
    return first


def _ll1_compute_follow(prods, first_sets, start):
    infos = []
    for lhs, rhs in prods:
        if rhs == "#":
            infos.append((lhs, []))
            continue
        positions = []
        for i, sym in enumerate(rhs):
            if _ll1_terminal(sym):
                continue
            rest = rhs[i + 1:]
            f_rest = _ll1_first_of_string(rest, first_sets)
            positions.append((sym, f_rest - set(["#"]),
                              "#" in f_rest))
        infos.append((lhs, positions))
    follow = {start: set(["$"])}
    for _lhs, positions in infos:
        for sym, no_eps, _nullable in positions:
            follow.setdefault(sym, set()).update(no_eps)
    prop = {}
    for lhs, positions in infos:
        for sym, _no_eps, nullable in positions:
            if nullable:
                prop.setdefault(lhs, []).append(sym)
    seeds = set([lhs for lhs in prop if follow.get(lhs)])
    if follow.get(start):
        seeds.add(start)
    queue = sorted(seeds)
    in_queue = set(seeds)
    while queue:
        sym = queue.pop(0)
        in_queue.discard(sym)
        src = follow.get(sym, set())
        for tgt in prop.get(sym, ()):
            old = follow.get(tgt, set())
            new = old | src
            if new != old:
                follow[tgt] = new
                if tgt not in in_queue:
                    queue.append(tgt)
                    in_queue.add(tgt)
    return follow


def _ll1_build_table(prods, first_sets, follow_sets):
    table = {}
    ok = True
    for idx, (lhs, rhs) in enumerate(prods):
        f_rhs = _ll1_first_of_string(rhs, first_sets)
        for term in sorted(f_rhs):
            if term == "#":
                continue
            key = (lhs, term)
            if key in table and table[key] != idx:
                ok = False
            table[key] = idx
        if "#" in f_rhs:
            for term in sorted(follow_sets.get(lhs, ())):
                key = (lhs, term)
                if key in table and table[key] != idx:
                    ok = False
                table[key] = idx
    return table, ok


def _ll1_parse(prods, table, word):
    """Exact port of the predictive-parse loop. Returns one line."""
    text = word + "$"
    total = len(text)
    stack = ["$", _LL1_START]
    pos = 0
    deriv = []
    while True:
        if not stack:
            if pos == total:
                return " ".join("r{0}".format(d) for d in deriv)
            return "SYNTAX ERROR"
        top = stack.pop()
        look = text[pos] if pos < total else "$"
        if top == look:
            if top == "$":
                if pos + 1 == total:
                    return " ".join("r{0}".format(d) for d in deriv)
                return "SYNTAX ERROR"
            pos += 1
        elif _ll1_terminal(top):
            return "SYNTAX ERROR"
        else:
            rule = table.get((top, look))
            if rule is None:
                return "SYNTAX ERROR"
            rhs = prods[rule][1]
            if rhs != "#":
                stack.extend(reversed(rhs))
            deriv.append(rule)


_LL1_START = "S"
_LL1_PRODS = [("S", "aS"), ("S", "b")]


class Ll1ParserGenerator(object):
    """Fixed LL(1) grammar S -> aS | b with valid + invalid queries.

    Valid queries a^j b exercise the predictive-parse loop (derivations
    r0^j r1); invalid queries over a/b/c plus a bare EPS token (no
    EPS-mapping in this driver) pin SYNTAX ERROR paths. FIRST/FOLLOW
    table construction and the parse loop are ported exactly.
    """

    _VALID = 4
    _INVALID = 2

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        each = max(1, n // self._VALID)
        queries = []
        for _ in range(self._VALID):
            queries.append("a" * each + "b")
        invalid = []
        while len(invalid) < self._INVALID:
            word = "".join(rng.choice("abc")
                            for _ in range(rng.randint(1, 20)))
            if re.match("\\Aa*b\\Z", word):
                continue
            invalid.append(word)
        invalid[0] = "EPS"
        queries.extend(invalid)
        rng.shuffle(queries)
        toks = ["2", _LL1_START]
        for lhs, rhs in _LL1_PRODS:
            toks += [lhs, "->", rhs]
        toks.append(str(len(queries)))
        toks.extend(queries)
        terminals = set(["$"])
        for _lhs, rhs in _LL1_PRODS:
            for c in rhs:
                if _ll1_terminal(c):
                    terminals.add(c)
        first_init = dict((t, set([t])) for t in terminals)
        first_sets = _ll1_compute_first(_LL1_PRODS, first_init)
        follow_sets = _ll1_compute_follow(_LL1_PRODS, first_sets,
                                           _LL1_START)
        table, ok = _ll1_build_table(_LL1_PRODS, first_sets, follow_sets)
        if not ok:
            out = ["GRAMMAR IS NOT LL1"]
        else:
            out = ["GRAMMAR IS LL1"]
            for word in queries:
                out.append(_ll1_parse(_LL1_PRODS, table, word))
        return _b(" ".join(toks)), _b("\n".join(out)), n


_LR_PRODS = {1: ("E -> E + T", 3), 2: ("E -> T", 1),
             3: ("T -> T * F", 3), 4: ("T -> F", 1),
             5: ("F -> ( E )", 3), 6: ("F -> id", 1)}

_LR_ACTION = {
    (0, "id"): 5, (0, "("): 4,
    (1, "+"): 6, (1, "$"): 1000,
    (2, "+"): -2, (2, "*"): 7, (2, ")"): -2, (2, "$"): -2,
    (3, "+"): -4, (3, "*"): -4, (3, ")"): -4, (3, "$"): -4,
    (4, "id"): 5, (4, "("): 4,
    (5, "+"): -6, (5, "*"): -6, (5, ")"): -6, (5, "$"): -6,
    (6, "id"): 5, (6, "("): 4,
    (7, "id"): 5, (7, "("): 4,
    (8, "+"): 6, (8, ")"): 11,
    (9, "+"): -1, (9, "*"): 7, (9, ")"): -1, (9, "$"): -1,
    (10, "+"): -3, (10, "*"): -3, (10, ")"): -3, (10, "$"): -3,
    (11, "+"): -5, (11, "*"): -5, (11, ")"): -5, (11, "$"): -5}

_LR_GOTO = {(0, "E"): 1, (0, "T"): 2, (0, "F"): 3,
            (4, "E"): 8, (4, "T"): 2, (4, "F"): 3,
            (6, "T"): 9, (6, "F"): 3, (7, "F"): 10}


def _lr_run(tokens):
    """Exact port of parse-tokens over the fixed tables."""
    toks = list(tokens)
    if not toks or toks[-1] != "$":
        toks = toks + ["$"]
    out = []
    stack = [0]
    pos = 0
    while True:
        state = stack[-1]
        look = toks[pos] if pos < len(toks) else None
        act = _LR_ACTION.get((state, look))
        if act is None:
            out.append("SYNTAX_ERROR")
            return out
        if act == 1000:
            out.append("ACCEPTED")
            return out
        if act > 0:
            stack.append(act)
            pos += 1
        elif act < 0:
            text, size = _LR_PRODS[-act]
            out.append(text)
            del stack[len(stack) - size:]
            nxt = _LR_GOTO.get((stack[-1], text[0]))
            if nxt is None:
                out.append("SYNTAX_ERROR")
                return out
            stack.append(nxt)
        else:
            out.append("SYNTAX_ERROR")
            return out


class LrParserGenerator(object):
    """Valid expression token streams over id ( ) + * for fixed E/T/F.

    Operands are randomly parenthesized and joined by +/*; the
    generator ports the fixed action/goto tables (with $ auto-append)
    to replay every reduction line plus the ACCEPTED verdict.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        toks = []
        for i in range(n):
            if rng.random() < 0.25:
                toks += ["(", "id", ")"]
            else:
                toks.append("id")
            if i + 1 < n:
                toks.append(rng.choice(["+", "*"]))
        return _b(" ".join(toks)), _b("\n".join(_lr_run(toks))), n


def _pratt_tokenize(text):
    toks = []
    i = 0
    total = len(text)
    while i < total:
        c = text[i]
        if c in " \t\n\r\f\v":
            i += 1
        elif "0" <= c <= "9":
            val = 0
            while i < total and "0" <= text[i] <= "9":
                val = val * 10 + (ord(text[i]) - ord("0"))
                i += 1
            toks.append(("num", val))
        elif c in "+-*/^()":
            toks.append((c, None))
            i += 1
        else:
            i += 1
    return toks


def _pratt_bp(op):
    if op == "+" or op == "-":
        return (10, 11)
    if op == "*" or op == "/":
        return (20, 21)
    if op == "^":
        return (31, 30)
    return (-1, -1)


def _pratt_parse(toks, pos, min_bp):
    """Exact port of parse-expr. Returns (next-pos, ast) or None."""
    if pos >= len(toks):
        return None
    kind = toks[pos][0]
    if kind == "num":
        prefix = (pos + 1, ("num", toks[pos][1]))
    elif kind == "-":
        sub = _pratt_parse(toks, pos + 1, 40)
        if sub is None:
            return None
        prefix = (sub[0], ("unary", "-", sub[1]))
    elif kind == "(":
        sub = _pratt_parse(toks, pos + 1, 0)
        if sub is None:
            return None
        end, node = sub
        # Missing rparen is tolerated, matching the driver.
        if end < len(toks) and toks[end][0] == ")":
            end += 1
        prefix = (end, node)
    else:
        return None
    cur, node = prefix
    while True:
        if cur >= len(toks):
            return (cur, node)
        op = toks[cur][0]
        left_bp, right_bp = _pratt_bp(op)
        if left_bp < min_bp:
            return (cur, node)
        sub = _pratt_parse(toks, cur + 1, right_bp)
        if sub is None:
            return None
        cur, node = sub[0], ("binary", op, node, sub[1])


def _pratt_quot(a, b):
    """Truncating division (Clojure quot), b != 0."""
    quo = abs(a) // abs(b)
    if quo and (a < 0) != (b < 0):
        return -quo
    return quo


def _pratt_eval(node):
    kind = node[0]
    if kind == "num":
        return node[1]
    if kind == "unary":
        child = _pratt_eval(node[2])
        return -child if node[1] == "-" else child
    _k, op, left, right = node
    left = _pratt_eval(left)
    right = _pratt_eval(right)
    if op == "+":
        return left + right
    if op == "-":
        return left - right
    if op == "*":
        return left * right
    if op == "/":
        return 0 if right == 0 else _pratt_quot(left, right)
    # :pow with HEAD semantics (non-positive exponent yields 1).
    return 1 if right <= 0 else left ** right


def _pratt_sexpr(node):
    kind = node[0]
    if kind == "num":
        return str(node[1])
    if kind == "unary":
        return "({0} {1})".format(node[1], _pratt_sexpr(node[2]))
    return "({0} {1} {2})".format(
        node[1], _pratt_sexpr(node[2]), _pratt_sexpr(node[3]))


class PrattParserGenerator(object):
    """Random arithmetic expressions with an exact Pratt port.

    Covers binary +-*/^ (one ^ per expression with a small literal
    exponent, so values stay bounded), unary minus, parens (balanced;
    the tolerated-missing-rparen quirk only triggers in invalid cases),
    skipped unknown chars, div-by-zero, and quot-truncation. About 5%
    of lines are invalid (PARSE_ERROR). Trailing tokens after a valid
    prefix are ignored, exactly like the driver.
    """

    _INVALID = ("+", "1 +", "* 2", "(+", "2 +* 3", "^ 2", "2 ^",
                "()", ")(", "( 1 + )", "-")

    def __init__(self, scales):
        self.scales = list(scales)

    def _operand(self, rng, depth, budget):
        draw = rng.random()
        if depth >= 4 or draw < 0.5:
            return str(rng.randint(0, 99))
        if draw < 0.6:
            return "-" + self._operand(rng, depth + 1, budget)
        if draw < 0.8:
            return "(" + self._expr(rng, depth + 1, budget) + ")"
        return str(rng.randint(0, 999))

    def _expr(self, rng, depth, budget):
        parts = [self._operand(rng, depth, budget)]
        count = rng.randint(0, 3)
        for _ in range(count):
            ops = ["+", "-", "*", "/"]
            if budget[0]:
                ops.append("^")
            op = rng.choice(ops)
            if op == "^":
                budget[0] -= 1
                parts += [op, str(rng.randint(0, 4))]
            else:
                parts += [op, self._operand(rng, depth, budget)]
        text = parts[0]
        for part in parts[1:]:
            if rng.random() < 0.5:
                text += " "
            text += part
            if rng.random() < 0.3:
                text += " "
        if rng.random() < 0.03:
            pos = rng.randint(0, len(text))
            text = text[:pos] + rng.choice("x?") + text[pos:]
        return text

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        exprs = []
        for _ in range(n):
            if rng.random() < 0.05:
                exprs.append(self._INVALID[
                    rng.randint(0, len(self._INVALID) - 1)])
            else:
                exprs.append(self._expr(rng, 0, [1]))
        lines = [str(n)] + exprs
        out = []
        for text in exprs:
            toks = _pratt_tokenize(text)
            parsed = _pratt_parse(toks, 0, 0)
            if parsed is None:
                out.append("PARSE_ERROR")
            else:
                node = parsed[1]
                out.append("{0} = {1}".format(_pratt_sexpr(node),
                                              _pratt_eval(node)))
        return _b("\n".join(lines)), _b("\n".join(out)), n


class RegisterAllocationGenerator(object):
    """Random graphs (multiedges, rare self-loops) with a Chaitin port.

    Simplify removes the smallest-id low-degree node (else the
    smallest-id remaining node, spilled); select pops in reverse
    simplify order with first-fit coloring (k = 6). Lazy-deletion
    heaps stand in for the Clojure sorted-sets with identical minima.
    Note the ``v<i>:R<c>`` shape (no space) and no summary line.
    """

    _K = 6
    _M_PER_N = 4

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        k = self._K
        edges = []
        for _ in range(self._M_PER_N * n):
            if rng.random() < 0.01:
                u = rng.randint(0, n - 1)
                edges.append((u, u))
            else:
                edges.append((rng.randint(0, n - 1),
                              rng.randint(0, n - 1)))
        toks = [str(n), str(len(edges)), str(k)]
        for u, v in edges:
            toks += [str(u), str(v)]
        adj = [[] for _ in range(n)]
        for u, v in edges:
            adj[u].append(v)
            adj[v].append(u)
        deg = [len(row) for row in adj]
        active = set(range(n))
        low = set(i for i in range(n) if deg[i] < k)
        active_heap = list(range(n))
        heapify(active_heap)
        low_heap = sorted(low)
        heapify(low_heap)
        stack = []
        spilled = set()
        while active:
            if low:
                while low_heap[0] not in low:
                    heappop(low_heap)
                node = heappop(low_heap)
                low.discard(node)
                active.discard(node)
                simplify = True
            else:
                while active_heap[0] not in active:
                    heappop(active_heap)
                node = heappop(active_heap)
                active.discard(node)
                simplify = False
            freqs = {}
            for v in adj[node]:
                freqs[v] = freqs.get(v, 0) + 1
            for v in sorted(freqs):
                if v in active:
                    old = deg[v]
                    new = old - freqs[v]
                    deg[v] = new
                    if old >= k and new < k and v not in low:
                        low.add(v)
                        heappush(low_heap, v)
            if simplify:
                stack.append(node)
            else:
                spilled.add(node)
        color = [-1] * n
        while stack:
            u = stack.pop()
            used = set()
            for v in adj[u]:
                if color[v] != -1:
                    used.add(color[v])
            pick = None
            for c in range(k):
                if c not in used:
                    pick = c
                    break
            if pick is None:
                spilled.add(u)
            else:
                color[u] = pick
        out = []
        for i in range(n):
            if i in spilled:
                out.append("v{0}:SPILL".format(i))
            else:
                out.append("v{0}:R{1}".format(i, color[i]))
        return _b(" ".join(toks)), _b("\n".join(out)), n


def _ssa_rpo(succ, n):
    visited = set([1])
    stack = [[1, 0]]
    post = []
    while stack:
        node, idx = stack[-1]
        succs = succ[node]
        if idx < len(succs):
            stack[-1][1] += 1
            nxt = succs[idx]
            if nxt not in visited:
                visited.add(nxt)
                stack.append([nxt, 0])
        else:
            stack.pop()
            post.append(node)
    rpo = post[::-1]
    return rpo, dict((v, i) for i, v in enumerate(rpo))


def _ssa_idom(succ, pred, rpo, rpo_num):
    idom = {1: 1}

    def intersect(a, b):
        while a != b:
            if rpo_num[a] > rpo_num[b]:
                a = idom[a]
            else:
                b = idom[b]
        return a

    changed = True
    while changed:
        changed = False
        for blk in rpo[1:]:
            procs = [p for p in pred[blk] if p in idom]
            if not procs:
                continue
            new = procs[0]
            for p in procs[1:]:
                new = intersect(new, p)
            if idom.get(blk) != new:
                idom[blk] = new
                changed = True
    return idom


def _ssa_naive_df(succ, idomv, n):
    df = [set() for _ in range(n + 1)]
    for blk in range(1, n + 1):
        for nxt in succ[blk]:
            if idomv[nxt] != blk:
                df[blk].add(nxt)
    children = [[] for _ in range(n + 1)]
    for blk in range(1, n + 1):
        children[idomv[blk]].append(blk)
    # Single ascending up-sweep over dom-tree children (blocks that are
    # not children are no-ops, so skipping them is result-identical).
    for blk in range(1, n + 1):
        for child in children[blk]:
            for tgt in sorted(df[child]):
                if idomv[tgt] != blk:
                    df[blk].add(tgt)
    return df


def _ssa_phi(df, defs):
    defs_set = set(defs)
    phi = set()
    queue = deque(sorted(defs_set))
    while queue:
        blk = queue.popleft()
        for tgt in sorted(df[blk]):
            if tgt not in phi:
                phi.add(tgt)
                if tgt not in defs_set:
                    queue.append(tgt)
    return phi


class SsaDominanceGenerator(object):
    """Fully-reachable random flow graphs (spanning tree + extra edges).

    Ports RPO + Cooper-Harvey-Kennedy idoms + the single ordered
    up-sweep naive-DF quirk + IDF worklist. Restricting to graphs where
    every node is reachable from entry 1 keeps the bitset-fixpoint
    fallback (only for unreachable nodes) out of reach.
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        edges = []
        for i in range(2, n + 1):
            edges.append((rng.randint(1, i - 1), i))
        for _ in range(2 * n):
            edges.append((rng.randint(1, n), rng.randint(1, n)))
        defs = [rng.randint(1, n) for _ in range(max(1, n // 10))]
        toks = [str(n), str(len(edges))]
        for u, v in edges:
            toks += [str(u), str(v)]
        toks.append(str(len(defs)))
        toks.extend(str(d) for d in defs)
        succ = [[] for _ in range(n + 1)]
        pred = [[] for _ in range(n + 1)]
        for u, v in edges:
            succ[u].append(v)
            pred[v].append(u)
        rpo, rpo_num = _ssa_rpo(succ, n)
        idom = _ssa_idom(succ, pred, rpo, rpo_num)
        idomv = [0] * (n + 1)
        for blk, dom in idom.items():
            idomv[blk] = dom
        idomv[1] = 0
        df = _ssa_naive_df(succ, idomv, n)
        phi = _ssa_phi(df, defs)
        line1 = " ".join("idom({0})={1}".format(i, idomv[i])
                        for i in range(2, n + 1))
        line2 = "NONE" if not phi else " ".join(
            str(v) for v in sorted(phi))
        return _b(" ".join(toks)), _b(line1 + "\n" + line2), n


def _subset_closure(states, trans):
    closure = set(states)
    queue = deque(states)
    while queue:
        node = queue.popleft()
        for sym, tgt in trans.get(node, ()):
            if sym == "#" and tgt not in closure:
                closure.add(tgt)
                queue.append(tgt)
    return frozenset(closure)


def _subset_move(states, sym, trans):
    out = set()
    for node in states:
        for edge_sym, tgt in trans.get(node, ()):
            if edge_sym == sym:
                out.add(tgt)
    return out


class SubsetConstructionGenerator(object):
    """NFA for (a|b)*a(a|b)^k: DFA has 2^(k+1) reachable states.

    k grows mildly with n (3..7, so 16..256 DFA states) while Q ~ n/20
    queries of length L = n carry the simulation work (n^2/20 chars).
    Reachable-subset construction is canonical, so any correct port
    reproduces the driver's counts; queries mix a/b strings, short
    strings, c-spiked strings, and EPS (empty).
    """

    def __init__(self, scales):
        self.scales = list(scales)

    def generate(self, n, seed=1000):
        n = int(n)
        rng = random.Random(seed)
        k = min(3 + n // 4000, 7)
        states = k + 2
        trans_list = [(0, 0, "a"), (0, 0, "b"), (0, 1, "a")]
        for i in range(1, k + 1):
            trans_list.append((i, i + 1, "a"))
            trans_list.append((i, i + 1, "b"))
        accept = states - 1
        count = max(4, n // 20)
        width = n
        queries = []
        for _ in range(count):
            draw = rng.random()
            if draw < 0.85:
                queries.append("".join(rng.choice("ab")
                                       for _ in range(width)))
            elif draw < 0.90:
                queries.append("".join(rng.choice("ab")
                                       for _ in range(rng.randint(1, 20))))
            elif draw < 0.95:
                chars = [rng.choice("ab") for _ in range(width)]
                chars[rng.randint(0, width - 1)] = "c"
                queries.append("".join(chars))
            else:
                queries.append("EPS")
        toks = [str(states), "0", "1", str(accept),
                str(len(trans_list))]
        for u, v, c in trans_list:
            toks += [str(u), str(v), c]
        toks.append(str(count))
        toks.extend(queries)
        trans = {}
        for u, v, c in trans_list:
            trans.setdefault(u, []).append((c, v))
        alphabet = sorted(set(c for _u, _v, c in trans_list
                              if c != "#"))
        start_set = _subset_closure([0], trans)
        state_id = {start_set: 0}
        dfa_states = [start_set]
        dfa_accept = [accept in start_set]
        dfa_trans = {0: {}}
        queue = deque([0])
        while queue:
            cur = queue.popleft()
            cur_set = dfa_states[cur]
            for sym in alphabet:
                nxt = _subset_closure(_subset_move(cur_set, sym, trans),
                                      trans)
                if not nxt:
                    continue
                if nxt in state_id:
                    dfa_trans[cur][sym] = state_id[nxt]
                else:
                    nid = len(dfa_states)
                    state_id[nxt] = nid
                    dfa_states.append(nxt)
                    dfa_accept.append(accept in nxt)
                    dfa_trans[cur][sym] = nid
                    dfa_trans[nid] = {}
                    queue.append(nid)
        out = ["DFA states: {0}".format(len(dfa_states)),
               "DFA accept states: {0}".format(
                   sum(1 for a in dfa_accept if a))]
        for word in queries:
            text = "" if word == "EPS" else word
            state = 0
            verdict = None
            for c in text:
                nxt = dfa_trans[state].get(c)
                if nxt is None:
                    verdict = "REJECT"
                    break
                state = nxt
            if verdict is None:
                verdict = "ACCEPT" if dfa_accept[state] else "REJECT"
            out.append(verdict)
        return _b(" ".join(toks)), _b("\n".join(out)), n


# Calibrated slope scales per benchmark (problem stem -> 4 scales with
# max/min >= 4; top scale targets 5-60 s local solve cpu). Updated by
# calibration diffs against the real JVM solutions; see task report.
SCALES = {
    "cyk_parser": [60, 120, 240, 480],
    "dfa_minimization": [2000, 4000, 8000, 16000],
    "earley_parser": [60, 120, 240, 480],
    "gvn": [50000, 100000, 200000, 400000],
    "lexer_regex_nfa": [800, 1600, 3200, 6400],
    "linear_scan_regalloc": [2000, 4000, 8000, 16000],
    "ll1_parser": [800000, 1600000, 3200000, 6400000],
    "lr_parser": [50000, 100000, 200000, 400000],
    "pratt_parser": [50000, 100000, 200000, 400000],
    "register_allocation": [20000, 40000, 80000, 160000],
    "ssa_dominance": [2000, 4000, 8000, 16000],
    "subset_construction": [4000, 8000, 16000, 32000],
}

GENERATORS = {
    "cyk_parser": CykParserGenerator,
    "dfa_minimization": DfaMinimizationGenerator,
    "earley_parser": EarleyParserGenerator,
    "gvn": GvnGenerator,
    "lexer_regex_nfa": LexerRegexNfaGenerator,
    "linear_scan_regalloc": LinearScanRegallocGenerator,
    "ll1_parser": Ll1ParserGenerator,
    "lr_parser": LrParserGenerator,
    "pratt_parser": PrattParserGenerator,
    "register_allocation": RegisterAllocationGenerator,
    "ssa_dominance": SsaDominanceGenerator,
    "subset_construction": SubsetConstructionGenerator,
}
