// Package generators holds slope input generators ported from
// tools/generators/*.py. Each generator exposes Scales and Generate
// and emits byte-identical outputs to its Python counterpart.
package generators

import (
	"bytes"
	"container/heap"
	"math/big"
	"sort"
	"strconv"
	"strings"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// mod is the output modulus shared by counting generators.
const mod = 1000000007

// tokBuf accumulates space-separated tokens without per-element concat.
type tokBuf struct {
	buf   bytes.Buffer
	first bool
}

func newTokBuf(cap int) *tokBuf {
	t := &tokBuf{first: true}
	if cap > 0 {
		t.buf.Grow(cap)
	}
	return t
}

func (t *tokBuf) s(s string) {
	if !t.first {
		t.buf.WriteByte(' ')
	}
	t.first = false
	t.buf.WriteString(s)
}

func (t *tokBuf) i(v int64) {
	if !t.first {
		t.buf.WriteByte(' ')
	}
	t.first = false
	t.buf.WriteString(strconv.FormatInt(v, 10))
}

func (t *tokBuf) bytes() []byte { return t.buf.Bytes() }

// lineBuf accumulates newline-separated output lines.
type lineBuf struct {
	buf   bytes.Buffer
	first bool
}

func newLineBuf(cap int) *lineBuf {
	l := &lineBuf{first: true}
	if cap > 0 {
		l.buf.Grow(cap)
	}
	return l
}

func (l *lineBuf) s(s string) {
	if !l.first {
		l.buf.WriteByte('\n')
	}
	l.first = false
	l.buf.WriteString(s)
}

func (l *lineBuf) bytes() []byte { return l.buf.Bytes() }

// CykParserGenerator emits grammar S -> SS | a with query a^n
// (ways = Catalan(n-1)).
type CykParserGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *CykParserGenerator) Scales() []int64 { return g.scales }

// cykCatalan returns Catalan(k) mod mod via the O(k) recurrence.
func cykCatalan(k int64) string {
	acc := big.NewInt(1)
	two := big.NewInt(2)
	tmp := new(big.Int)
	div := new(big.Int)
	for i := int64(0); i < k; i++ {
		tmp.SetInt64(2*i + 1)
		acc.Mul(acc, two)
		acc.Mul(acc, tmp)
		div.SetInt64(i + 2)
		acc.Quo(acc, div)
	}
	acc.Mod(acc, big.NewInt(mod))
	return acc.String()
}

// Generate builds the stdin bytes, the YES Catalan(n-1) verdict, and fit n.
func (g *CykParserGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	query := strings.Repeat("a", int(n))
	in := newTokBuf(32 + len(query))
	in.s("2")
	in.s("S")
	in.s("S")
	in.s("->")
	in.s("SS")
	in.s("S")
	in.s("->")
	in.s("a")
	in.s("1")
	in.s(query)
	out := "YES " + cykCatalan(n-1)
	return in.bytes(), []byte(out), FitNFromInt64(n), nil
}

// DfaMinimizationGenerator emits a minimal-by-construction mod-k counter
// over 'a' with accept {0} and queries a^j.
type DfaMinimizationGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *DfaMinimizationGenerator) Scales() []int64 { return g.scales }

// Generate builds the counter DFA, ACCEPT/REJECT verdicts, and fit k (= n).
func (g *DfaMinimizationGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	k := n
	in := newTokBuf(int(16*k + 64))
	in.i(k)
	in.s("0")
	in.s("1")
	in.s("0")
	in.s("1")
	in.s("a")
	in.i(k)
	for i := int64(0); i < k; i++ {
		in.i(i)
		in.i((i + 1) % k)
		in.s("a")
	}
	lengths := []int64{0, 1, k, 2 * k, 2*k + 1}
	in.i(int64(len(lengths)))
	for _, j := range lengths {
		if j == 0 {
			in.s("EPS")
		} else {
			in.s(strings.Repeat("a", int(j)))
		}
	}
	out := newLineBuf(64)
	out.s("Minimal DFA states: " + strconv.FormatInt(k, 10))
	out.s("Accept states: 1")
	for _, j := range lengths {
		if j%k == 0 {
			out.s("ACCEPT")
		} else {
			out.s("REJECT")
		}
	}
	return in.bytes(), out.bytes(), FitNFromInt64(k), nil
}

// earleyItem is one chart entry (rule id, dot position, origin set).
type earleyItem struct {
	rule   int
	dot    int
	origin int
}

// earleyIsNonterm mirrors the predictor guard (uppercase or #).
func earleyIsNonterm(c byte) bool {
	return c == '#' || ('A' <= c && c <= 'Z')
}

// earleyRun is an exact port of run-earley. It returns
// (accepted, totalItems).
func earleyRun(word string, lhs []byte, rhs []string, rulesByLHS map[byte][]int) (bool, int) {
	n := len(word)
	charts := make([][]earleyItem, n+1)
	present := make([]map[earleyItem]bool, n+1)
	for k := range present {
		present[k] = map[earleyItem]bool{}
	}
	charts[0] = append(charts[0], earleyItem{0, 0, 0})
	present[0][earleyItem{0, 0, 0}] = true
	tot := 0
	for k := 0; k <= n; k++ {
		i := 0
		// NOTE: len(charts[k]) is re-read like the Clojure loop, so
		// items added at k are processed in this same pass.
		for i < len(charts[k]) {
			it := charts[k][i]
			r := rhs[it.rule]
			if it.dot < len(r) {
				nxt := r[it.dot]
				if earleyIsNonterm(nxt) {
					for _, rid := range rulesByLHS[nxt] {
						item := earleyItem{rid, 0, k}
						if !present[k][item] {
							charts[k] = append(charts[k], item)
							present[k][item] = true
						}
					}
				} else if k < n && word[k] == nxt {
					item := earleyItem{it.rule, it.dot + 1, it.origin}
					if !present[k+1][item] {
						charts[k+1] = append(charts[k+1], item)
						present[k+1][item] = true
					}
				}
			} else {
				completed := lhs[it.rule]
				// Snapshot: items appended to charts[origin] while
				// completing (possible when origin == k) are not seen.
				snapshot := make([]earleyItem, len(charts[it.origin]))
				copy(snapshot, charts[it.origin])
				for _, p := range snapshot {
					pr := rhs[p.rule]
					if p.dot < len(pr) && pr[p.dot] == completed {
						item := earleyItem{p.rule, p.dot + 1, p.origin}
						if !present[k][item] {
							charts[k] = append(charts[k], item)
							present[k][item] = true
						}
					}
				}
			}
			i++
		}
		tot += len(charts[k])
	}
	return present[n][earleyItem{0, 1, 0}], tot
}

// EarleyParserGenerator emits grammar S -> SS | a with query a^n
// (expected total = chart items from an exact run-earley port).
type EarleyParserGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *EarleyParserGenerator) Scales() []int64 { return g.scales }

// Generate builds the stdin bytes, the ACCEPT/REJECT item verdict, and fit n.
func (g *EarleyParserGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	query := strings.Repeat("a", int(n))
	in := newTokBuf(32 + len(query))
	in.s("2")
	in.s("S")
	in.s("S")
	in.s("->")
	in.s("SS")
	in.s("S")
	in.s("->")
	in.s("a")
	in.s("1")
	in.s(query)
	lhs := []byte{'@', 'S', 'S'}
	rhs := []string{"S", "SS", "a"}
	rulesByLHS := map[byte][]int{'@': {0}, 'S': {1, 2}}
	accepted, tot := earleyRun(query, lhs, rhs, rulesByLHS)
	verdict := "ACCEPT"
	if !accepted {
		verdict = "REJECT"
	}
	out := verdict + " " + strconv.Itoa(tot)
	return in.bytes(), []byte(out), FitNFromInt64(n), nil
}

// gvnOps are the binary opcodes (CONST/COPY handled separately).
var gvnOps = [4]string{"ADD", "SUB", "MUL", "PHI"}

// gvnInst is one straight-line program instruction.
type gvnInst struct {
	dest   string
	opcode string
	arg1   string
	arg2   string
}

// GvnGenerator emits randomized straight-line programs with an exact
// hash-cons port for the expected per-instruction lines.
type GvnGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *GvnGenerator) Scales() []int64 { return g.scales }

// Generate builds the program tokens, the value-numbering lines, and fit n.
func (g *GvnGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	var insts []gvnInst
	var dests []string
	in := newTokBuf(nn*24 + 16)
	in.i(n)
	for i := 0; i < nn; i++ {
		dest := "t" + strconv.Itoa(i)
		draw := r.Random()
		switch {
		case i == 0 || draw < 0.15:
			arg1 := strconv.FormatInt(r.RandInt(0, 9), 10)
			in.s(dest)
			in.s("=")
			in.s("CONST")
			in.s(arg1)
			insts = append(insts, gvnInst{dest, "CONST", arg1, ""})
		case draw < 0.25:
			var arg1 string
			if len(dests) > 0 && r.Random() < 0.8 {
				arg1 = dests[r.Choice(len(dests))]
			} else {
				arg1 = "x" + strconv.FormatInt(r.RandInt(0, 7), 10)
			}
			in.s(dest)
			in.s("=")
			in.s("COPY")
			in.s(arg1)
			insts = append(insts, gvnInst{dest, "COPY", arg1, ""})
		default:
			opcode := gvnOps[r.RandInt(0, int64(len(gvnOps)-1))]
			args := make([]string, 0, 2)
			for j := 0; j < 2; j++ {
				if len(dests) > 0 && r.Random() < 0.85 {
					args = append(args, dests[r.Choice(len(dests))])
				} else {
					args = append(args, "x"+strconv.FormatInt(r.RandInt(0, 7), 10))
				}
			}
			in.s(dest)
			in.s("=")
			in.s(opcode)
			in.s(args[0])
			in.s(args[1])
			insts = append(insts, gvnInst{dest, opcode, args[0], args[1]})
		}
		dests = append(dests, dest)
	}
	out := newLineBuf(nn*32 + 64)
	varWrite := func(parts ...string) {
		var sb strings.Builder
		for _, p := range parts {
			sb.WriteString(p)
		}
		out.s(sb.String())
	}
	vnOf := func(vn int) string { return "VN" + strconv.Itoa(vn) }
	varVN := map[string]int{}
	vnRep := map[int]string{}
	type exprKey struct {
		op     string
		v1, v2 int
	}
	exprVN := map[exprKey]int{}
	constVN := map[int64]int{}
	nxt := 1
	redundant := 0
	getVN := func(v string) int {
		if vn, ok := varVN[v]; ok {
			return vn
		}
		vn := nxt
		nxt++
		varVN[v] = vn
		vnRep[vn] = v
		return vn
	}
	for _, ins := range insts {
		switch ins.opcode {
		case "CONST":
			val, _ := strconv.ParseInt(ins.arg1, 10, 64)
			if vn, ok := constVN[val]; ok {
				varWrite(ins.dest, " = ", vnRep[vn], " (", vnOf(vn), " REDUNDANT)")
				varVN[ins.dest] = vn
				redundant++
			} else {
				vn := nxt
				nxt++
				varWrite(ins.dest, " = CONST ", ins.arg1, " (", vnOf(vn), ")")
				constVN[val] = vn
				varVN[ins.dest] = vn
				vnRep[vn] = ins.dest
			}
		case "COPY":
			vn := getVN(ins.arg1)
			varWrite(ins.dest, " = ", vnRep[vn], " (", vnOf(vn), " REDUNDANT)")
			varVN[ins.dest] = vn
			redundant++
		case "PHI":
			vn1 := getVN(ins.arg1)
			vn2 := getVN(ins.arg2)
			if vn1 == vn2 {
				varWrite(ins.dest, " = ", vnRep[vn1], " (", vnOf(vn1), " REDUNDANT PHI)")
				varVN[ins.dest] = vn1
				redundant++
			} else {
				lo, hi := vn1, vn2
				if lo > hi {
					lo, hi = hi, lo
				}
				key := exprKey{"PHI", lo, hi}
				if vn, ok := exprVN[key]; ok {
					varWrite(ins.dest, " = ", vnRep[vn], " (", vnOf(vn), " REDUNDANT)")
					varVN[ins.dest] = vn
					redundant++
				} else {
					vn := nxt
					nxt++
					varWrite(ins.dest, " = PHI ", vnOf(lo), " ", vnOf(hi), " (", vnOf(vn), ")")
					exprVN[key] = vn
					varVN[ins.dest] = vn
					vnRep[vn] = ins.dest
				}
			}
		default:
			vn1 := getVN(ins.arg1)
			vn2 := getVN(ins.arg2)
			if (ins.opcode == "ADD" || ins.opcode == "MUL") && vn1 > vn2 {
				vn1, vn2 = vn2, vn1
			}
			key := exprKey{ins.opcode, vn1, vn2}
			if vn, ok := exprVN[key]; ok {
				varWrite(ins.dest, " = ", vnRep[vn], " (", vnOf(vn), " REDUNDANT)")
				varVN[ins.dest] = vn
				redundant++
			} else {
				vn := nxt
				nxt++
				varWrite(ins.dest, " = ", ins.opcode, " ", vnOf(vn1), " ", vnOf(vn2), " (", vnOf(vn), ")")
				exprVN[key] = vn
				varVN[ins.dest] = vn
				vnRep[vn] = ins.dest
			}
		}
	}
	out.s("Total unique VNs: " + strconv.Itoa(nxt-1))
	out.s("Redundant instructions eliminated: " + strconv.Itoa(redundant))
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// lexerCases is the number of match cases per input.
const lexerCases = 10

// lexerMatch reports whether s fullmatches (a|b)*a(a|b)^k: all chars in
// {a,b}, length >= k+1, and the (k+1)-th char from the end is 'a'.
func lexerMatch(s string, k int) bool {
	if len(s) < k+1 {
		return false
	}
	for i := 0; i < len(s); i++ {
		if s[i] != 'a' && s[i] != 'b' {
			return false
		}
	}
	return s[len(s)-k-1] == 'a'
}

// LexerRegexNfaGenerator emits pattern (a|b)*a(a|b)^k with random
// match/no-match cases.
type LexerRegexNfaGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *LexerRegexNfaGenerator) Scales() []int64 { return g.scales }

// Generate builds the pattern plus cases, the MATCH verdicts, and fit n.
func (g *LexerRegexNfaGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	k := nn / 100
	if k < 1 {
		k = 1
	}
	pattern := "(a|b)*a" + strings.Repeat("(a|b)", k)
	cases := make([]string, 0, lexerCases)
	for i := 0; i < lexerCases; i++ {
		draw := r.Random()
		switch {
		case draw < 0.80:
			var sb strings.Builder
			sb.Grow(nn)
			for j := 0; j < nn; j++ {
				sb.WriteByte("ab"[r.Choice(2)])
			}
			cases = append(cases, sb.String())
		case draw < 0.90:
			ln := int(r.RandInt(1, 5))
			var sb strings.Builder
			sb.Grow(ln)
			for j := 0; j < ln; j++ {
				sb.WriteByte("ab"[r.Choice(2)])
			}
			cases = append(cases, sb.String())
		default:
			chars := make([]byte, nn)
			for j := 0; j < nn; j++ {
				chars[j] = "ab"[r.Choice(2)]
			}
			chars[r.RandInt(0, n-1)] = 'c'
			cases = append(cases, string(chars))
		}
	}
	in := newTokBuf(len(pattern) + nn*lexerCases + 32)
	in.s(pattern)
	in.i(int64(len(cases)))
	for _, c := range cases {
		in.s(c)
	}
	out := newLineBuf(lexerCases * 9)
	for _, c := range cases {
		if lexerMatch(c, k) {
			out.s("MATCH")
		} else {
			out.s("NO_MATCH")
		}
	}
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// linScanTriple is one live interval (variable id, start, end).
type linScanTriple struct {
	iid   int
	start int64
	end   int64
}

// linScanActive is an active allocation (end, variable id, register).
type linScanActive struct {
	end int64
	iid int
	reg int
}

// LinearScanRegallocGenerator emits randomized live intervals with an
// exact linear-scan port for the expected allocation.
type LinearScanRegallocGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *LinearScanRegallocGenerator) Scales() []int64 { return g.scales }

// Generate builds the interval tokens, the R/SPILL lines, and fit n.
func (g *LinearScanRegallocGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	k := nn / 16
	if k < 1 {
		k = 1
	}
	ids := make([]int, nn)
	for i := range ids {
		ids[i] = i
	}
	r.Shuffle(len(ids), func(i, j int) { ids[i], ids[j] = ids[j], ids[i] })
	triples := make([]linScanTriple, 0, nn)
	for _, iid := range ids {
		start := r.RandInt(0, 3*n)
		end := start + r.RandInt(0, n/2)
		triples = append(triples, linScanTriple{iid, start, end})
	}
	in := newTokBuf(nn*24 + 16)
	in.i(n)
	in.i(int64(k))
	for _, t := range triples {
		in.i(int64(t.iid))
		in.i(t.start)
		in.i(t.end)
	}
	order := make([]linScanTriple, len(triples))
	copy(order, triples)
	sort.SliceStable(order, func(i, j int) bool {
		if order[i].start != order[j].start {
			return order[i].start < order[j].start
		}
		return order[i].end < order[j].end
	})
	free := make([]bool, k)
	for i := range free {
		free[i] = true
	}
	var active []linScanActive
	assigned := make([]int, nn)
	for i := range assigned {
		assigned[i] = -1
	}
	spilled := map[int]bool{}
	endsOf := func(list []linScanActive) []int64 {
		ends := make([]int64, len(list))
		for i, e := range list {
			ends[i] = e.end
		}
		return ends
	}
	insertByEnd := func(list []linScanActive, e linScanActive) []linScanActive {
		ends := endsOf(list)
		pos := sort.Search(len(list), func(i int) bool { return ends[i] > e.end })
		list = append(list, linScanActive{})
		copy(list[pos+1:], list[pos:])
		list[pos] = e
		return list
	}
	for _, t := range order {
		var unexpired []linScanActive
		for _, e := range active {
			if e.end < t.start {
				free[e.reg] = true
			} else {
				unexpired = append(unexpired, e)
			}
		}
		if len(unexpired) == k {
			if k == 0 {
				spilled[t.iid] = true
			} else {
				// Clojure max-key keeps the LAST maximal element on
				// ties, so scan from the back.
				latest := unexpired[len(unexpired)-1]
				for i := len(unexpired) - 2; i >= 0; i-- {
					if unexpired[i].end > latest.end {
						latest = unexpired[i]
					}
				}
				if latest.end > t.end {
					reg := latest.reg
					var kept []linScanActive
					for _, e := range unexpired {
						if e.iid != latest.iid {
							kept = append(kept, e)
						}
					}
					unexpired = insertByEnd(kept, linScanActive{t.end, t.iid, reg})
					assigned[latest.iid] = -1
					assigned[t.iid] = reg
					spilled[latest.iid] = true
				} else {
					spilled[t.iid] = true
				}
			}
			active = unexpired
		} else {
			reg := 0
			for !free[reg] {
				reg++
			}
			free[reg] = false
			unexpired = insertByEnd(unexpired, linScanActive{t.end, t.iid, reg})
			assigned[t.iid] = reg
			active = unexpired
		}
	}
	out := newLineBuf(nn*12 + 32)
	allocated := 0
	spilledCount := 0
	for i := 0; i < nn; i++ {
		if spilled[i] {
			out.s("v" + strconv.Itoa(i) + ": SPILL")
			spilledCount++
		} else {
			out.s("v" + strconv.Itoa(i) + ": R" + strconv.Itoa(assigned[i]))
			allocated++
		}
	}
	out.s("Allocated: " + strconv.Itoa(allocated) + " Spilled: " + strconv.Itoa(spilledCount))
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// byteSet is a small set of grammar symbols.
type byteSet map[byte]bool

func (s byteSet) clone() byteSet {
	c := make(byteSet, len(s))
	for b := range s {
		c[b] = true
	}
	return c
}

// sorted returns the set members in ascending byte order.
func (s byteSet) sorted() []byte {
	out := make([]byte, 0, len(s))
	for b := range s {
		out = append(out, b)
	}
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

func byteSetEqual(a, b byteSet) bool {
	if len(a) != len(b) {
		return false
	}
	for c := range a {
		if !b[c] {
			return false
		}
	}
	return true
}

func unionByteSet(a, b byteSet) byteSet {
	u := make(byteSet, len(a)+len(b))
	for c := range a {
		u[c] = true
	}
	for c := range b {
		u[c] = true
	}
	return u
}

// sortedUniqueBytes returns the sorted distinct bytes of s.
func sortedUniqueBytes(s string) []byte {
	set := byteSet{}
	for i := 0; i < len(s); i++ {
		set[s[i]] = true
	}
	return set.sorted()
}

// ll1Upper reports whether c is an uppercase nonterminal.
func ll1Upper(c byte) bool { return 'A' <= c && c <= 'Z' }

// ll1Terminal reports whether c is a terminal symbol.
func ll1Terminal(c byte) bool { return !ll1Upper(c) && c != '#' }

// ll1FirstOfString computes FIRST(s) under the given FIRST sets.
func ll1FirstOfString(s string, firstSets map[byte]byteSet) byteSet {
	if s == "#" || s == "" {
		return byteSet{'#': true}
	}
	acc := byteSet{}
	for i := 0; i < len(s); i++ {
		symFirst := firstSets[s[i]]
		for c := range symFirst {
			if c != '#' {
				acc[c] = true
			}
		}
		if !symFirst['#'] {
			return acc
		}
	}
	acc['#'] = true
	return acc
}

// ll1Prod is one grammar production (lhs, rhs with # for epsilon).
type ll1Prod struct {
	lhs byte
	rhs string
}

// ll1ComputeFirst builds FIRST sets via the queued worklist.
func ll1ComputeFirst(prods []ll1Prod, firstInit map[byte]byteSet) map[byte]byteSet {
	bySym := map[byte][]int{}
	for idx, p := range prods {
		if p.rhs == "#" {
			continue
		}
		for _, sym := range sortedUniqueBytes(p.rhs) {
			bySym[sym] = append(bySym[sym], idx)
		}
	}
	first := map[byte]byteSet{}
	for k, v := range firstInit {
		first[k] = v.clone()
	}
	changed := byteSet{}
	for _, p := range prods {
		old := first[p.lhs]
		var add byteSet
		if p.rhs == "#" {
			add = byteSet{'#': true}
		} else {
			add = ll1FirstOfString(p.rhs, first)
		}
		nw := unionByteSet(old, add)
		if !byteSetEqual(nw, old) {
			first[p.lhs] = nw
			changed[p.lhs] = true
		}
	}
	queue := changed.sorted()
	inQueue := changed.clone()
	for len(queue) > 0 {
		sym := queue[0]
		queue = queue[1:]
		delete(inQueue, sym)
		for _, idx := range bySym[sym] {
			lhs, rhs := prods[idx].lhs, prods[idx].rhs
			old := first[lhs]
			var add byteSet
			if rhs == "#" {
				add = byteSet{'#': true}
			} else {
				add = ll1FirstOfString(rhs, first)
			}
			nw := unionByteSet(old, add)
			if !byteSetEqual(nw, old) {
				first[lhs] = nw
				if !inQueue[lhs] {
					queue = append(queue, lhs)
					inQueue[lhs] = true
				}
			}
		}
	}
	return first
}

// ll1Pos is one nonterminal occurrence with its FIRST-of-rest data.
type ll1Pos struct {
	sym      byte
	noEps    byteSet
	nullable bool
}

// ll1Info precomputes per-production occurrence data.
type ll1Info struct {
	lhs       byte
	positions []ll1Pos
}

// ll1ComputeFollow builds FOLLOW sets via the propagation worklist.
func ll1ComputeFollow(prods []ll1Prod, firstSets map[byte]byteSet, start byte) map[byte]byteSet {
	var infos []ll1Info
	for _, p := range prods {
		if p.rhs == "#" {
			infos = append(infos, ll1Info{p.lhs, nil})
			continue
		}
		var positions []ll1Pos
		for i := 0; i < len(p.rhs); i++ {
			sym := p.rhs[i]
			if ll1Terminal(sym) {
				continue
			}
			fRest := ll1FirstOfString(p.rhs[i+1:], firstSets)
			noEps := byteSet{}
			for c := range fRest {
				if c != '#' {
					noEps[c] = true
				}
			}
			positions = append(positions, ll1Pos{sym, noEps, fRest['#']})
		}
		infos = append(infos, ll1Info{p.lhs, positions})
	}
	follow := map[byte]byteSet{start: {'$': true}}
	for _, info := range infos {
		for _, ps := range info.positions {
			f := follow[ps.sym]
			if f == nil {
				f = byteSet{}
				follow[ps.sym] = f
			}
			for c := range ps.noEps {
				f[c] = true
			}
		}
	}
	prop := map[byte][]byte{}
	for _, info := range infos {
		for _, ps := range info.positions {
			if ps.nullable {
				prop[info.lhs] = append(prop[info.lhs], ps.sym)
			}
		}
	}
	seeds := byteSet{}
	for lhs := range prop {
		if len(follow[lhs]) > 0 {
			seeds[lhs] = true
		}
	}
	if len(follow[start]) > 0 {
		seeds[start] = true
	}
	queue := seeds.sorted()
	inQueue := seeds.clone()
	for len(queue) > 0 {
		sym := queue[0]
		queue = queue[1:]
		delete(inQueue, sym)
		src := follow[sym]
		for _, tgt := range prop[sym] {
			old := follow[tgt]
			nw := unionByteSet(old, src)
			if !byteSetEqual(nw, old) {
				follow[tgt] = nw
				if !inQueue[tgt] {
					queue = append(queue, tgt)
					inQueue[tgt] = true
				}
			}
		}
	}
	return follow
}

// ll1Key is one predictive-table key (nonterminal, lookahead).
type ll1Key struct {
	lhs  byte
	term byte
}

// ll1BuildTable builds the predictive table, reporting LL(1)-ness.
func ll1BuildTable(prods []ll1Prod, firstSets, followSets map[byte]byteSet) (map[ll1Key]int, bool) {
	table := map[ll1Key]int{}
	ok := true
	for idx, p := range prods {
		fRhs := ll1FirstOfString(p.rhs, firstSets)
		for _, term := range fRhs.sorted() {
			if term == '#' {
				continue
			}
			key := ll1Key{p.lhs, term}
			if old, has := table[key]; has && old != idx {
				ok = false
			}
			table[key] = idx
		}
		if fRhs['#'] {
			for _, term := range followSets[p.lhs].sorted() {
				key := ll1Key{p.lhs, term}
				if old, has := table[key]; has && old != idx {
					ok = false
				}
				table[key] = idx
			}
		}
	}
	return table, ok
}

// ll1Parse is an exact port of the predictive-parse loop.
func ll1Parse(prods []ll1Prod, table map[ll1Key]int, start byte, word string) string {
	text := word + "$"
	total := len(text)
	stack := []byte{'$', start}
	pos := 0
	var deriv []int
	joinDeriv := func() string {
		var sb strings.Builder
		for i, d := range deriv {
			if i > 0 {
				sb.WriteByte(' ')
			}
			sb.WriteByte('r')
			sb.WriteString(strconv.Itoa(d))
		}
		return sb.String()
	}
	for {
		if len(stack) == 0 {
			if pos == total {
				return joinDeriv()
			}
			return "SYNTAX ERROR"
		}
		top := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		var look byte
		if pos < total {
			look = text[pos]
		} else {
			look = '$'
		}
		if top == look {
			if top == '$' {
				if pos+1 == total {
					return joinDeriv()
				}
				return "SYNTAX ERROR"
			}
			pos++
		} else if ll1Terminal(top) {
			return "SYNTAX ERROR"
		} else {
			rule, has := table[ll1Key{top, look}]
			if !has {
				return "SYNTAX ERROR"
			}
			rhs := prods[rule].rhs
			if rhs != "#" {
				for i := len(rhs) - 1; i >= 0; i-- {
					stack = append(stack, rhs[i])
				}
			}
			deriv = append(deriv, rule)
		}
	}
}

// ll1Start is the fixed grammar's start symbol.
const ll1Start = 'S'

// ll1Prods is the fixed LL(1) grammar S -> aS | b.
var ll1Prods = []ll1Prod{{'S', "aS"}, {'S', "b"}}

// ll1ValidCount and ll1InvalidCount are the query mix sizes.
const (
	ll1ValidCount   = 4
	ll1InvalidCount = 2
)

// ll1MatchesStar reports whether word fullmatches a*b (and so must be
// regenerated as an invalid query).
func ll1MatchesStar(word string) bool {
	if len(word) == 0 || word[len(word)-1] != 'b' {
		return false
	}
	for i := 0; i < len(word)-1; i++ {
		if word[i] != 'a' {
			return false
		}
	}
	return true
}

// Ll1ParserGenerator emits the fixed LL(1) grammar S -> aS | b with
// valid + invalid queries.
type Ll1ParserGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *Ll1ParserGenerator) Scales() []int64 { return g.scales }

// Generate builds the grammar plus queries, the parse lines, and fit n.
func (g *Ll1ParserGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	each := nn / ll1ValidCount
	if each < 1 {
		each = 1
	}
	queries := make([]string, 0, ll1ValidCount+ll1InvalidCount)
	for i := 0; i < ll1ValidCount; i++ {
		queries = append(queries, strings.Repeat("a", each)+"b")
	}
	var invalid []string
	for len(invalid) < ll1InvalidCount {
		ln := int(r.RandInt(1, 20))
		var sb strings.Builder
		sb.Grow(ln)
		for j := 0; j < ln; j++ {
			sb.WriteByte("abc"[r.Choice(3)])
		}
		word := sb.String()
		if ll1MatchesStar(word) {
			continue
		}
		invalid = append(invalid, word)
	}
	invalid[0] = "EPS"
	queries = append(queries, invalid...)
	r.Shuffle(len(queries), func(i, j int) { queries[i], queries[j] = queries[j], queries[i] })
	in := newTokBuf(nn + 64)
	in.s("2")
	in.s(string([]byte{ll1Start}))
	for _, p := range ll1Prods {
		in.s(string([]byte{p.lhs}))
		in.s("->")
		in.s(p.rhs)
	}
	in.i(int64(len(queries)))
	for _, q := range queries {
		in.s(q)
	}
	terminals := byteSet{'$': true}
	for _, p := range ll1Prods {
		for i := 0; i < len(p.rhs); i++ {
			if ll1Terminal(p.rhs[i]) {
				terminals[p.rhs[i]] = true
			}
		}
	}
	firstInit := map[byte]byteSet{}
	for t := range terminals {
		firstInit[t] = byteSet{t: true}
	}
	firstSets := ll1ComputeFirst(ll1Prods, firstInit)
	followSets := ll1ComputeFollow(ll1Prods, firstSets, ll1Start)
	table, ok := ll1BuildTable(ll1Prods, firstSets, followSets)
	out := newLineBuf(nn*3 + 32)
	if !ok {
		out.s("GRAMMAR IS NOT LL1")
	} else {
		out.s("GRAMMAR IS LL1")
		for _, word := range queries {
			out.s(ll1Parse(ll1Prods, table, ll1Start, word))
		}
	}
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// lrProd is one fixed-grammar production (driver text, RHS size).
type lrProd struct {
	text string
	size int
}

// lrProds is the fixed E/T/F grammar.
var lrProds = map[int]lrProd{
	1: {"E -> E + T", 3},
	2: {"E -> T", 1},
	3: {"T -> T * F", 3},
	4: {"T -> F", 1},
	5: {"F -> ( E )", 3},
	6: {"F -> id", 1},
}

// lrActionKey is one action-table key (state, lookahead token).
type lrActionKey struct {
	state int
	look  string
}

// lrAction is the fixed LR action table (1000 = accept).
var lrAction = map[lrActionKey]int{
	{0, "id"}: 5, {0, "("}: 4,
	{1, "+"}: 6, {1, "$"}: 1000,
	{2, "+"}: -2, {2, "*"}: 7, {2, ")"}: -2, {2, "$"}: -2,
	{3, "+"}: -4, {3, "*"}: -4, {3, ")"}: -4, {3, "$"}: -4,
	{4, "id"}: 5, {4, "("}: 4,
	{5, "+"}: -6, {5, "*"}: -6, {5, ")"}: -6, {5, "$"}: -6,
	{6, "id"}: 5, {6, "("}: 4,
	{7, "id"}: 5, {7, "("}: 4,
	{8, "+"}: 6, {8, ")"}: 11,
	{9, "+"}: -1, {9, "*"}: 7, {9, ")"}: -1, {9, "$"}: -1,
	{10, "+"}: -3, {10, "*"}: -3, {10, ")"}: -3, {10, "$"}: -3,
	{11, "+"}: -5, {11, "*"}: -5, {11, ")"}: -5, {11, "$"}: -5,
}

// lrGotoKey is one goto-table key (state, nonterminal).
type lrGotoKey struct {
	state int
	sym   byte
}

// lrGoto is the fixed LR goto table.
var lrGoto = map[lrGotoKey]int{
	{0, 'E'}: 1, {0, 'T'}: 2, {0, 'F'}: 3,
	{4, 'E'}: 8, {4, 'T'}: 2, {4, 'F'}: 3,
	{6, 'T'}: 9, {6, 'F'}: 3, {7, 'F'}: 10,
}

// lrRun is an exact port of parse-tokens over the fixed tables.
func lrRun(tokens []string) []string {
	toks := tokens
	if len(toks) == 0 || toks[len(toks)-1] != "$" {
		toks = make([]string, 0, len(tokens)+1)
		toks = append(toks, tokens...)
		toks = append(toks, "$")
	}
	var out []string
	stack := []int{0}
	pos := 0
	for {
		state := stack[len(stack)-1]
		var look string
		if pos < len(toks) {
			look = toks[pos]
		}
		act, has := lrAction[lrActionKey{state, look}]
		if !has {
			out = append(out, "SYNTAX_ERROR")
			return out
		}
		if act == 1000 {
			out = append(out, "ACCEPTED")
			return out
		}
		if act > 0 {
			stack = append(stack, act)
			pos++
		} else if act < 0 {
			p := lrProds[-act]
			out = append(out, p.text)
			stack = stack[:len(stack)-p.size]
			nxt, has := lrGoto[lrGotoKey{stack[len(stack)-1], p.text[0]}]
			if !has {
				out = append(out, "SYNTAX_ERROR")
				return out
			}
			stack = append(stack, nxt)
		} else {
			out = append(out, "SYNTAX_ERROR")
			return out
		}
	}
}

// LrParserGenerator emits valid expression token streams over
// id ( ) + * for the fixed E/T/F grammar.
type LrParserGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *LrParserGenerator) Scales() []int64 { return g.scales }

// Generate builds the token stream, the reduction lines, and fit n.
func (g *LrParserGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	toks := make([]string, 0, 3*nn)
	ops := []string{"+", "*"}
	for i := 0; i < nn; i++ {
		if r.Random() < 0.25 {
			toks = append(toks, "(", "id", ")")
		} else {
			toks = append(toks, "id")
		}
		if i+1 < nn {
			toks = append(toks, ops[r.Choice(len(ops))])
		}
	}
	in := newTokBuf(3*len(toks) + 8)
	for _, t := range toks {
		in.s(t)
	}
	out := newLineBuf(12 * len(toks))
	for _, line := range lrRun(toks) {
		out.s(line)
	}
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// prattTok is one lexer token (number or single-char operator).
type prattTok struct {
	op    byte
	num   int64
	isNum bool
}

// prattTokenize splits text into numbers and operators, skipping
// whitespace and unknown characters.
func prattTokenize(text string) []prattTok {
	var toks []prattTok
	i, total := 0, len(text)
	for i < total {
		c := text[i]
		switch {
		case c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v':
			i++
		case '0' <= c && c <= '9':
			var val int64
			for i < total && '0' <= text[i] && text[i] <= '9' {
				val = val*10 + int64(text[i]-'0')
				i++
			}
			toks = append(toks, prattTok{num: val, isNum: true})
		case c == '+' || c == '-' || c == '*' || c == '/' || c == '^' || c == '(' || c == ')':
			toks = append(toks, prattTok{op: c})
			i++
		default:
			i++
		}
	}
	return toks
}

// prattBP returns the (left, right) binding powers of an operator.
func prattBP(op byte) (int, int) {
	switch op {
	case '+', '-':
		return 10, 11
	case '*', '/':
		return 20, 21
	case '^':
		return 31, 30
	}
	return -1, -1
}

// prattNode is one AST node: num, unary, or binary.
type prattNode struct {
	kind        byte // 'n', 'u', or 'b'
	op          byte
	num         int64
	left, right *prattNode
}

// prattParse is an exact port of parse-expr. It returns
// (nextPos, node, ok); trailing tokens after a valid prefix are
// ignored by the caller, exactly like the driver.
func prattParse(toks []prattTok, pos, minBP int) (int, *prattNode, bool) {
	if pos >= len(toks) {
		return 0, nil, false
	}
	t := toks[pos]
	var cur int
	var node *prattNode
	switch {
	case t.isNum:
		cur, node = pos+1, &prattNode{kind: 'n', num: t.num}
	case t.op == '-':
		end, sub, ok := prattParse(toks, pos+1, 40)
		if !ok {
			return 0, nil, false
		}
		cur, node = end, &prattNode{kind: 'u', op: '-', left: sub}
	case t.op == '(':
		end, sub, ok := prattParse(toks, pos+1, 0)
		if !ok {
			return 0, nil, false
		}
		// Missing rparen is tolerated, matching the driver.
		if end < len(toks) && !toks[end].isNum && toks[end].op == ')' {
			end++
		}
		cur, node = end, sub
	default:
		return 0, nil, false
	}
	for {
		if cur >= len(toks) {
			return cur, node, true
		}
		t := toks[cur]
		lbp, rbp := -1, -1
		if !t.isNum {
			lbp, rbp = prattBP(t.op)
		}
		if lbp < minBP {
			return cur, node, true
		}
		end, sub, ok := prattParse(toks, cur+1, rbp)
		if !ok {
			return 0, nil, false
		}
		cur, node = end, &prattNode{kind: 'b', op: t.op, left: node, right: sub}
	}
}

// prattEval evaluates the AST with unbounded integers (quot-style
// truncating division, div-by-zero yields 0, non-positive exponents
// yield 1).
func prattEval(node *prattNode) *big.Int {
	switch node.kind {
	case 'n':
		return big.NewInt(node.num)
	case 'u':
		child := prattEval(node.left)
		if node.op == '-' {
			child.Neg(child)
		}
		return child
	}
	left := prattEval(node.left)
	right := prattEval(node.right)
	switch node.op {
	case '+':
		return left.Add(left, right)
	case '-':
		return left.Sub(left, right)
	case '*':
		return left.Mul(left, right)
	case '/':
		if right.Sign() == 0 {
			return big.NewInt(0)
		}
		return left.Quo(left, right)
	default:
		if right.Sign() <= 0 {
			return big.NewInt(1)
		}
		return new(big.Int).Exp(left, right, nil)
	}
}

// prattWriteSexpr renders the AST s-expression into buf.
func prattWriteSexpr(buf *bytes.Buffer, node *prattNode) {
	switch node.kind {
	case 'n':
		buf.WriteString(strconv.FormatInt(node.num, 10))
	case 'u':
		buf.WriteByte('(')
		buf.WriteByte(node.op)
		buf.WriteByte(' ')
		prattWriteSexpr(buf, node.left)
		buf.WriteByte(')')
	default:
		buf.WriteByte('(')
		buf.WriteByte(node.op)
		buf.WriteByte(' ')
		prattWriteSexpr(buf, node.left)
		buf.WriteByte(' ')
		prattWriteSexpr(buf, node.right)
		buf.WriteByte(')')
	}
}

// prattInvalid holds the invalid-expression spellings (~5% of lines).
var prattInvalid = []string{"+", "1 +", "* 2", "(+", "2 +* 3", "^ 2", "2 ^", "()", ")(", "( 1 + )", "-"}

// prattOperand builds one random operand (literal, unary minus,
// parenthesized expression, or wide literal).
func prattOperand(r *mtrand.Rand, depth int, budget *int) string {
	draw := r.Random()
	if depth >= 4 || draw < 0.5 {
		return strconv.FormatInt(r.RandInt(0, 99), 10)
	}
	if draw < 0.6 {
		return "-" + prattOperand(r, depth+1, budget)
	}
	if draw < 0.8 {
		return "(" + prattExpr(r, depth+1, budget) + ")"
	}
	return strconv.FormatInt(r.RandInt(0, 999), 10)
}

// prattExpr builds one random expression with spaced parts and an
// occasional unknown-char insertion.
func prattExpr(r *mtrand.Rand, depth int, budget *int) string {
	parts := []string{prattOperand(r, depth, budget)}
	count := r.RandInt(0, 3)
	for i := int64(0); i < count; i++ {
		ops := []byte{'+', '-', '*', '/'}
		if *budget != 0 {
			ops = append(ops, '^')
		}
		op := ops[r.Choice(len(ops))]
		if op == '^' {
			*budget--
			parts = append(parts, "^", strconv.FormatInt(r.RandInt(0, 4), 10))
		} else {
			parts = append(parts, string([]byte{op}), prattOperand(r, depth, budget))
		}
	}
	var sb strings.Builder
	sb.WriteString(parts[0])
	for _, part := range parts[1:] {
		if r.Random() < 0.5 {
			sb.WriteByte(' ')
		}
		sb.WriteString(part)
		if r.Random() < 0.3 {
			sb.WriteByte(' ')
		}
	}
	text := sb.String()
	if r.Random() < 0.03 {
		pos := int(r.RandInt(0, int64(len(text))))
		ch := "x?"[r.Choice(2)]
		text = text[:pos] + string([]byte{ch}) + text[pos:]
	}
	return text
}

// PrattParserGenerator emits random arithmetic expressions with an
// exact Pratt-parser port for the expected s-expressions and values.
type PrattParserGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *PrattParserGenerator) Scales() []int64 { return g.scales }

// Generate builds the expression lines, the value lines, and fit n.
func (g *PrattParserGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	exprs := make([]string, 0, nn)
	for i := 0; i < nn; i++ {
		if r.Random() < 0.05 {
			exprs = append(exprs, prattInvalid[r.RandInt(0, int64(len(prattInvalid)-1))])
		} else {
			budget := 1
			exprs = append(exprs, prattExpr(r, 0, &budget))
		}
	}
	in := newLineBuf(nn*24 + 16)
	in.s(strconv.FormatInt(n, 10))
	for _, e := range exprs {
		in.s(e)
	}
	out := newLineBuf(nn*32 + 16)
	for _, text := range exprs {
		toks := prattTokenize(text)
		_, node, ok := prattParse(toks, 0, 0)
		if !ok {
			out.s("PARSE_ERROR")
			continue
		}
		var sb bytes.Buffer
		prattWriteSexpr(&sb, node)
		sb.WriteString(" = ")
		sb.WriteString(prattEval(node).String())
		out.s(sb.String())
	}
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// regallocK is the register count for graph coloring.
const regallocK = 6

// regallocMPerN is the edge count per node.
const regallocMPerN = 4

// intMinHeap is a min-heap of node ids with lazy deletion by the
// caller's membership sets.
type intMinHeap []int

func (h intMinHeap) Len() int            { return len(h) }
func (h intMinHeap) Less(i, j int) bool  { return h[i] < h[j] }
func (h intMinHeap) Swap(i, j int)       { h[i], h[j] = h[j], h[i] }
func (h *intMinHeap) Push(x interface{}) { *h = append(*h, x.(int)) }
func (h *intMinHeap) Pop() interface{} {
	old := *h
	v := old[len(old)-1]
	*h = old[:len(old)-1]
	return v
}

// RegisterAllocationGenerator emits random graphs (multiedges, rare
// self-loops) with a Chaitin-port expected coloring.
type RegisterAllocationGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *RegisterAllocationGenerator) Scales() []int64 { return g.scales }

// Generate builds the graph tokens, the coloring lines, and fit n.
func (g *RegisterAllocationGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	k := regallocK
	type edge struct{ u, v int }
	edges := make([]edge, 0, regallocMPerN*nn)
	for i := 0; i < regallocMPerN*nn; i++ {
		if r.Random() < 0.01 {
			u := int(r.RandInt(0, n-1))
			edges = append(edges, edge{u, u})
		} else {
			edges = append(edges, edge{int(r.RandInt(0, n-1)), int(r.RandInt(0, n-1))})
		}
	}
	in := newTokBuf(8*len(edges) + 24)
	in.i(n)
	in.i(int64(len(edges)))
	in.i(int64(k))
	for _, e := range edges {
		in.i(int64(e.u))
		in.i(int64(e.v))
	}
	adj := make([][]int, nn)
	for _, e := range edges {
		adj[e.u] = append(adj[e.u], e.v)
		adj[e.v] = append(adj[e.v], e.u)
	}
	deg := make([]int, nn)
	for i, row := range adj {
		deg[i] = len(row)
	}
	active := make(map[int]bool, nn)
	for i := 0; i < nn; i++ {
		active[i] = true
	}
	low := map[int]bool{}
	for i := 0; i < nn; i++ {
		if deg[i] < k {
			low[i] = true
		}
	}
	activeHeap := make(intMinHeap, nn)
	for i := range activeHeap {
		activeHeap[i] = i
	}
	heap.Init(&activeHeap)
	lowList := make([]int, 0, len(low))
	for i := range low {
		lowList = append(lowList, i)
	}
	sort.Ints(lowList)
	lowHeap := intMinHeap(lowList)
	heap.Init(&lowHeap)
	var stack []int
	spilled := map[int]bool{}
	for len(active) > 0 {
		var node int
		var simplify bool
		if len(low) > 0 {
			for !low[lowHeap[0]] {
				heap.Pop(&lowHeap)
			}
			node = heap.Pop(&lowHeap).(int)
			delete(low, node)
			delete(active, node)
			simplify = true
		} else {
			for !active[activeHeap[0]] {
				heap.Pop(&activeHeap)
			}
			node = heap.Pop(&activeHeap).(int)
			delete(active, node)
			simplify = false
		}
		freqs := map[int]int{}
		for _, v := range adj[node] {
			freqs[v]++
		}
		neighbors := make([]int, 0, len(freqs))
		for v := range freqs {
			neighbors = append(neighbors, v)
		}
		sort.Ints(neighbors)
		for _, v := range neighbors {
			if active[v] {
				old := deg[v]
				nw := old - freqs[v]
				deg[v] = nw
				if old >= k && nw < k && !low[v] {
					low[v] = true
					heap.Push(&lowHeap, v)
				}
			}
		}
		if simplify {
			stack = append(stack, node)
		} else {
			spilled[node] = true
		}
	}
	color := make([]int, nn)
	for i := range color {
		color[i] = -1
	}
	for len(stack) > 0 {
		u := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		used := map[int]bool{}
		for _, v := range adj[u] {
			if color[v] != -1 {
				used[color[v]] = true
			}
		}
		pick := -1
		for c := 0; c < k; c++ {
			if !used[c] {
				pick = c
				break
			}
		}
		if pick == -1 {
			spilled[u] = true
		} else {
			color[u] = pick
		}
	}
	out := newLineBuf(nn*10 + 8)
	for i := 0; i < nn; i++ {
		if spilled[i] {
			out.s("v" + strconv.Itoa(i) + ":SPILL")
		} else {
			out.s("v" + strconv.Itoa(i) + ":R" + strconv.Itoa(color[i]))
		}
	}
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// ssaRPO returns the reverse-postorder walk from entry 1 plus node
// positions, mirroring the iterative DFS exactly.
func ssaRPO(succ [][]int, n int) ([]int, map[int]int) {
	visited := map[int]bool{1: true}
	type frame struct{ node, idx int }
	stack := []frame{{1, 0}}
	var post []int
	for len(stack) > 0 {
		f := &stack[len(stack)-1]
		succs := succ[f.node]
		if f.idx < len(succs) {
			nxt := succs[f.idx]
			f.idx++
			if !visited[nxt] {
				visited[nxt] = true
				stack = append(stack, frame{nxt, 0})
			}
		} else {
			post = append(post, f.node)
			stack = stack[:len(stack)-1]
		}
	}
	rpo := make([]int, len(post))
	for i, v := range post {
		rpo[len(post)-1-i] = v
	}
	num := make(map[int]int, len(rpo))
	for i, v := range rpo {
		num[v] = i
	}
	return rpo, num
}

// ssaIDom runs the Cooper-Harvey-Kennedy idom fixpoint.
func ssaIDom(pred [][]int, rpo []int, rpoNum map[int]int) map[int]int {
	idom := map[int]int{1: 1}
	intersect := func(a, b int) int {
		for a != b {
			if rpoNum[a] > rpoNum[b] {
				a = idom[a]
			} else {
				b = idom[b]
			}
		}
		return a
	}
	changed := true
	for changed {
		changed = false
		for _, blk := range rpo[1:] {
			var procs []int
			for _, p := range pred[blk] {
				if _, ok := idom[p]; ok {
					procs = append(procs, p)
				}
			}
			if len(procs) == 0 {
				continue
			}
			nw := procs[0]
			for _, p := range procs[1:] {
				nw = intersect(nw, p)
			}
			if idom[blk] != nw {
				idom[blk] = nw
				changed = true
			}
		}
	}
	return idom
}

// ssaNaiveDF computes dominance frontiers with the single ascending
// up-sweep over dom-tree children.
func ssaNaiveDF(succ [][]int, idomv []int, n int) []map[int]bool {
	df := make([]map[int]bool, n+1)
	for i := range df {
		df[i] = map[int]bool{}
	}
	for blk := 1; blk <= n; blk++ {
		for _, nxt := range succ[blk] {
			if idomv[nxt] != blk {
				df[blk][nxt] = true
			}
		}
	}
	children := make([][]int, n+1)
	for blk := 1; blk <= n; blk++ {
		children[idomv[blk]] = append(children[idomv[blk]], blk)
	}
	// Single ascending up-sweep over dom-tree children (blocks that are
	// not children are no-ops, so skipping them is result-identical).
	for blk := 1; blk <= n; blk++ {
		for _, child := range children[blk] {
			tgts := make([]int, 0, len(df[child]))
			for t := range df[child] {
				tgts = append(tgts, t)
			}
			sort.Ints(tgts)
			for _, tgt := range tgts {
				if idomv[tgt] != blk {
					df[blk][tgt] = true
				}
			}
		}
	}
	return df
}

// ssaPhi runs the IDF worklist over the sorted definition blocks.
func ssaPhi(df []map[int]bool, defs []int) map[int]bool {
	defsSet := map[int]bool{}
	for _, d := range defs {
		defsSet[d] = true
	}
	phi := map[int]bool{}
	queue := make([]int, 0, len(defsSet))
	for d := range defsSet {
		queue = append(queue, d)
	}
	sort.Ints(queue)
	for head := 0; head < len(queue); head++ {
		blk := queue[head]
		tgts := make([]int, 0, len(df[blk]))
		for t := range df[blk] {
			tgts = append(tgts, t)
		}
		sort.Ints(tgts)
		for _, tgt := range tgts {
			if !phi[tgt] {
				phi[tgt] = true
				if !defsSet[tgt] {
					queue = append(queue, tgt)
				}
			}
		}
	}
	return phi
}

// SsaDominanceGenerator emits fully-reachable random flow graphs
// (spanning tree + extra edges) with an exact idom/DF/IDF port.
type SsaDominanceGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *SsaDominanceGenerator) Scales() []int64 { return g.scales }

// Generate builds the graph plus defs, the idom/phi lines, and fit n.
func (g *SsaDominanceGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	type edge struct{ u, v int }
	var edges []edge
	for i := 2; i <= nn; i++ {
		edges = append(edges, edge{int(r.RandInt(1, int64(i-1))), i})
	}
	for i := 0; i < 2*nn; i++ {
		edges = append(edges, edge{int(r.RandInt(1, n)), int(r.RandInt(1, n))})
	}
	defCount := nn / 10
	if defCount < 1 {
		defCount = 1
	}
	defs := make([]int, 0, defCount)
	for i := 0; i < defCount; i++ {
		defs = append(defs, int(r.RandInt(1, n)))
	}
	in := newTokBuf(8*len(edges) + 8*len(defs) + 24)
	in.i(n)
	in.i(int64(len(edges)))
	for _, e := range edges {
		in.i(int64(e.u))
		in.i(int64(e.v))
	}
	in.i(int64(len(defs)))
	for _, d := range defs {
		in.i(int64(d))
	}
	succ := make([][]int, nn+1)
	pred := make([][]int, nn+1)
	for _, e := range edges {
		succ[e.u] = append(succ[e.u], e.v)
		pred[e.v] = append(pred[e.v], e.u)
	}
	rpo, rpoNum := ssaRPO(succ, nn)
	idom := ssaIDom(pred, rpo, rpoNum)
	idomv := make([]int, nn+1)
	for blk, dom := range idom {
		idomv[blk] = dom
	}
	idomv[1] = 0
	df := ssaNaiveDF(succ, idomv, nn)
	phi := ssaPhi(df, defs)
	var line1 strings.Builder
	for i := 2; i <= nn; i++ {
		if i > 2 {
			line1.WriteByte(' ')
		}
		line1.WriteString("idom(")
		line1.WriteString(strconv.Itoa(i))
		line1.WriteString(")=")
		line1.WriteString(strconv.Itoa(idomv[i]))
	}
	out := newLineBuf(line1.Len() + nn*6 + 8)
	out.s(line1.String())
	if len(phi) == 0 {
		out.s("NONE")
	} else {
		vs := make([]int, 0, len(phi))
		for v := range phi {
			vs = append(vs, v)
		}
		sort.Ints(vs)
		var sb strings.Builder
		for i, v := range vs {
			if i > 0 {
				sb.WriteByte(' ')
			}
			sb.WriteString(strconv.Itoa(v))
		}
		out.s(sb.String())
	}
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}

// subsetEdge is one NFA transition (symbol, target).
type subsetEdge struct {
	sym byte
	tgt int
}

// SubsetConstructionGenerator emits the NFA for (a|b)*a(a|b)^k with
// mixed queries; the DFA counts come from reachable-subset
// construction over bitmask state sets.
type SubsetConstructionGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *SubsetConstructionGenerator) Scales() []int64 { return g.scales }

// Generate builds the NFA plus queries, the DFA verdicts, and fit n.
func (g *SubsetConstructionGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	r := mtrand.New(seed)
	k := 3 + nn/4000
	if k > 7 {
		k = 7
	}
	states := k + 2
	type rawEdge struct{ u, v int }
	var transList []rawEdge
	var transSym []byte
	addEdge := func(u, v int, c byte) {
		transList = append(transList, rawEdge{u, v})
		transSym = append(transSym, c)
	}
	addEdge(0, 0, 'a')
	addEdge(0, 0, 'b')
	addEdge(0, 1, 'a')
	for i := 1; i <= k; i++ {
		addEdge(i, i+1, 'a')
		addEdge(i, i+1, 'b')
	}
	accept := states - 1
	count := nn / 20
	if count < 4 {
		count = 4
	}
	width := nn
	queries := make([]string, 0, count)
	for i := 0; i < count; i++ {
		draw := r.Random()
		switch {
		case draw < 0.85:
			var sb strings.Builder
			sb.Grow(width)
			for j := 0; j < width; j++ {
				sb.WriteByte("ab"[r.Choice(2)])
			}
			queries = append(queries, sb.String())
		case draw < 0.90:
			ln := int(r.RandInt(1, 20))
			var sb strings.Builder
			sb.Grow(ln)
			for j := 0; j < ln; j++ {
				sb.WriteByte("ab"[r.Choice(2)])
			}
			queries = append(queries, sb.String())
		case draw < 0.95:
			chars := make([]byte, width)
			for j := 0; j < width; j++ {
				chars[j] = "ab"[r.Choice(2)]
			}
			chars[r.RandInt(0, int64(width)-1)] = 'c'
			queries = append(queries, string(chars))
		default:
			queries = append(queries, "EPS")
		}
	}
	in := newTokBuf(width*count + 8*len(transList) + 64)
	in.i(int64(states))
	in.s("0")
	in.s("1")
	in.i(int64(accept))
	in.i(int64(len(transList)))
	for i, e := range transList {
		in.i(int64(e.u))
		in.i(int64(e.v))
		in.s(string([]byte{transSym[i]}))
	}
	in.i(int64(count))
	for _, q := range queries {
		in.s(q)
	}
	trans := make([][]subsetEdge, states)
	for i, e := range transList {
		trans[e.u] = append(trans[e.u], subsetEdge{transSym[i], e.v})
	}
	alphabet := []byte{'a', 'b'}
	closure := func(mask uint32) uint32 {
		var queue []int
		for i := 0; i < states; i++ {
			if mask>>uint(i)&1 == 1 {
				queue = append(queue, i)
			}
		}
		for head := 0; head < len(queue); head++ {
			node := queue[head]
			for _, e := range trans[node] {
				if e.sym == '#' && mask>>uint(e.tgt)&1 == 0 {
					mask |= 1 << uint(e.tgt)
					queue = append(queue, e.tgt)
				}
			}
		}
		return mask
	}
	move := func(mask uint32, sym byte) uint32 {
		var out uint32
		for i := 0; i < states; i++ {
			if mask>>uint(i)&1 == 0 {
				continue
			}
			for _, e := range trans[i] {
				if e.sym == sym {
					out |= 1 << uint(e.tgt)
				}
			}
		}
		return out
	}
	startSet := closure(1 << 0)
	stateID := map[uint32]int{startSet: 0}
	dfaStates := []uint32{startSet}
	dfaAccept := []bool{startSet>>uint(accept)&1 == 1}
	dfaTrans := []map[byte]int{{}}
	queue := []int{0}
	for head := 0; head < len(queue); head++ {
		cur := queue[head]
		curSet := dfaStates[cur]
		for _, sym := range alphabet {
			nxt := closure(move(curSet, sym))
			if nxt == 0 {
				continue
			}
			if id, has := stateID[nxt]; has {
				dfaTrans[cur][sym] = id
			} else {
				nid := len(dfaStates)
				stateID[nxt] = nid
				dfaStates = append(dfaStates, nxt)
				dfaAccept = append(dfaAccept, nxt>>uint(accept)&1 == 1)
				dfaTrans[cur][sym] = nid
				dfaTrans = append(dfaTrans, map[byte]int{})
				queue = append(queue, nid)
			}
		}
	}
	out := newLineBuf(64 + count*7)
	out.s("DFA states: " + strconv.Itoa(len(dfaStates)))
	acceptCount := 0
	for _, a := range dfaAccept {
		if a {
			acceptCount++
		}
	}
	out.s("DFA accept states: " + strconv.Itoa(acceptCount))
	for _, word := range queries {
		text := word
		if word == "EPS" {
			text = ""
		}
		state := 0
		verdict := ""
		for i := 0; i < len(text); i++ {
			nxt, has := dfaTrans[state][text[i]]
			if !has {
				verdict = "REJECT"
				break
			}
			state = nxt
		}
		if verdict == "" {
			if dfaAccept[state] {
				verdict = "ACCEPT"
			} else {
				verdict = "REJECT"
			}
		}
		out.s(verdict)
	}
	return in.bytes(), out.bytes(), FitNFromInt64(n), nil
}
