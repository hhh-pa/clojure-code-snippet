// Slope input generators for data-structure benchmarks (T20),
// ported byte-for-byte from tools/generators/data_structure.py.
// All unexported helpers carry a ds prefix to avoid collisions
// with sibling generator files in this package.
package generators

import (
	"bytes"
	"container/heap"
	"sort"
	"strconv"
	"strings"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// dsMod is the wiring-cost modulus (direct connections).
const dsMod = 1000000007

// dsEmitLines joins lines with newline plus a trailing newline, or
// returns nothing when there are no lines.
func dsEmitLines(lines []string) []byte {
	if len(lines) == 0 {
		return []byte{}
	}
	var b bytes.Buffer
	for i, l := range lines {
		if i > 0 {
			b.WriteByte('\n')
		}
		b.WriteString(l)
	}
	b.WriteByte('\n')
	return b.Bytes()
}

// dsPrintln renders a single println line.
func dsPrintln(text string) []byte {
	return []byte(text + "\n")
}

// dsWriteInt appends v in decimal without allocating.
func dsWriteInt(b *bytes.Buffer, v int64) {
	var tmp [32]byte
	b.Write(strconv.AppendInt(tmp[:0], v, 10))
}

// DefaultAvlTreeScales holds the calibrated measurement scales.
var DefaultAvlTreeScales = []int64{37500, 75000, 150000, 300000}

// AvlTreeGenerator emits a self-balancing AVL tree workload: n,
// n values, 1 extra value.
type AvlTreeGenerator struct{ scales []int64 }

// NewAvlTreeGenerator returns an AvlTreeGenerator over scales.
func NewAvlTreeGenerator(scales []int64) *AvlTreeGenerator {
	return &AvlTreeGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *AvlTreeGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *AvlTreeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	vals := make([]int64, n)
	for i := range vals {
		vals[i] = rng.RandInt(-n, n)
	}
	extra := rng.RandInt(-n, n)
	var inb bytes.Buffer
	inb.Grow(int(n) * 8)
	dsWriteInt(&inb, n)
	for _, v := range vals {
		inb.WriteByte(' ')
		dsWriteInt(&inb, v)
	}
	inb.WriteByte(' ')
	dsWriteInt(&inb, extra)
	var root *dsAvlNode
	for _, v := range vals {
		root = dsAvlInsert(root, v)
	}
	root = dsAvlInsert(root, extra)
	var exp bytes.Buffer
	exp.Grow(int(n) * 28)
	for _, nd := range dsAvlInorder(root) {
		dsWriteInt(&exp, nd.val)
		exp.WriteString("(BF=")
		dsWriteInt(&exp, int64(dsAvlBF(nd)))
		exp.WriteString(") ")
	}
	exp.WriteByte('\n')
	for _, nd := range dsAvlPreorder(root) {
		dsWriteInt(&exp, nd.val)
		exp.WriteString("(BF=")
		dsWriteInt(&exp, int64(dsAvlBF(nd)))
		exp.WriteString(") ")
	}
	exp.WriteByte('\n')
	return inb.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// dsAvlNode is one AVL tree node.
type dsAvlNode struct {
	val         int64
	left, right *dsAvlNode
	ht          int
}

// dsAvlHeight returns the node height (-1 for nil).
func dsAvlHeight(nd *dsAvlNode) int {
	if nd == nil {
		return -1
	}
	return nd.ht
}

// dsAvlRefresh recomputes the node height.
func dsAvlRefresh(nd *dsAvlNode) *dsAvlNode {
	lh := dsAvlHeight(nd.left)
	if rh := dsAvlHeight(nd.right); rh > lh {
		lh = rh
	}
	nd.ht = 1 + lh
	return nd
}

// dsAvlBF returns the node balance factor.
func dsAvlBF(nd *dsAvlNode) int {
	if nd == nil {
		return 0
	}
	return dsAvlHeight(nd.left) - dsAvlHeight(nd.right)
}

// dsAvlRotateRight performs a right rotation.
func dsAvlRotateRight(y *dsAvlNode) *dsAvlNode {
	x := y.left
	y.left = x.right
	dsAvlRefresh(y)
	x.right = y
	return dsAvlRefresh(x)
}

// dsAvlRotateLeft performs a left rotation.
func dsAvlRotateLeft(x *dsAvlNode) *dsAvlNode {
	y := x.right
	x.right = y.left
	dsAvlRefresh(x)
	y.left = x
	return dsAvlRefresh(y)
}

// dsAvlInsert mirrors the Clojure avl-insert: dups go right and the
// double-rotation choice compares val against the child root value.
func dsAvlInsert(node *dsAvlNode, val int64) *dsAvlNode {
	if node == nil {
		return &dsAvlNode{val: val}
	}
	if val < node.val {
		node.left = dsAvlInsert(node.left, val)
	} else {
		node.right = dsAvlInsert(node.right, val)
	}
	grown := dsAvlRefresh(node)
	b := dsAvlBF(grown)
	if b > 1 {
		if val > grown.left.val {
			grown.left = dsAvlRotateLeft(grown.left)
		}
		return dsAvlRotateRight(grown)
	}
	if b < -1 {
		if val < grown.right.val {
			grown.right = dsAvlRotateRight(grown.right)
		}
		return dsAvlRotateLeft(grown)
	}
	return grown
}

// dsAvlInorder returns the nodes in inorder.
func dsAvlInorder(root *dsAvlNode) []*dsAvlNode {
	var out []*dsAvlNode
	var stack []*dsAvlNode
	cur := root
	for len(stack) > 0 || cur != nil {
		for cur != nil {
			stack = append(stack, cur)
			cur = cur.left
		}
		cur = stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		out = append(out, cur)
		cur = cur.right
	}
	return out
}

// dsAvlPreorder returns the nodes in preorder.
func dsAvlPreorder(root *dsAvlNode) []*dsAvlNode {
	var out []*dsAvlNode
	var stack []*dsAvlNode
	if root != nil {
		stack = append(stack, root)
	}
	for len(stack) > 0 {
		cur := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		out = append(out, cur)
		if cur.right != nil {
			stack = append(stack, cur.right)
		}
		if cur.left != nil {
			stack = append(stack, cur.left)
		}
	}
	return out
}

// DefaultBstLcaScales holds the calibrated measurement scales.
var DefaultBstLcaScales = []int64{187500, 375000, 750000, 1500000}

// BstLcaGenerator emits a BST lowest-common-ancestor workload: n,
// n values, v1 v2 queries.
type BstLcaGenerator struct{ scales []int64 }

// NewBstLcaGenerator returns a BstLcaGenerator over scales.
func NewBstLcaGenerator(scales []int64) *BstLcaGenerator {
	return &BstLcaGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *BstLcaGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *BstLcaGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	vals := make([]int64, n)
	for i := range vals {
		vals[i] = rng.RandInt(-n, n)
	}
	v1 := vals[rng.Choice(len(vals))]
	var v2 int64
	if rng.Random() < 0.7 {
		v2 = vals[rng.Choice(len(vals))]
	} else {
		v2 = rng.RandInt(-3*n, 3*n)
	}
	var inb bytes.Buffer
	inb.Grow(int(n) * 8)
	dsWriteInt(&inb, n)
	for _, v := range vals {
		inb.WriteByte(' ')
		dsWriteInt(&inb, v)
	}
	inb.WriteByte(' ')
	dsWriteInt(&inb, v1)
	inb.WriteByte(' ')
	dsWriteInt(&inb, v2)
	var root *dsBstNode
	for _, v := range vals {
		root = dsBstInsert(root, v)
	}
	lo, hi := v1, v2
	if hi < lo {
		lo, hi = hi, lo
	}
	found, ok := dsBstLCA(root, lo, hi)
	var expected []byte
	if ok {
		expected = dsPrintln(strconv.FormatInt(found, 10))
	} else {
		expected = []byte{}
	}
	return inb.Bytes(), expected, FitNFromInt64(n), nil
}

// DefaultBTreeScales holds the calibrated measurement scales.
var DefaultBTreeScales = []int64{37500, 75000, 150000, 300000}

// BTreeGenerator emits a geek-for-geeks B-tree op stream: dummy
// token[0], then ops to EOF. n is the op count.
type BTreeGenerator struct{ scales []int64 }

// NewBTreeGenerator returns a BTreeGenerator over scales.
func NewBTreeGenerator(scales []int64) *BTreeGenerator {
	return &BTreeGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *BTreeGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *BTreeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var toks bytes.Buffer
	toks.Grow(int(n) * 10)
	toks.WriteByte('0')
	counts := map[int64]int64{}
	var vals []int64
	var sval []string
	var total int64
	var lines []string
	var live []int64
	for i := int64(0); i < n; i++ {
		r := rng.Random()
		switch {
		case r < 0.47:
			v := rng.RandInt(-1000000, 1000000)
			toks.WriteString(" i ")
			dsWriteInt(&toks, v)
			counts[v]++
			idx := sort.Search(len(vals), func(j int) bool { return vals[j] >= v })
			vals = append(vals, 0)
			copy(vals[idx+1:], vals[idx:])
			vals[idx] = v
			sv := strconv.FormatInt(v, 10)
			sval = append(sval, "")
			copy(sval[idx+1:], sval[idx:])
			sval[idx] = sv
			total++
			live = append(live, v)
		case r < 0.94:
			var v int64
			var hit bool
			if len(live) > 0 && rng.Random() < 0.97 {
				j := rng.RandRangeStop(int64(len(live)))
				v = live[j]
				live[j] = live[len(live)-1]
				live = live[:len(live)-1]
				hit = true
			} else {
				v = rng.RandInt(-1000000, 1000000)
				hit = counts[v] > 0
				if hit {
					k := 0
					for live[k] != v {
						k++
					}
					live[k] = live[len(live)-1]
					live = live[:len(live)-1]
				}
			}
			toks.WriteString(" r ")
			dsWriteInt(&toks, v)
			if total == 0 {
				lines = append(lines, "The tree is empty")
			} else if !hit {
				lines = append(lines, "The key "+strconv.FormatInt(v, 10)+
					" is does not exist in the tree")
			} else {
				if counts[v] == 1 {
					delete(counts, v)
				} else {
					counts[v]--
				}
				idx := sort.Search(len(vals), func(j int) bool { return vals[j] >= v })
				vals = append(vals[:idx], vals[idx+1:]...)
				sval = append(sval[:idx], sval[idx+1:]...)
				total--
			}
		case r < 0.945:
			toks.WriteString(" t")
			lines = append(lines, strings.Join(sval, " "))
		default:
			toks.WriteString(" z")
		}
	}
	return toks.Bytes(), dsEmitLines(lines), FitNFromInt64(n), nil
}

// DefaultDisjointSetScales holds the calibrated measurement scales.
var DefaultDisjointSetScales = []int64{50000, 100000, 200000, 400000}

// DisjointSetGenerator emits connected components: n, then pairs
// to EOF (2*n pairs).
type DisjointSetGenerator struct{ scales []int64 }

// NewDisjointSetGenerator returns a DisjointSetGenerator over scales.
func NewDisjointSetGenerator(scales []int64) *DisjointSetGenerator {
	return &DisjointSetGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *DisjointSetGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *DisjointSetGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	npairs := 2 * n
	parent := map[int64]int64{}
	size := map[int64]int64{}
	find := func(x int64) int64 {
		for {
			px, ok := parent[x]
			if !ok {
				px = x
			}
			if px == x {
				return x
			}
			ppx, ok := parent[px]
			if !ok {
				ppx = px
			}
			parent[x] = ppx
			x = parent[x]
		}
	}
	var toks bytes.Buffer
	toks.Grow(int(npairs) * 14)
	dsWriteInt(&toks, n)
	var lines []string
	comps := n
	for i := int64(0); i < npairs; i++ {
		var u, v int64
		r := rng.Random()
		switch {
		case r < 0.80:
			u = rng.RandInt(1, n)
			v = rng.RandInt(1, n)
		case r < 0.90:
			u = rng.RandInt(1, n)
			v = u
		default:
			u = rng.RandInt(1, n)
			v = (u % n) + 1
		}
		toks.WriteByte(' ')
		dsWriteInt(&toks, u)
		toks.WriteByte(' ')
		dsWriteInt(&toks, v)
		ru := find(u)
		rv := find(v)
		if ru == rv {
			lines = append(lines, strconv.FormatInt(comps, 10)+" CYCLE FORMED!")
		} else {
			su, ok := size[ru]
			if !ok {
				su = 1
			}
			sv, ok := size[rv]
			if !ok {
				sv = 1
			}
			if su < sv {
				ru, rv = rv, ru
				su, sv = sv, su
			}
			parent[rv] = ru
			size[ru] = su + sv
			comps--
			lines = append(lines, strconv.FormatInt(comps, 10))
		}
	}
	return toks.Bytes(), dsEmitLines(lines), FitNFromInt64(npairs), nil
}

// DefaultDirectConnectionsScales holds the calibrated measurement scales.
var DefaultDirectConnectionsScales = []int64{20000, 40000, 80000, 160000}

// DirectConnectionsGenerator emits direct connections: t=3 cases
// of n xs and n pops each.
type DirectConnectionsGenerator struct{ scales []int64 }

// NewDirectConnectionsGenerator returns a DirectConnectionsGenerator
// over scales.
func NewDirectConnectionsGenerator(scales []int64) *DirectConnectionsGenerator {
	return &DirectConnectionsGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *DirectConnectionsGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *DirectConnectionsGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	const t = 3
	var toks bytes.Buffer
	toks.Grow(int(t*n) * 12)
	dsWriteInt(&toks, t)
	var lines []string
	for c := 0; c < t; c++ {
		xs := make([]int64, n)
		for i := range xs {
			xs[i] = rng.RandInt(0, 2*n)
		}
		ps := make([]int64, n)
		for i := range ps {
			ps[i] = rng.RandInt(0, 1000000000)
		}
		toks.WriteByte(' ')
		dsWriteInt(&toks, n)
		for _, v := range xs {
			toks.WriteByte(' ')
			dsWriteInt(&toks, v)
		}
		for _, v := range ps {
			toks.WriteByte(' ')
			dsWriteInt(&toks, v)
		}
		lines = append(lines, strconv.FormatInt(dsDirectCase(xs, ps), 10))
	}
	return toks.Bytes(), dsEmitLines(lines), FitNFromInt64(t * n), nil
}

// dsCrem is Clojure (rem a m) for positive m: truncated remainder.
func dsCrem(a, m int64) int64 {
	if a >= 0 {
		return a % m
	}
	return -((-a) % m)
}

// dsDirectCase computes the wiring cost emulating the solution
// op-for-op: the sweep runs in stable population order and every
// modular reduction applies Clojure rem semantics.
func dsDirectCase(xs, ps []int64) int64 {
	n := len(xs)
	order := make([]int, n)
	for i := range order {
		order[i] = i
	}
	sort.SliceStable(order, func(i, j int) bool { return ps[order[i]] < ps[order[j]] })
	sx := append([]int64(nil), xs...)
	sort.Slice(sx, func(i, j int) bool { return sx[i] < sx[j] })
	bitC := make([]int64, n+2)
	bitS := make([]int64, n+2)
	bitAdd := func(bit []int64, i int, v int64) {
		for i <= n {
			bit[i] = dsCrem(bit[i]+v, dsMod)
			i += i & (-i)
		}
	}
	bitSum := func(bit []int64, i int) int64 {
		var s int64
		for i > 0 {
			s = dsCrem(s+bit[i], dsMod)
			i -= i & (-i)
		}
		return s
	}
	cbsearch := func(v []int64, target int64) int {
		lo, hi := 0, len(v)-1
		for lo <= hi {
			mid := lo + (hi-lo)/2
			got := v[mid]
			if got == target {
				return mid
			} else if got < target {
				lo = mid + 1
			} else {
				hi = mid - 1
			}
		}
		return -1
	}
	ranks := map[int64]int{}
	var ans int64
	for _, idx := range order {
		xi := xs[idx]
		pi := dsCrem(ps[idx], dsMod)
		r, ok := ranks[xi]
		if !ok {
			r = cbsearch(sx, xi) + 1
			ranks[xi] = r
		}
		cl := bitSum(bitC, r)
		sl := bitSum(bitS, r)
		tc := bitSum(bitC, n)
		ts := bitSum(bitS, n)
		cr := dsCrem(tc-cl+dsMod, dsMod)
		sr := dsCrem(ts-sl+dsMod, dsMod)
		xm := dsCrem(xi, dsMod)
		leftPart := dsCrem(cl*xm-sl+dsMod, dsMod)
		rightPart := dsCrem(sr-cr*xm+dsMod, dsMod)
		stepAns := dsCrem(pi*dsCrem(leftPart+rightPart, dsMod), dsMod)
		bitAdd(bitC, r, 1)
		bitAdd(bitS, r, xi)
		ans = dsCrem(ans+stepAns, dsMod)
	}
	return ans
}

// DefaultFibonacciHeapScales holds the calibrated measurement scales.
var DefaultFibonacciHeapScales = []int64{93750, 187500, 375000, 750000}

// FibonacciHeapGenerator emits a Fibonacci heap script: q ops;
// MIN/EXTRACT_MIN produce output. Ids are unique (sequential) and
// live keys are kept distinct so no tie-break can differ between
// the reference and the solution.
type FibonacciHeapGenerator struct{ scales []int64 }

// NewFibonacciHeapGenerator returns a FibonacciHeapGenerator over scales.
func NewFibonacciHeapGenerator(scales []int64) *FibonacciHeapGenerator {
	return &FibonacciHeapGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *FibonacciHeapGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *FibonacciHeapGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var toks bytes.Buffer
	toks.Grow(int(n) * 16)
	dsWriteInt(&toks, n)
	var lines []string
	h := &dsFibHeap{}
	id2key := map[int64]int64{}
	live := map[int64]bool{}
	var allIDs []int64
	var nextID int64 = 1
	seq := 0
	clean := func() {
		for h.Len() > 0 {
			top := (*h)[0]
			if k, ok := id2key[top.id]; ok && k == top.k {
				break
			}
			heap.Pop(h)
		}
	}
	for i := int64(0); i < n; i++ {
		r := rng.Random()
		switch {
		case r < 0.50:
			k := rng.RandInt(-1000000000, 1000000000)
			for live[k] {
				k++
			}
			id := nextID
			nextID++
			toks.WriteString(" INSERT ")
			dsWriteInt(&toks, id)
			toks.WriteByte(' ')
			dsWriteInt(&toks, k)
			id2key[id] = k
			live[k] = true
			heap.Push(h, dsFibItem{k: k, seq: seq, id: id})
			seq++
			allIDs = append(allIDs, id)
		case r < 0.65:
			toks.WriteString(" MIN")
			clean()
			if h.Len() > 0 {
				top := (*h)[0]
				lines = append(lines, strconv.FormatInt(top.id, 10)+
					" "+strconv.FormatInt(top.k, 10))
			} else {
				lines = append(lines, "EMPTY")
			}
		case r < 0.85:
			toks.WriteString(" EXTRACT_MIN")
			clean()
			if h.Len() > 0 {
				top := heap.Pop(h).(dsFibItem)
				delete(id2key, top.id)
				delete(live, top.k)
				lines = append(lines, strconv.FormatInt(top.id, 10)+
					" "+strconv.FormatInt(top.k, 10))
			} else {
				lines = append(lines, "EMPTY")
			}
		case r < 0.98:
			var id int64
			if len(allIDs) > 0 && rng.Random() < 0.80 {
				id = allIDs[rng.Choice(len(allIDs))]
			} else {
				id = nextID + rng.RandInt(1, 1000)
			}
			cur, ok := id2key[id]
			var nw int64
			if ok && rng.Random() < 0.90 {
				nw = cur - rng.RandInt(1, 1000)
				for live[nw] {
					nw--
				}
			} else if ok {
				nw = cur + rng.RandInt(1, 100)
				for live[nw] {
					nw++
				}
			} else {
				nw = rng.RandInt(-1000000000, 1000000000)
			}
			toks.WriteString(" DECREASE_KEY ")
			dsWriteInt(&toks, id)
			toks.WriteByte(' ')
			dsWriteInt(&toks, nw)
			if cur2, ok2 := id2key[id]; ok2 && nw <= cur2 {
				delete(live, cur2)
				id2key[id] = nw
				live[nw] = true
				heap.Push(h, dsFibItem{k: nw, seq: seq, id: id})
				seq++
			}
		default:
			toks.WriteString(" BOGUS")
		}
	}
	return toks.Bytes(), dsEmitLines(lines), FitNFromInt64(n), nil
}

// dsFibItem is one lazy-deletion heap entry ordered by (k, seq, id).
type dsFibItem struct {
	k   int64
	seq int
	id  int64
}

// dsFibHeap is a min-heap of dsFibItem.
type dsFibHeap []dsFibItem

func (h dsFibHeap) Len() int { return len(h) }

func (h dsFibHeap) Less(i, j int) bool {
	if h[i].k != h[j].k {
		return h[i].k < h[j].k
	}
	if h[i].seq != h[j].seq {
		return h[i].seq < h[j].seq
	}
	return h[i].id < h[j].id
}

func (h dsFibHeap) Swap(i, j int) { h[i], h[j] = h[j], h[i] }

// Push appends an entry.
func (h *dsFibHeap) Push(x interface{}) { *h = append(*h, x.(dsFibItem)) }

// Pop removes the last entry.
func (h *dsFibHeap) Pop() interface{} {
	old := *h
	top := old[len(old)-1]
	*h = old[:len(old)-1]
	return top
}

// DefaultRunningMedianScales holds the calibrated measurement scales.
var DefaultRunningMedianScales = []int64{125000, 250000, 500000, 1000000}

// RunningMedianGenerator emits a running median workload: n, then
// n values (negatives included).
type RunningMedianGenerator struct{ scales []int64 }

// NewRunningMedianGenerator returns a RunningMedianGenerator over scales.
func NewRunningMedianGenerator(scales []int64) *RunningMedianGenerator {
	return &RunningMedianGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *RunningMedianGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *RunningMedianGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	vals := make([]int64, n)
	for i := range vals {
		vals[i] = rng.RandInt(-1000000, 1000000)
	}
	var inb bytes.Buffer
	inb.Grow(int(n) * 8)
	dsWriteInt(&inb, n)
	for _, v := range vals {
		inb.WriteByte(' ')
		dsWriteInt(&inb, v)
	}
	lo := &dsInt64Heap{}
	hi := &dsInt64Heap{}
	var lines []string
	for i, v := range vals {
		if lo.Len() == 0 || v <= -(*lo)[0] {
			heap.Push(lo, -v)
		} else {
			heap.Push(hi, v)
		}
		if lo.Len() > hi.Len()+1 {
			heap.Push(hi, -heap.Pop(lo).(int64))
		} else if hi.Len() > lo.Len() {
			heap.Push(lo, -heap.Pop(hi).(int64))
		}
		if (i+1)%2 == 1 {
			lines = append(lines, dsMedianLine(-(*lo)[0], -(*lo)[0], true))
		} else {
			lines = append(lines, dsMedianLine(-(*lo)[0], (*hi)[0], false))
		}
	}
	return inb.Bytes(), dsEmitLines(lines), FitNFromInt64(n), nil
}

// dsMedianLine formats one running-median answer.
func dsMedianLine(a, b int64, odd bool) string {
	if odd {
		return strconv.FormatInt(a, 10) + ".0"
	}
	s := a + b
	q := s / 2
	if s%2 == 0 {
		return strconv.FormatInt(q, 10) + ".0"
	}
	if s < 0 && q == 0 {
		return "-0.5"
	}
	return strconv.FormatInt(q, 10) + ".5"
}

// dsInt64Heap is a min-heap of int64.
type dsInt64Heap []int64

func (h dsInt64Heap) Len() int           { return len(h) }
func (h dsInt64Heap) Less(i, j int) bool { return h[i] < h[j] }
func (h dsInt64Heap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }

// Push appends a value.
func (h *dsInt64Heap) Push(x interface{}) { *h = append(*h, x.(int64)) }

// Pop removes the last value.
func (h *dsInt64Heap) Pop() interface{} {
	old := *h
	top := old[len(old)-1]
	*h = old[:len(old)-1]
	return top
}

// DefaultRangeMinimumQueryScales holds the calibrated measurement scales.
var DefaultRangeMinimumQueryScales = []int64{50000, 100000, 200000, 400000}

// RangeMinimumQueryGenerator emits a range minimum query workload:
// n q, n values, q ops (q=n). Queries use "q x y" with
// 1<=x<=y<=n; updates use "u". All values stay within
// [0, INT32_MAX] so the solution's disjoint sentinel (2147483647)
// can never leak into an answer.
type RangeMinimumQueryGenerator struct{ scales []int64 }

// NewRangeMinimumQueryGenerator returns a RangeMinimumQueryGenerator
// over scales.
func NewRangeMinimumQueryGenerator(scales []int64) *RangeMinimumQueryGenerator {
	return &RangeMinimumQueryGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *RangeMinimumQueryGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *RangeMinimumQueryGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	q := n
	a := make([]int64, n)
	for i := range a {
		a[i] = rng.RandInt(0, 1000000000)
	}
	var toks bytes.Buffer
	toks.Grow(int(n) * 22)
	dsWriteInt(&toks, n)
	toks.WriteByte(' ')
	dsWriteInt(&toks, q)
	for _, v := range a {
		toks.WriteByte(' ')
		dsWriteInt(&toks, v)
	}
	tree := dsNewMinSegTree(a)
	var lines []string
	for i := int64(0); i < q; i++ {
		if rng.Random() < 0.50 {
			x := rng.RandInt(1, n)
			y := rng.RandInt(x, n)
			toks.WriteString(" q ")
			dsWriteInt(&toks, x)
			toks.WriteByte(' ')
			dsWriteInt(&toks, y)
			lines = append(lines, strconv.FormatInt(tree.query(int(x-1), int(y-1)), 10))
		} else {
			x := rng.RandInt(1, n)
			v := rng.RandInt(0, 1000000000)
			toks.WriteString(" u ")
			dsWriteInt(&toks, x)
			toks.WriteByte(' ')
			dsWriteInt(&toks, v)
			tree.update(int(x-1), v)
		}
	}
	return toks.Bytes(), dsEmitLines(lines), FitNFromInt64(n), nil
}

// dsMinSegTree is an iterative min segment tree with point assignment.
type dsMinSegTree struct {
	size int
	inf  int64
	t    []int64
}

// dsNewMinSegTree builds a min segment tree over a.
func dsNewMinSegTree(a []int64) *dsMinSegTree {
	size := 1
	for size < len(a) {
		size *= 2
	}
	const inf = int64(1) << 62
	t := make([]int64, 2*size)
	for i := range t {
		t[i] = inf
	}
	for i, v := range a {
		t[size+i] = v
	}
	for i := size - 1; i >= 1; i-- {
		x, y := t[2*i], t[2*i+1]
		if x < y {
			t[i] = x
		} else {
			t[i] = y
		}
	}
	return &dsMinSegTree{size: size, inf: inf, t: t}
}

// query returns the minimum over the inclusive range [l, r].
func (st *dsMinSegTree) query(l, r int) int64 {
	t := st.t
	res := st.inf
	l += st.size
	r += st.size
	for l <= r {
		if l&1 == 1 {
			if t[l] < res {
				res = t[l]
			}
			l++
		}
		if r&1 == 0 {
			if t[r] < res {
				res = t[r]
			}
			r--
		}
		l /= 2
		r /= 2
	}
	return res
}

// update assigns v at index i.
func (st *dsMinSegTree) update(i int, v int64) {
	t := st.t
	p := st.size + i
	t[p] = v
	p /= 2
	for p != 0 {
		x, y := t[2*p], t[2*p+1]
		if x < y {
			t[p] = x
		} else {
			t[p] = y
		}
		p /= 2
	}
}

// DefaultLazySegmentTreeScales holds the calibrated measurement scales.
var DefaultLazySegmentTreeScales = []int64{20000, 40000, 80000, 160000}

// LazySegmentTreeGenerator emits a lazy segment tree workload:
// n q, values, UPDATE/QUERY ops (q=n).
type LazySegmentTreeGenerator struct{ scales []int64 }

// NewLazySegmentTreeGenerator returns a LazySegmentTreeGenerator over scales.
func NewLazySegmentTreeGenerator(scales []int64) *LazySegmentTreeGenerator {
	return &LazySegmentTreeGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *LazySegmentTreeGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *LazySegmentTreeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	q := n
	a := make([]int64, n)
	for i := range a {
		a[i] = rng.RandInt(-1000000, 1000000)
	}
	var toks bytes.Buffer
	toks.Grow(int(n) * 26)
	dsWriteInt(&toks, n)
	toks.WriteByte(' ')
	dsWriteInt(&toks, q)
	for _, v := range a {
		toks.WriteByte(' ')
		dsWriteInt(&toks, v)
	}
	bit := dsNewRangeAddRangeSum(a)
	var lines []string
	for i := int64(0); i < q; i++ {
		if rng.Random() < 0.50 {
			l := rng.RandInt(1, n)
			r := rng.RandInt(l, n)
			v := rng.RandInt(-1000, 1000)
			toks.WriteString(" UPDATE ")
			dsWriteInt(&toks, l)
			toks.WriteByte(' ')
			dsWriteInt(&toks, r)
			toks.WriteByte(' ')
			dsWriteInt(&toks, v)
			bit.rangeAdd(int(l), int(r), v)
		} else {
			l := rng.RandInt(1, n)
			r := rng.RandInt(l, n)
			toks.WriteString(" QUERY ")
			dsWriteInt(&toks, l)
			toks.WriteByte(' ')
			dsWriteInt(&toks, r)
			lines = append(lines, strconv.FormatInt(bit.rangeSum(int(l), int(r)), 10))
		}
	}
	return toks.Bytes(), dsEmitLines(lines), FitNFromInt64(n), nil
}

// dsRangeAddRangeSum is a two-BIT structure for range add /
// range sum with exact ints.
type dsRangeAddRangeSum struct {
	n      int
	b1, b2 []int64
}

// dsNewRangeAddRangeSum builds the structure over a.
func dsNewRangeAddRangeSum(a []int64) *dsRangeAddRangeSum {
	st := &dsRangeAddRangeSum{
		n:  len(a),
		b1: make([]int64, len(a)+2),
		b2: make([]int64, len(a)+2),
	}
	for i, v := range a {
		st.rangeAdd(i+1, i+1, v)
	}
	return st
}

func (st *dsRangeAddRangeSum) add(bit []int64, i int, v int64) {
	for i <= st.n {
		bit[i] += v
		i += i & (-i)
	}
}

func (st *dsRangeAddRangeSum) sum(bit []int64, i int) int64 {
	var s int64
	for i > 0 {
		s += bit[i]
		i -= i & (-i)
	}
	return s
}

// rangeAdd adds v over the inclusive 1-based range [l, r].
func (st *dsRangeAddRangeSum) rangeAdd(l, r int, v int64) {
	st.add(st.b1, l, v)
	st.add(st.b1, r+1, -v)
	st.add(st.b2, l, v*int64(l-1))
	st.add(st.b2, r+1, -v*int64(r))
}

func (st *dsRangeAddRangeSum) prefix(p int) int64 {
	return st.sum(st.b1, p)*int64(p) - st.sum(st.b2, p)
}

// rangeSum returns the sum over the inclusive 1-based range [l, r].
func (st *dsRangeAddRangeSum) rangeSum(l, r int) int64 {
	return st.prefix(r) - st.prefix(l-1)
}

// DefaultSkipListScales holds the calibrated measurement scales.
var DefaultSkipListScales = []int64{5000, 10000, 20000, 40000}

// SkipListGenerator emits a skip list script:
// INSERT/SEARCH/DELETE/PRINT ops.
type SkipListGenerator struct{ scales []int64 }

// NewSkipListGenerator returns a SkipListGenerator over scales.
func NewSkipListGenerator(scales []int64) *SkipListGenerator {
	return &SkipListGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *SkipListGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *SkipListGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	toks, lines := dsSetScript(n, seed, "SEARCH", "DELETE")
	return toks, dsEmitLines(lines), FitNFromInt64(n), nil
}

// DefaultSplayTreeScales holds the calibrated measurement scales.
var DefaultSplayTreeScales = []int64{12500, 25000, 50000, 100000}

// SplayTreeGenerator emits a splay tree script:
// INSERT/FIND/DELETE/PRINT ops.
type SplayTreeGenerator struct{ scales []int64 }

// NewSplayTreeGenerator returns a SplayTreeGenerator over scales.
func NewSplayTreeGenerator(scales []int64) *SplayTreeGenerator {
	return &SplayTreeGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *SplayTreeGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *SplayTreeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	toks, lines := dsSetScript(n, seed, "FIND", "DELETE")
	return toks, dsEmitLines(lines), FitNFromInt64(n), nil
}

// DefaultTreapScales holds the calibrated measurement scales.
var DefaultTreapScales = []int64{15000, 30000, 60000, 120000}

// TreapGenerator emits a treap script: INSERT/FIND/ERASE/PRINT ops.
type TreapGenerator struct{ scales []int64 }

// NewTreapGenerator returns a TreapGenerator over scales.
func NewTreapGenerator(scales []int64) *TreapGenerator {
	return &TreapGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *TreapGenerator) Scales() []int64 { return g.scales }

// Generate builds the input and expected output for scale n.
func (g *TreapGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	toks, lines := dsSetScript(n, seed, "FIND", "ERASE")
	return toks, dsEmitLines(lines), FitNFromInt64(n), nil
}

// dsSortedSetRef holds set semantics with a sorted parallel string
// list for PRINT.
type dsSortedSetRef struct {
	present map[int64]bool
	vals    []int64
	sval    []string
}

// insert adds v unless already present.
func (s *dsSortedSetRef) insert(v int64) {
	if s.present[v] {
		return
	}
	s.present[v] = true
	idx := sort.Search(len(s.vals), func(j int) bool { return s.vals[j] >= v })
	s.vals = append(s.vals, 0)
	copy(s.vals[idx+1:], s.vals[idx:])
	s.vals[idx] = v
	sv := strconv.FormatInt(v, 10)
	s.sval = append(s.sval, "")
	copy(s.sval[idx+1:], s.sval[idx:])
	s.sval[idx] = sv
}

// discard removes v unless absent.
func (s *dsSortedSetRef) discard(v int64) {
	if !s.present[v] {
		return
	}
	delete(s.present, v)
	idx := sort.Search(len(s.vals), func(j int) bool { return s.vals[j] >= v })
	s.vals = append(s.vals[:idx], s.vals[idx+1:]...)
	s.sval = append(s.sval[:idx], s.sval[idx+1:]...)
}

// line renders the sorted values for PRINT.
func (s *dsSortedSetRef) line() string {
	return strings.Join(s.sval, " ")
}

// dsSetScript is the shared op-script driver for skip list,
// splay and treap.
func dsSetScript(n, seed int64, findOp, eraseOp string) ([]byte, []string) {
	rng := mtrand.New(seed)
	var toks bytes.Buffer
	toks.Grow(int(n) * 14)
	dsWriteInt(&toks, n)
	ref := &dsSortedSetRef{present: map[int64]bool{}}
	var lines []string
	var pool []int64
	for i := int64(0); i < n; i++ {
		r := rng.Random()
		switch {
		case r < 0.50:
			v := rng.RandInt(0, 2*n)
			toks.WriteString(" INSERT ")
			dsWriteInt(&toks, v)
			ref.insert(v)
			pool = append(pool, v)
		case r < 0.80:
			v := rng.RandInt(0, 2*n)
			toks.WriteByte(' ')
			toks.WriteString(findOp)
			toks.WriteByte(' ')
			dsWriteInt(&toks, v)
			if ref.present[v] {
				lines = append(lines, "FOUND")
			} else {
				lines = append(lines, "NOT_FOUND")
			}
		case r < 0.999:
			var v int64
			if len(pool) > 0 && rng.Random() < 0.5 {
				v = pool[rng.Choice(len(pool))]
			} else {
				v = rng.RandInt(0, 2*n)
			}
			toks.WriteByte(' ')
			toks.WriteString(eraseOp)
			toks.WriteByte(' ')
			dsWriteInt(&toks, v)
			ref.discard(v)
		default:
			toks.WriteString(" PRINT")
			lines = append(lines, ref.line())
		}
	}
	return toks.Bytes(), lines
}

// dsBstNode is one plain BST node.
type dsBstNode struct {
	val         int64
	left, right *dsBstNode
}

// dsBstInsert inserts iteratively; duplicates go right.
func dsBstInsert(root *dsBstNode, v int64) *dsBstNode {
	node := &dsBstNode{val: v}
	if root == nil {
		return node
	}
	cur := root
	for {
		if v < cur.val {
			if cur.left == nil {
				cur.left = node
				return root
			}
			cur = cur.left
		} else {
			if cur.right == nil {
				cur.right = node
				return root
			}
			cur = cur.right
		}
	}
}

// dsBstLCA walks to the split point; ok is false when the walk
// falls off the tree.
func dsBstLCA(root *dsBstNode, lo, hi int64) (found int64, ok bool) {
	cur := root
	for cur != nil {
		if hi < cur.val {
			cur = cur.left
		} else if lo > cur.val {
			cur = cur.right
		} else {
			return cur.val, true
		}
	}
	return 0, false
}
