// Package generators ports the Python benchmark input generators to Go.
//
// This file ports tools/generators/graph.py: graph-benchmark input
// generators with construction-known answers. Each generator emits
// inputs whose correct output is trivially derivable (a complete
// graph, a line graph, a directed cycle, ...), so the expected bytes
// cost O(1)/O(n) to build even when the subject solution does
// superlinear work. Outputs are byte-identical to the Python originals.
package generators

import (
	"bytes"
	"strconv"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// graphBlockSize mirrors Python _BLOCK: tokens per string-building
// block, keeping peak memory near one input copy.
const graphBlockSize = 100000

// graphGrow pre-sizes b, clamping absurd requests.
func graphGrow(b *bytes.Buffer, want int64) {
	const maxGrow = 1 << 30
	if want <= 0 {
		return
	}
	if want > maxGrow {
		want = maxGrow
	}
	b.Grow(int(want))
}

// graphAppendInt appends the decimal form of v without allocating.
func graphAppendInt(b *bytes.Buffer, v int64) {
	var tmp [32]byte
	b.Write(strconv.AppendInt(tmp[:0], v, 10))
}

// graphInts formats ints as one space-separated block.
func graphInts(v ...int64) string {
	var b bytes.Buffer
	for i, x := range v {
		if i > 0 {
			b.WriteByte(' ')
		}
		graphAppendInt(&b, x)
	}
	return b.String()
}

// graphPacker accumulates space-joined input blocks, mirroring Python
// _pack (" ".join(blocks)).
type graphPacker struct {
	buf   bytes.Buffer
	first bool
}

func newGraphPacker() *graphPacker { return &graphPacker{first: true} }

func (p *graphPacker) bytes() []byte { return p.buf.Bytes() }

// lit appends one preformatted block.
func (p *graphPacker) lit(s string) {
	if !p.first {
		p.buf.WriteByte(' ')
	}
	p.first = false
	p.buf.WriteString(s)
}

// chunk appends one block built token by token.
func (p *graphPacker) chunk(fn func(w *graphToks)) {
	if !p.first {
		p.buf.WriteByte(' ')
	}
	p.first = false
	w := graphToks{buf: &p.buf}
	fn(&w)
}

// graphToks writes space-separated tokens into a single block.
type graphToks struct {
	buf   *bytes.Buffer
	count int
}

// num appends one integer token.
func (w *graphToks) num(v int64) {
	if w.count > 0 {
		w.buf.WriteByte(' ')
	}
	w.count++
	graphAppendInt(w.buf, v)
}

// graphCycleCover mirrors Python _cycle_cover: the largest vertex
// count <= n covered by disjoint size-3 cycles (at least one cycle),
// plus the block count.
func graphCycleCover(n int64) (nverts, nblocks int64) {
	nblocks = n / 3
	if nblocks < 1 {
		nblocks = 1
	}
	return 3 * nblocks, nblocks
}

// graphFlowBipartite mirrors Python _FlowBipartite._generate: shared
// s -> layer -> t unit-cap forcing; max flow = layer size.
func graphFlowBipartite(n int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	t := n + 1
	p := newGraphPacker()
	graphGrow(&p.buf, 24*n)
	p.lit(graphInts(n+2, 2*n, 0, t))
	for start := int64(1); start < n+1; start += graphBlockSize {
		end := min(start+graphBlockSize, n+1)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(0)
				w.num(i)
				w.num(1)
				w.num(i)
				w.num(t)
				w.num(1)
			}
		})
	}
	var exp bytes.Buffer
	graphAppendInt(&exp, n)
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// FloydGenerator emits a complete digraph (weight 1) plus random queries; every answer is 1.
type FloydGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *FloydGenerator) Scales() []int64 { return g.scales }

// Generate builds the complete digraph on n vertices with n random queries.
func (g *FloydGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 2 {
		n = 2
	}
	rng := mtrand.New(seed)
	p := newGraphPacker()
	graphGrow(&p.buf, 24*n*n+32*n)
	p.lit(graphInts(n, n*(n-1)))
	for u := int64(1); u <= n; u++ {
		p.chunk(func(w *graphToks) {
			for v := int64(1); v <= n; v++ {
				if v == u {
					continue
				}
				w.num(u)
				w.num(v)
				w.num(1)
			}
		})
	}
	q := n
	us := make([]int64, q)
	vs := make([]int64, q)
	for i := range us {
		u := rng.RandInt(1, n)
		v := rng.RandInt(1, n-1)
		if v >= u {
			v++
		}
		us[i] = u
		vs[i] = v
	}
	p.lit(graphInts(q))
	p.chunk(func(w *graphToks) {
		for i := range us {
			w.num(us[i])
			w.num(vs[i])
		}
	})
	exp := make([]byte, 0, 2*q)
	for i := int64(0); i < q; i++ {
		exp = append(exp, '1', '\n')
	}
	return p.bytes(), exp, FitNFromInt64(n), nil
}

// JohnsonGenerator emits a bidirected line graph (weight 1); dist(i, j) = |i - j|.
type JohnsonGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *JohnsonGenerator) Scales() []int64 { return g.scales }

// Generate builds the bidirected line graph with the full distance matrix.
func (g *JohnsonGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 32*n)
	p.lit(graphInts(n, 2*(n-1)))
	for start := int64(0); start < n-1; start += graphBlockSize {
		end := min(start+graphBlockSize, n-1)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i + 1)
				w.num(1)
				w.num(i + 1)
				w.num(i)
				w.num(1)
			}
		})
	}
	var exp bytes.Buffer
	graphGrow(&exp, 3*n*n)
	for i := int64(0); i < n; i++ {
		for j := int64(0); j < n; j++ {
			if j > 0 {
				exp.WriteByte(' ')
			}
			d := i - j
			if d < 0 {
				d = -d
			}
			graphAppendInt(&exp, d)
		}
		exp.WriteByte('\n')
	}
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// EvenTreeGenerator emits the 1-2-...-n path; root-1 even-subtree edge count is (n-1)//2.
type EvenTreeGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *EvenTreeGenerator) Scales() []int64 { return g.scales }

// Generate builds the path graph with its even-subtree edge count.
func (g *EvenTreeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 16*n)
	p.lit(graphInts(n, n-1))
	for start := int64(1); start < n; start += graphBlockSize {
		end := min(start+graphBlockSize, n)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i + 1)
			}
		})
	}
	var exp bytes.Buffer
	graphAppendInt(&exp, (n-1)/2)
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// HierholzerGenerator emits a directed n-cycle; the trail 0 1 ... n-1 0 is a CIRCUIT.
type HierholzerGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *HierholzerGenerator) Scales() []int64 { return g.scales }

// Generate builds the directed cycle with its CIRCUIT trail.
func (g *HierholzerGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 16*n)
	p.lit(graphInts(n, n))
	for start := int64(0); start < n; start += graphBlockSize {
		end := min(start+graphBlockSize, n)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num((i + 1) % n)
			}
		})
	}
	var exp bytes.Buffer
	graphGrow(&exp, 8*n)
	exp.WriteString("CIRCUIT\n")
	for i := int64(0); i < n; i++ {
		if i > 0 {
			exp.WriteByte(' ')
		}
		graphAppendInt(&exp, i)
	}
	exp.WriteString(" 0\n")
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// DinicGenerator emits s -> layer -> t unit caps (s != t); max flow = layer size.
type DinicGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *DinicGenerator) Scales() []int64 { return g.scales }

// Generate builds the bipartite flow forcing with max flow n.
func (g *DinicGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	return graphFlowBipartite(n)
}

// PushRelabelGenerator emits the same forcing as Dinic (s != t, nonnegative caps only).
type PushRelabelGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *PushRelabelGenerator) Scales() []int64 { return g.scales }

// Generate builds the bipartite flow forcing with max flow n.
func (g *PushRelabelGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	return graphFlowBipartite(n)
}

// HopcroftKarpGenerator emits diagonal-only edges (i -> i); the matching is unique and forced.
type HopcroftKarpGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *HopcroftKarpGenerator) Scales() []int64 { return g.scales }

// Generate builds the diagonal bipartite graph with its forced matching.
func (g *HopcroftKarpGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 16*n)
	p.lit(graphInts(n, n, n))
	for start := int64(1); start < n+1; start += graphBlockSize {
		end := min(start+graphBlockSize, n+1)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i)
			}
		})
	}
	var exp bytes.Buffer
	graphGrow(&exp, 16*n)
	graphAppendInt(&exp, n)
	exp.WriteByte('\n')
	for start := int64(1); start < n+1; start += graphBlockSize {
		end := min(start+graphBlockSize, n+1)
		for i := start; i < end; i++ {
			graphAppendInt(&exp, i)
			exp.WriteByte(' ')
			graphAppendInt(&exp, i)
			exp.WriteByte('\n')
		}
	}
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// HungarianGenerator emits all-zero costs; optimum is total 0 with the identity assignment.
type HungarianGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *HungarianGenerator) Scales() []int64 { return g.scales }

// Generate builds the all-zero cost matrix with the identity assignment.
func (g *HungarianGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 2*n*n)
	p.lit(graphInts(n, n))
	var rb bytes.Buffer
	graphGrow(&rb, 2*n)
	for i := int64(0); i < n; i++ {
		if i > 0 {
			rb.WriteByte(' ')
		}
		rb.WriteByte('0')
	}
	row := rb.String()
	for i := int64(0); i < n; i++ {
		p.lit(row)
	}
	var exp bytes.Buffer
	graphGrow(&exp, 16*n)
	exp.WriteString("0\n")
	for i := int64(1); i <= n; i++ {
		graphAppendInt(&exp, i)
		exp.WriteByte(' ')
		graphAppendInt(&exp, i)
		exp.WriteByte('\n')
	}
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// BoruvkaGenerator emits a 0-based line graph (weight 1); MST weight is n - 1.
type BoruvkaGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *BoruvkaGenerator) Scales() []int64 { return g.scales }

// Generate builds the 0-based line graph with MST weight n - 1.
func (g *BoruvkaGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 24*n)
	p.lit(graphInts(n, n-1))
	for start := int64(0); start < n-1; start += graphBlockSize {
		end := min(start+graphBlockSize, n-1)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i + 1)
				w.num(1)
			}
		})
	}
	var exp bytes.Buffer
	graphAppendInt(&exp, n-1)
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// KruskalMstGenerator emits a 1-based line graph (weight 1); forest weight is n - 1.
type KruskalMstGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *KruskalMstGenerator) Scales() []int64 { return g.scales }

// Generate builds the 1-based line graph with forest weight n - 1.
func (g *KruskalMstGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 24*n)
	p.lit(graphInts(n, n-1))
	for start := int64(1); start < n; start += graphBlockSize {
		end := min(start+graphBlockSize, n)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i + 1)
				w.num(1)
			}
		})
	}
	var exp bytes.Buffer
	graphAppendInt(&exp, n-1)
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// PrimsMstGenerator emits a 1-based line graph (weight 1), start = 1; MST weight is n - 1.
type PrimsMstGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *PrimsMstGenerator) Scales() []int64 { return g.scales }

// Generate builds the 1-based line graph with start vertex 1.
func (g *PrimsMstGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 24*n)
	p.lit(graphInts(n, n-1))
	for start := int64(1); start < n; start += graphBlockSize {
		end := min(start+graphBlockSize, n)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i + 1)
				w.num(1)
			}
		})
	}
	p.lit("1")
	var exp bytes.Buffer
	graphAppendInt(&exp, n-1)
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// BellmanFordGenerator emits a 0-based directed path (weight 1) from s = 0, edges in REVERSE.
type BellmanFordGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *BellmanFordGenerator) Scales() []int64 { return g.scales }

// Generate builds the reverse-ordered directed path with dist(i) = i.
func (g *BellmanFordGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 24*n)
	p.lit(graphInts(n, n-1, 0))
	for start := n - 2; start >= 0; start -= graphBlockSize {
		lo := max(start-graphBlockSize, -1)
		p.chunk(func(w *graphToks) {
			for i := start; i > lo; i-- {
				w.num(i)
				w.num(i + 1)
				w.num(1)
			}
		})
	}
	var exp bytes.Buffer
	graphGrow(&exp, 8*n)
	for start := int64(0); start < n; start += graphBlockSize {
		end := min(start+graphBlockSize, n)
		for i := start; i < end; i++ {
			graphAppendInt(&exp, i)
			exp.WriteByte('\n')
		}
	}
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// BfsShortReachGenerator emits one 1-based path case from src 1; dists are 6, 12, ... .
type BfsShortReachGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *BfsShortReachGenerator) Scales() []int64 { return g.scales }

// Generate builds the single path case with 6-weighted distances.
func (g *BfsShortReachGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 16*n)
	p.lit(graphInts(1, n, n-1))
	for start := int64(1); start < n; start += graphBlockSize {
		end := min(start+graphBlockSize, n)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i + 1)
			}
		})
	}
	p.lit("1")
	var exp bytes.Buffer
	graphGrow(&exp, 8*n)
	for i := int64(1); i < n; i++ {
		if i > 1 {
			exp.WriteByte(' ')
		}
		graphAppendInt(&exp, 6*i)
	}
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// DijkstraShortReachGenerator emits one 1-based unit-weight path case; dists 1..n-1 plus trail space.
type DijkstraShortReachGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *DijkstraShortReachGenerator) Scales() []int64 { return g.scales }

// Generate builds the single unit-weight path case with dists 1..n-1.
func (g *DijkstraShortReachGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	p := newGraphPacker()
	graphGrow(&p.buf, 24*n)
	p.lit(graphInts(1, n, n-1))
	for start := int64(1); start < n; start += graphBlockSize {
		end := min(start+graphBlockSize, n)
		p.chunk(func(w *graphToks) {
			for i := start; i < end; i++ {
				w.num(i)
				w.num(i + 1)
				w.num(1)
			}
		})
	}
	p.lit("1")
	var exp bytes.Buffer
	graphGrow(&exp, 8*n)
	for i := int64(1); i < n; i++ {
		graphAppendInt(&exp, i)
		exp.WriteByte(' ')
	}
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// KosarajuGenerator emits disjoint directed 3-cycles (1-based); odd-size sum is +nverts.
type KosarajuGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *KosarajuGenerator) Scales() []int64 { return g.scales }

// Generate builds the disjoint 1-based 3-cycles with component sum nverts.
func (g *KosarajuGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nverts, nblocks := graphCycleCover(n)
	p := newGraphPacker()
	graphGrow(&p.buf, 24*nblocks)
	p.lit(graphInts(nverts, 0))
	for start := int64(0); start < nblocks; start += graphBlockSize {
		end := min(start+graphBlockSize, nblocks)
		p.chunk(func(w *graphToks) {
			for b := start; b < end; b++ {
				a := 3*b + 1
				w.num(a)
				w.num(a + 1)
				w.num(a + 1)
				w.num(a + 2)
				w.num(a + 2)
				w.num(a)
			}
		})
	}
	var exp bytes.Buffer
	graphAppendInt(&exp, nverts)
	exp.WriteByte('\n')
	return p.bytes(), exp.Bytes(), FitNFromInt64(nverts), nil
}

// TarjanSccGenerator emits disjoint directed 3-cycles (0-based); components print in order.
type TarjanSccGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *TarjanSccGenerator) Scales() []int64 { return g.scales }

// Generate builds the disjoint 0-based 3-cycles with the ordered component print.
func (g *TarjanSccGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nverts, nblocks := graphCycleCover(n)
	p := newGraphPacker()
	graphGrow(&p.buf, 24*nblocks)
	p.lit(graphInts(nverts, nverts))
	var exp bytes.Buffer
	graphGrow(&exp, 20*nblocks)
	graphAppendInt(&exp, nblocks)
	exp.WriteByte('\n')
	for start := int64(0); start < nblocks; start += graphBlockSize {
		end := min(start+graphBlockSize, nblocks)
		p.chunk(func(w *graphToks) {
			for b := start; b < end; b++ {
				a := 3 * b
				w.num(a)
				w.num(a + 1)
				w.num(a + 1)
				w.num(a + 2)
				w.num(a + 2)
				w.num(a)
			}
		})
		for b := start; b < end; b++ {
			a := 3 * b
			graphAppendInt(&exp, a)
			exp.WriteByte(' ')
			graphAppendInt(&exp, a+1)
			exp.WriteByte(' ')
			graphAppendInt(&exp, a+2)
			exp.WriteByte('\n')
		}
	}
	return p.bytes(), exp.Bytes(), FitNFromInt64(nverts), nil
}

// MakingMakeGenerator emits an empty edge set; MAX-FIRST Kahn order is n-1 .. 0 plus trail space.
type MakingMakeGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *MakingMakeGenerator) Scales() []int64 { return g.scales }

// Generate builds the edgeless graph with the descending Kahn order.
func (g *MakingMakeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		n = 0
	}
	var exp bytes.Buffer
	graphGrow(&exp, 8*n)
	for i := n - 1; i >= 0; i-- {
		graphAppendInt(&exp, i)
		exp.WriteByte(' ')
	}
	exp.WriteByte('\n')
	return []byte(graphInts(n, 0)), exp.Bytes(), FitNFromInt64(n), nil
}
