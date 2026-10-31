// Package generators ports tools/generators/search.py: deterministic slope
// input generators for four search benchmarks. Every Generate method is a
// pure deterministic function of (n, seed) emitting byte-identical output
// to its Python counterpart.
package generators

import (
	"bytes"
	"sort"
	"strconv"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// searchBlock bounds transient memory when joining multi-million-element
// inputs (same pattern as the Python _BLOCK).
const searchBlock = 100000

// writeSearchInts appends "<v0> <v1> ..." for n values to buf in bounded
// blocks, mirroring the Python _join_ints helper exactly (empty input
// appends nothing).
func writeSearchInts(buf *bytes.Buffer, n int, at func(i int) int64) {
	for start := 0; start < n; start += searchBlock {
		if start > 0 {
			buf.WriteByte(' ')
		}
		end := start + searchBlock
		if end > n {
			end = n
		}
		for i := start; i < end; i++ {
			if i > start {
				buf.WriteByte(' ')
			}
			buf.WriteString(strconv.FormatInt(at(i), 10))
		}
	}
}

// PacmanAstarGenerator emits snake-corridor mazes with a forced,
// known-by-construction path.
type PacmanAstarGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *PacmanAstarGenerator) Scales() []int64 { return g.scales }

// Generate returns (input, expected, fitN) for side length n.
//
// Even rows are fully open and odd rows hold a single open cell, so the
// open cells form one unbranched corridor from (0, 0) to the far end.
// n is rounded up to an odd side length of at least 3; fitN is the
// corridor length in cells, the true work scale.
func (g *PacmanAstarGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	_ = seed
	s := int(n)
	if s < 3 {
		s = 3
	}
	if s%2 == 0 {
		s++
	}
	grid := make([][]byte, s)
	for r := 0; r < s; r++ {
		row := make([]byte, s)
		if r%2 == 0 {
			for c := range row {
				row[c] = '-'
			}
		} else {
			for c := range row {
				row[c] = '%'
			}
			if (r/2)%2 == 0 {
				row[s-1] = '-'
			} else {
				row[0] = '-'
			}
		}
		grid[r] = row
	}
	// Walk the corridor from (0, 0); the path is forced.
	type cell struct{ r, c int }
	path := []cell{{0, 0}}
	prev := cell{-1, -1}
	cur := cell{0, 0}
	dirs := [][2]int{{-1, 0}, {0, -1}, {0, 1}, {1, 0}}
	for {
		var nxt *cell
		for _, d := range dirs {
			nr, nc := cur.r+d[0], cur.c+d[1]
			if nr >= 0 && nr < s && nc >= 0 && nc < s &&
				grid[nr][nc] == '-' && (cell{nr, nc} != prev) {
				nxt = &cell{nr, nc}
				break
			}
		}
		if nxt == nil {
			break
		}
		path = append(path, *nxt)
		prev = cur
		cur = *nxt
	}
	gr, gc := path[len(path)-1].r, path[len(path)-1].c
	var dataIn bytes.Buffer
	dataIn.Grow(s*s + 4*s + 32)
	dataIn.WriteString("0 0 " + strconv.Itoa(gr) + " " + strconv.Itoa(gc) +
		" " + strconv.Itoa(s) + " " + strconv.Itoa(s) + "\n")
	for _, row := range grid {
		dataIn.Write(row)
		dataIn.WriteByte('\n')
	}
	var dataOut bytes.Buffer
	dataOut.Grow(len(path) * 8)
	dataOut.WriteString(strconv.Itoa(len(path) - 1))
	dataOut.WriteByte('\n')
	for _, p := range path {
		dataOut.WriteString(strconv.Itoa(p.r))
		dataOut.WriteByte(' ')
		dataOut.WriteString(strconv.Itoa(p.c))
		dataOut.WriteByte('\n')
	}
	return dataIn.Bytes(), dataOut.Bytes(), FitNFromInt64(int64(len(path))), nil
}

// bfsSearchLine returns the space-joined BFS x6 distances for nodes
// 1..n except start+1, mirroring the Python _bfs_line helper.
func bfsSearchLine(n int, edges []int64, m int, start int) string {
	adj := make([][]int, n)
	for i := 0; i < m; i++ {
		u := int(edges[2*i] - 1)
		v := int(edges[2*i+1] - 1)
		adj[u] = append(adj[u], v)
		adj[v] = append(adj[v], u)
	}
	dist := make([]int, n)
	for i := range dist {
		dist[i] = -1
	}
	dist[start] = 0
	queue := []int{start}
	for head := 0; head < len(queue); head++ {
		cur := queue[head]
		for _, nxt := range adj[cur] {
			if dist[nxt] == -1 {
				dist[nxt] = dist[cur] + 6
				queue = append(queue, nxt)
			}
		}
	}
	var buf bytes.Buffer
	buf.Grow(n * 4)
	first := true
	for i := 0; i < n; i++ {
		if i == start {
			continue
		}
		if !first {
			buf.WriteByte(' ')
		}
		first = false
		buf.WriteString(strconv.Itoa(dist[i]))
	}
	return buf.String()
}

// BfsShortestReachGenerator emits two queries per input: a line graph
// plus a denser random graph.
type BfsShortestReachGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *BfsShortestReachGenerator) Scales() []int64 { return g.scales }

// Generate returns (input, expected, fitN) for node count n.
//
// Query 1 is the Hamiltonian path 1-2-...-n with a random start; query 2
// adds random extra edges up to 4n total (capped at the complete-graph
// edge count) with a random start. fitN is n.
func (g *BfsShortestReachGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	if nn < 2 {
		nn = 2
	}
	rng := mtrand.New(seed)
	e1 := make([]int64, 2*(nn-1))
	for i := 1; i < nn; i++ {
		e1[2*(i-1)] = int64(i)
		e1[2*(i-1)+1] = int64(i + 1)
	}
	s1 := rng.RandRange(1, int64(nn)+1)
	m2 := 4 * nn
	if cap := nn * (nn - 1) / 2; m2 > cap {
		m2 = cap
	}
	e2 := make([]int64, len(e1), 2*m2)
	copy(e2, e1)
	seen := make(map[int64]struct{}, m2)
	for i := 1; i < nn; i++ {
		seen[int64(i*nn+i+1)] = struct{}{}
	}
	for len(e2)/2 < m2 {
		u := rng.RandRange(1, int64(nn)+1)
		v := rng.RandRange(1, int64(nn)+1)
		if u == v {
			continue
		}
		if u > v {
			u, v = v, u
		}
		key := u*int64(nn) + v
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		e2 = append(e2, u, v)
	}
	s2 := rng.RandRange(1, int64(nn)+1)
	var buf bytes.Buffer
	buf.Grow(2*len(e1) + 2*len(e2) + 64)
	buf.WriteString("2\n")
	buf.WriteString(strconv.Itoa(nn) + " " + strconv.Itoa(nn-1) + "\n")
	writeSearchInts(&buf, len(e1), func(i int) int64 { return e1[i] })
	buf.WriteString("\n" + strconv.FormatInt(s1, 10) + "\n")
	buf.WriteString(strconv.Itoa(nn) + " " + strconv.Itoa(len(e2)/2) + "\n")
	writeSearchInts(&buf, len(e2), func(i int) int64 { return e2[i] })
	buf.WriteString("\n" + strconv.FormatInt(s2, 10) + "\n")
	dataOut := bfsSearchLine(nn, e1, nn-1, int(s1-1)) + "\n" +
		bfsSearchLine(nn, e2, len(e2)/2, int(s2-1)) + "\n"
	return buf.Bytes(), []byte(dataOut), FitNFromInt64(int64(nn)), nil
}

// floodSearchBest returns the largest 8-connected region of 1s, or -1
// when no land exists, mirroring the Python _flood_best helper.
func floodSearchBest(cells []byte, n, m int) int {
	nm := n * m
	seen := make([]byte, nm)
	best := -1
	for idx := 0; idx < nm; idx++ {
		if cells[idx] != 1 || seen[idx] != 0 {
			continue
		}
		size := 0
		stack := []int{idx}
		seen[idx] = 1
		for len(stack) > 0 {
			cur := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			size++
			cr := cur / m
			cc := cur % m
			for _, dr := range [3]int{-1, 0, 1} {
				nr := cr + dr
				if nr < 0 || nr >= n {
					continue
				}
				for _, dc := range [3]int{-1, 0, 1} {
					if dr == 0 && dc == 0 {
						continue
					}
					nc := cc + dc
					if nc < 0 || nc >= m {
						continue
					}
					ni := nr*m + nc
					if cells[ni] == 1 && seen[ni] == 0 {
						seen[ni] = 1
						stack = append(stack, ni)
					}
				}
			}
		}
		if size > best {
			best = size
		}
	}
	return best
}

// DfsRegionGenerator emits square grids: all-land blocks and
// random-density grids.
type DfsRegionGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *DfsRegionGenerator) Scales() []int64 { return g.scales }

// Generate returns (input, expected, fitN) for side length n.
//
// Even seeds yield all-land grids (answer n*n by construction); odd
// seeds yield density-0.5 grids answered by a flood-fill port. fitN is
// the cell count n*n.
func (g *DfsRegionGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	s := int(n)
	if s < 2 {
		s = 2
	}
	rng := mtrand.New(seed)
	var cells []byte
	var ans int
	if seed%2 == 0 {
		cells = make([]byte, s*s)
		for i := range cells {
			cells[i] = 1
		}
		ans = s * s
	} else {
		cells = make([]byte, s*s)
		for i := range cells {
			if rng.Random() < 0.5 {
				cells[i] = 1
			}
		}
		ans = floodSearchBest(cells, s, s)
	}
	var buf bytes.Buffer
	buf.Grow(2*s*s + 32)
	buf.WriteString(strconv.Itoa(s) + " " + strconv.Itoa(s) + "\n")
	writeSearchInts(&buf, len(cells), func(i int) int64 { return int64(cells[i]) })
	buf.WriteByte('\n')
	dataOut := strconv.Itoa(ans) + "\n"
	return buf.Bytes(), []byte(dataOut), FitNFromInt64(int64(s * s)), nil
}

// countSearchPairs ports the driver scan over sorted values verbatim:
// i=0, j=1; both advance on a match.
func countSearchPairs(vals []int64, k int64) int64 {
	s := make([]int64, len(vals))
	copy(s, vals)
	sort.Slice(s, func(i, j int) bool { return s[i] < s[j] })
	n := int64(len(s))
	var i, j, ans int64
	j = 1
	for i < n && j < n {
		if j <= i {
			j = i + 1
			continue
		}
		diff := s[j] - s[i]
		if diff == k {
			i++
			j++
			ans++
		} else if diff < k {
			j++
		} else {
			i++
		}
	}
	return ans
}

// PairsGenerator emits random values with heavy duplicates and
// small/zero/negative k.
type PairsGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *PairsGenerator) Scales() []int64 { return g.scales }

// Generate returns (input, expected, fitN) for value count n.
//
// Values are uniform over roughly n/8 distinct values so duplicates are
// common; k spans -2..6 across (seed, n), covering k=0 and negative-k
// semantics. fitN is n.
func (g *PairsGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	if nn < 0 {
		nn = 0
	}
	rng := mtrand.New(seed)
	k := (rng.RandRange(0, 9)+int64(nn))%9 - 2
	hi := nn/8 + 1
	if hi < 2 {
		hi = 2
	}
	vals := make([]int64, nn)
	for i := range vals {
		vals[i] = rng.RandRange(0, int64(hi))
	}
	var buf bytes.Buffer
	buf.Grow(2*nn + 32)
	buf.WriteString(strconv.Itoa(nn) + " " + strconv.FormatInt(k, 10) + "\n")
	writeSearchInts(&buf, len(vals), func(i int) int64 { return vals[i] })
	buf.WriteByte('\n')
	dataOut := strconv.FormatInt(countSearchPairs(vals, k), 10) + "\n"
	return buf.Bytes(), []byte(dataOut), FitNFromInt64(int64(nn)), nil
}
