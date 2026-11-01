package generators

import (
	"bytes"
	"fmt"
	"strconv"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// sortBlock bounds transient memory when joining multi-million-element
// inputs (same pattern as the Python _BLOCK).
const sortBlock = 100000

// writeSortShuffled appends the shuffled values of range(n) after the
// leading n, mirroring the Python input-building loop exactly.
func writeSortShuffled(buf *bytes.Buffer, vals []int64) {
	n := len(vals)
	for start := 0; start < n; start += sortBlock {
		buf.WriteByte(' ')
		end := start + sortBlock
		if end > n {
			end = n
		}
		for i := start; i < end; i++ {
			if i > start {
				buf.WriteByte(' ')
			}
			buf.WriteString(strconv.FormatInt(vals[i], 10))
		}
	}
}

// SortArrayGenerator generates shuffled-range inputs at fixed scales.
type SortArrayGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *SortArrayGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
//
// Input is a fixed-seed shuffle of range(n): average-case for both
// insertion sort (Theta(n^2)) and quicksort (Theta(n log n)).
// Expected output is the sorted identity [0,1,...,n-1], matching the
// driver's exact print format (comma-separated, no spaces, no trailing
// newline).
func (g *SortArrayGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	rng := mtrand.New(seed)
	var vals []int64
	if nn > 0 {
		vals = make([]int64, nn)
		for i := range vals {
			vals[i] = int64(i)
		}
		rng.Shuffle(nn, func(i, j int) { vals[i], vals[j] = vals[j], vals[i] })
	}
	var buf bytes.Buffer
	if nn > 0 {
		buf.Grow(8 * nn)
	}
	buf.WriteString(strconv.FormatInt(n, 10))
	writeSortShuffled(&buf, vals)
	var out bytes.Buffer
	if nn > 0 {
		out.Grow(8 * nn)
	}
	out.WriteByte('[')
	for start := 0; start < nn; start += sortBlock {
		if start > 0 {
			out.WriteByte(',')
		}
		end := start + sortBlock
		if end > nn {
			end = nn
		}
		for v := start; v < end; v++ {
			if v > start {
				out.WriteByte(',')
			}
			out.WriteString(strconv.Itoa(v))
		}
	}
	out.WriteByte(']')
	return buf.Bytes(), out.Bytes(), FitNFromInt64(n), nil
}

// QuickselectGenerator generates shuffled-range inputs for the
// quickselect driver.
type QuickselectGenerator struct{ scales []int64 }

// Scales returns the measurement scales.
func (g *QuickselectGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
//
// Input is a fixed-seed shuffle of range(n): average-case linear for
// randomized quickselect. Expected output is the rank-quot(n,2) order
// statistic (n//2 for a permutation of range(n)) plus the driver's
// trailing newline. The driver strictly requires n >= 1 with exactly
// n values.
func (g *QuickselectGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	nn := int(n)
	if nn < 1 {
		return nil, nil, FitN{}, fmt.Errorf("quickselect needs n >= 1, got %d", nn)
	}
	rng := mtrand.New(seed)
	vals := make([]int64, nn)
	for i := range vals {
		vals[i] = int64(i)
	}
	rng.Shuffle(nn, func(i, j int) { vals[i], vals[j] = vals[j], vals[i] })
	var buf bytes.Buffer
	buf.Grow(8 * nn)
	buf.WriteString(strconv.FormatInt(n, 10))
	writeSortShuffled(&buf, vals)
	dataOut := strconv.FormatInt(n/2, 10) + "\n"
	return buf.Bytes(), []byte(dataOut), FitNFromInt64(n), nil
}
