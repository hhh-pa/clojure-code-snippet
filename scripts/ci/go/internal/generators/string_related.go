// Slope input generators for the string_related benchmarks (T20),
// ported from tools/generators/string_related.py. Each generator
// produces (input_bytes, expected_bytes, fit_n) for one benchmark's
// stdin contract at requested scales. Every forcing input is
// construction-known: the expected output is derived from the
// construction, never by porting the solver.
package generators

import (
	"bytes"
	"strconv"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// stringRelatedBlock bounds transient copies when building
// multi-megabyte uniform runs (mirrors Python _BLOCK).
const stringRelatedBlock = 1000000

// stringRelatedRepeat appends byte c n times to buf in bounded chunks.
func stringRelatedRepeat(buf *bytes.Buffer, c byte, n int64) {
	if n <= 0 {
		return
	}
	size := n
	if size > stringRelatedBlock {
		size = stringRelatedBlock
	}
	chunk := bytes.Repeat([]byte{c}, int(size))
	for n > 0 {
		take := int64(len(chunk))
		if take > n {
			take = n
		}
		buf.Write(chunk[:take])
		n -= take
	}
}

// DnaHealthStrandLen is the fixed strand length of DnaHealthGenerator.
const DnaHealthStrandLen = 2000

// DnaHealthGenerator builds Aho-Corasick DNA health inputs: genes over
// {a}, strands of b's only (zero matches, stdout `0 0`).
type DnaHealthGenerator struct {
	scales []int64
}

// NewDnaHealthGenerator returns a DnaHealthGenerator with the given scales.
func NewDnaHealthGenerator(scales []int64) *DnaHealthGenerator {
	return &DnaHealthGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *DnaHealthGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *DnaHealthGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	rng := mtrand.New(seed)
	genes := make([]string, n)
	for i := range genes {
		genes[i] = string(bytes.Repeat([]byte{'a'}, 5+(i%16)))
	}
	healths := make([]string, n)
	for i := range healths {
		healths[i] = strconv.FormatInt(rng.RandRange(-1000000, 1000000), 10)
	}
	var buf bytes.Buffer
	buf.Grow(int(n)*(DnaHealthStrandLen+32) + 32)
	buf.WriteString(strconv.FormatInt(n, 10))
	for _, gene := range genes {
		buf.WriteByte(' ')
		buf.WriteString(gene)
	}
	for _, h := range healths {
		buf.WriteByte(' ')
		buf.WriteString(h)
	}
	buf.WriteByte(' ')
	buf.WriteString(strconv.FormatInt(n, 10))
	tripHead := "0 " + strconv.FormatInt(n-1, 10) + " "
	for i := int64(0); i < n; i++ {
		buf.WriteByte(' ')
		buf.WriteString(tripHead)
		stringRelatedRepeat(&buf, 'b', DnaHealthStrandLen)
	}
	return buf.Bytes(), []byte("0 0\n"), FitNFromInt64(n), nil
}

// SherlockAnagramsGenerator builds q=n all-distinct-char strings (every
// count is 0).
type SherlockAnagramsGenerator struct {
	scales []int64
}

// NewSherlockAnagramsGenerator returns a SherlockAnagramsGenerator with
// the given scales.
func NewSherlockAnagramsGenerator(scales []int64) *SherlockAnagramsGenerator {
	return &SherlockAnagramsGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *SherlockAnagramsGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *SherlockAnagramsGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	rng := mtrand.New(seed)
	alpha := []byte("abcdefghijklmnopqrstuvwxyz")
	var buf bytes.Buffer
	buf.Grow(int(n)*27 + 16)
	buf.WriteString(strconv.FormatInt(n, 10))
	for i := int64(0); i < n; i++ {
		rng.Shuffle(len(alpha), func(a, b int) { alpha[a], alpha[b] = alpha[b], alpha[a] })
		buf.WriteByte(' ')
		buf.Write(alpha)
	}
	var out bytes.Buffer
	out.Grow(int(n) * 2)
	for i := int64(0); i < n; i++ {
		out.WriteString("0\n")
	}
	return buf.Bytes(), out.Bytes(), FitNFromInt64(n), nil
}

// Boyer-Moore substring search constants: pair count and pattern length.
const (
	BoyerMoorePairs      = 2
	BoyerMoorePatternLen = 1000
)

// BoyerMooreSubstringGenerator builds text `a`*n with pattern
// `b`+`a`*999 (every answer NO).
type BoyerMooreSubstringGenerator struct {
	scales []int64
}

// NewBoyerMooreSubstringGenerator returns a BoyerMooreSubstringGenerator
// with the given scales.
func NewBoyerMooreSubstringGenerator(scales []int64) *BoyerMooreSubstringGenerator {
	return &BoyerMooreSubstringGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *BoyerMooreSubstringGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *BoyerMooreSubstringGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	pat := append([]byte{'b'}, bytes.Repeat([]byte{'a'}, BoyerMoorePatternLen-1)...)
	var buf bytes.Buffer
	buf.Grow(int(n)*BoyerMoorePairs + BoyerMoorePairs*(BoyerMoorePatternLen+2) + 8)
	buf.WriteString(strconv.FormatInt(BoyerMoorePairs, 10))
	for p := 0; p < BoyerMoorePairs; p++ {
		buf.WriteByte(' ')
		stringRelatedRepeat(&buf, 'a', n)
		buf.WriteByte(' ')
		buf.Write(pat)
	}
	return buf.Bytes(), bytes.Repeat([]byte("NO\n"), BoyerMoorePairs), FitNFromInt64(n), nil
}

// KMP substring search constants: pair count and pattern length.
const (
	KmpPairs      = 2
	KmpPatternLen = 1000
)

// KmpSubstringGenerator builds text `a`*n with pattern `a`*999+`b`
// (every answer NO).
type KmpSubstringGenerator struct {
	scales []int64
}

// NewKmpSubstringGenerator returns a KmpSubstringGenerator with the given
// scales.
func NewKmpSubstringGenerator(scales []int64) *KmpSubstringGenerator {
	return &KmpSubstringGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *KmpSubstringGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *KmpSubstringGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	pat := append(bytes.Repeat([]byte{'a'}, KmpPatternLen-1), 'b')
	var buf bytes.Buffer
	buf.Grow(int(n)*KmpPairs + KmpPairs*(KmpPatternLen+2) + 8)
	buf.WriteString(strconv.FormatInt(KmpPairs, 10))
	for p := 0; p < KmpPairs; p++ {
		buf.WriteByte(' ')
		stringRelatedRepeat(&buf, 'a', n)
		buf.WriteByte(' ')
		buf.Write(pat)
	}
	return buf.Bytes(), bytes.Repeat([]byte("NO\n"), KmpPairs), FitNFromInt64(n), nil
}

// Z-function substring search constants: pair count and pattern length.
const (
	ZFunctionPairs      = 4
	ZFunctionPatternLen = 1000
)

// ZFunctionSubstringGenerator builds the same forcing shape as the KMP
// one (every answer NO).
type ZFunctionSubstringGenerator struct {
	scales []int64
}

// NewZFunctionSubstringGenerator returns a ZFunctionSubstringGenerator
// with the given scales.
func NewZFunctionSubstringGenerator(scales []int64) *ZFunctionSubstringGenerator {
	return &ZFunctionSubstringGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *ZFunctionSubstringGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *ZFunctionSubstringGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	pat := append(bytes.Repeat([]byte{'a'}, ZFunctionPatternLen-1), 'b')
	var buf bytes.Buffer
	buf.Grow(int(n)*ZFunctionPairs + ZFunctionPairs*(ZFunctionPatternLen+2) + 8)
	buf.WriteString(strconv.FormatInt(ZFunctionPairs, 10))
	for p := 0; p < ZFunctionPairs; p++ {
		buf.WriteByte(' ')
		stringRelatedRepeat(&buf, 'a', n)
		buf.WriteByte(' ')
		buf.Write(pat)
	}
	return buf.Bytes(), bytes.Repeat([]byte("NO\n"), ZFunctionPairs), FitNFromInt64(n), nil
}

// CommonChildGenerator builds line-based LCS inputs `a`*n vs `b`*n
// (length 0).
type CommonChildGenerator struct {
	scales []int64
}

// NewCommonChildGenerator returns a CommonChildGenerator with the given
// scales.
func NewCommonChildGenerator(scales []int64) *CommonChildGenerator {
	return &CommonChildGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *CommonChildGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *CommonChildGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	var buf bytes.Buffer
	buf.Grow(int(n)*2 + 2)
	stringRelatedRepeat(&buf, 'a', n)
	buf.WriteByte('\n')
	stringRelatedRepeat(&buf, 'b', n)
	buf.WriteByte('\n')
	return buf.Bytes(), []byte("0\n"), FitNFromInt64(n), nil
}

// HirschbergGenerator builds `a`*n vs `b`*n alignment inputs (score -n,
// aligned lines equal the inputs).
type HirschbergGenerator struct {
	scales []int64
}

// NewHirschbergGenerator returns a HirschbergGenerator with the given
// scales.
func NewHirschbergGenerator(scales []int64) *HirschbergGenerator {
	return &HirschbergGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *HirschbergGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *HirschbergGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	var in, out bytes.Buffer
	in.Grow(int(n)*2 + 1)
	stringRelatedRepeat(&in, 'a', n)
	in.WriteByte(' ')
	stringRelatedRepeat(&in, 'b', n)
	out.Grow(int(n)*2 + 32)
	out.WriteString(strconv.FormatInt(-n, 10))
	out.WriteByte('\n')
	stringRelatedRepeat(&out, 'a', n)
	out.WriteByte('\n')
	stringRelatedRepeat(&out, 'b', n)
	out.WriteByte('\n')
	return in.Bytes(), out.Bytes(), FitNFromInt64(n), nil
}

// SequenceAlignmentGenerator builds global alignment inputs `a`*n vs
// `b`*n (score -n, originals reprinted).
type SequenceAlignmentGenerator struct {
	scales []int64
}

// NewSequenceAlignmentGenerator returns a SequenceAlignmentGenerator with
// the given scales.
func NewSequenceAlignmentGenerator(scales []int64) *SequenceAlignmentGenerator {
	return &SequenceAlignmentGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *SequenceAlignmentGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *SequenceAlignmentGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	var in, out bytes.Buffer
	in.Grow(int(n)*2 + 8)
	in.WriteString("GLOBAL ")
	stringRelatedRepeat(&in, 'a', n)
	in.WriteByte(' ')
	stringRelatedRepeat(&in, 'b', n)
	out.Grow(int(n)*2 + 32)
	out.WriteString(strconv.FormatInt(-n, 10))
	out.WriteByte('\n')
	stringRelatedRepeat(&out, 'a', n)
	out.WriteByte('\n')
	stringRelatedRepeat(&out, 'b', n)
	out.WriteByte('\n')
	return in.Bytes(), out.Bytes(), FitNFromInt64(n), nil
}

// ManacherGenerator builds the `a`*n palindrome input (max length n,
// count n*(n+1)/2).
type ManacherGenerator struct {
	scales []int64
}

// NewManacherGenerator returns a ManacherGenerator with the given scales.
func NewManacherGenerator(scales []int64) *ManacherGenerator {
	return &ManacherGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *ManacherGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *ManacherGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	var in, out bytes.Buffer
	in.Grow(int(n))
	stringRelatedRepeat(&in, 'a', n)
	out.Grow(int(n) + 64)
	out.WriteString(strconv.FormatInt(n, 10))
	out.WriteByte('\n')
	stringRelatedRepeat(&out, 'a', n)
	out.WriteByte('\n')
	out.WriteString(strconv.FormatInt(n*(n+1)/2, 10))
	out.WriteByte('\n')
	return in.Bytes(), out.Bytes(), FitNFromInt64(n), nil
}

// SuffixArrayFunctionGenerator builds the `a`*n string-function input
// (value (n+1)^2//4).
type SuffixArrayFunctionGenerator struct {
	scales []int64
}

// NewSuffixArrayFunctionGenerator returns a SuffixArrayFunctionGenerator
// with the given scales.
func NewSuffixArrayFunctionGenerator(scales []int64) *SuffixArrayFunctionGenerator {
	return &SuffixArrayFunctionGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *SuffixArrayFunctionGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *SuffixArrayFunctionGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	var in bytes.Buffer
	in.Grow(int(n))
	stringRelatedRepeat(&in, 'a', n)
	out := strconv.FormatInt((n+1)*(n+1)/4, 10) + "\n"
	return in.Bytes(), []byte(out), FitNFromInt64(n), nil
}

// SuffixTreeFunctionGenerator builds the `a`*n string-function input
// (value (n+1)^2//4).
type SuffixTreeFunctionGenerator struct {
	scales []int64
}

// NewSuffixTreeFunctionGenerator returns a SuffixTreeFunctionGenerator
// with the given scales.
func NewSuffixTreeFunctionGenerator(scales []int64) *SuffixTreeFunctionGenerator {
	return &SuffixTreeFunctionGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *SuffixTreeFunctionGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *SuffixTreeFunctionGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	var in bytes.Buffer
	in.Grow(int(n))
	stringRelatedRepeat(&in, 'a', n)
	out := strconv.FormatInt((n+1)*(n+1)/4, 10) + "\n"
	return in.Bytes(), []byte(out), FitNFromInt64(n), nil
}

// SuffixAutomatonGenerator builds suffix automaton queries over `a`*n
// (n+1 states, n transitions, n distinct substrings).
type SuffixAutomatonGenerator struct {
	scales []int64
}

// NewSuffixAutomatonGenerator returns a SuffixAutomatonGenerator with the
// given scales.
func NewSuffixAutomatonGenerator(scales []int64) *SuffixAutomatonGenerator {
	return &SuffixAutomatonGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *SuffixAutomatonGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *SuffixAutomatonGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	q1 := n / 4
	if q1 < 1 {
		q1 = 1
	}
	var in bytes.Buffer
	in.Grow(int(n)*2 + int(q1) + 16)
	stringRelatedRepeat(&in, 'a', n)
	in.WriteString(" 4 ")
	stringRelatedRepeat(&in, 'a', q1)
	in.WriteByte(' ')
	stringRelatedRepeat(&in, 'a', n)
	in.WriteString(" b ab")
	var out bytes.Buffer
	out.WriteString("States: ")
	out.WriteString(strconv.FormatInt(n+1, 10))
	out.WriteString("\nTransitions: ")
	out.WriteString(strconv.FormatInt(n, 10))
	out.WriteString("\nDistinct substrings: ")
	out.WriteString(strconv.FormatInt(n, 10))
	out.WriteString("\nYES\nYES\nNO\nNO\n")
	return in.Bytes(), out.Bytes(), FitNFromInt64(n), nil
}

// NoPrefixSetGenerator builds n fixed-width distinct words (GOOD SET).
type NoPrefixSetGenerator struct {
	scales []int64
}

// NewNoPrefixSetGenerator returns a NoPrefixSetGenerator with the given
// scales.
func NewNoPrefixSetGenerator(scales []int64) *NoPrefixSetGenerator {
	return &NoPrefixSetGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *NoPrefixSetGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *NoPrefixSetGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	rng := mtrand.New(seed)
	width := len(strconv.FormatInt(n-1, 10))
	if width < 6 {
		width = 6
	}
	words := make([]string, n)
	for i := range words {
		num := strconv.Itoa(i)
		w := make([]byte, 1+width)
		w[0] = 'w'
		for k := 1; k <= width; k++ {
			w[k] = '0'
		}
		copy(w[1+width-len(num):], num)
		words[i] = string(w)
	}
	rng.Shuffle(len(words), func(a, b int) { words[a], words[b] = words[b], words[a] })
	var buf bytes.Buffer
	buf.Grow(int(n)*(width+2) + 16)
	buf.WriteString(strconv.FormatInt(n, 10))
	for _, w := range words {
		buf.WriteByte(' ')
		buf.WriteString(w)
	}
	return buf.Bytes(), []byte("GOOD SET\n"), FitNFromInt64(n), nil
}

// UkkonenGenerator builds Ukkonen suffix tree queries over `a`*n (Nodes:
// 2, n distinct substrings).
type UkkonenGenerator struct {
	scales []int64
}

// NewUkkonenGenerator returns an UkkonenGenerator with the given scales.
func NewUkkonenGenerator(scales []int64) *UkkonenGenerator {
	return &UkkonenGenerator{scales: append([]int64(nil), scales...)}
}

// Scales returns the measurement scales.
func (g *UkkonenGenerator) Scales() []int64 { return g.scales }

// Generate returns (input_bytes, expected_bytes, fit_n) for size n.
func (g *UkkonenGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 1 {
		n = 1
	}
	q1 := n / 4
	if q1 < 1 {
		q1 = 1
	}
	var in bytes.Buffer
	in.Grow(int(n)*2 + int(q1) + 16)
	stringRelatedRepeat(&in, 'a', n)
	in.WriteString(" 4 ")
	stringRelatedRepeat(&in, 'a', q1)
	in.WriteByte(' ')
	stringRelatedRepeat(&in, 'a', n)
	in.WriteString(" b ab")
	var out bytes.Buffer
	out.WriteString("Nodes: 2\nDistinct substrings: ")
	out.WriteString(strconv.FormatInt(n, 10))
	out.WriteString("\nYES\nYES\nNO\nNO\n")
	return in.Bytes(), out.Bytes(), FitNFromInt64(n), nil
}
