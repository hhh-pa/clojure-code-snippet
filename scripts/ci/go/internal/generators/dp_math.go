package generators

import (
	"bytes"
	"fmt"
	"math/big"
	"sort"
	"strconv"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// dpBlockSize bounds transient memory when building multi-million-token
// inputs, mirroring _BLOCK in tools/generators/dp_math.py.
const dpBlockSize = 100000

const (
	dpCoinChangeCoins   = 100
	dpEditDistanceCases = 2
	dpEditAlphabet      = "abcd"
	dpKnapsackCases     = 2
	dpUnboundedCases    = 2
	dpLcsAlphabet       = 8
	dpMaxSubCases       = 4
	dpMaxSubLO          = -1000000
	dpMaxSubHI          = 1000001
	dpCrtModBits        = 512
	dpMillerRabinCount  = 16
)

// dpLev is the Levenshtein distance of strings a and b, two-row DP.
func dpLev(a, b string) int {
	if len(a) > len(b) {
		a, b = b, a
	}
	m := len(a)
	prev := make([]int, m+1)
	for j := range prev {
		prev[j] = j
	}
	for i := 1; i <= len(b); i++ {
		bi := b[i-1]
		cur := make([]int, m+1)
		cur[0] = i
		for j := 1; j <= m; j++ {
			if a[j-1] == bi {
				cur[j] = prev[j-1]
			} else {
				x := prev[j]
				if y := cur[j-1]; y < x {
					x = y
				}
				if z := prev[j-1]; z < x {
					x = z
				}
				cur[j] = x + 1
			}
		}
		prev = cur
	}
	return prev[m]
}

// dpLcsScores is the row of LCS lengths of x against each prefix of y.
// Exact port of lcs-scores in src/dp/lcs/solution.clj.
func dpLcsScores(x, y []int) []int {
	m := len(y)
	prev := make([]int, m+1)
	for _, xi := range x {
		cur := make([]int, m+1)
		for j := 1; j <= m; j++ {
			if xi == y[j-1] {
				cur[j] = prev[j-1] + 1
			} else if a, b := prev[j], cur[j-1]; a >= b {
				cur[j] = a
			} else {
				cur[j] = b
			}
		}
		prev = cur
	}
	return prev
}

// dpContainsInt reports whether v occurs in s (membership scan).
func dpContainsInt(s []int, v int) bool {
	for _, e := range s {
		if e == v {
			return true
		}
	}
	return false
}

// dpReverseInts returns a reversed copy of s.
func dpReverseInts(s []int) []int {
	out := make([]int, len(s))
	for i, v := range s {
		out[len(s)-1-i] = v
	}
	return out
}

// dpHirschberg is the Hirschberg LCS of x and y. Exact port of
// hirschberg in src/dp/lcs/solution.clj, including tie-breaks:
// first-max-k split (strict >), len-1 base cases via membership scan,
// mid = len(x) // 2. Callers must pass (b, a) (second run first),
// exactly as the solution does.
func dpHirschberg(x, y []int) []int {
	if len(x) == 0 || len(y) == 0 {
		return nil
	}
	if len(x) == 1 {
		if dpContainsInt(y, x[0]) {
			return []int{x[0]}
		}
		return nil
	}
	if len(y) == 1 {
		if dpContainsInt(x, y[0]) {
			return []int{y[0]}
		}
		return nil
	}
	mid := len(x) / 2
	x1, x2 := x[:mid], x[mid:]
	fwd := dpLcsScores(x1, y)
	back := dpLcsScores(dpReverseInts(x2), dpReverseInts(y))
	m := len(y)
	bestVal := -1
	bestK := 0
	for k := 0; k <= m; k++ {
		if v := fwd[k] + back[m-k]; v > bestVal {
			bestVal = v
			bestK = k
		}
	}
	return append(dpHirschberg(x1, y[:bestK]), dpHirschberg(x2, y[bestK:])...)
}

// dpCdivmod is the truncating divmod matching Clojure quot/rem
// (b nonzero). math/big QuoRem is T-division, so it matches directly.
func dpCdivmod(a, b *big.Int) (q, r *big.Int) {
	q = new(big.Int)
	r = new(big.Int)
	q.QuoRem(a, b, r)
	return q, r
}

// dpExtGcd is an exact port of ext-gcd in
// src/math/number_theory/crt.clj.
func dpExtGcd(a, b *big.Int) (g, s, t *big.Int) {
	oldR := new(big.Int).Set(a)
	r := new(big.Int).Set(b)
	oldS := big.NewInt(1)
	s = big.NewInt(0)
	oldT := big.NewInt(0)
	t = big.NewInt(1)
	for r.Sign() != 0 {
		q, _ := dpCdivmod(oldR, r)
		tmp := new(big.Int).Mul(q, r)
		tmp.Sub(oldR, tmp)
		oldR, r = r, tmp
		tmp = new(big.Int).Mul(q, s)
		tmp.Sub(oldS, tmp)
		oldS, s = s, tmp
		tmp = new(big.Int).Mul(q, t)
		tmp.Sub(oldT, tmp)
		oldT, t = t, tmp
	}
	return oldR, oldS, oldT
}

// dpCrtMerge is an exact port of merge-two in
// src/math/number_theory/crt.clj. ok is false when the pair is
// inconsistent.
func dpCrtMerge(r1, m1, r2, m2 *big.Int) (newR, newM *big.Int, ok bool) {
	g, p, _ := dpExtGcd(m1, m2)
	diff := new(big.Int).Sub(r2, r1)
	if _, rem := dpCdivmod(diff, g); rem.Sign() != 0 {
		return nil, nil, false
	}
	m2r, _ := dpCdivmod(m2, g)
	dq, _ := dpCdivmod(diff, g)
	_, factor := dpCdivmod(dq, m2r)
	_, pm := dpCdivmod(p, m2r)
	_, k1 := dpCdivmod(new(big.Int).Mul(pm, factor), m2r)
	if k1.Sign() < 0 {
		k1.Add(k1, m2r)
	}
	newM = new(big.Int).Mul(m1, m2r)
	_, newR = dpCdivmod(new(big.Int).Add(r1, new(big.Int).Mul(m1, k1)), newM)
	if newR.Sign() < 0 {
		newR.Add(newR, newM)
	}
	return newR, newM, true
}

// dpMR64Bases are the deterministic Miller-Rabin bases for n < 2**64.
var dpMR64Bases = []int64{2, 325, 9375, 28178, 450775, 9780504, 1795265022}

// dpTrialPrimes are the trial-division primes used before Miller-Rabin.
var dpTrialPrimes = []int64{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
	41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97}

// dpIsPrime is deterministic primality for n < 2**64
// (trial division + Miller-Rabin).
func dpIsPrime(n *big.Int) bool {
	if n.Cmp(big.NewInt(2)) < 0 {
		return false
	}
	for _, p := range dpTrialPrimes {
		if new(big.Int).Mod(n, big.NewInt(p)).Sign() == 0 {
			return n.Cmp(big.NewInt(p)) == 0
		}
	}
	d := new(big.Int).Sub(n, big.NewInt(1))
	s := 0
	for d.Bit(0) == 0 {
		d.Rsh(d, 1)
		s++
	}
	nm1 := new(big.Int).Sub(n, big.NewInt(1))
	one := big.NewInt(1)
	for _, a := range dpMR64Bases {
		ab := big.NewInt(a)
		if new(big.Int).Mod(ab, n).Sign() == 0 {
			continue
		}
		x := new(big.Int).Exp(ab, d, n)
		if x.Cmp(one) == 0 || x.Cmp(nm1) == 0 {
			continue
		}
		composite := true
		for i := 0; i < s-1; i++ {
			x.Mul(x, x)
			x.Mod(x, n)
			if x.Cmp(nm1) == 0 {
				composite = false
				break
			}
		}
		if composite {
			return false
		}
	}
	return true
}

// dpNextPrime is the smallest prime >= start (start odd, > 97 in use).
func dpNextPrime(start *big.Int) *big.Int {
	cand := new(big.Int).Set(start)
	cand.SetBit(cand, 0, 1)
	two := big.NewInt(2)
	for !dpIsPrime(cand) {
		cand.Add(cand, two)
	}
	return cand
}

// CoinChangeGenerator emits stdin `amount m c1..cm` with stdout one
// integer via print (no newline). Coins are fixed 1..100; n is the
// amount.
type CoinChangeGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *CoinChangeGenerator) Scales() []int64 { return g.scales }

// Generate builds the fixed-coin input for amount n and the 1-D
// coin-loop DP count. It reports an error for a negative amount.
func (g *CoinChangeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		return nil, nil, FitN{}, fmt.Errorf("coin_change: negative amount %d", n)
	}
	amt := int(n)
	row := make([]big.Int, amt+1)
	row[0].SetInt64(1)
	for c := 1; c <= dpCoinChangeCoins; c++ {
		for j := c; j <= amt; j++ {
			row[j].Add(&row[j], &row[j-c])
		}
	}
	var in bytes.Buffer
	in.Grow(64 + dpCoinChangeCoins*4)
	in.WriteString(strconv.FormatInt(n, 10))
	in.WriteByte(' ')
	in.WriteString(strconv.Itoa(dpCoinChangeCoins))
	for c := 1; c <= dpCoinChangeCoins; c++ {
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(c))
	}
	return in.Bytes(), []byte(row[amt].String()), FitNFromInt64(n), nil
}

// EditDistanceGenerator emits stdin `t a1 b1 ...` with stdout t
// Levenshtein lines over a 4-letter alphabet.
type EditDistanceGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *EditDistanceGenerator) Scales() []int64 { return g.scales }

// Generate builds CASES pairs of random length-n strings and their
// Levenshtein distances. It reports an error for a negative n.
func (g *EditDistanceGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		return nil, nil, FitN{}, fmt.Errorf("edit_distance: negative length %d", n)
	}
	ln := int(n)
	rng := mtrand.New(seed)
	var in bytes.Buffer
	in.Grow(2*ln*dpEditDistanceCases + 16)
	in.WriteString(strconv.Itoa(dpEditDistanceCases))
	var exp bytes.Buffer
	for range dpEditDistanceCases {
		a := make([]byte, ln)
		for i := range a {
			a[i] = dpEditAlphabet[rng.RandRangeStop(4)]
		}
		b := make([]byte, ln)
		for i := range b {
			b[i] = dpEditAlphabet[rng.RandRangeStop(4)]
		}
		in.WriteByte(' ')
		in.Write(a)
		in.WriteByte(' ')
		in.Write(b)
		exp.WriteString(strconv.Itoa(dpLev(string(a), string(b))))
		exp.WriteByte('\n')
	}
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// Knapsack01Generator emits stdin `t` then per case `s n (w v)*n`
// (cap FIRST) with 0/1-knapsack optima.
type Knapsack01Generator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *Knapsack01Generator) Scales() []int64 { return g.scales }

// Generate builds CASES cases with cap = items = n, weights in
// [0, cap//2] and values in [-50, 200], solved by the 1-D downward
// DP. It reports an error for a negative n.
func (g *Knapsack01Generator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		return nil, nil, FitN{}, fmt.Errorf("knapsack: negative size %d", n)
	}
	cap_ := int(n)
	nitems := int(n)
	wmax := cap_ / 2
	if wmax < 1 {
		wmax = 1
	}
	rng := mtrand.New(seed)
	var in bytes.Buffer
	in.Grow(nitems*dpKnapsackCases*8 + 32)
	in.WriteString(strconv.Itoa(dpKnapsackCases))
	var exp bytes.Buffer
	for range dpKnapsackCases {
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(cap_))
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(nitems))
		row := make([]int64, cap_+1)
		for range nitems {
			w := rng.RandRange(0, int64(wmax)+1)
			v := rng.RandRange(-50, 201)
			in.WriteByte(' ')
			in.WriteString(strconv.FormatInt(w, 10))
			in.WriteByte(' ')
			in.WriteString(strconv.FormatInt(v, 10))
			wi := int(w)
			for c := cap_; c >= wi; c-- {
				if nv := row[c-wi] + v; nv > row[c] {
					row[c] = nv
				}
			}
		}
		exp.WriteString(strconv.FormatInt(row[cap_], 10))
		exp.WriteByte('\n')
	}
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// UnboundedKnapsackGenerator emits stdin `t` then per case
// `n s w1..wn` (count FIRST, cap second) with value==weight.
type UnboundedKnapsackGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *UnboundedKnapsackGenerator) Scales() []int64 { return g.scales }

// Generate builds CASES cases with items = cap = n and weights in
// [1, 100], solved by the unbounded upward DP. It reports an error
// for a negative n.
func (g *UnboundedKnapsackGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		return nil, nil, FitN{}, fmt.Errorf("unbounded_knapsack: negative size %d", n)
	}
	cap_ := int(n)
	nitems := int(n)
	rng := mtrand.New(seed)
	var in bytes.Buffer
	in.Grow(nitems*dpUnboundedCases*4 + 32)
	in.WriteString(strconv.Itoa(dpUnboundedCases))
	var exp bytes.Buffer
	for range dpUnboundedCases {
		vals := make([]int64, nitems)
		for i := range vals {
			vals[i] = rng.RandRange(1, 101)
		}
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(nitems))
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(cap_))
		for _, w := range vals {
			in.WriteByte(' ')
			in.WriteString(strconv.FormatInt(w, 10))
		}
		row := make([]int64, cap_+1)
		for c := 0; c <= cap_; c++ {
			best := row[c]
			for _, w := range vals {
				if w <= int64(c) {
					if nv := row[c-int(w)] + w; nv > best {
						best = nv
					}
				}
			}
			row[c] = best
		}
		exp.WriteString(strconv.FormatInt(row[cap_], 10))
		exp.WriteByte('\n')
	}
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// LcsGenerator emits stdin `n m a1..an b1..bm` with stdout one println
// of the LCS over an 8-value alphabet.
type LcsGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *LcsGenerator) Scales() []int64 { return g.scales }

// Generate builds two random length-n runs and their Hirschberg LCS
// (called as (hirschberg b a), exactly as the solution does). An
// empty LCS yields a blank line. It reports an error for a negative n.
func (g *LcsGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		return nil, nil, FitN{}, fmt.Errorf("lcs: negative length %d", n)
	}
	ln := int(n)
	rng := mtrand.New(seed)
	a := make([]int, ln)
	for i := range a {
		a[i] = int(rng.RandRangeStop(dpLcsAlphabet))
	}
	b := make([]int, ln)
	for i := range b {
		b[i] = int(rng.RandRangeStop(dpLcsAlphabet))
	}
	ans := dpHirschberg(b, a)
	var in bytes.Buffer
	in.Grow(2*ln*2 + 16)
	in.WriteString(strconv.Itoa(ln))
	in.WriteByte(' ')
	in.WriteString(strconv.Itoa(ln))
	for _, v := range a {
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(v))
	}
	for _, v := range b {
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(v))
	}
	var exp bytes.Buffer
	exp.Grow(len(ans)*2 + 1)
	for i, v := range ans {
		if i > 0 {
			exp.WriteByte(' ')
		}
		exp.WriteString(strconv.Itoa(v))
	}
	exp.WriteByte('\n')
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// LisGenerator emits stdin `n x1..xn` with stdout the length of the
// strictly increasing run.
type LisGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *LisGenerator) Scales() []int64 { return g.scales }

// Generate builds a fixed-seed shuffle of range(n) and its patience
// pile count (bisect_left lower bound). It reports an error for a
// negative n.
func (g *LisGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		return nil, nil, FitN{}, fmt.Errorf("lis: negative length %d", n)
	}
	ln := int(n)
	rng := mtrand.New(seed)
	vals := make([]int, ln)
	for i := range vals {
		vals[i] = i
	}
	rng.Shuffle(ln, func(i, j int) { vals[i], vals[j] = vals[j], vals[i] })
	tails := make([]int, 0)
	for _, v := range vals {
		i := sort.SearchInts(tails, v)
		if i == len(tails) {
			tails = append(tails, v)
		} else {
			tails[i] = v
		}
	}
	var in bytes.Buffer
	in.Grow(ln*4 + 16)
	in.WriteString(strconv.Itoa(ln))
	for start := 0; start < ln; start += dpBlockSize {
		end := start + dpBlockSize
		if end > ln {
			end = ln
		}
		for _, v := range vals[start:end] {
			in.WriteByte(' ')
			in.WriteString(strconv.Itoa(v))
		}
	}
	var exp bytes.Buffer
	exp.WriteString(strconv.Itoa(len(tails)))
	exp.WriteByte('\n')
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// MaxSubarrayGenerator emits stdin `t` then per case `n v1..vn`
// (n>=1) with Kadane pairs.
type MaxSubarrayGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *MaxSubarrayGenerator) Scales() []int64 { return g.scales }

// Generate builds CASES cases totalling n values uniform in
// [-1e6, 1e6] with the one-pass Kadane maximum plus the
// sum-of-positives-else-max. It reports an error for a negative n.
func (g *MaxSubarrayGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	if n < 0 {
		return nil, nil, FitN{}, fmt.Errorf("max_subarray: negative size %d", n)
	}
	ln := int(n)
	rng := mtrand.New(seed)
	base := ln / dpMaxSubCases
	var in bytes.Buffer
	in.Grow(ln*8 + 32)
	in.WriteString(strconv.Itoa(dpMaxSubCases))
	var exp bytes.Buffer
	for ci := range dpMaxSubCases {
		m := base
		if ci == dpMaxSubCases-1 {
			m = ln - base*(dpMaxSubCases-1)
		}
		if m < 1 {
			m = 1
		}
		in.WriteByte(' ')
		in.WriteString(strconv.Itoa(m))
		var cur, best, maxe int64
		first := true
		var pos int64
		for start := 0; start < m; start += dpBlockSize {
			sz := dpBlockSize
			if start+sz > m {
				sz = m - start
			}
			for range sz {
				v := rng.RandRange(dpMaxSubLO, dpMaxSubHI)
				in.WriteByte(' ')
				in.WriteString(strconv.FormatInt(v, 10))
				if first {
					cur, best, maxe = v, v, v
					if v > 0 {
						pos = v
					}
					first = false
				} else {
					nv := cur + v
					if v > nv {
						cur = v
					} else {
						cur = nv
					}
					if cur > best {
						best = cur
					}
					if v > maxe {
						maxe = v
					}
					if v > 0 {
						pos += v
					}
				}
			}
		}
		noncontig := pos
		if pos <= 0 {
			noncontig = maxe
		}
		exp.WriteString(strconv.FormatInt(best, 10))
		exp.WriteByte(' ')
		exp.WriteString(strconv.FormatInt(noncontig, 10))
		exp.WriteByte('\n')
	}
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// CrtGenerator emits stdin `k (r m)*k` with stdout `NO SOLUTION` or
// `<R> <M>` for k congruences with random 512-bit odd moduli.
type CrtGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *CrtGenerator) Scales() []int64 { return g.scales }

// Generate builds k consistent congruences sharing one secret and
// folds them with the generalized-CRT merge. It reports an error
// when k < 1.
func (g *CrtGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	k := int(n)
	if k < 1 {
		return nil, nil, FitN{}, fmt.Errorf("crt: need at least one congruence, got %d", k)
	}
	rng := mtrand.New(seed)
	top := new(big.Int).Lsh(big.NewInt(1), dpCrtModBits-1)
	one := big.NewInt(1)
	mods := make([]*big.Int, k)
	for i := range mods {
		m := rng.RandBits(dpCrtModBits)
		m.Or(m, top)
		m.Or(m, one)
		mods[i] = m
	}
	secretBits := 256
	if k > secretBits {
		secretBits = k
	}
	secret := rng.RandBits(secretBits)
	rs := make([]*big.Int, k)
	for i, m := range mods {
		rs[i] = new(big.Int).Rem(secret, m)
	}
	var in bytes.Buffer
	in.Grow(k*320 + 16)
	in.WriteString(strconv.Itoa(k))
	for i := range mods {
		in.WriteByte(' ')
		in.WriteString(rs[i].String())
		in.WriteByte(' ')
		in.WriteString(mods[i].String())
	}
	r, m := rs[0], mods[0]
	for i := 1; i < k; i++ {
		mergedR, mergedM, ok := dpCrtMerge(r, m, rs[i], mods[i])
		if !ok { // unreachable for consistent residues
			return in.Bytes(), []byte("NO SOLUTION\n"), FitNFromInt64(int64(k)), nil
		}
		r, m = mergedR, mergedM
	}
	var exp bytes.Buffer
	exp.WriteString(r.String())
	exp.WriteByte(' ')
	exp.WriteString(m.String())
	exp.WriteByte('\n')
	return in.Bytes(), exp.Bytes(), FitNFromInt64(int64(k)), nil
}

// MillerRabinPollardGenerator emits stdin `t v1..vt` with stdout per
// value PRIME or COMPOSITE + factors, for construction-known
// semiprimes.
type MillerRabinPollardGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *MillerRabinPollardGenerator) Scales() []int64 { return g.scales }

// Generate builds COUNT semiprimes n=p*q with distinct random
// (bits//2)-bit primes and their known sorted factors; fit_n is
// max N (up to ~2^78, carried as a big integer). It reports an
// error for a bit width below 2 or a failed factor check.
func (g *MillerRabinPollardGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	bits := int(n)
	if bits < 2 {
		return nil, nil, FitN{}, fmt.Errorf("negative shift count")
	}
	half := bits / 2
	rng := mtrand.New(seed)
	top := new(big.Int).Lsh(big.NewInt(1), uint(half-1))
	one := big.NewInt(1)
	two := big.NewInt(2)
	vals := make([]*big.Int, 0, dpMillerRabinCount)
	los := make([]*big.Int, 0, dpMillerRabinCount)
	his := make([]*big.Int, 0, dpMillerRabinCount)
	for range dpMillerRabinCount {
		p := rng.RandBits(half)
		p.Or(p, top)
		p.Or(p, one)
		p = dpNextPrime(p)
		q := rng.RandBits(half)
		q.Or(q, top)
		q.Or(q, one)
		q = dpNextPrime(q)
		if q.Cmp(p) == 0 {
			q = dpNextPrime(new(big.Int).Add(q, two))
		}
		lo, hi := p, q
		if lo.Cmp(hi) > 0 {
			lo, hi = hi, lo
		}
		ordered := new(big.Int).Mul(lo, hi)
		if ordered.Cmp(new(big.Int).Mul(p, q)) != 0 {
			return nil, nil, FitN{}, fmt.Errorf("factor check failed")
		}
		vals = append(vals, new(big.Int).Mul(p, q))
		los = append(los, lo)
		his = append(his, hi)
	}
	var in bytes.Buffer
	in.Grow(dpMillerRabinCount * 32)
	in.WriteString(strconv.Itoa(dpMillerRabinCount))
	fit := new(big.Int)
	for _, v := range vals {
		in.WriteByte(' ')
		in.WriteString(v.String())
		if v.Cmp(fit) > 0 {
			fit.Set(v)
		}
	}
	var exp bytes.Buffer
	for i := range vals {
		exp.WriteString("COMPOSITE ")
		exp.WriteString(los[i].String())
		exp.WriteByte(' ')
		exp.WriteString(his[i].String())
		exp.WriteByte('\n')
	}
	return in.Bytes(), exp.Bytes(), FitNFromBig(fit), nil
}
