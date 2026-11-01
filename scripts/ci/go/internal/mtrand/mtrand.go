// Package mtrand replicates CPython's random.Random bit-for-bit so the
// Go slope generators emit byte-identical inputs to the Python
// generators they replace. Only the API surface the generators use
// is implemented: Random, RandBits, RandBelow, RandRange, RandInt,
// Choice, Shuffle.
//
// Semantics follow CPython's Lib/random.py and Modules/_randommodule.c
// exactly: MT19937 core, init_by_array seeding from the little-endian
// 32-bit decomposition of abs(seed), genrand_res53 for Random, and
// rejection-sampled RandBelow. Empty ranges panic, mirroring the
// Python ValueError/IndexError (both are caller bugs that must crash
// loudly, never silently).
package mtrand

import (
	"math/big"
	"strconv"
)

const (
	mtN         = 624
	mtM         = 397
	mtMatrixA   = 0x9908b0df
	mtUpperMask = 0x80000000
	mtLowerMask = 0x7fffffff
)

// Rand is a Mersenne Twister generator with CPython-compatible output.
type Rand struct {
	mt    [mtN]uint32
	index int
}

// New seeds a generator exactly like random.Random(seed) for int seed:
// the absolute value is split into 32-bit little-endian chunks and
// fed to init_by_array (seed 0 yields one zero chunk).
func New(seed int64) *Rand {
	r := &Rand{}
	if seed < 0 {
		seed = -seed
	}
	u := uint64(seed)
	var key []uint32
	for {
		key = append(key, uint32(u&0xffffffff))
		u >>= 32
		if u == 0 {
			break
		}
	}
	r.initByArray(key)
	return r
}

func (r *Rand) initGenrand(s uint32) {
	r.mt[0] = s
	for i := 1; i < mtN; i++ {
		r.mt[i] = 1812433253*(r.mt[i-1]^(r.mt[i-1]>>30)) + uint32(i)
	}
	r.index = mtN
}

func (r *Rand) initByArray(key []uint32) {
	r.initGenrand(19650218)
	i, j := 1, 0
	k := mtN
	if len(key) > k {
		k = len(key)
	}
	for ; k > 0; k-- {
		r.mt[i] = (r.mt[i] ^ ((r.mt[i-1] ^ (r.mt[i-1] >> 30)) * 1664525)) + key[j] + uint32(j)
		i++
		j++
		if i >= mtN {
			r.mt[0] = r.mt[mtN-1]
			i = 1
		}
		if j >= len(key) {
			j = 0
		}
	}
	for k = mtN - 1; k > 0; k-- {
		r.mt[i] = (r.mt[i] ^ ((r.mt[i-1] ^ (r.mt[i-1] >> 30)) * 1566083941)) - uint32(i)
		i++
		if i >= mtN {
			r.mt[0] = r.mt[mtN-1]
			i = 1
		}
	}
	r.mt[0] = 0x80000000
}

// Uint32 draws one raw tempered output (genrand_uint32).
func (r *Rand) Uint32() uint32 {
	if r.index >= mtN {
		var y uint32
		mag01 := [2]uint32{0, mtMatrixA}
		kk := 0
		for ; kk < mtN-mtM; kk++ {
			y = (r.mt[kk] & mtUpperMask) | (r.mt[kk+1] & mtLowerMask)
			r.mt[kk] = r.mt[kk+mtM] ^ (y >> 1) ^ mag01[y&1]
		}
		for ; kk < mtN-1; kk++ {
			y = (r.mt[kk] & mtUpperMask) | (r.mt[kk+1] & mtLowerMask)
			r.mt[kk] = r.mt[kk+(mtM-mtN)] ^ (y >> 1) ^ mag01[y&1]
		}
		y = (r.mt[mtN-1] & mtUpperMask) | (r.mt[0] & mtLowerMask)
		r.mt[mtN-1] = r.mt[mtM-1] ^ (y >> 1) ^ mag01[y&1]
		r.index = 0
	}
	y := r.mt[r.index]
	r.index++
	y ^= y >> 11
	y ^= (y << 7) & 0x9d2c5680
	y ^= (y << 15) & 0xefc60000
	y ^= y >> 18
	return y
}

// Random returns a float64 in [0,1) with 53-bit resolution,
// exactly like random.Random.random (genrand_res53).
func (r *Rand) Random() float64 {
	a := r.Uint32() >> 5
	b := r.Uint32() >> 6
	return (float64(a)*67108864.0 + float64(b)) * (1.0 / 9007199254740992.0)
}

// RandBits returns an int with k random bits, exactly like
// random.Random.getrandbits: ceil(k/32) draws fill the value from
// the least significant word up, the top word shifted down.
func (r *Rand) RandBits(k int) *big.Int {
	if k < 0 {
		panic("mtrand: number of bits must be non-negative")
	}
	if k == 0 {
		return big.NewInt(0)
	}
	if k <= 32 {
		return big.NewInt(int64(r.Uint32() >> (32 - uint(k))))
	}
	words := (k-1)/32 + 1
	out := big.NewInt(0)
	rest := k
	var word big.Int
	for i := 0; i < words; i++ {
		d := r.Uint32()
		if rest < 32 {
			d >>= 32 - uint(rest)
		}
		word.SetUint64(uint64(d))
		word.Lsh(&word, uint(32*i))
		out.Or(out, &word)
		rest -= 32
	}
	return out
}

// RandBelowBig returns a random int in [0,n), exactly like
// random.Random._randbelow (rejection sampling on bit_length).
func (r *Rand) RandBelowBig(n *big.Int) *big.Int {
	if n.Sign() <= 0 {
		panic("mtrand: empty range for randbelow()")
	}
	k := uint(n.BitLen())
	for {
		cand := r.RandBits(int(k))
		if cand.Cmp(n) < 0 {
			return cand
		}
	}
}

// RandBelow returns a random int64 in [0,n).
func (r *Rand) RandBelow(n int64) int64 {
	if n <= 0 {
		panic("mtrand: empty range for randbelow()")
	}
	return r.RandBelowBig(big.NewInt(n)).Int64()
}

// RandRangeBig mirrors random.Random.randrange(start, stop, step)
// for arbitrary ranges.
func (r *Rand) RandRangeBig(start, stop *big.Int, step int64) *big.Int {
	width := big.NewInt(0).Sub(stop, start)
	if step == 1 {
		if width.Sign() <= 0 {
			panic("mtrand: empty range in randrange(" + start.String() + ", " + stop.String() + ")")
		}
		return big.NewInt(0).Add(start, r.RandBelowBig(width))
	}
	n := big.NewInt(0)
	if step > 0 {
		// (width + step - 1) // step
		n.Add(width, big.NewInt(step-1))
		n.Quo(n, big.NewInt(step))
	} else if step < 0 {
		// (width + step + 1) // step
		n.Add(width, big.NewInt(step+1))
		n.Quo(n, big.NewInt(step))
	} else {
		panic("mtrand: zero step for randrange()")
	}
	if n.Sign() <= 0 {
		panic("mtrand: empty range in randrange(" + start.String() + ", " + stop.String() + ", " + strconv.FormatInt(step, 10) + ")")
	}
	off := r.RandBelowBig(n)
	off.Mul(off, big.NewInt(step))
	return off.Add(off, start)
}

// RandRangeStop mirrors randrange(stop).
func (r *Rand) RandRangeStop(stop int64) int64 {
	if stop <= 0 {
		panic("mtrand: empty range for randrange()")
	}
	return r.RandBelow(stop)
}

// RandRange mirrors randrange(start, stop).
func (r *Rand) RandRange(start, stop int64) int64 {
	if stop-start <= 0 {
		panic("mtrand: empty range in randrange(" + strconv.FormatInt(start, 10) + ", " + strconv.FormatInt(stop, 10) + ")")
	}
	return start + r.RandBelow(stop-start)
}

// RandRangeStep mirrors randrange(start, stop, step).
func (r *Rand) RandRangeStep(start, stop, step int64) int64 {
	return r.RandRangeBig(big.NewInt(start), big.NewInt(stop), step).Int64()
}

// RandIntBig mirrors randint(a, b) (both ends included) for big ranges.
func (r *Rand) RandIntBig(a, b *big.Int) *big.Int {
	return r.RandRangeBig(a, big.NewInt(0).Add(b, big.NewInt(1)), 1)
}

// RandInt mirrors randint(a, b) (both ends included).
func (r *Rand) RandInt(a, b int64) int64 {
	return r.RandRange(a, b+1)
}

// Choice mirrors random.Random.choice: the index of the picked
// element of a length-n sequence.
func (r *Rand) Choice(n int) int {
	if n <= 0 {
		panic("mtrand: cannot choose from an empty sequence")
	}
	return int(r.RandBelow(int64(n)))
}

// Shuffle mirrors random.Random.shuffle on indices 0..n-1,
// calling swap for each exchange.
func (r *Rand) Shuffle(n int, swap func(i, j int)) {
	for i := n - 1; i >= 1; i-- {
		j := int(r.RandBelow(int64(i + 1)))
		swap(i, j)
	}
}
