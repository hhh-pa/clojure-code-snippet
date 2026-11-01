// Package generators ports tools/generators/*.py: scaled-input
// generators for complexity-slope measurement. Each generator
// produces (input_bytes, expected_bytes, fit_n) for one benchmark's
// stdin contract at requested scales. Outputs are byte-identical to
// the Python generators they replace.
package generators

import (
	"math/big"
	"strconv"
	"strings"
)

// Generator produces a scaled benchmark input, its expected output,
// and the exact problem size the slope fit runs against.
type Generator interface {
	// Scales returns the measurement scales for this generator.
	Scales() []int64
	// Generate returns (input, expected, fitN) for size n.
	// Invalid n values yield a non-nil error.
	Generate(n int64, seed int64) (input, expected []byte, fitN FitN, err error)
}

// FitN is an exact non-negative integer problem size (Python-int
// semantics). Most generators fit on n itself, but some fit on a
// derived quantity (e.g. Miller-Rabin fits on max N, up to ~2^78),
// so the value is kept as canonical decimal digits, never int64.
type FitN struct {
	digits string
}

// FitNFromInt64 builds a FitN from a small problem size.
func FitNFromInt64(v int64) FitN {
	return FitN{digits: strconv.FormatInt(v, 10)}
}

// FitNFromBig builds a FitN from an arbitrary-size problem size.
// big.Int.String is already canonical (no leading zeros).
func FitNFromBig(v *big.Int) FitN {
	return FitN{digits: v.String()}
}

// String returns the canonical decimal digits (no leading zeros).
func (f FitN) String() string {
	if f.digits == "" {
		return "0"
	}
	return f.digits
}

// Float64 converts to float64, correctly rounded like Python
// float(int): the decimal digits parse exactly into 256 bits of
// precision (all fit sizes are far below 2^256), so the single
// rounding to float64 matches CPython's correctly-rounded result.
func (f FitN) Float64() float64 {
	fl, _, err := big.ParseFloat(f.String(), 10, 256, big.ToNearestEven)
	if err != nil {
		return 0
	}
	v, _ := fl.Float64()
	return v
}

// MarshalJSON emits the raw digits (a JSON number, no quotes).
func (f FitN) MarshalJSON() ([]byte, error) {
	return []byte(f.String()), nil
}

// Cmp compares numerically: -1, 0, or +1.
func (f FitN) Cmp(o FitN) int {
	a, b := f.String(), o.String()
	if len(a) != len(b) {
		if len(a) < len(b) {
			return -1
		}
		return 1
	}
	return strings.Compare(a, b)
}
