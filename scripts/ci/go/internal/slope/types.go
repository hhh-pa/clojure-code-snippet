// Package slope fits complexity exponents t = C + k*n**p from scaled
// runs (T20). The fit is pure math; the scaled-run driver takes its
// generator registry and measure function as callbacks so this
// package stays dependency-free apart from pyjson.
package slope

import (
	"bytes"
	"fmt"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
)

// Digits is an exact integer fit-axis value (Python int semantics,
// e.g. Miller-Rabin maxima near 2**78). It marshals as a bare JSON
// number with full digits.
type Digits string

// MarshalJSON emits the raw digits.
func (d Digits) MarshalJSON() ([]byte, error) {
	if !validDigits(string(d)) {
		return nil, fmt.Errorf("slope: bad fit digits %q", string(d))
	}
	return []byte(d), nil
}

// UnmarshalJSON reads a bare JSON integer literal.
func (d *Digits) UnmarshalJSON(raw []byte) error {
	text := string(bytes.TrimSpace(raw))
	if !validDigits(text) {
		return fmt.Errorf("slope: bad fit digits %q", text)
	}
	*d = Digits(text)
	return nil
}

func validDigits(text string) bool {
	if text == "" {
		return false
	}
	for i := 0; i < len(text); i++ {
		if text[i] < '0' || text[i] > '9' {
			return false
		}
	}
	return true
}

// Point is one fitted scale sample.
type Point struct {
	N         Digits     `json:"n"`
	CpuMedian pyjson.Num `json:"cpu_median"`
	CpuMin    pyjson.Num `json:"cpu_min"`
	CpuMax    pyjson.Num `json:"cpu_max"`
}

// Fit is a power-law fit result: the success keys or the skip keys,
// never both (mirroring fit_power_law's two dict shapes).
type Fit struct {
	Ok       bool        `json:"ok"`
	Slope    *pyjson.Num `json:"slope,omitempty"`
	Lower    *pyjson.Num `json:"lower,omitempty"`
	Upper    *pyjson.Num `json:"upper,omitempty"`
	FloorC   *pyjson.Num `json:"floor_c,omitempty"`
	K        *pyjson.Num `json:"k,omitempty"`
	RSquared *pyjson.Num `json:"r_squared,omitempty"`
	Reason   *string     `json:"reason,omitempty"`
	Detail   *string     `json:"detail,omitempty"`
}

// Entry is one benchmark's slope record.
type Entry struct {
	Scales      []Digits `json:"scales"`
	Points      []Point  `json:"points"`
	Ok          bool     `json:"ok"`
	Fit         *Fit     `json:"fit"`
	Error       *string  `json:"error"`
	ScaleFailed bool     `json:"scale_failed"`
}
