package slope

import (
	"fmt"
	"math"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
)

// Fit search range for the exponent (P_MIN/P_MAX).
const (
	PMin = 0.0
	PMax = 3.0
	// SearchIters golden-section iterations (SEARCH_ITERS).
	SearchIters = 80
	// Garbage-in guards (each skip records its reason).
	MinPoints   = 3
	MinNRange   = 4.0
	MinTSpread  = 0.05
	MinRSquared = 0.8
)

// ols is the closed-form OLS of y on x: (intercept, slope, sse, ok).
func ols(x, y []float64) (float64, float64, float64, bool) {
	n := float64(len(x))
	var sx, sy float64
	for i := range x {
		sx += x[i]
		sy += y[i]
	}
	mx, my := sx/n, sy/n
	var sxx, sxy float64
	for i := range x {
		dx := x[i] - mx
		sxx += dx * dx
		sxy += dx * (y[i] - my)
	}
	if sxx <= 0 {
		return 0, 0, 0, false
	}
	slope := sxy / sxx
	intercept := my - slope*mx
	var sse float64
	for i := range x {
		d := y[i] - (intercept + slope*x[i])
		sse += d * d
	}
	return intercept, slope, sse, true
}

// sseAtP is the SSE of the best (C, k) fit at fixed p.
func sseAtP(ns, ts []float64, p float64) (float64, float64, float64, bool) {
	xs := make([]float64, len(ns))
	for i, n := range ns {
		v := math.Pow(n, p)
		if math.IsInf(v, 0) || math.IsNaN(v) {
			return 0, 0, 0, false
		}
		xs[i] = v
	}
	c, k, sse, ok := ols(xs, ts)
	if !ok {
		return 0, 0, 0, false
	}
	return c, k, sse, true
}

// search golden-sections argmin SSE(p) over [PMin, PMax].
func search(ns, ts []float64) (p, c, k, sse float64, ok bool) {
	invPhi := (math.Sqrt(5) - 1) / 2
	lo, hi := PMin, PMax
	cl := hi - invPhi*(hi-lo)
	d := lo + invPhi*(hi-lo)
	_, _, fc, okC := sseAtP(ns, ts, cl)
	_, _, fd, okD := sseAtP(ns, ts, d)
	if !okC {
		fc = math.Inf(1)
	}
	if !okD {
		fd = math.Inf(1)
	}
	for i := 0; i < SearchIters; i++ {
		if fc < fd {
			hi, d, fd = d, cl, fc
			cl = hi - invPhi*(hi-lo)
			_, _, fc, okC = sseAtP(ns, ts, cl)
			if !okC {
				fc = math.Inf(1)
			}
		} else {
			lo, cl, fc = cl, d, fd
			d = lo + invPhi*(hi-lo)
			_, _, fd, okD = sseAtP(ns, ts, d)
			if !okD {
				fd = math.Inf(1)
			}
		}
	}
	p = (lo + hi) / 2
	c, k, sse, ok = sseAtP(ns, ts, p)
	return p, c, k, sse, ok
}

func rSquared(ts []float64, sse float64) float64 {
	var sum float64
	for _, t := range ts {
		sum += t
	}
	mean := sum / float64(len(ts))
	var sst float64
	for _, t := range ts {
		d := t - mean
		sst += d * d
	}
	if sst <= 0 {
		return 0
	}
	return math.Max(0, 1-sse/sst)
}

// band is the leave-one-out min/max slope band.
func band(ns, ts []float64, slope float64) (float64, float64) {
	var slopes []float64
	for drop := range ns {
		var subN, subT []float64
		for i := range ns {
			if i == drop {
				continue
			}
			subN = append(subN, ns[i])
			subT = append(subT, ts[i])
		}
		if len(subN) < MinPoints {
			continue
		}
		if maxFloat(subN)/minFloat(subN) < MinNRange {
			continue
		}
		p, _, k, _, ok := search(subN, subT)
		if !ok || math.IsNaN(p) || math.IsInf(p, 0) || k <= 0 {
			continue
		}
		slopes = append(slopes, p)
	}
	if len(slopes) < 2 {
		return slope, slope
	}
	slopes = append(slopes, slope)
	return minFloat(slopes), maxFloat(slopes)
}

func minFloat(vs []float64) float64 {
	m := vs[0]
	for _, v := range vs[1:] {
		if v < m {
			m = v
		}
	}
	return m
}

func maxFloat(vs []float64) float64 {
	m := vs[0]
	for _, v := range vs[1:] {
		if v > m {
			m = v
		}
	}
	return m
}

// skip builds a skipped-fit result.
func skip(reason, detail string) *Fit {
	return &Fit{Ok: false, Reason: &reason, Detail: &detail}
}

func numPtr(v float64) *pyjson.Num {
	n := pyjson.FloatNum(v)
	return &n
}

// FitPowerLaw ports fit_power_law. Float results may differ from
// Python around the 10th digit on shallow SSE valleys (math.Pow
// implementations); callers and tests compare fits within 1e-9
// relative, never byte-exact.
func FitPowerLaw(ns, ts []float64) *Fit {
	if len(ns) != len(ts) || len(ns) < MinPoints {
		return skip("too-few-points", fmt.Sprintf("need >= %d points, got %d", MinPoints, len(ns)))
	}
	for _, v := range ns {
		if v <= 0 {
			return skip("non-positive", "n and t must all be > 0")
		}
	}
	for _, v := range ts {
		if v <= 0 {
			return skip("non-positive", "n and t must all be > 0")
		}
	}
	if maxFloat(ns)/minFloat(ns) < MinNRange {
		return skip("tiny-range", fmt.Sprintf("max(n)/min(n) = %.2f < %s",
			maxFloat(ns)/minFloat(ns), pyjson.FloatRepr(MinNRange)))
	}
	if (maxFloat(ts)-minFloat(ts))/maxFloat(ts) < MinTSpread {
		return skip("flat", fmt.Sprintf("t spread < %.0f%% of max: no measurable scaling", MinTSpread*100))
	}
	p, floorC, k, sse, ok := search(ns, ts)
	if !ok {
		return skip("search-failed", "degenerate design matrix over p range")
	}
	// Python catches ArithmeticError/ValueError around the search;
	// math.Pow never raises in Go, and NaN/Inf land here instead.
	if math.IsNaN(p) || math.IsNaN(floorC) || math.IsNaN(k) || math.IsNaN(sse) ||
		math.IsInf(p, 0) || math.IsInf(floorC, 0) || math.IsInf(k, 0) || math.IsInf(sse, 0) {
		return skip("search-failed", "non-finite fit parameters")
	}
	if k <= 0 {
		return skip("non-increasing", "fitted k <= 0: t does not grow with n")
	}
	r2 := rSquared(ts, sse)
	if r2 < MinRSquared {
		return skip("poor-fit", fmt.Sprintf("R^2 = %.3f < %s", r2, pyjson.FloatRepr(MinRSquared)))
	}
	lo, hi := band(ns, ts, p)
	return &Fit{
		Ok: true, Slope: numPtr(p), Lower: numPtr(lo), Upper: numPtr(hi),
		FloorC: numPtr(floorC), K: numPtr(k), RSquared: numPtr(r2),
	}
}
