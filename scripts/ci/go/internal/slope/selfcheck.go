package slope

import (
	"fmt"
	"math"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// SelfChecks ports the fit half of slope.run_checks (the runner
// checks wire up with the generators registry). Names match Python
// so `snip measure --self-test` output covers the same checks.
func SelfChecks() []snipcore.Check {
	var out []snipcore.Check
	check := func(name string, ok bool, detail string) {
		out = append(out, snipcore.Check{Name: name, Ok: ok, Detail: detail})
	}
	linN := []float64{100000, 200000, 400000, 800000}
	linT := []float64{2.0 + 1e-6*100000, 2.0 + 1e-6*200000, 2.0 + 1e-6*400000, 2.0 + 1e-6*800000}
	r := FitPowerLaw(linN, linT)
	check("slope-linear", r.Ok && inBand(r.Slope.Float64(), 0.95, 1.05) &&
		r.RSquared.Float64() > 0.999, fmt.Sprintf("%+v", r))

	quadN := []float64{20000, 40000, 80000, 160000}
	quadT := []float64{2.0 + 1e-9*20000*20000, 2.0 + 1e-9*40000*40000,
		2.0 + 1e-9*80000*80000, 2.0 + 1e-9*160000*160000}
	r = FitPowerLaw(quadN, quadT)
	check("slope-quadratic", r.Ok && inBand(r.Slope.Float64(), 1.95, 2.05) &&
		r.RSquared.Float64() > 0.999, fmt.Sprintf("%+v", r))

	nlognN := []float64{1000000, 2000000, 4000000, 8000000}
	var nlognT []float64
	for _, n := range nlognN {
		nlognT = append(nlognT, 2.0+1e-7*n*math.Log(n))
	}
	r = FitPowerLaw(nlognN, nlognT)
	check("slope-nlogn", r.Ok && inBand(r.Slope.Float64(), 1.0, 1.15) &&
		r.RSquared.Float64() > 0.99, fmt.Sprintf("%+v", r))

	var floorT []float64
	for _, n := range quadN {
		floorT = append(floorT, 1.5+1e-9*n*n)
	}
	r = FitPowerLaw(quadN, floorT)
	check("slope-floor", r.Ok && math.Abs(r.FloorC.Float64()-1.5) < 0.05,
		fmt.Sprintf("%+v", r))

	a := FitPowerLaw(quadN, quadT)
	b := FitPowerLaw(quadN, quadT)
	check("slope-deterministic", fitEqual(a, b), fmt.Sprintf("%+v %+v", a, b))
	check("slope-band", a.Ok && a.Lower.Float64() <= a.Slope.Float64() &&
		a.Slope.Float64() <= a.Upper.Float64() &&
		a.Upper.Float64()-a.Lower.Float64() < 0.05, fmt.Sprintf("%+v", a))

	flat := FitPowerLaw(quadN, []float64{2.0, 2.001, 1.999, 2.0})
	check("slope-skip-flat", !flat.Ok && *flat.Reason == "flat", fmt.Sprintf("%+v", flat))
	tiny := FitPowerLaw([]float64{100, 110, 120, 130}, []float64{1, 2, 3, 4})
	check("slope-skip-tiny-range", !tiny.Ok && *tiny.Reason == "tiny-range", fmt.Sprintf("%+v", tiny))
	few := FitPowerLaw([]float64{1, 2}, []float64{1, 4})
	check("slope-skip-too-few", !few.Ok && *few.Reason == "too-few-points", fmt.Sprintf("%+v", few))
	nonpos := FitPowerLaw(quadN, []float64{2.0, 0.0, 3.0, 4.0})
	check("slope-skip-non-positive", !nonpos.Ok && *nonpos.Reason == "non-positive",
		fmt.Sprintf("%+v", nonpos))
	dec := FitPowerLaw(quadN, []float64{8.0, 4.0, 2.0, 1.0})
	check("slope-skip-non-increasing", !dec.Ok && *dec.Reason == "non-increasing",
		fmt.Sprintf("%+v", dec))
	return out
}

func inBand(v, lo, hi float64) bool { return v >= lo && v <= hi }

func fitEqual(a, b *Fit) bool {
	if a.Ok != b.Ok {
		return false
	}
	if !a.Ok {
		return *a.Reason == *b.Reason && *a.Detail == *b.Detail
	}
	return a.Slope.Float64() == b.Slope.Float64() &&
		a.Lower.Float64() == b.Lower.Float64() &&
		a.Upper.Float64() == b.Upper.Float64() &&
		a.FloorC.Float64() == b.FloorC.Float64() &&
		a.K.Float64() == b.K.Float64() &&
		a.RSquared.Float64() == b.RSquared.Float64()
}
