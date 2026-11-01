package slope

import (
	"encoding/json"
	"math"
	"os"
	"path/filepath"
	"testing"
)

func closeEnough(got, want float64) bool {
	if got == want {
		return true
	}
	scale := math.Max(math.Abs(want), 1e-300)
	return math.Abs(got-want)/scale < 1e-9
}

func TestFitOracle(t *testing.T) {
	raw, err := os.ReadFile(filepath.Join("testdata", "fits.json"))
	if err != nil {
		t.Fatal(err)
	}
	var cases map[string]struct {
		Ns  []float64      `json:"ns"`
		Ts  []float64      `json:"ts"`
		Fit map[string]any `json:"fit"`
	}
	if err := json.Unmarshal(raw, &cases); err != nil {
		t.Fatal(err)
	}
	for name, c := range cases {
		got := FitPowerLaw(c.Ns, c.Ts)
		wantOk, _ := c.Fit["ok"].(bool)
		if got.Ok != wantOk {
			t.Fatalf("%s: ok=%v want %v (%v)", name, got.Ok, wantOk, c.Fit)
		}
		if !wantOk {
			if got.Reason == nil || *got.Reason != c.Fit["reason"] {
				t.Fatalf("%s: reason=%v want %v", name, got.Reason, c.Fit["reason"])
			}
			if got.Detail == nil || *got.Detail != c.Fit["detail"] {
				t.Fatalf("%s: detail=%q want %q", name, *got.Detail, c.Fit["detail"])
			}
			continue
		}
		for _, key := range []string{"slope", "lower", "upper", "floor_c", "k", "r_squared"} {
			want, _ := c.Fit[key].(float64)
			var gotV float64
			switch key {
			case "slope":
				gotV = got.Slope.Float64()
			case "lower":
				gotV = got.Lower.Float64()
			case "upper":
				gotV = got.Upper.Float64()
			case "floor_c":
				gotV = got.FloorC.Float64()
			case "k":
				gotV = got.K.Float64()
			case "r_squared":
				gotV = got.RSquared.Float64()
			}
			if !closeEnough(gotV, want) {
				t.Fatalf("%s.%s = %.17g want %.17g", name, key, gotV, want)
			}
		}
	}
}

func TestFitChecks(t *testing.T) {
	// The CLI self-test assertions (single source in selfcheck.go).
	for _, c := range SelfChecks() {
		if !c.Ok {
			t.Fatalf("%s: %s", c.Name, c.Detail)
		}
	}
}

func TestDigitsJSON(t *testing.T) {
	raw, err := json.Marshal(struct {
		N Digits `json:"n"`
	}{N: Digits("123456789012345678901234567890")})
	if err != nil {
		t.Fatal(err)
	}
	if string(raw) != `{"n":123456789012345678901234567890}` {
		t.Fatalf("digits marshal: %s", raw)
	}
	var back struct {
		N Digits `json:"n"`
	}
	if err := json.Unmarshal(raw, &back); err != nil || back.N != "123456789012345678901234567890" {
		t.Fatalf("digits round trip: %v %q", err, back.N)
	}
}
