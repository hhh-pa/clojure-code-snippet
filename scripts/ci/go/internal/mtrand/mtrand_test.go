package mtrand

import (
	"math"
	"math/big"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// replay runs the oracle op sequence and returns the values as strings.
func replay(seed int64) []string {
	r := New(seed)
	var out []string
	for i := 0; i < 3; i++ {
		out = append(out, strconv.FormatUint(uint64(r.RandBits(32).Uint64()), 10))
	}
	for _, k := range []int{0, 1, 2, 7, 8, 31, 33, 63, 64, 100, 512} {
		out = append(out, r.RandBits(k).String())
	}
	for i := 0; i < 4; i++ {
		out = append(out, strconv.FormatFloat(r.Random(), 'x', -1, 64))
	}
	out = append(out, strconv.FormatInt(r.RandInt(0, 0), 10))
	for i := 0; i < 3; i++ {
		out = append(out, strconv.FormatInt(r.RandInt(-5, 5), 10))
	}
	ten30, _ := big.NewInt(0).SetString("1000000000000000000000000000000", 10)
	for i := 0; i < 2; i++ {
		out = append(out, r.RandIntBig(big.NewInt(0), ten30).String())
	}
	for i := 0; i < 3; i++ {
		out = append(out, strconv.FormatInt(r.RandRangeStop(10), 10))
	}
	for i := 0; i < 2; i++ {
		out = append(out, strconv.FormatInt(r.RandRange(5, 15), 10))
	}
	for i := 0; i < 2; i++ {
		out = append(out, strconv.FormatInt(r.RandRangeStep(0, 100, 7), 10))
	}
	for i := 0; i < 2; i++ {
		out = append(out, strconv.FormatInt(r.RandRangeStep(100, 0, -3), 10))
	}
	for i := 0; i < 3; i++ {
		out = append(out, strconv.Itoa(r.Choice(20)))
	}
	perm := make([]int, 20)
	for i := range perm {
		perm[i] = i
	}
	r.Shuffle(20, func(i, j int) { perm[i], perm[j] = perm[j], perm[i] })
	var sb strings.Builder
	for i, v := range perm {
		if i > 0 {
			sb.WriteByte(' ')
		}
		sb.WriteString(strconv.Itoa(v))
	}
	out = append(out, sb.String())
	return out
}

func TestOracleParity(t *testing.T) {
	cases := map[string]int64{
		"oracle_0.txt":      0,
		"oracle_1.txt":      1,
		"oracle_1000.txt":   1000,
		"oracle_1001.txt":   1001,
		"oracle_2p32m1.txt": 1<<32 - 1,
		"oracle_2p32.txt":   1 << 32,
		"oracle_neg5.txt":   -5,
	}
	for name, seed := range cases {
		raw, err := os.ReadFile(filepath.Join("testdata", name))
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		var want []string
		for _, line := range strings.Split(string(raw), "\n") {
			if line == "" || strings.HasPrefix(line, "#") {
				continue
			}
			want = append(want, line)
		}
		got := replay(seed)
		if len(got) != len(want) {
			t.Fatalf("%s: got %d values, want %d", name, len(got), len(want))
		}
		for i := range want {
			if i >= 14 && i < 18 {
				// Floats: compare exact bits via hex-float parse.
				gf, err1 := strconv.ParseFloat(got[i], 64)
				wf, err2 := strconv.ParseFloat(want[i], 64)
				if err1 != nil || err2 != nil || math.Float64bits(gf) != math.Float64bits(wf) {
					t.Fatalf("%s value %d: got %s want %s", name, i, got[i], want[i])
				}
				continue
			}
			if got[i] != want[i] {
				t.Fatalf("%s value %d: got %s want %s", name, i, got[i], want[i])
			}
		}
	}
}

func TestPanics(t *testing.T) {
	r := New(1)
	for name, fn := range map[string]func(){
		"randbelow-zero":      func() { r.RandBelow(0) },
		"randrange-empty":     func() { r.RandRange(5, 5) },
		"randrange-step-zero": func() { r.RandRangeStep(0, 5, 0) },
		"choice-empty":        func() { r.Choice(0) },
		"randbits-negative":   func() { r.RandBits(-1) },
	} {
		func() {
			defer func() {
				if recover() == nil {
					t.Fatalf("%s: expected panic", name)
				}
			}()
			fn()
		}()
	}
}
