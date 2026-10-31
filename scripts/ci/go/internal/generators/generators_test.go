package generators

import (
	"bytes"
	"math/big"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
)

const (
	goldenSeed = 1000
	smallN     = 10
)

// mediumCases holds one much-larger-than-small scale per ported
// Python module; several cross the _BLOCK chunk boundaries.
var mediumCases = []struct {
	key string
	n   int64
}{
	{"compiler/gvn/gvn", 20000},
	{"data_structure/heap/find_the_running_median", 150000},
	{"distributed/clocks/vector_clocks", 20000},
	{"dp/lis/longest_increasing_subsequent", 150000},
	{"graph/shortest_path/bfsshortreach", 150000},
	{"search/two_pointers/pairs", 150000},
	{"sort/merge_sort/solution", 250000},
	{"string_related/manacher/manacher", 1500000},
}

func stem(key string) string {
	return strings.ReplaceAll(key, "/", "__")
}

func checkGolden(t *testing.T, key string, gen Generator, n int64) {
	t.Helper()
	gotIn, gotOut, gotFit, err := gen.Generate(n, goldenSeed)
	if err != nil {
		t.Fatalf("%s.Generate(%d): unexpected error: %v", key, n, err)
	}
	base := filepath.Join("testdata",
		stem(key)+"."+strconv.FormatInt(n, 10))
	wantIn, err := os.ReadFile(base + ".in")
	if err != nil {
		t.Fatalf("%s: read golden input: %v", key, err)
	}
	wantOut, err := os.ReadFile(base + ".out")
	if err != nil {
		t.Fatalf("%s: read golden expected: %v", key, err)
	}
	wantFit, err := os.ReadFile(base + ".fit")
	if err != nil {
		t.Fatalf("%s: read golden fit: %v", key, err)
	}
	if !bytes.Equal(gotIn, wantIn) {
		t.Fatalf("%s n=%d: input differs (%d vs %d bytes)",
			key, n, len(gotIn), len(wantIn))
	}
	if !bytes.Equal(gotOut, wantOut) {
		t.Fatalf("%s n=%d: expected differs (%d vs %d bytes)",
			key, n, len(gotOut), len(wantOut))
	}
	if gotFit.String() != string(wantFit) {
		t.Fatalf("%s n=%d: fit %s, want %s",
			key, n, gotFit.String(), wantFit)
	}
}

func sortedKeys() []string {
	keys := make([]string, 0, len(GENERATORS))
	for k := range GENERATORS {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

func TestGoldenSmall(t *testing.T) {
	for _, key := range sortedKeys() {
		checkGolden(t, key, GENERATORS[key], smallN)
	}
}

func TestGoldenMedium(t *testing.T) {
	for _, c := range mediumCases {
		gen, ok := GENERATORS[c.key]
		if !ok {
			t.Fatalf("medium case %s not registered", c.key)
		}
		checkGolden(t, c.key, gen, c.n)
	}
}

func TestGoldenUnregistered(t *testing.T) {
	// Delaunay/Voronoi are ported but have no registry entry.
	checkGolden(t, "geo_dist__delaunay", &DelaunayGenerator{}, smallN)
	checkGolden(t, "geo_dist__voronoi", &VoronoiGenerator{}, smallN)
}

func TestRegistryKeys(t *testing.T) {
	raw, err := os.ReadFile(filepath.Join("testdata", "registry_keys.txt"))
	if err != nil {
		t.Fatalf("read registry keys golden: %v", err)
	}
	want := strings.Split(strings.TrimSpace(string(raw)), "\n")
	if got := sortedKeys(); !equalStrings(got, want) {
		t.Fatalf("registry key set differs:\n got %d keys\nwant %d keys",
			len(got), len(want))
	}
	for _, key := range want {
		if len(GENERATORS[key].Scales()) == 0 {
			t.Fatalf("%s: empty scales", key)
		}
	}
}

func equalStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func TestFitN(t *testing.T) {
	floats := map[string]float64{
		// Reference values from Python float(int).
		"0":                               0,
		"10":                              10,
		"1147":                            1147,
		"150000":                          150000,
		"302231454903657293676544":        3.022314549036573e+23,
		"302231454903657293688889":        3.022314549036573e+23,
		"1000000000000000000000000000007": 1e30,
	}
	for digits, want := range floats {
		bi, ok := new(big.Int).SetString(digits, 10)
		if !ok {
			t.Fatalf("bad fixture %s", digits)
		}
		for name, f := range map[string]FitN{
			"big": FitNFromBig(bi),
		} {
			if f.String() != digits {
				t.Fatalf("%s.String() = %s, want %s",
					name, f.String(), digits)
			}
			if got := f.Float64(); got != want {
				t.Fatalf("FitN(%s).Float64() = %v, want %v",
					digits, got, want)
			}
			raw, err := f.MarshalJSON()
			if err != nil || string(raw) != digits {
				t.Fatalf("FitN(%s).MarshalJSON() = %s,%v",
					digits, raw, err)
			}
		}
		if f := FitNFromInt64(1147); f.String() != "1147" {
			t.Fatalf("FitNFromInt64(1147) = %s", f.String())
		}
	}
	cmps := []struct {
		a, b string
		want int
	}{
		{"10", "10", 0},
		{"9", "10", -1},
		{"10", "9", 1},
		{"999", "1000", -1},
		{"302231454903657293676544", "302231454903657293688889", -1},
		{"1000000000000000000000000000007", "99999999999999999999999999999999", -1},
	}
	mustBig := func(s string) FitN {
		bi, ok := new(big.Int).SetString(s, 10)
		if !ok {
			t.Fatalf("bad fixture %s", s)
		}
		return FitNFromBig(bi)
	}
	for _, c := range cmps {
		if got := mustBig(c.a).Cmp(mustBig(c.b)); got != c.want {
			t.Fatalf("Cmp(%s,%s) = %d, want %d",
				c.a, c.b, got, c.want)
		}
		if got := mustBig(c.b).Cmp(mustBig(c.a)); got != -c.want {
			t.Fatalf("Cmp(%s,%s) = %d, want %d",
				c.b, c.a, got, -c.want)
		}
	}
}

func TestGenerateErrors(t *testing.T) {
	gen := GENERATORS["sort/quickselect/find_the_median"]
	if _, _, _, err := gen.Generate(0, goldenSeed); err == nil {
		t.Fatalf("quickselect n=0: expected error, got nil")
	} else if want := "quickselect needs n >= 1, got 0"; err.Error() != want {
		t.Fatalf("quickselect n=0: error %q, want %q", err, want)
	}
	// SortArray accepts n=0 like the Python version (empty output).
	sa := GENERATORS["sort/merge_sort/solution"]
	in, out, fit, err := sa.Generate(0, goldenSeed)
	if err != nil {
		t.Fatalf("sort n=0: unexpected error: %v", err)
	}
	if string(in) != "0" || string(out) != "[]" || fit.String() != "0" {
		t.Fatalf("sort n=0: got %q %q %s", in, out, fit.String())
	}
}
