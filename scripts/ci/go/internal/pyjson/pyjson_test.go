package pyjson

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

func TestFloatReprOracle(t *testing.T) {
	raw, err := os.ReadFile(filepath.Join("testdata", "floats.txt"))
	if err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimRight(string(raw), "\n"), "\n")
	bad := 0
	for i, want := range lines {
		f, err := strconv.ParseFloat(want, 64)
		if err != nil {
			t.Fatalf("line %d: parse %q: %v", i, want, err)
		}
		// repr() spells these lowercase, json.dumps capitalized;
		// FloatRepr targets JSON.
		switch want {
		case "inf":
			want = "Infinity"
		case "-inf":
			want = "-Infinity"
		case "nan":
			want = "NaN"
		}
		if got := FloatRepr(f); got != want {
			if bad < 10 {
				t.Errorf("line %d: FloatRepr(%v) = %q, want %q", i, f, got, want)
			}
			bad++
		}
	}
	if bad > 0 {
		t.Fatalf("%d/%d mismatches", bad, len(lines))
	}
}

func TestMarshalIndent2(t *testing.T) {
	// Ordered struct mirroring a small Python dict; golden generated
	// from json.dumps(obj, indent=2).
	f := FloatNum(1e-9)
	doc := struct {
		Name   string         `json:"name"`
		Count  int            `json:"count"`
		Ratio  Num            `json:"ratio"`
		Big    Num            `json:"big"`
		Small  Num            `json:"small"`
		Null   *Num           `json:"null"`
		Flag   bool           `json:"flag"`
		List   []Num          `json:"list"`
		Nested map[string]Num `json:"nested"`
		Uni    string         `json:"uni"`
		Ctrl   string         `json:"ctrl"`
		HTML   string         `json:"html"`
	}{
		Name:  "x",
		Count: 105,
		Ratio: FloatNum(105.0),
		Big:   FloatNum(1e16),
		Small: f,
		Null:  nil,
		Flag:  true,
		List:  []Num{IntNum(1), FloatNum(2.5)},
		Nested: map[string]Num{
			"b": IntNum(2),
			"a": IntNum(1),
		},
		Uni:  "café \U0001f600",
		Ctrl: "a\bb\fc\nd\te\"f\\g",
		HTML: "<tag>&\"q\"",
	}
	got, err := MarshalIndent2(doc)
	if err != nil {
		t.Fatal(err)
	}
	want := Golden(t, filepath.Join("testdata", "doc.json"))
	if string(got) != want {
		t.Fatalf("MarshalIndent2:\n got: %q\nwant: %q", got, want)
	}
	// A literal backslash-u sequence in source text must survive.
	raw, err := MarshalIndent2(map[string]string{"s": `x\u0008y`})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(raw), `x\\u0008y`) {
		t.Fatalf("escaped literal corrupted: %q", raw)
	}
}

func TestNumSemantics(t *testing.T) {
	if got := MedianInt64([]int64{100, 110, 120}); !got.IsInt || got.I != 110 {
		t.Fatalf("odd median: %+v", got)
	}
	if got := MedianInt64([]int64{100, 110}); got.IsInt || got.F != 105.0 {
		t.Fatalf("even median: %+v", got)
	}
	sum := Sum([]Num{IntNum(1), IntNum(2)})
	if !sum.IsInt || sum.I != 3 {
		t.Fatalf("int sum: %+v", sum)
	}
	sum = Sum([]Num{IntNum(1), FloatNum(2.5)})
	if sum.IsInt || sum.F != 3.5 {
		t.Fatalf("mixed sum: %+v", sum)
	}
	if got := Max([]Num{IntNum(3), IntNum(7)}); !got.IsInt || got.I != 7 {
		t.Fatalf("int max: %+v", got)
	}
	enc, _ := json.Marshal(IntNum(107520))
	if string(enc) != "107520" {
		t.Fatalf("int marshal: %s", enc)
	}
	enc, _ = json.Marshal(FloatNum(105.0))
	if string(enc) != "105.0" {
		t.Fatalf("float marshal: %s", enc)
	}
}

// Golden reads a testdata golden file.
func Golden(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}
