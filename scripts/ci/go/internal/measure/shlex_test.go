package measure

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
)

func TestSplitCommandOracle(t *testing.T) {
	raw, err := os.ReadFile(filepath.Join("testdata", "shlex.json"))
	if err != nil {
		t.Fatal(err)
	}
	var cases []struct {
		In     string   `json:"in"`
		Tokens []string `json:"tokens"`
		Error  string   `json:"error"`
	}
	if err := json.Unmarshal(raw, &cases); err != nil {
		t.Fatal(err)
	}
	for _, c := range cases {
		got, err := SplitCommand(c.In)
		if c.Error != "" {
			if err == nil || err.Error() != c.Error {
				t.Fatalf("SplitCommand(%q) err = %v, want %q", c.In, err, c.Error)
			}
			continue
		}
		if err != nil {
			t.Fatalf("SplitCommand(%q) err = %v", c.In, err)
		}
		if len(got) != len(c.Tokens) {
			t.Fatalf("SplitCommand(%q) = %q, want %q", c.In, got, c.Tokens)
		}
		for i := range got {
			if got[i] != c.Tokens[i] {
				t.Fatalf("SplitCommand(%q) = %q, want %q", c.In, got, c.Tokens)
			}
		}
	}
}
