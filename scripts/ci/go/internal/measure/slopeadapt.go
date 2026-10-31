package measure

import (
	"sort"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/generators"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/slope"
)

// SlopeAdapter adapts the generators registry to the slope runner.
type SlopeAdapter struct {
	Gen generators.Generator
}

func (a SlopeAdapter) Scales() []int64 { return a.Gen.Scales() }

func (a SlopeAdapter) Generate(n, seed int64) ([]byte, []byte, string, error) {
	in, exp, fitN, err := a.Gen.Generate(n, seed)
	if err != nil {
		return nil, nil, "", err
	}
	return in, exp, fitN.String(), nil
}

func sortedEntryKeys(m map[string]slope.Entry) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
