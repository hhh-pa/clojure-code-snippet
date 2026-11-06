package snipcore

import (
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Case is one test_case_N directory with its input/expected files.
type Case struct {
	Name   string
	Input  string
	Output string
}

// Found is one discovered snippet plus its cases, mirroring the
// (area, problem, script, cases) tuples of run_tests.discover.
type Found struct {
	Area    string
	Problem string
	Script  string
	Cases   []Case
	// Label is Area/Problem with forward slashes (measure.py form).
	Label string
	// Stem is the script file name without the script extension.
	Stem string
}

// Discover ports run_tests.discover exactly: every src/**/*<ext>
// under root, sorted, joined against
// resources/<area_problem>/<stem>/ test_case_*, including the
// area/problem split quirk (first path separator wins; a bare file
// falls back to (dir, stem)). ext is the script extension with its
// leading dot (".clj"); the head-to-head gate measures the python
// twin repo through the same code with ".py".
func Discover(root, resources, ext string) ([]Found, error) {
	srcDir := filepath.Join(root, "src")
	var scripts []string
	err := filepath.WalkDir(srcDir, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() {
			return nil
		}
		if strings.HasSuffix(entry.Name(), ext) {
			scripts = append(scripts, path)
		}
		return nil
	})
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}
	sort.Strings(scripts)
	var out []Found
	for _, script := range scripts {
		rel, err := filepath.Rel(srcDir, script)
		if err != nil {
			return nil, err
		}
		areaProblem := filepath.Dir(rel)
		stem := strings.TrimSuffix(filepath.Base(rel), ext)
		caseDir := filepath.Join(resources, areaProblem, stem)
		var cases []Case
		entries, err := os.ReadDir(caseDir)
		if err == nil {
			for _, entry := range entries {
				name := entry.Name()
				if !strings.HasPrefix(name, "test_case_") {
					continue
				}
				info, err := entry.Info()
				if err != nil || !info.IsDir() {
					continue
				}
				cases = append(cases, Case{
					Name:   name,
					Input:  filepath.Join(caseDir, name, "input.txt"),
					Output: filepath.Join(caseDir, name, "output.txt"),
				})
			}
			sort.Slice(cases, func(i, j int) bool { return cases[i].Name < cases[j].Name })
		}
		area, problem := areaProblem, stem
		if idx := strings.Index(areaProblem, string(os.PathSeparator)); idx >= 0 {
			area = areaProblem[:idx]
			problem = areaProblem[idx+1:]
		}
		label := area + "/" + problem
		if os.PathSeparator != '/' {
			label = strings.ReplaceAll(label, string(os.PathSeparator), "/")
		}
		out = append(out, Found{
			Area:    area,
			Problem: problem,
			Script:  script,
			Cases:   cases,
			Label:   label,
			Stem:    stem,
		})
	}
	return out, nil
}
