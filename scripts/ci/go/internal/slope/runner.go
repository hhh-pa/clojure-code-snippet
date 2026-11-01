package slope

import (
	"fmt"
	"math/big"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
)

// ScaleRequest is one slope scale measurement.
type ScaleRequest struct {
	Label, Stem, Script, CaseName, InPath, OutPath string
	Repeat, AllocRepeat, Timeout                   int
}

// ScaleOutcome is the measured result (cpu summary only; the fit
// reads medians).
type ScaleOutcome struct {
	Ok                        bool
	Error                     string
	CpuMin, CpuMedian, CpuMax float64
}

// MeasureScaleFunc measures one scale (alloc_repeat is always 0:
// slope scales are timing-only probes).
type MeasureScaleFunc func(req ScaleRequest) ScaleOutcome

// Generator produces scaled inputs (satisfied by an adapter over
// the generators registry).
type Generator interface {
	Scales() []int64
	Generate(n, seed int64) (input, expected []byte, fitDigits string, err error)
}

// Group identifies one benchmark's regular entry.
type Group struct {
	Label, Stem string
}

// Shard selects slope entries coherently with the main battery.
type Shard struct {
	Index, Total int
}

// RunSlopeAll ports run_slope_all: for every registry benchmark with
// a discovered script, generate its scales, measure them with the
// harness, and fit t = C + k*n**p. Benchmarks run sequentially
// (one tmpdir at a time, crash-safe cleanup); scales within a
// benchmark run on a jobs-wide pool. A generator failure aborts the
// run with an error (Python crashes the same way). progress, when
// non-nil, fires after each benchmark with its 1-based index,
// the task count, and the finished entry, so callers can stream
// per-benchmark progress (tasks run in sorted-name order).
func RunSlopeAll(measure MeasureScaleFunc, registry map[string]Generator,
	scripts map[Group]string, only string, allGroups []Group,
	shard *Shard, repeat, timeout, jobs int,
	progress func(index, total int, name string, entry Entry)) (map[string]Entry, error) {
	var keep map[Group]bool
	if shard != nil {
		ordered := append([]Group(nil), allGroups...)
		sort.Slice(ordered, func(i, j int) bool {
			if ordered[i].Label != ordered[j].Label {
				return ordered[i].Label < ordered[j].Label
			}
			return ordered[i].Stem < ordered[j].Stem
		})
		keep = map[Group]bool{}
		for i, g := range ordered {
			if i%shard.Total == shard.Index {
				keep[g] = true
			}
		}
	}
	var names []string
	for name := range registry {
		names = append(names, name)
	}
	sort.Strings(names)
	var tasks []slopeTask
	for _, name := range names {
		label, stem := splitName(name)
		if only != "" && !strings.Contains(label, only) {
			continue
		}
		script, ok := scripts[Group{label, stem}]
		if !ok {
			continue
		}
		if keep != nil && !keep[Group{label, stem}] {
			continue
		}
		tasks = append(tasks, slopeTask{name, label, stem, script, registry[name]})
	}
	complexity := map[string]Entry{}
	for i, t := range tasks {
		entry, err := runOne(measure, t, repeat, timeout, jobs)
		if err != nil {
			return nil, err
		}
		complexity[t.name] = entry
		if progress != nil {
			progress(i+1, len(tasks), t.name, entry)
		}
	}
	return complexity, nil
}

func splitName(name string) (label, stem string) {
	if idx := strings.LastIndex(name, "/"); idx >= 0 {
		return name[:idx], name[idx+1:]
	}
	return "", name
}

type scaleJob struct {
	fitDigits, inPath, outPath string
}

type slopeTask struct {
	name, label, stem, script string
	gen                       Generator
}

func runOne(measure MeasureScaleFunc, t slopeTask, repeat, timeout, jobs int) (Entry, error) {
	entry := Entry{Scales: []Digits{}, Points: []Point{}}
	tmpdir, err := os.MkdirTemp("", "slope-")
	if err != nil {
		return entry, err
	}
	defer os.RemoveAll(tmpdir)
	var scaleJobs []scaleJob
	for idx, n := range t.gen.Scales() {
		dataIn, dataOut, fitDigits, err := t.gen.Generate(n, 1000+int64(idx))
		if err != nil {
			return entry, fmt.Errorf("slope %s: generator failed for n=%d: %v", t.name, n, err)
		}
		inPath := filepath.Join(tmpdir, fmt.Sprintf("%s-%d.in", t.stem, idx))
		outPath := filepath.Join(tmpdir, fmt.Sprintf("%s-%d.expected", t.stem, idx))
		if err := os.WriteFile(inPath, dataIn, 0o644); err != nil {
			return entry, err
		}
		if err := os.WriteFile(outPath, dataOut, 0o644); err != nil {
			return entry, err
		}
		scaleJobs = append(scaleJobs, scaleJob{fitDigits, inPath, outPath})
	}
	if jobs < 1 {
		jobs = 1
	}
	type outcome struct {
		fitDigits string
		result    ScaleOutcome
	}
	work := make(chan scaleJob)
	results := make(chan outcome)
	var workers sync.WaitGroup
	for w := 0; w < jobs; w++ {
		workers.Add(1)
		go func() {
			defer workers.Done()
			for job := range work {
				results <- outcome{job.fitDigits, measure(ScaleRequest{
					Label: t.label, Stem: t.stem, Script: t.script,
					CaseName: "slope_n_" + job.fitDigits,
					InPath:   job.inPath, OutPath: job.outPath,
					Repeat: repeat, AllocRepeat: 0, Timeout: timeout,
				})}
			}
		}()
	}
	go func() {
		for _, job := range scaleJobs {
			work <- job
		}
		close(work)
	}()
	var perScale []outcome
	for range scaleJobs {
		perScale = append(perScale, <-results)
	}
	workers.Wait()
	close(results)
	sort.Slice(perScale, func(i, j int) bool {
		return cmpDigits(perScale[i].fitDigits, perScale[j].fitDigits) < 0
	})
	for _, s := range perScale {
		entry.Scales = append(entry.Scales, Digits(s.fitDigits))
	}
	for _, s := range perScale {
		if !s.result.Ok {
			entry.Error = strPtr(s.result.Error)
			entry.ScaleFailed = true
			return entry, nil
		}
	}
	var ns []float64
	var medians []float64
	for _, s := range perScale {
		entry.Points = append(entry.Points, Point{
			N:         Digits(s.fitDigits),
			CpuMedian: pyjson.FloatNum(s.result.CpuMedian),
			CpuMin:    pyjson.FloatNum(s.result.CpuMin),
			CpuMax:    pyjson.FloatNum(s.result.CpuMax),
		})
		ns = append(ns, digitsFloat(s.fitDigits))
		medians = append(medians, s.result.CpuMedian)
	}
	fit := FitPowerLaw(ns, medians)
	entry.Fit = fit
	entry.Ok = fit.Ok
	if !fit.Ok {
		entry.Error = strPtr(fmt.Sprintf("slope fit skipped: %s (%s)", *fit.Reason, *fit.Detail))
	}
	return entry, nil
}

// cmpDigits compares canonical digit strings numerically.
func cmpDigits(a, b string) int {
	a = strings.TrimLeft(a, "0")
	b = strings.TrimLeft(b, "0")
	if len(a) != len(b) {
		if len(a) < len(b) {
			return -1
		}
		return 1
	}
	return strings.Compare(a, b)
}

// digitsFloat converts exact digits to float64 with a single
// rounding, like Python float(int).
func digitsFloat(digits string) float64 {
	f, _, err := big.ParseFloat(digits, 10, 256, big.ToNearestEven)
	if err != nil {
		return 0
	}
	v, _ := f.Float64()
	return v
}

func strPtr(s string) *string { return &s }
