package measure

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
	"strings"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/pyjson"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/slope"
)

// PrintTable ports print_table: one line per result on out.
func PrintTable(out io.Writer, results []CaseResult, failures, missing []string) {
	for _, res := range results {
		tag := res.Label + "/" + res.Stem + "/" + res.Case
		if !res.Ok {
			errText := ""
			if res.Error != nil {
				errText = *res.Error
			}
			fmt.Fprintf(out, "FAIL %s\n", errText)
			continue
		}
		s := res.Summary
		if s == nil {
			fmt.Fprintf(out, "%s: ok no timed runs\n", tag)
			continue
		}
		if s.Runs == 0 {
			// Pin-only (--repeat 0): no timed signals, so
			// report the instrument counts instead of zeros.
			var parts []string
			if s.PinInstructions != nil {
				parts = append(parts, fmt.Sprintf("pin_min=%s pin_med=%s pin_max=%s",
					numPlain(s.PinInstructions.Min), numPlain(s.PinInstructions.Median), numPlain(s.PinInstructions.Max)))
			}
			if s.CallgrindInstructions != nil {
				parts = append(parts, fmt.Sprintf("cg_min=%s cg_med=%s cg_max=%s",
					numPlain(s.CallgrindInstructions.Min), numPlain(s.CallgrindInstructions.Median), numPlain(s.CallgrindInstructions.Max)))
			}
			if len(parts) == 0 {
				parts = append(parts, "no timed runs")
			}
			fmt.Fprintf(out, "%s: ok %s\n", tag, strings.Join(parts, " "))
			continue
		}
		insn := "None"
		if s.Instructions != nil {
			insn = numPlain(s.Instructions.Median)
		}
		alloc := "n/a"
		if s.AllocatedBytes != nil {
			alloc = fmt.Sprintf("%dB", s.AllocatedBytes.Min)
		}
		fmt.Fprintf(out, "%s: ok cpu_min=%.3fs cpu_med=%.3fs rss_max=%dK wall_min=%.3fs insn_med=%s alloc_min=%s\n",
			tag, s.CpuS.Min.Float64(), s.CpuS.Median.Float64(),
			s.MaxRSSKb.Max.I, s.WallS.Min.Float64(), insn, alloc)
	}
	for _, label := range missing {
		fmt.Fprintf(out, "FAIL %s: no resources found\n", label)
	}
	ok := 0
	for _, r := range results {
		if r.Ok {
			ok++
		}
	}
	fmt.Fprintf(out, "measure: %d ok, %d failed, %d missing-resource problems\n",
		ok, len(failures)-len(missing), len(missing))
}

// numPlain renders a Num the way Python str() renders its int/float:
// bare digits for ints, repr for floats (11.0 keeps its .0).
func numPlain(n pyjson.Num) string {
	if n.IsInt {
		return strconv.FormatInt(n.I, 10)
	}
	return pyjson.FloatRepr(n.F)
}

// SlopeFileLines ports slope_file_lines.
func SlopeFileLines(complexity map[string]slope.Entry) []string {
	if len(complexity) == 0 {
		return nil
	}
	lines := []string{"", "**Complexity exponents (T20):**", ""}
	for _, name := range sortedComplexity(complexity) {
		entry := complexity[name]
		if entry.Ok && entry.Fit != nil && entry.Fit.Ok {
			lines = append(lines, fmt.Sprintf("- %s: p = %.3f [%.3f, %.3f], R^2 = %.4f",
				name, entry.Fit.Slope.Float64(), entry.Fit.Lower.Float64(),
				entry.Fit.Upper.Float64(), entry.Fit.RSquared.Float64()))
			continue
		}
		errText := ""
		if entry.Error != nil {
			errText = *entry.Error
		}
		lines = append(lines, fmt.Sprintf("- %s: skipped (%s)", name, errText))
	}
	return lines
}

func sortedComplexity(complexity map[string]slope.Entry) []string {
	names := make([]string, 0, len(complexity))
	for name := range complexity {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

// AppendSummary ports append_summary: the Markdown results table,
// with the conditional cxp column and the new conditional cg_insn /
// cg_alloc columns (present only when some benchmark reports
// callgrind data).
func AppendSummary(path string, report Report, bmf BMF) error {
	showCxp := false
	showCg := false
	showCgAlloc := false
	showPin := false
	for _, m := range bmf {
		if m.ComplexityExponent != nil {
			showCxp = true
		}
		if m.CallgrindInstructions != nil {
			showCg = true
		}
		if m.CallgrindAllocatedBytes != nil {
			showCgAlloc = true
		}
		if m.PinInstructions != nil {
			showPin = true
		}
	}
	header := "| snippet | cases | cpu total (s) | peak RSS (MB) | alloc total (MB) | instructions |"
	sep := "| --- | ---: | ---: | ---: | ---: | ---: |"
	if showCg {
		header += " cg_insn |"
		sep += " ---: |"
	}
	if showCgAlloc {
		header += " cg_alloc (MB) |"
		sep += " ---: |"
	}
	if showPin {
		header += " pin_insn |"
		sep += " ---: |"
	}
	header += " wall total (s) |"
	sep += " ---: |"
	if showCxp {
		header += " cxp (p) |"
		sep += " ---: |"
	}
	cpuModel := "unknown cpu"
	if report.Host.CpuModel != nil {
		cpuModel = *report.Host.CpuModel
	}
	javaVersion := "unknown"
	if report.Runtime.JavaVersion != nil {
		javaVersion = *report.Runtime.JavaVersion
	}
	perfUsed := "no"
	if report.Runtime.PerfUsed {
		perfUsed = "yes"
	}
	allocRepeat := "n/a"
	if report.Config.AllocRepeat != nil {
		allocRepeat = fmt.Sprintf("%d", *report.Config.AllocRepeat)
	}
	ok := 0
	for _, r := range report.Results {
		if r.Ok {
			ok++
		}
	}
	lines := []string{
		"## Perf measurement (`tools/measure.py`)",
		"",
		fmt.Sprintf("- %s (%s), java: %s", report.TimestampUTC, cpuModel, javaVersion),
		fmt.Sprintf("- runtime: %s, perf counters: %s, repeat: %d, jobs: %d, alloc_repeat: %s",
			report.Runtime.Name, perfUsed, report.Config.Repeat, report.Config.Jobs, allocRepeat),
		fmt.Sprintf("- %d ok, %d failed, %d missing-resource problems",
			ok, len(report.Failures)-len(report.Missing), len(report.Missing)),
		"- Primary signals: min CPU time, max peak RSS, min allocated " +
			"bytes, median instructions (load-independent); wall time is " +
			"secondary. Compare same-machine ratios only.",
		"",
		header,
		sep,
	}
	perSnippetCases := map[string]int{}
	perSnippetWall := map[string]float64{}
	perSnippetInsn := map[string][]*pyjson.Num{}
	for _, res := range report.Results {
		if !res.Ok || res.Summary == nil {
			continue
		}
		key := res.Label + "/" + res.Stem
		perSnippetCases[key]++
		perSnippetWall[key] += res.Summary.WallS.Median.Float64()
		if res.Summary.Instructions != nil {
			median := res.Summary.Instructions.Median
			perSnippetInsn[key] = append(perSnippetInsn[key], &median)
		} else {
			perSnippetInsn[key] = append(perSnippetInsn[key], nil)
		}
	}
	for _, name := range SortedNames(bmf) {
		m := bmf[name]
		if m.CpuTime == nil || m.PeakMemory == nil {
			// Slope-only entries have no time/memory: Python
			// raises KeyError here, so fail loudly too.
			panic(fmt.Sprintf("measure: BMF entry %q has no cpu_time/peak_memory; cannot render summary", name))
		}
		parts := perSnippetInsn[name]
		insn := "n/a"
		if len(parts) > 0 {
			var nums []pyjson.Num
			complete := true
			for _, p := range parts {
				if p == nil {
					complete = false
					break
				}
				nums = append(nums, *p)
			}
			if complete {
				insn = numPlain(pyjson.Sum(nums))
			}
		}
		alloc := "n/a"
		if m.AllocatedBytes != nil {
			alloc = fmt.Sprintf("%.1f", m.AllocatedBytes.Value.Float64()/1048576)
		}
		row := fmt.Sprintf("| %s | %d | %.3f | %.1f | %s | %s |",
			name, perSnippetCases[name],
			m.CpuTime.Value.Float64(), m.PeakMemory.Value.Float64()/1048576,
			alloc, insn)
		if showCg {
			cg := "n/a"
			if m.CallgrindInstructions != nil {
				cg = numPlain(m.CallgrindInstructions.Value)
			}
			row += fmt.Sprintf(" %s |", cg)
		}
		if showCgAlloc {
			cga := "n/a"
			if m.CallgrindAllocatedBytes != nil {
				cga = fmt.Sprintf("%.1f", m.CallgrindAllocatedBytes.Value.Float64()/1048576)
			}
			row += fmt.Sprintf(" %s |", cga)
		}
		if showPin {
			pi := "n/a"
			if m.PinInstructions != nil {
				pi = numPlain(m.PinInstructions.Value)
			}
			row += fmt.Sprintf(" %s |", pi)
		}
		row += fmt.Sprintf(" %.3f |", perSnippetWall[name])
		if showCxp {
			cxp := "n/a"
			if m.ComplexityExponent != nil {
				cxp = fmt.Sprintf("%.2f", m.ComplexityExponent.Value.Float64())
			}
			row += fmt.Sprintf(" %s |", cxp)
		}
		lines = append(lines, row)
	}
	lines = append(lines, "")
	fd, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
	if err != nil {
		return fmt.Errorf("cannot append summary to %s: %v", path, err)
	}
	defer fd.Close()
	_, err = fd.WriteString(strings.Join(lines, "\n"))
	return err
}
