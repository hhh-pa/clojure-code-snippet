// Package measure ports tools/measure.py: load-independent performance
// measurement for the Clojure snippet repo, plus the gungraun-style
// callgrind instruction backend and the AOT run mode. Report JSON,
// BMF JSON, and step-summary tables stay byte-identical to Python
// for identical runs (timestamps/host excluded); new signals appear
// only under the new flags.
package measure

// TimeFormat is the GNU time -f layout (TIME_FORMAT).
const TimeFormat = "wall=%e user=%U sys=%S maxrss_kb=%M cpu_pct=%P major_faults=%F minor_faults=%R"

// PerfEvents are the user-space counters (PERF_EVENTS).
const PerfEvents = "instructions:u,cycles:u"

// ToolVersion tags the report; 3 adds the optional callgrind/aot fields.
const ToolVersion = 3

// DefaultCommandTemplate runs snippets on the JVM.
const DefaultCommandTemplate = "java -cp {cp} clojure.main {script}"

// EpsilonFlags make heap-used-at-exit equal total allocation.
var EpsilonFlags = []string{
	"-XX:+UnlockExperimentalVMOptions",
	"-XX:+UseEpsilonGC",
	"-Xms2g",
	"-Xmx2g",
}

// AllocRepeatDefault mirrors ALLOC_REPEAT_DEFAULT.
const AllocRepeatDefault = 3

// CallgrindRepeatDefault mirrors the one-shot gungraun default.
const CallgrindRepeatDefault = 1

// AllocExempt labels skip EpsilonGC runs (see measure_case).
var AllocExempt = map[string]bool{"search/beam_search": true}
