package measure

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// Stub GNU time: consumes leading -f FMT and -o FILE flags, writes
// a fixed valid time line, then execs the remaining command.
const stubTime = `#!/bin/sh
out=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift 2;;
    -f) shift 2;;
    *) break;;
  esac
done
printf 'wall=1.500 user=1.000 sys=0.500 maxrss_kb=4321 cpu_pct=99%% major_faults=0 minor_faults=7' > "$out"
exec "$@"
`

// Stub Pin launcher: consumes -t TOOL and -o OUT, writes a fixed
// Count line, then execs the command after --.
const stubPin = `#!/bin/sh
out=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift 2;;
    -t) shift 2;;
    --) shift; break;;
    *) break;;
  esac
done
printf 'Count 777\n' > "$out"
exec "$@"
`

// Stub snippet: drains stdin like a real case runner, prints fixed
// stdout.
const stubInner = `#!/bin/sh
cat > /dev/null
printf 'want-stdout'
`

func writeStub(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(body), 0o755); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestRunInstrumented(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("stub scripts need /bin/sh")
	}
	dir := t.TempDir()
	timeBin := writeStub(t, dir, "time", stubTime)
	pinBin := writeStub(t, dir, "pin", stubPin)
	inner := writeStub(t, dir, "inner.sh", stubInner)
	input := filepath.Join(dir, "in.txt")
	if err := os.WriteFile(input, []byte("some-input"), 0o644); err != nil {
		t.Fatal(err)
	}
	stdout, insns, rss, runErr := RunInstrumented(timeBin, pinBin, filepath.Join(dir, "tool.so"),
		[]string{inner}, input, 30)
	if runErr != "" {
		t.Fatalf("runErr: %s", runErr)
	}
	if string(stdout) != "want-stdout" {
		t.Fatalf("stdout: %q", stdout)
	}
	if insns != 777 {
		t.Fatalf("insns: %d", insns)
	}
	if rss != 4321 {
		t.Fatalf("rss: %d", rss)
	}
}

func TestRunInstrumentedBadCount(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("stub scripts need /bin/sh")
	}
	dir := t.TempDir()
	timeBin := writeStub(t, dir, "time", stubTime)
	badPin := writeStub(t, dir, "pin", `#!/bin/sh
out=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift 2;;
    -t) shift 2;;
    --) shift; break;;
    *) break;;
  esac
done
printf 'no counts here\n' > "$out"
exec "$@"
`)
	inner := writeStub(t, dir, "inner.sh", stubInner)
	input := filepath.Join(dir, "in.txt")
	if err := os.WriteFile(input, []byte("x"), 0o644); err != nil {
		t.Fatal(err)
	}
	_, _, _, runErr := RunInstrumented(timeBin, badPin, filepath.Join(dir, "tool.so"),
		[]string{inner}, input, 30)
	if !strings.HasPrefix(runErr, "pin count unreadable:") {
		t.Fatalf("runErr: %q", runErr)
	}
}

func TestRunInstrumentedExit(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("stub scripts need /bin/sh")
	}
	dir := t.TempDir()
	timeBin := writeStub(t, dir, "time", stubTime)
	pinBin := writeStub(t, dir, "pin", stubPin)
	failing := writeStub(t, dir, "fail.sh", "#!/bin/sh\necho boom >&2\nexit 3\n")
	input := filepath.Join(dir, "in.txt")
	if err := os.WriteFile(input, []byte("x"), 0o644); err != nil {
		t.Fatal(err)
	}
	_, _, _, runErr := RunInstrumented(timeBin, pinBin, filepath.Join(dir, "tool.so"),
		[]string{failing}, input, 30)
	if runErr != "exit 3: boom" {
		t.Fatalf("runErr: %q", runErr)
	}
}
