package measure

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"runtime"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// pinLauncher prefixes the Pin command with setarch -R on Linux:
// HotSpot's heap reserve otherwise loses an ASLR lottery against
// Pin 4.4's mappings (mmap-overlap exit 127 on ~half the runs),
// while the fixed no-ASLR layout places them deterministically
// apart. Every compared leg runs the same fixed layout, so
// layout can never bias a comparison either way.
func pinLauncher(pinBin string) []string {
	if runtime.GOOS == "linux" {
		if setarch, err := exec.LookPath("setarch"); err == nil {
			return []string{setarch, "-R", pinBin}
		}
	}
	return []string{pinBin}
}

// PinNoASLRUsable reports whether JVM pin legs can fix the
// address layout (Linux setarch present; non-Linux has no Pin
// legs to protect).
func PinNoASLRUsable() bool {
	if runtime.GOOS != "linux" {
		return true
	}
	_, err := exec.LookPath("setarch")
	return err == nil
}

// RunInstrumented runs inner once under GNU time and Intel Pin with
// the inscount tool, so one execution yields stdout plus the dynamic
// instruction count and peak RSS: the test suite instruments during
// its single correctness run instead of re-running every case under
// a separate measure phase. JVM argv gets PinJVMFlags (Pin needs
// HotSpot's cooperation with signal handlers); native argv runs
// plain. Memory is RSS only, never traced.
//
// runErr is "" on success, otherwise a human line in the same shape
// as the other runners ("open input: ...", "TIMEOUT (>Ns)",
// "exit N: ...", "pin count unreadable: ...", "time output
// unreadable: ...").
func RunInstrumented(timeBin, pinBin, tool string, inner []string, inputPath string, timeout int) (stdout []byte, insns int64, maxRSSKb int64, runErr string) {
	if len(inner) == 0 {
		return nil, 0, 0, "empty argv"
	}
	input, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, 0, 0, "open input: " + err.Error()
	}
	pinOut, err := tempFile("snip-pin-")
	if err != nil {
		return nil, 0, 0, "pin output unreadable: " + err.Error()
	}
	defer os.Remove(pinOut)
	timeOut, err := tempFile("snip-time-")
	if err != nil {
		return nil, 0, 0, "time output unreadable: " + err.Error()
	}
	defer os.Remove(timeOut)
	var pinCmd []string
	if InnerIsJava(inner) {
		pinCmd = pinLauncher(pinBin)
	} else {
		pinCmd = []string{pinBin}
	}
	pinCmd = append(pinCmd, "-t", tool, "-o", pinOut, "--", ResolveArgv0(inner[0]))
	if InnerIsJava(inner) {
		pinCmd = append(pinCmd, PinJVMFlags...)
	}
	pinCmd = append(pinCmd, inner[1:]...)
	cmd := BuildCommand(timeBin, timeOut, false, "", pinCmd)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, 0, 0, fmt.Sprintf("exit %d: %s", snipcore.ExitCode(err), snipcore.LastLine(stderr))
	}
	pinRaw, err := os.ReadFile(pinOut)
	if err != nil {
		return nil, 0, 0, "pin output unreadable: " + err.Error()
	}
	insns, err = ParsePinCount(string(pinRaw))
	if err != nil {
		return nil, 0, 0, err.Error()
	}
	timeRaw, err := os.ReadFile(timeOut)
	if err != nil {
		return nil, 0, 0, "time output unreadable: " + err.Error()
	}
	parsed, err := ParseTimeText(string(timeRaw))
	if err != nil {
		return nil, 0, 0, "time output unreadable: " + err.Error()
	}
	return stdout, insns, parsed.MaxRSSKb, ""
}
