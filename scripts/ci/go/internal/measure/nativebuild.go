package measure

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// NativeBuildCache compiles each script once with a native AOT
// compiler (--build-template, e.g. `clojure-rs build {script} -o
// {out}`) into a content-hash cache, and runs the produced binary as
// the measured command (issue #66). The build happens before any
// timed or instrumented run of the case, so compile time never
// reaches a metric; a failed build fails every case of the script
// loudly, like an output mismatch.
//
// The cache key covers the compiler's own bytes (its fingerprint),
// the build template, the label/stem, and the script text, so a new
// compiler revision or a changed snippet always rebuilds.
type NativeBuildCache struct {
	Template    string // holes: {script} {stem} {out}
	CacheDir    string
	Fingerprint string // NativeFingerprint of the compiler
	Timeout     int

	mu    sync.Mutex
	built map[string]string
	errs  map[string]error
	calls map[string]*nativeCall
}

type nativeCall struct {
	done chan struct{}
	bin  string
	err  error
}

// nativeMarker versions the cache layout.
const nativeMarker = "native-v1\n"

// ExpandBuildTemplate fills the {script}/{stem}/{out} holes (same
// escapes and errors as ExpandTemplate) and splits the argv.
func ExpandBuildTemplate(template, script, stem, out string) ([]string, error) {
	expanded, err := expandHoles(template, map[string]string{
		"script": script, "stem": stem, "out": out,
	})
	if err != nil {
		return nil, fmt.Errorf("bad build template: %v", err)
	}
	argv, err := SplitCommand(expanded)
	if err != nil {
		return nil, fmt.Errorf("bad build template: %v", err)
	}
	if len(argv) == 0 {
		return nil, errors.New("bad build template: expands to empty argv")
	}
	return argv, nil
}

// NativeFingerprint resolves the build template's compiler (argv0,
// via PATH) and hashes its bytes, returning the resolved path and
// the hex digest.
func NativeFingerprint(template string) (string, string, error) {
	argv, err := ExpandBuildTemplate(template, "x.clj", "x", "x.out")
	if err != nil {
		return "", "", err
	}
	path, err := exec.LookPath(argv[0])
	if err != nil {
		return "", "", fmt.Errorf("build template compiler %s: %v", snipcore.PyRepr(argv[0]), err)
	}
	fd, err := os.Open(path)
	if err != nil {
		return "", "", err
	}
	defer fd.Close()
	h := sha256.New()
	if _, err := io.Copy(h, fd); err != nil {
		return "", "", err
	}
	return path, hex.EncodeToString(h.Sum(nil)), nil
}

// Get returns the built binary for a script, compiling on first use
// (concurrent requests for one script collapse into one build).
func (c *NativeBuildCache) Get(label, stem, script string) (string, error) {
	c.mu.Lock()
	if c.built == nil {
		c.built = map[string]string{}
		c.errs = map[string]error{}
		c.calls = map[string]*nativeCall{}
	}
	if bin, ok := c.built[script]; ok {
		c.mu.Unlock()
		return bin, nil
	}
	if err, failed := c.errs[script]; failed {
		c.mu.Unlock()
		return "", err
	}
	if call, running := c.calls[script]; running {
		c.mu.Unlock()
		<-call.done
		return call.bin, call.err
	}
	call := &nativeCall{done: make(chan struct{})}
	c.calls[script] = call
	c.mu.Unlock()

	bin, err := c.build(label, stem, script)

	c.mu.Lock()
	if err != nil {
		c.errs[script] = err
	} else {
		c.built[script] = bin
	}
	call.bin, call.err = bin, err
	delete(c.calls, script)
	close(call.done)
	c.mu.Unlock()
	return bin, err
}

// build compiles into <cache>/<key>/<stem> unless the slot's marker
// says it is complete. Builds land in a temp file first and are
// renamed into place, so an interrupted build never leaves a
// half-written binary behind a valid marker.
func (c *NativeBuildCache) build(label, stem, script string) (string, error) {
	raw, err := os.ReadFile(script)
	if err != nil {
		return "", err
	}
	h := sha256.New()
	for _, part := range []string{c.Fingerprint, c.Template, label, stem} {
		h.Write([]byte(part))
		h.Write([]byte{0})
	}
	h.Write(raw)
	slot := filepath.Join(c.CacheDir, hex.EncodeToString(h.Sum(nil)))
	bin, err := filepath.Abs(filepath.Join(slot, stem))
	if err != nil {
		return "", err
	}
	marker := filepath.Join(slot, "ok")
	if got, err := os.ReadFile(marker); err == nil && string(got) == nativeMarker {
		if _, err := os.Stat(bin); err == nil {
			return bin, nil
		}
	}
	if err := os.MkdirAll(slot, 0o755); err != nil {
		return "", err
	}
	tmp := bin + ".tmp"
	argv, err := ExpandBuildTemplate(c.Template, script, stem, tmp)
	if err != nil {
		return "", err
	}
	_, stderr, err := snipcore.Run(argv, nil, c.Timeout)
	if err != nil {
		os.Remove(tmp)
		if _, ok := err.(*snipcore.TimeoutError); ok {
			return "", fmt.Errorf("build TIMEOUT (>%ds)", c.Timeout)
		}
		return "", fmt.Errorf("build exit %d: %s", snipcore.ExitCode(err), lastLines(stderr, 3))
	}
	if err := os.Rename(tmp, bin); err != nil {
		return "", fmt.Errorf("no binary after build: %v", err)
	}
	if err := os.WriteFile(marker, []byte(nativeMarker), 0o644); err != nil {
		return "", err
	}
	return bin, nil
}

// lastLines keeps the final n non-empty lines of a tool's stderr.
func lastLines(b []byte, n int) string {
	var lines []string
	for _, line := range strings.Split(string(b), "\n") {
		if strings.TrimSpace(line) != "" {
			lines = append(lines, strings.TrimSpace(line))
		}
	}
	if len(lines) > n {
		lines = lines[len(lines)-n:]
	}
	return strings.Join(lines, " | ")
}
