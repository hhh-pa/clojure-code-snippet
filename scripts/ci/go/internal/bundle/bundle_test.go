package bundle

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestBuildRoundTrip(t *testing.T) {
	if _, err := exec.LookPath("gpg"); err != nil {
		t.Skip("gpg not installed")
	}
	if _, err := exec.LookPath("tar"); err != nil {
		t.Skip("tar not installed")
	}
	root := t.TempDir()
	shared := filepath.Join(root, "resource", "shared")
	if err := os.MkdirAll(filepath.Join(shared, "sort"), 0o755); err != nil {
		t.Fatal(err)
	}
	payload := []byte("hello bundle\n")
	if err := os.WriteFile(filepath.Join(shared, "sort", "cases.txt"), payload, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(root, "tests"), 0o755); err != nil {
		t.Fatal(err)
	}
	res, err := Build(root, shared, "")
	if err != nil {
		t.Fatal(err)
	}
	if !res.Fresh || len(res.Passphrase) != 64 {
		t.Fatalf("fresh passphrase: %q", res.Passphrase)
	}
	if res.Bytes <= 0 {
		t.Fatal("empty bundle")
	}
	if !strings.Contains(res.Report(), "FRESH PASSPHRASE") {
		t.Fatalf("report:\n%s", res.Report())
	}
	// Decrypt with the printed passphrase and compare the payload.
	work := t.TempDir()
	passFile := filepath.Join(work, "pass.txt")
	os.WriteFile(passFile, []byte(res.Passphrase), 0o600)
	dec := exec.Command("gpg", "--batch", "--yes", "--pinentry-mode", "loopback",
		"--passphrase-file", passFile, "-d", res.Out)
	ball, err := dec.Output()
	if err != nil {
		t.Fatalf("decrypt: %v", err)
	}
	ballPath := filepath.Join(work, "bundle.tar.xz")
	os.WriteFile(ballPath, ball, 0o644)
	untar := exec.Command("tar", "-xJf", ballPath, "-C", work)
	if out, err := untar.CombinedOutput(); err != nil {
		t.Fatalf("untar: %v %s", err, out)
	}
	got, err := os.ReadFile(filepath.Join(work, "shared", "sort", "cases.txt"))
	if err != nil || string(got) != string(payload) {
		t.Fatalf("round trip: %q %v", got, err)
	}
	// Existing-passphrase rebuild reuses the secret (no fresh block).
	res2, err := Build(root, shared, passFile)
	if err != nil {
		t.Fatal(err)
	}
	if res2.Fresh || strings.Contains(res2.Report(), "FRESH PASSPHRASE") {
		t.Fatalf("expected reuse:\n%s", res2.Report())
	}
}

func TestBuildErrors(t *testing.T) {
	root := t.TempDir()
	if _, err := Build(root, filepath.Join(root, "missing"), ""); err == nil {
		t.Fatal("missing shared dir accepted")
	}
	shared := filepath.Join(root, "shared")
	os.MkdirAll(shared, 0o755)
	if _, err := Build(root, shared, filepath.Join(root, "no-pass")); err == nil {
		t.Fatal("missing passphrase file accepted")
	}
}
