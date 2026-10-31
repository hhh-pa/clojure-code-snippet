//go:build windows

package snipcore

import (
	"os/exec"
)

// setProcessGroup is a no-op on Windows (no process groups);
// timeouts there kill only the direct child, like Python.
func setProcessGroup(cmd *exec.Cmd) {}

// KillProcessGroup is a no-op on Windows.
func KillProcessGroup(pid int) {}
