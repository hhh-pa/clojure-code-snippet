//go:build !windows

package snipcore

import (
	"os/exec"
	"syscall"
)

// setProcessGroup makes the child a group leader so KillProcessGroup
// can reap valgrind/perf/time grandchildren on timeout. (The Python
// tools kill only the direct child and leak orphaned JVMs;
// see KillProcessGroup.)
func setProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}

// KillProcessGroup kills the whole group of a timed-out child.
// Errors (already dead) are ignored by design.
func KillProcessGroup(pid int) {
	_ = syscall.Kill(-pid, syscall.SIGKILL)
}
