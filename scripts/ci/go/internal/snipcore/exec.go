package snipcore

import (
	"bytes"
	"errors"
	"os/exec"
	"time"
)

// TimeoutError marks a subprocess that exceeded its deadline, so
// callers can format the Python-compatible TIMEOUT message.
type TimeoutError struct {
	Seconds int
}

func (e *TimeoutError) Error() string {
	return "timeout"
}

// Run executes argv with stdin bytes on stdin, capturing stdout and
// stderr, killing the whole process group after timeoutSecs (so
// perf/time/valgrind grandchildren never outlive a timeout). It
// returns stdout, stderr, and either nil, *TimeoutError, or
// *exec.ExitError.
//
// The group kill fires from the deadline timer itself rather than
// after Wait: grandchildren inherit the capture pipes, and Wait
// blocks for pipe EOF, so killing after Wait would hang until the
// grandchildren exit on their own.
func Run(argv []string, stdin []byte, timeoutSecs int) (stdout, stderr []byte, err error) {
	cmd := exec.Command(argv[0], argv[1:]...)
	setProcessGroup(cmd)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	var outBuf, errBuf bytes.Buffer
	cmd.Stdout = &outBuf
	cmd.Stderr = &errBuf
	timer := time.AfterFunc(time.Duration(timeoutSecs)*time.Second, func() {
		if cmd.Process != nil {
			KillProcessGroup(cmd.Process.Pid)
		}
	})
	runErr := cmd.Run()
	if !timer.Stop() {
		return nil, nil, &TimeoutError{Seconds: timeoutSecs}
	}
	return outBuf.Bytes(), errBuf.Bytes(), runErr
}

// ExitCode extracts the process exit code from a Run error (0 when nil).
func ExitCode(err error) int {
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return exitErr.ExitCode()
	}
	return 0
}
