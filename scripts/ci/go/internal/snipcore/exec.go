package snipcore

import (
	"bytes"
	"context"
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
// stderr, killing the process after timeoutSecs. It returns stdout,
// stderr, and either nil, *TimeoutError, or *exec.ExitError.
func Run(argv []string, stdin []byte, timeoutSecs int) (stdout, stderr []byte, err error) {
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(timeoutSecs)*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	var outBuf, errBuf bytes.Buffer
	cmd.Stdout = &outBuf
	cmd.Stderr = &errBuf
	runErr := cmd.Run()
	if ctx.Err() == context.DeadlineExceeded {
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
