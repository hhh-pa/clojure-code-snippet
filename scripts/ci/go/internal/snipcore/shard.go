package snipcore

import (
	"errors"
	"strconv"
	"strings"
)

// ParseTestShard parses the 1-based I/N --shard of run_tests.py,
// returning the 0-based index and total. Messages match Python exactly.
func ParseTestShard(text string) (index, total int, err error) {
	picked, total, ok := splitShard(text)
	if !ok {
		return 0, 0, errors.New("bad --shard '" + text + "': want I/N")
	}
	index = picked - 1
	if total < 1 || index < 0 || index >= total {
		return 0, 0, errors.New("bad --shard '" + text + "': want 1 <= I <= N")
	}
	return index, total, nil
}

// ParseMeasureShard parses the 0-based K/N --shard of measure.py.
// Messages match Python exactly.
func ParseMeasureShard(text string) (index, total int, err error) {
	picked, total, ok := splitShard(text)
	if !ok {
		return 0, 0, errors.New("bad --shard '" + text + "': want K/N")
	}
	if total < 1 || picked < 0 || picked >= total {
		return 0, 0, errors.New("bad --shard '" + text + "': need 0 <= K < N")
	}
	return picked, total, nil
}

func splitShard(text string) (picked, total int, ok bool) {
	parts := strings.Split(text, "/")
	if len(parts) != 2 {
		return 0, 0, false
	}
	// Python int() tolerates surrounding whitespace and signs.
	picked, err1 := parsePyInt(parts[0])
	total, err2 := parsePyInt(parts[1])
	if err1 != nil || err2 != nil {
		return 0, 0, false
	}
	return picked, total, true
}

func parsePyInt(text string) (int, error) {
	return strconv.Atoi(strings.TrimSpace(text))
}
