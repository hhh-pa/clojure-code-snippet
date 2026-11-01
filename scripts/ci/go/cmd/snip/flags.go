package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// flagSpec declares one subcommand's flags. Values land in Strs, Ints,
// and Bools; Positionals collects the rest.
type flagSpec struct {
	Strs  map[string]*string
	Ints  map[string]*int
	Bools map[string]*bool
}

// parseFlags parses argv in the argparse long-option shape the Python
// tools accept: --name value, --name=value, and the single-dash
// spellings, plus --no-name negation for default-true bools.
// Unknown flags, missing values, and bad ints are errors.
func parseFlags(sub string, argv []string, spec flagSpec) (positionals []string, err error) {
	fail := func(format string, args ...any) ([]string, error) {
		return nil, fmt.Errorf("snip "+sub+": "+format, args...)
	}
	i := 0
	for i < len(argv) {
		arg := argv[i]
		if len(arg) < 2 || arg[0] != '-' {
			positionals = append(positionals, arg)
			i++
			continue
		}
		if arg == "--" {
			positionals = append(positionals, argv[i+1:]...)
			break
		}
		name := strings.TrimLeft(arg, "-")
		var value string
		hasValue := false
		if idx := strings.IndexByte(name, '='); idx >= 0 {
			value = name[idx+1:]
			name = name[:idx]
			hasValue = true
		}
		if name == "h" || name == "help" {
			return nil, errHelp
		}
		if target, ok := spec.Bools[name]; ok {
			if hasValue {
				return fail("argument --%s: ignored explicit argument %q", name, value)
			}
			*target = true
			i++
			continue
		}
		if strings.HasPrefix(name, "no-") {
			if target, ok := spec.Bools[name[len("no-"):]]; ok {
				if hasValue {
					return fail("argument --%s: ignored explicit argument %q", name, value)
				}
				*target = false
				i++
				continue
			}
		}
		if target, ok := spec.Strs[name]; ok {
			if !hasValue {
				i++
				if i >= len(argv) {
					return fail("argument --%s: expected one argument", name)
				}
				value = argv[i]
			}
			*target = value
			i++
			continue
		}
		if target, ok := spec.Ints[name]; ok {
			if !hasValue {
				i++
				if i >= len(argv) {
					return fail("argument --%s: expected one argument", name)
				}
				value = argv[i]
			}
			n, convErr := strconv.Atoi(strings.TrimSpace(value))
			if convErr != nil {
				return fail("argument --%s: invalid int value: %q", name, value)
			}
			*target = n
			i++
			continue
		}
		return fail("unknown flag --%s", name)
	}
	return positionals, nil
}

// errHelp signals -h/--help; callers print their usage and exit 0.
var errHelp = errors.New("help")
