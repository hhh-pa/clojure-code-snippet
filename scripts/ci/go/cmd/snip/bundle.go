package main

import (
	"errors"
	"fmt"
	"os"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/bundle"
	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

const bundleUsage = `Build (or rebuild) the encrypted test-resource bundle from the recipes
superproject, and print the rotation instructions.

Usage: snip bundle <resource/shared dir> [passphrase-file]

With no passphrase file, a fresh 64-char passphrase is generated and
printed ONCE (save it, then set it as the RESOURCES_PASSPHRASE secret).
With a passphrase file, the bundle is re-encrypted under the existing
secret value (use after resource/shared changes, no rotation needed).

Output: tests/resources.tar.xz.asc (tar.xz, AES-256, ASCII-armored).
`

func runBundle(argv []string) int {
	if len(argv) == 1 && (argv[0] == "-h" || argv[0] == "--help") {
		fmt.Print(bundleUsage)
		return 0
	}
	positionals, err := parseFlags("bundle", argv, flagSpec{})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(bundleUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if len(positionals) < 1 || len(positionals) > 2 {
		fmt.Fprintln(os.Stderr, "snip bundle: usage: snip bundle <resource/shared dir> [passphrase-file]")
		return 2
	}
	shared := positionals[0]
	passFile := ""
	if len(positionals) == 2 {
		passFile = positionals[1]
	}
	res, err := bundle.Build(snipcore.RepoRoot(), shared, passFile)
	if err != nil {
		fmt.Fprintln(os.Stderr, "snip bundle:", err)
		return 1
	}
	fmt.Print(res.Report())
	return 0
}
