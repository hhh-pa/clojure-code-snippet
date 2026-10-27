#!/usr/bin/env bash
# Build (or rebuild) the encrypted test-resource bundle from the recipes
# superproject, and print the rotation instructions.
#
# Usage: tools/build_bundle.sh <recipes resource/shared dir> [passphrase-file]
#
# With no passphrase file, a fresh 64-char passphrase is generated and
# printed ONCE (save it, then set it as the RESOURCES_PASSPHRASE secret:
#   gh secret set RESOURCES_PASSPHRASE --repo m-hu/clojure-code-snippet
# pasting the value when prompted).
# With a passphrase file, the bundle is re-encrypted under the existing
# secret value (use after resource/shared changes, no rotation needed).
#
# Output: tests/resources.tar.xz.asc (tar.xz, AES-256, ASCII-armored).
set -euo pipefail

shared_dir="${1:?usage: build_bundle.sh <resource/shared dir> [passphrase-file]}"
pass_file="${2:-}"
repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out="$repo_root/tests/resources.tar.xz.asc"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

if [ -n "$pass_file" ]; then
  pass_opt=(--passphrase-file "$pass_file")
else
  openssl rand -base64 48 | tr -d '\n' > "$work/pass.txt"
  pass_opt=(--passphrase-file "$work/pass.txt")
fi

tar -cJf "$work/bundle.tar.xz" -C "$(dirname "$shared_dir")" "$(basename "$shared_dir")"
gpg --batch --yes --pinentry-mode loopback "${pass_opt[@]}" \
  --symmetric --cipher-algo AES256 --armor -o "$out" "$work/bundle.tar.xz"

echo "wrote $out ($(stat -c%s "$out") bytes)"
if [ -z "$pass_file" ]; then
  echo "FRESH PASSPHRASE (set as RESOURCES_PASSPHRASE, then destroy this copy):"
  cat "$work/pass.txt"
  echo
fi
