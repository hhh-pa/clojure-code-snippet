#!/usr/bin/env bash
# Build the GitHub Actions resource bundle from the recipes superproject.
#
# Usage: tools/build_secrets.sh <resource/shared dir> <output dir>
#
# Writes <output dir>/RESOURCES_B64_NNNN chunk files (60000 chars each,
# under GitHub's 64KB secret limit) plus a MANIFEST with the chunk count.
# The chunks together are: base64(tar.xz(shared/)). Never commit them.
set -euo pipefail

shared_dir="${1:?usage: build_secrets.sh <resource/shared dir> <output dir>}"
out_dir="${2:?usage: build_secrets.sh <resource/shared dir> <output dir>}"
chunk_chars=60000

mkdir -p "$out_dir"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

tar -cJf "$work/bundle.tar.xz" -C "$(dirname "$shared_dir")" "$(basename "$shared_dir")"
base64 -w0 "$work/bundle.tar.xz" > "$work/bundle.b64"

split -b "$chunk_chars" -d -a 4 "$work/bundle.b64" "$out_dir/RESOURCES_B64_"
count="$(ls "$out_dir"/RESOURCES_B64_* | wc -l)"
compressed="$(stat -c%s "$work/bundle.tar.xz")"
echo "$count chunks, $compressed compressed bytes" > "$out_dir/MANIFEST"
cat "$out_dir/MANIFEST"
