#!/usr/bin/env bash
# Upload resource bundle chunks as GitHub Actions secrets.
#
# Usage: tools/upload_secrets.sh <chunk dir> [owner/repo]
#
# Requires the GitHub CLI (https://cli.github.com) authenticated as a user
# with admin access to the repo (`gh auth login`). Each RESOURCES_B64_NNNN
# file becomes one repository secret of the same name; the test workflow
# reassembles them in order.
set -euo pipefail

chunk_dir="${1:?usage: upload_secrets.sh <chunk dir> [owner/repo]}"
repo="${2:-m-hu/clojure-code-snippet}"

count="$(ls "$chunk_dir"/RESOURCES_B64_* | wc -l)"
echo "uploading $count secrets to $repo"
for chunk in "$chunk_dir"/RESOURCES_B64_*; do
  name="$(basename "$chunk")"
  gh secret set "$name" --repo "$repo" < "$chunk"
done
echo "done: $count secrets set on $repo"
