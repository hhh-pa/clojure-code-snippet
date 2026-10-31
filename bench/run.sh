#!/usr/bin/env bash
# Run one Clojure snippet on its generated CodSpeed input.
# Usage: bench/run.sh <key> <script>   (see bench/codspeed_bench.py)
# Classpath: $CLOJURE_CP, else every jar under .ci/jars/.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cp="${CLOJURE_CP:-}"
if [ -z "$cp" ]; then
  cp="$(printf '%s:' "$root"/.ci/jars/*.jar)"
  cp="${cp%:}"
fi
exec java -cp "$cp" clojure.main "$root/$2" < "$root/bench/inputs/$1.in" > /dev/null
