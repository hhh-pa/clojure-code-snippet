# Clojure version of the algorithms and data structures

Solutions mirror the other snippet repos: one script per problem at
`src/<topic>/<problem>.clj`, executed by the recipes test harness as
`java -cp <clojure.jar> clojure.main <script>` (stdin in, stdout out).

## Testing

`tests/run_tests.py` runs every `src/**/*.clj` script against the shared
test cases from the recipes superproject (`resource/shared`). Resources
are not committed here; GitHub Actions receives them as a compressed,
base64-encoded tarball split across `RESOURCES_B64_NNNN` secrets:

- `tools/build_secrets.sh <resource/shared dir> <chunk dir>` builds chunks
- `tools/upload_secrets.sh <chunk dir>` uploads them with `gh secret set`
- `.github/workflows/test.yml` reassembles, extracts, and runs the tests
