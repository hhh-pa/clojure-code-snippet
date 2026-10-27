# Clojure version of the algorithms and data structures

Solutions mirror the other snippet repos: one script per problem at
`src/<topic>/<problem>.clj`, executed by the recipes test harness as
`java -cp <clojure.jar> clojure.main <script>` (stdin in, stdout out).

## Testing

`tests/run_tests.py` runs every `src/**/*.clj` script against the shared
test cases from the recipes superproject (`resource/shared`). The cases
ship as an encrypted bundle, `tests/resources.tar.xz.asc` (tar.xz,
AES-256, ASCII-armored); the passphrase is the `RESOURCES_PASSPHRASE`
repository secret, and `.github/workflows/test.yml` decrypts it in CI.
Rebuild the bundle after resource changes with
`tools/build_bundle.sh <resource/shared dir> <passphrase-file>`.
