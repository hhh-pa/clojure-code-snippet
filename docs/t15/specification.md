# T15: Rename Bencher project to clojure-code-snippet

## Part 1 — goal and acceptance criteria

Goal: the Bencher project slug/name matches the repo name.

Acceptance criteria:
- `bencher project update hhh-pa --slug clojure-code-snippet
  --name clojure-code-snippet` succeeds (needs a user JWT;
  run-scoped keys are rejected by the update endpoint).
- Same project UUID after rename (history, branches, thresholds,
  plots preserved); `project view clojure-code-snippet` works.
- Workflow `--project` flags (PR + push paths) updated in the
  same move; no other file references the old slug.
- PR CI green against the renamed project (Bencher step posts
  to the new slug), then land.

## Part 2 — technical notes

- Order matters: rename FIRST via CLI, then push/CI (CI
  against a nonexistent slug fails the report job).
- Old slug-based bencher.dev URLs in past comments may break;
  acceptable (UUID-based data is intact).
