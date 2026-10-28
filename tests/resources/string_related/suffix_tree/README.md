# Committed sample cases (visible by design)

Unlike the rest of `tests/resources/` (extracted at CI time from the
encrypted bundle), this directory is committed to git: force-added
despite the `tests/resources/` gitignore rule.

Why: the test harness fails a snippet with no resources by design,
and extending the encrypted bundle needs the resources passphrase.
These self-generated inputs (with outputs produced by the landed
suffix-automaton reference) leak nothing about the hidden bundle
and give the new `suffix_tree` path real CI coverage.

If official encrypted cases are ever added for this path, the
bundle extract will simply overlay this directory.
