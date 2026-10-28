# T17: Ukkonen suffix-tree variant of string_function_calculation

## Part 1 — goal and acceptance criteria

Goal: a second implementation of `string_function_calculation`
(max over distinct substrings of length x occurrences) built on
Ukkonen's online suffix-tree construction, in
`src/string_related/suffix_tree/string_function_calculation.clj`,
byte-identical to the suffix-automaton version on the same inputs,
and measured against it for speed.

Acceptance criteria:
- Real Ukkonen: online construction with active point, suffix
  links, edge compression — not an automaton in disguise, not a
  naive O(n^2) suffix trie.
- Same contract: single string token from stdin (lowercase a-z,
  n up to 1e5), decimal 64-bit answer + newline on stdout;
  byte-identical outputs to the automaton on every tested input.
- Value = max over tree nodes of string-depth x subtree leaf
  count (leaves = 1), computed by iterative post-order (tree
  depth can be O(n): no deep recursion anywhere).
- Differential automaton-vs-Ukkonen passes on: random strings,
  all-same-char, periodic, adversarial (e.g. Fibonacci words,
  many distinct substrings), empty/single-char edges, and n=1e5
  scale inputs within the 300s timeout.
- `tools/measure.py` before/after on shared fixtures; honest
  numbers in Part 3 (the automaton is already linear — ship
  for algorithmic diversity, report the measured delta either
  way). Reflection-clean, no per-node allocation in hot paths.
- New Bencher benchmark auto-tracked; CI test job is expected
  to report missing-resource for the new path (no encrypted
  cases; adding them needs the resources passphrase).

## Part 2 — technical notes

- Alphabet: lowercase a-z (children as per-node int[26] or
  equivalent; mirror the automaton's assumptions — inputs are
  lowercase by contract).
- Node budget O(n): at most 2n nodes; int-array based links,
  edge (start, end) indices, suffix links; global end trick
  for leaf edges (open-ended via shared end reference — as an
  int-array cell or equivalent, not per-edge updates).
- Occurrences: leaf = 1 (each suffix ends in exactly one leaf
  subtree... precisely: occ(node) = number of leaf descendants
  = number of end positions, computed bottom-up iteratively).
- Answer init: n (whole string occurs once) — same as automaton.
- TDD: byte-compare against the automaton from the first
  running version; grow the corpus before benchmarking.
- Driver: same shape as the automaton file (re-seq first token,
  blank-input skip, StringBuilder/print output).
