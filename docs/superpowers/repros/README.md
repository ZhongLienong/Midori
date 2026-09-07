# Reduced reproductions

Minimal programs that trigger known compiler defects. Each is reduced until it
cannot reduce further — this project has twice attributed a symptom to the most
complex thing in view and been wrong, so a repro here is the *smallest* program
that still fails, not the one it was found in.

These are **not** part of the test suite. They fail by design.

| files | defect | status |
|---|---|---|
| `imported-struct-construction*.mdr` | constructing a struct imported from another module crashes the type checker | **fixed** `a281843`; kept as the reduction record |
