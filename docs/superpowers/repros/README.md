# Reduced reproductions

Minimal programs that trigger known compiler defects. Each is reduced until it
cannot reduce further — this project has twice attributed a symptom to the most
complex thing in view and been wrong, so a repro here is the *smallest* program
that still fails, not the one it was found in.

These are **not** part of the test suite. They fail by design.

| files | defect | status |
|---|---|---|
| `imported-struct-construction*.mdr` | constructing a struct imported from another module crashes the type checker | **fixed** `a281843`; kept as the reduction record |
| `generic-call-in-instance*.mdr` | a generic function called from an instance method returns 0 for its parameters, across three modules | **fixed** `d0d0cf4`; kept as the reduction record |
| `plain-call-in-generic*.mdr` | a generic function calling a non-generic helper crashes the VM when the caller is in a third module | **fixed** — `47275f9` for exported helpers, `0871c4b` for private ones |
| `recursive-append-generic-struct*.mdr` | a recursive generic helper appending a constructed generic struct fails to resolve its typeclass instance across modules | **fixed** `eb05275`; kept as the reduction record |
