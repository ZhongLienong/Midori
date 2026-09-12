# `Binding 'x' is never read` false positive at two levels of closure nesting

Found 2026-09-11 while migrating `test/closure` off assignment; the rewritten
`nested_and_recursion.mdr` drew a warning for a binding its next line reads.

## Repro

```midori
// warns: "Binding 'total' is never read" — wrongly
def c = fn(p : Int) -> fn(Int) -> fn() -> Int =>
	fn(v : Int) -> fn() -> Int =>
	{
		def total = v + 1;
		fn() -> Int => total
	};

// does not warn — same shape, one level shallower
def d = fn(v : Int) -> fn() -> Int =>
{
	def total = v + 1;
	fn() -> Int => total
};
```

Both evaluate correctly (`4` and `4`). The defect is diagnostic-only: a wrong
warning, never a wrong value.

## Trigger

Closure nesting depth alone. A local `def` read only from a nested closure is
handled correctly one level deep and reported never-read two levels deep.

Ruled out by bisection, each disproved before the real cause was found:

- the local's initializer reading an outer capture — variant without it warns too
- the intermediate `def mid` binding — variant without it warns too
- `MarkRead`'s outward frame walk, which does visit every enclosing frame

## Mechanism

Two index spaces that coincide only at depth 1. From `Parser.cpp`
`ResolveQualifiedName`:

```cpp
int parent_base = (var_depth >= 1) ? m_state.m_function_base_variable_index[var_depth - 1] : 0;
int cell_index = find_result->second.m_absolute_index.value() - parent_base;
```

so `cell = absolute - base(depth - 1)`, relative to the frame **above** the
declaring one. But `UnusedLocalDiagnostic::m_active_locals` is keyed by
`m_local_index`, which is `absolute - base(depth)` — relative to the declaring
frame itself. `MarkRead` looked a cell index up in that map.

Measured, by printing both spaces for the same binding:

| case | `rel` (map key) | `cell` (lookup key) | probe |
|---|---|---|---|
| depth 1 (`d`) | 1 | 1 | hit |
| depth 2 (`c`) | 1 | 2 | miss on every frame |

At depth 1 both bases are 0, so the keys are equal by coincidence, which is why
every existing test passed.

This is the same shape as the other defects recorded here: **a recorded index and
a derived index that agree until something perturbs the sequence.** Reading the
frame walk suggested it was correct, and it was — the walk was never the bug.

## Fix

`MarkCapturedReadByName`: resolve a captured read by walking enclosing frames
outward matching the mangled lexeme, innermost live binding first. That is the
same resolution the parser performed to emit the cell, so it cannot drift from
it. Cell indices are left alone — the code generator depends on them.

## Method note

Bisecting the source shape found the *trigger* (depth) but not the cause; three
plausible mechanisms read off the source were all wrong. Printing the two index
values side by side identified it immediately.
