# Recursive Generic Helper + Constructed Generic Struct Loses Its Instance

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** a recursive generic helper may append a constructed generic struct to an
array and still resolve `Appendable` when called from another module.

**Repro:** `docs/superpowers/repros/recursive-append-generic-struct*.mdr`.

```
Code Generator Error: Unresolved method 'Appendable::Append': no matching concrete instance found.
```

This is a **clean error**, not a crash — unlike the other defects in this family.
It blocks a rewrite rather than corrupting a result.

---

## Both ingredients are required

Established one axis at a time. All four variants are cross-module.

| helper | element | result |
|---|---|---|
| non-recursive | constructed `Pair<K,V>` | works |
| non-recursive, matches a generic union | constructed `Pair<K,V>` | works |
| **recursive** | plain type parameter `K` | works — prints `2` |
| **recursive** | **constructed `Pair<K,V>`** | **fails** |

So neither recursion nor a generic-struct element is sufficient alone. The
combination is.

## It explains exactly what the prelude does and does not tolerate

While converting `Map` and `Set` off loops (`7b935bb`, `09b9df7`):

- `SetToArray` / `SetAppendFrom` — recursive, element is a plain `T` — converted.
- `MapKeys` / `MapValues` — recursive, elements are plain `K` and `V` — converted.
- `MapEntries` — recursive, element is `Entry<K,V>` — **left as a loop**, because
  it is exactly this shape. The identical `ArrayUtil::Append(entries, Entry(key, value))`
  written inline in `MapEntries` resolves.

`MapEntries` is the only prelude function still on a counter loop for this reason.

## Where to look

The specialization of `AppendRecStruct` must resolve
`Appendable<Array<Pair<K,V>>>` at the concrete instantiation. It manages that
without recursion, so the failure is likely in how a *self-referential*
specialization records or re-resolves its constraints — the recursive call
re-enters specialization for the same signature, and the second entry may see a
different constraint environment than the first.

`ResolveInstanceNameForTypeArgs` and `SpecializeGenericFunction` are the places to
instrument. Compare the constraint set on the first entry against the recursive
one.

**Hypothesis only.** The score on mechanism guesses in this project is poor;
instrument before changing anything. What has worked repeatedly here is
(1) suppressing one candidate emission at a time and (2) printing the compiler's
own tables — names against sizes, indices against indices — rather than reasoning
about them.

## Tasks

- [ ] **Task 1: Instrument** the constraint set seen by `AppendRecStruct`'s
  specialization on first entry versus on the recursive call.
- [ ] **Task 2: Fix.**
- [ ] **Task 3: Cover it.** A cross-module test with both variants, so the plain
  case guards against over-correction.
- [ ] **Task 4: Convert `MapEntries`** to `MapAppendEntries`, completing the
  loop removal in `Map`.

## Done when

1. The repro prints `2` then `2`.
2. `MapEntries` is a recursive helper like its neighbours.
3. Covered by a test that bites; suite green at 382/382 plus the new case.


---

## Task 1 findings — 2026-09-10, instrumented

### The argument's type still carries type variables

`CodeGenerator::ResolveConcreteTypeclassMethodName` (`:5980`) matches the call's
actual argument types against the registered instance type arguments. Printing
the actual types on every `Appendable::Append` call, for the working and failing
programs:

```
non-recursive (works):  [Array<Pair<Int, Text>>]  [Pair<Int, Text>]
recursive    (fails):   [Array<Pair<Int, Text>>]  [Pair<T5, T6>]
recursive, plain (ok):  [Array<Int>]              [Int]
```

Argument 0 is concrete in every case. Argument 1 — the constructed
`Pair(key, value)` — is concrete without recursion and carries **type variables**
with it.

### Why that argument is the one that breaks

`GetConcreteTypeForExpression` (`:6273`) has two paths:

- a `NameAccess` whose name is in `m_param_type_map` returns the parameter's
  concrete type — this is why argument 0, the `out` parameter, is always right;
- anything else takes the node's recorded type and runs
  `SubstituteGenericTypes(type, m_generic_type_substitution)`.

That substitution is keyed by **generic parameter name** (`K`, `V`). After the
recursive call freshens the signature, the body's `Construct` node records
`Pair<T5, T6>` — **type variables**, not named generic params — so the map matches
nothing and the type passes through unchanged. Instance selection then looks for
`Appendable<Pair<T5, T6>>` and finds none.

This also explains the whole table in the section above: an element that is a bare
type parameter reaches instance selection through a `NameAccess` and the
`m_param_type_map` path, so it is concrete regardless of recursion. Only a
*constructed* element goes through the substitution path, and only recursion puts
type variables there.

### Candidate mechanism, unconfirmed

Why recursion leaves type variables in a body node has **not** been established.
The plausible route is per-use `Freshen` of the recursive callee's signature —
noted as an inherited hazard in `2026-09-02-generic-lambdas.md` — combined with
`Unify` writing through the shared pointer (`*left = *right_subst;`), which would
let a call-site unification overwrite a type node the body shares.

**Do not act on that paragraph.** Instrument whether the `Construct` node's
`m_type_data` is the same object the call-site unification touches before changing
anything.

### Two shapes of fix, to choose between

1. **Keep the body's types in terms of generic params**, so the existing
   name-keyed substitution keeps working. Correct if the freshening is the defect.
2. **Make `GetConcreteTypeForExpression` handle a `Construct` by rebuilding its
   type from its arguments' concrete types.** Narrower and local, but it special-
   cases one node kind and leaves the underlying type variables in place for
   anything else that reads them.

Option 1 is the real fix if the mechanism above holds; option 2 is a contained
workaround. Establish the mechanism first.
