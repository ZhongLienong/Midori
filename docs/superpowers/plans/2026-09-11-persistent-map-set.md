# Persistent `Map` and `Set` — the last of the prelude's mutation

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `MapInsert` and `SetInsert` return a new collection instead of mutating
one, so the prelude contains no assignment at all.

**Architecture:** spec §11 calls for hash array mapped tries. The current
collections are open-addressed tables over a flat `Array` with in-place bucket
writes, which cannot be made persistent without copying the whole table per
insert. A HAMT shares all untouched subtrees, so an insert copies one path.

**Tech Stack:** C++23 compiler; the work itself is `.mdr` prelude code.

---

## Why this is the last piece

Everything else in §11 is done. Prelude assignments stand at **21**, loops at
**5**, and all of them are in `Collections/Map.mdr`, `Collections/Set.mdr` and
`Collections/OpenAddressing.mdr`:

| file | assignments |
|---|---|
| `Collections/Map.mdr` | 10 |
| `Collections/Set.mdr` | 9 |
| `Collections/OpenAddressing.mdr` | 2 |

`List.mdr`, `ArrayUtil.mdr` and every other prelude module are already free of
both. With these three converted, deleting assignment from the language becomes a
grammar change rather than a rewrite.

## The primitives are all present — checked, not assumed

- `Hashable::Hash : fn(T) -> Int` for `Int`, `Bool`, `Byte`, `Word` and `Text`
  (`MIDORI_FFI_HashText`).
- Bitwise `<<`, `>>`, `&`, `|`, `^`, `~` all lex and type-check.
- Array comprehensions with variable bounds, `[expr for i in a..1..b]`.
- Recursive generic helpers calling private non-generic helpers across modules —
  fixed on 2026-09-11 (`eb05275`, `0871c4b`). This plan depends on both; before
  2026-09-11 most of it could not have been written.

## The shape

```
type Node < K, V > =
      Empty
    | Leaf(Int, K, V)                      // hash, key, value
    | Collision(Int, Array < Entry < K, V > >)   // hash, entries
    | Branch(Int, Array < Node < K, V > >);      // bitmap, children

type Map < K, V > = { root: Node < K, V >, count: Int };
```

Five bits of hash per level, 32-way branching. A child's slot in the sparse array
is the popcount of the bitmap below its bit.

`Set<T>` is `Map<T, Unit>` in structure but should stay its own type — spec §5
wants `Hashable<Text>` not to collapse, and a `Set` that is a `Map` alias would
leak the same way.

## What must be built first

The HAMT copies arrays; the prelude currently has no value-returning array
helpers. `ArrayUtil::Append`, `Prepend` and `Extend` all mutate.

Add to `ArrayUtil.mdr`, all comprehensions, none mutating:

```
def WithAppended  = fn < T >(array: Array < T >, value: T) -> Array < T >
def WithReplaced  = fn < T >(array: Array < T >, index: Int, value: T) -> Array < T >
def WithInserted  = fn < T >(array: Array < T >, index: Int, value: T) -> Array < T >
def WithRemoved   = fn < T >(array: Array < T >, index: Int) -> Array < T >
```

`WithAppended` and `WithRemoved` are one comprehension each. `WithReplaced` needs
a conditional element; `WithInserted` shifts indices past the insertion point.
**Write and test these on their own before any HAMT work** — every trie operation
is built from them, and a bug here will look like a trie bug.

## Known traps

1. **A `default =>` arm makes a `match` yield `Unit`.** An arm calling something
   that returns a value must discard it in a block, or the arms disagree and the
   match will not type. This bit the `Set` conversion.
2. **Counting assignments with a regex matches `default =>` arms.** Earlier
   figures of 62, 59 and 52 were inflated by this. Use
   `'^[[:space:]]*[A-Za-z_][A-Za-z0-9_]*([.]\\w+|\\[[^]]*\\])*[[:space:]]*([-+*/%|&^]|<<|>>)?=[^=>]'`.
3. **`SubstituteTypeParams` with an empty map is not the identity** — since
   `e730762` it rebuilds a `StructType` with `m_generic_params` cleared.
4. **This checkout is shared.** `git status` first; stage by explicit pathspec,
   never `git add -A`. Other sessions leave files here.

**Build and test — PowerShell, NOT Git Bash:**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **386/386** and **1024 assertions / 176 cases**, both green as of
2026-09-11. Do **not** use `build/out/Midori.exe` — a stale April build.

---

## Task 1: Immutable array helpers

**Files:** `MidoriPrelude/ArrayUtil.mdr`, `test/prelude/success/array_util_with.mdr`

- [ ] **Step 1: Write the tests first**

Cover each helper on an empty array, at index 0, at the last index, and past the
end. `WithRemoved` on a single-element array must give an empty array, not a
panic.

- [ ] **Step 2: Implement the four helpers as comprehensions**

- [ ] **Step 3: Prove the original is untouched**

`def a = [1, 2]; def b = WithAppended(a, 3);` must leave `#a == 2`. This is the
property the whole plan rests on; test it explicitly rather than assuming it.

- [ ] **Step 4: Commit**

---

## Task 2: Popcount and the bit helpers

**Files:** `MidoriPrelude/Collections/Bits.mdr` (create), tests alongside

- [ ] **Step 1: `PopCount : fn(Int) -> Int`**

Recursive, not a loop. Verify against known values — 0, 1, 0xFFFF, and a value
with bits above 32 if `Int` is 64-bit. **Establish `Int`'s width first**; the
branching factor and mask depend on it.

- [ ] **Step 2: `HashChunk : fn(Int, Int) -> Int`** — five bits at a given depth.

- [ ] **Step 3: Commit**

---

## Task 3: The trie, read-only

- [ ] **Step 1: `Node`, `MapGet`, `MapContains`, `MapCount`**

Build a tree by hand in a test, and read from it. No insertion yet — this
isolates lookup from construction.

- [ ] **Step 2: Commit**

---

## Task 4: Insert and remove, persistent

- [ ] **Step 1: `MapWith`** — returns a new map. Handle: empty, leaf collision at
  the same hash, leaf split into a branch, and branch update.
- [ ] **Step 2: `MapWithout`**, including collapsing a branch that drops to one
  child.
- [ ] **Step 3: Persistence test.** Insert into `m1` to get `m2`; assert `m1` is
  unchanged in count and contents. **This is the acceptance criterion for the
  whole plan.**
- [ ] **Step 4: Commit**

---

## Task 5: Iteration

- [ ] **Step 1: `Iterable` instance** over the trie, threading state as
  `Option<(Entry, Iter)>`. The iterator carries a path; with no mutation it must
  rebuild the path per step, which is why the signature changed in `318cdeb`.
- [ ] **Step 2: Every entry visited exactly once**, over a map large enough to
  branch — at least 40 keys, so the trie is more than one level deep.
- [ ] **Step 3: Commit**

---

## Task 6: `Set`, then retire the old collections

- [ ] **Step 1: `Set` over the same node type.**
- [ ] **Step 2: Delete `OpenAddressing.mdr`** and the old table code.
- [ ] **Step 3: Recount.** Prelude assignments and loops should both be **0**.
- [ ] **Step 4: Commit**

---

## Done when

1. `MapWith` and `SetWith` return new collections; the originals are unchanged.
2. No prelude file contains an assignment or a `loop`.
3. Every existing `hashmap` and `hashset` test passes, rewritten to the
   value-returning API.
4. Iteration visits every entry exactly once on a multi-level trie.
5. Suite green at 386/386 plus the new cases.

## Deliberately not decided here

Whether the mutating `MapInsert` / `SetInsert` names survive as wrappers. That is
a library-surface question, and spec §1's "one way to do one thing" argues
against keeping both — but it is a separate decision from the data structure, and
this plan does not make it.
