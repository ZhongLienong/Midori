# `Iterable::Next` Threads State

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `Iterable::Next : fn(Iter) -> Option<(Item, Iter)>`, so iteration returns
the next state instead of mutating the iterator.

**Architecture:** the current signature returns `Option<Item>` and every instance
mutates a field to advance. `ListIter`'s instance is literally
`iter.current = tail;`. That assignment cannot be removed while the signature
stands, so this change gates spec §11's library rewrite and, behind it, the
removal of assignment.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` suites run by the Midori CLI.

**Prerequisite, now met:** a union carrying a tuple payload could not be
constructed until `f45b410`. `Option::Some((item, next))` works now, verified by
`test/typeclass/success/stepper_threads_state_through_tuple.mdr`.

---

## Why the loop variable is still allowed to be assigned

The generated loop stores into local slots with `SET_LOCAL`. That is a VM
operation, not a language-level assignment, and it stays. What goes is user-written
mutation in prelude instances.

## Verified facts — traced 2026-09-06, do not re-derive

**The two lowering sites are symmetric.** `CodeGenerator.cpp:4959` (for-loop) and
`:5227` (array comprehension) emit the identical shape via
`EmitIterableNextCall` (`:1097`), which emits a one-argument call.

**Opcode semantics, read from the VM, not assumed:**

- `LOAD_TAG` (`VirtualMachine.cpp:2376`) pops the union, pushes every payload
  value, then pushes the tag index **on top**.
- `UNPACK_TUPLE` (`:1225`) pops the tuple and pushes elements in index order, so
  element 0 is deepest and the **last element ends on top**.
- `SET_LOCAL` reads the top of stack without popping; the existing code always
  follows it with an explicit `POP`.

**Current sequence at both sites, after the Some-tag test:**

```
LOAD_TAG                        // [item, tag]
POP                             // [item]
SET_LOCAL loop_variable_index   // [item]
POP                             // []
```

**Target sequence.** The payload is now a single tuple value:

```
LOAD_TAG                        // [tuple, tag]
POP                             // [tuple]
UNPACK_TUPLE                    // [item, next_iter]
SET_LOCAL hidden_array_index    // [item, next_iter]   iterator advances
POP                             // [item]
SET_LOCAL loop_variable_index   // [item]
POP                             // []
```

Two instructions added per site, and the order matters: the iterator is on top
because `UNPACK_TUPLE` pushes in index order. **Do not reorder these without
re-reading the VM** — an earlier plan in this project approved a `DUP`-based
sequence that crashed on first run because `DUP` copies the top rather than the
value intended.

**Files carrying an `Iterable` instance:** `MidoriPrelude/Prelude/List.mdr`,
`MidoriPrelude/Collections/Map.mdr`, `MidoriPrelude/Collections/Set.mdr`.

**Tests that exercise iteration:** `test/for_loop/`, `test/hashmap/success/iterable_map.mdr`,
`test/hashset/success/iterable_set.mdr`, `test/newtype/success/iterable_stays_nominal.mdr`,
`test/prelude/success/iterable_list.mdr`, `test/array_comprehension/`.

## Known traps

1. **`m_classes.at("")` throws, and MSVC reports it as `0xC0000409`** — which reads
   as a stack overflow and is not one. Check for an uncaught exception first.
2. **A symptom seen only in a complex program will be blamed on the complex part.**
   Both wrong diagnoses in the equality-constraint work came from this. Reduce
   until it cannot reduce further before concluding.
3. **`SubstituteTypeParams` with an empty map is not the identity** — since
   `e730762` it rebuilds a `StructType` with `m_generic_params` cleared.
4. **Field-by-field struct rebuilds are invisible to grep for the constructor.**
   Count reconstruction sites, not reference sites.

**Build and test — PowerShell, NOT Git Bash:**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **373/373** and **1024 assertions / 176 cases**, both green as of
2026-09-06. Do **not** use `build/out/Midori.exe` — a stale April build.

Checkout is shared. `git status` first; stage by explicit pathspec, never
`git add -A`. Branch `v2-expression-oriented`.

---

## Task 1: Prove the target shape works before changing anything

- [ ] **Step 1: Write a hand-rolled state-threading iterator that does not touch `Iterable`**

Define a local class with `Next : fn(S) -> Option<(Item, S)>`, an instance over a
list-like type, and drive it manually. This must pass before the real change
starts, and it isolates any remaining tuple-payload problem from the compiler
work. `test/typeclass/success/stepper_threads_state_through_tuple.mdr` already
does exactly this — **run it and confirm it passes**, rather than writing a new
one.

- [ ] **Step 2: Confirm the loop lowering is the only compiler consumer**

`EmitIterableNextCall` has exactly two callers. Confirm nothing else calls
`Iterable::Next` implicitly — grep `NEXT_METHOD_NAME` and `ITERABLE_CLASS_NAME`
across `src/` and report anything beyond the type checker's item-type resolution
and these two emit sites.

---

## Task 2: Change the class signature and one instance

Do `List` first and leave `Map` and `Set` broken between commits only if the
suite is not run in between; otherwise change all three in one commit.

- [ ] **Step 1: `MidoriPrelude/Iterable.mdr`**

```
class Iterable < Iter > {
    type Item;
    Next: fn(iter: Iter) -> Option < (Item, Iter) >;
};
```

- [ ] **Step 2: `MidoriPrelude/Prelude/List.mdr`**

Replace the mutating instance:

```
instance Iterable < ListIter < T >> {
    type Item = T;
    def Next = fn(iter: ListIter < T >) -> Option < (T, ListIter < T >) > =>
        match iter.current with
            case List::Cons(head, tail) => Option::Some ((head, ListIter (tail)))
            case List::Nil => Option::None ();
};
```

No assignment remains. Note the doubled parentheses — `Option::Some((a, b))` is
one tuple argument.

- [ ] **Step 3: `Map.mdr` and `Set.mdr`**

Same transformation. Read the existing instances first; they may advance an index
rather than a link, in which case the new state is the struct with the next index.

- [ ] **Step 4: Expect the build to fail at the type checker, and record where**

The compiler still expects `Option<Item>`. Record the exact errors — they name the
sites Task 3 must change.

---

## Task 3: Teach the compiler the new shape

- [ ] **Step 1: Type checker**

Wherever the item type is derived from `Next`'s return, it must now read element 0
of the tuple inside the `Option`, not the `Option` payload directly. The
`Iterable::Item` associated type is unchanged — it is still the element type — so
prefer fixing the place that *unwraps* `Next`'s result over changing what `Item`
means.

- [ ] **Step 2: Code generator, both sites**

Apply the target sequence from "Verified facts" at `CodeGenerator.cpp:4959` and
`:5227`. They are symmetric; change both identically.

- [ ] **Step 3: Build and run the suite**

Report every failure rather than fixing opportunistically — a broad break here
usually means the item type is being read from the wrong position.

- [ ] **Step 4: Commit**

```bash
git add MidoriPrelude/ src/Compiler/
git commit -m "feat(prelude,compiler): Iterable::Next threads state instead of mutating"
```

---

## Task 4: Confirm the assignments are gone

- [ ] **Step 1: Recount**

Before this work the prelude had **62** assignment statements across five files:
`ArrayUtil.mdr`, `Collections/Map.mdr`, `Collections/OpenAddressing.mdr`,
`Collections/Set.mdr`, `Prelude/List.mdr`. Recount and report the new figure and
which files still carry them.

- [ ] **Step 2: Add a regression test**

A user-defined `Iterable` instance driven by a `for` loop, proving the protocol
works from outside the prelude. Snapshot verified to bite.

- [ ] **Step 3: Commit**

---

## Done when

1. `Iterable::Next : fn(Iter) -> Option<(Item, Iter)>` in the prelude.
2. No `Iterable` instance mutates; `iter.current = tail;` is gone.
3. `for` loops and array comprehensions work unchanged from the user's side.
4. A user-defined iterable works in a `for` loop, covered by a biting snapshot.
5. Suite **373/373** plus new cases, unit tests **1024 assertions / 176 cases**.
