# Make `#` Resolve Only Through `Countable`

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Delete the compiler's hardcoded knowledge of `Map`, `Set` and `List`, so `#x` resolves through the `Countable` typeclass for every type.

**Architecture:** `#` currently lowers five different ways in `CodeGenerator.cpp`: a `GET_ARRAY_LENGTH` opcode for arrays, then `HasNameSuffix` string matching against `"List"`, `"Map"`, `"MapData"`, `"Set"` and `"SetData"`, and only then the `Countable` instance. Adding `Countable` instances for those four types lets all the name matching be deleted. `Array` keeps an opcode-backed instance — array length is a genuine VM primitive; what changes is that the *instance* is library code rather than a compiler special case.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` test suites run by the Midori CLI.

**Why this is worth doing beyond tidiness — corrected 2026-09-01.** My original claim, that a struct named `SiteMap` picks up `MapCount`, was **wrong**. `HasNameSuffix` is not a substring match: it requires the unqualified name to be *exactly* `Map`/`MapData`/`Set`/`SetData`/`List`, or a `::`-qualified name ending in one. `SiteMap` never matches and already errors cleanly.

The miscompile is real but narrower, and worse in kind. A user struct named exactly `MapData`, in a module that also imports `Collections/Map.mdr`:

```
struct MapData { url: Text };
def s = new MapData("example.com");
IO::PrintLine(#s as Text);
```

**compiles and prints `0`, exit code 0.** `MapData` has one field; `MapCount` reads field slot 1 (`map.count`). That is a silent out-of-bounds field read with no diagnostic. Without the `Map` import it fails at codegen with `Generic function 'MapCount' not found` — still wrong, but loud.

---

## Prerequisites — all verified present

| Need | Exists as |
|---|---|
| Array length primitive | `MIDORI_FFI_ArrayLength` (`MidoriFFIRegistry.h:153`) |
| Instance pattern to copy | `Countable<Text>` (`MidoriPrelude/Countable.mdr:6-9`) |
| Map count | `Map::MapCount` |
| Set count | `Set::SetCount` |
| List length | `List::ListLength` (`Prelude/List.mdr:28`) |

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `MidoriPrelude/Countable.mdr` | `Countable` class; `Text` and `Array` instances | Modify |
| `MidoriPrelude/Collections/Map.mdr` | add `instance Countable<Map<K,V>>` | Modify |
| `MidoriPrelude/Collections/Set.mdr` | add `instance Countable<Set<T>>` | Modify |
| `MidoriPrelude/Prelude/List.mdr` | add `instance Countable<List<T>>` | Modify |
| `src/Compiler/TypeChecker/TypeChecker.cpp` | delete the name-suffix dispatch (`:4665-4686`) and `HasNameSuffix` (`:756`) | Modify |
| `src/Compiler/CodeGenerator/CodeGenerator.cpp` | delete the name-suffix dispatch (`:3540-3556`) and `HasNameSuffix` (`:17`) | Modify |
| `test/typeclass/failure/countable_no_instance.mdr` | a type named exactly `MapData` must not silently read a wrong field | Create |

**Both copies must be deleted in the same commit.** `TypeChecker.cpp:4665-4686` holds an
identical block that short-circuits *before* the `Countable` lookup:
`if (resolved_type->IsType<ArrayType>() || is_list_type || is_map_type || is_set_type) { return Int; }`.
It never sets `unary.m_uses_countable` on that path — which is exactly why codegen's
`if (!unary.m_uses_countable)` branch is reachable at all. Removing only the codegen
block would leave the type checker still typing `#` on a prelude `Map` without
requiring an instance, while `m_uses_countable` stays false, so `EmitCountableCall`
becomes the fallback for a node whose instance was never resolved. Grep confirms no
other callers, so both `HasNameSuffix` definitions become dead.

**Instances live with their types, not in `Countable.mdr`.** Putting `instance Countable<Map<K,V>>` in `Countable.mdr` would make it import `Map.mdr`, and anything in that import chain using `#` would need `Countable` back — a cycle. Defining each instance in the module that declares the type is both the conventional placement and the one that avoids this.

**Build and test — PowerShell, NOT Git Bash** (Git Bash mangles the vcvars64 quoting and hangs):

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
```

Baseline 263/267. The four failures are pre-existing and unrelated: `concurrency/worker_cancel_blocked_receive`, `concurrency/worker_cancel_spin`, `static_analyzer/warning_then_codegen_failure`, `static_analyzer/unused_local_warning`. Do **not** use `build/out/Midori.exe` — a stale April build that silently gives wrong results.

---

## Task 1: Prove the miscompile exists

**Files:** Create `test/typeclass/failure/countable_name_collision.mdr` (temporary — deleted in Task 4)

- [ ] **Step 1: Write a program that should not compile**

```
module CountableNameCollision

import { "../../../MidoriPrelude/IO.mdr", }

struct SiteMap { url: Text };

def s = new SiteMap("example.com");
IO::PrintLine(#s as Text);
```

`SiteMap` has no `Countable` instance, so `#s` must be a compile error.

- [ ] **Step 2: Run it and record what happens**

```powershell
./out/build/ninja/x64-development/out/Midori.exe check test/typeclass/failure/countable_name_collision.mdr
```

Record the exact output. The expectation is that `HasNameSuffix(name, "Map")` matches `SiteMap` and emits a call to `MapCount`, which then fails at a confusing place — or worse, compiles.

**If it already errors cleanly**, stop and report. That would mean the dispatch is guarded somewhere this plan has not accounted for, and the plan needs revising before you continue.

- [ ] **Step 3: Commit the finding**

```bash
git add test/typeclass/failure/countable_name_collision.mdr
git commit -m "test: demonstrate # name-suffix dispatch matching a user type"
```

---

## Task 2: Add the four `Countable` instances

**Files:** `MidoriPrelude/Countable.mdr`, `Collections/Map.mdr`, `Collections/Set.mdr`, `Prelude/List.mdr`

- [ ] **Step 1: Add the `Array` instance**

In `MidoriPrelude/Countable.mdr`, beside the existing `Text` instance:

```
foreign "MIDORI_FFI_ArrayLength" ArrayLength: fn(Array < T >) -> Int;
instance Countable < Array < T >> {
    defun Count(value: Array < T >): Int => ArrayLength(value);
};
```

Match the surrounding file's spacing style exactly — the prelude is machine-formatted with spaces inside angle brackets.

- [ ] **Step 2: Verify the Array instance resolves before adding more**

Write a scratch file using `#` on an array, run it, and confirm it still produces the right length. At this point the codegen still short-circuits arrays to the opcode, so this proves the instance *exists and type-checks*, not that it is used yet.

- [ ] **Step 3: Add the `Map`, `Set` and `List` instances**

In `Collections/Map.mdr`, after `MapCount` is defined:

```
instance Countable < Map < K, V >> {
    defun Count(value: Map < K, V >): Int => MapCount(value);
};
```

In `Collections/Set.mdr`, after `SetCount`:

```
instance Countable < Set < T >> {
    defun Count(value: Set < T >): Int => SetCount(value);
};
```

In `Prelude/List.mdr`, after `ListLength`:

```
instance Countable < List < T >> {
    defun Count(value: List < T >): Int => ListLength(value);
};
```

Each file must import `Countable.mdr` and add `Countable` to its export list if it re-exports.

- [ ] **Step 4: Build and run the suite**

Expect 263/267 still, or 264/268 with the Task 1 test. Nothing should change behaviourally yet — the codegen special cases still win. If anything regresses, an instance is conflicting with the existing dispatch and you should report before continuing.

- [ ] **Step 5: Commit**

```bash
git add MidoriPrelude/Countable.mdr MidoriPrelude/Collections/Map.mdr MidoriPrelude/Collections/Set.mdr MidoriPrelude/Prelude/List.mdr
git commit -m "feat(prelude): add Countable instances for Array, Map, Set and List"
```

---

## Task 3: Delete the name-suffix dispatch

**Files:** `src/Compiler/CodeGenerator/CodeGenerator.cpp`

- [ ] **Step 1: Read the block being removed**

```bash
sed -n '3530,3565p' src/Compiler/CodeGenerator/CodeGenerator.cpp
```

Confirm the shape before editing: an `Array` opcode branch, then `is_list_type` / `is_map_type` / `is_set_type` computed from `HasNameSuffix`, then `EmitGenericLengthCall` for each, then `EmitCountableCall` as the fallback.

- [ ] **Step 2: Delete the three name-matched branches**

Remove the `is_list_type`, `is_map_type` and `is_set_type` declarations and the `if (!unary.m_uses_countable) { ... }` block that dispatches on them. Leave the `Array` opcode branch and the `EmitCountableCall` fallback.

- [ ] **Step 3: Delete `HasNameSuffix` if now unused**

```bash
grep -n "HasNameSuffix" src/Compiler/CodeGenerator/CodeGenerator.cpp
```

If the only remaining hit is the definition at `:17`, delete it too. If other callers exist, leave it and say which in your report.

- [ ] **Step 4: Build and run the full suite**

This is the step most likely to break. `#` on a `Map`, `Set` or `List` now has to resolve through the instances added in Task 2. Any test using `#` on those types exercises the new path.

If a test fails because the instance is not found, the likely cause is that the using module does not have `Countable` in scope. Report which module and which type rather than adding imports blindly across the prelude.

- [ ] **Step 5: Verify the miscompile is gone**

```powershell
./out/build/ninja/x64-development/out/Midori.exe check test/typeclass/failure/countable_name_collision.mdr
```

`#s` on a `SiteMap` must now be a clean "no `Countable` instance" error rather than a call to `MapCount`.

- [ ] **Step 6: Commit**

```bash
git add src/Compiler/CodeGenerator/CodeGenerator.cpp
git commit -m "refactor(codegen): resolve # only through Countable"
```

---

## Task 4: Convert the demonstration into a real test

**Files:** Delete `test/typeclass/failure/countable_name_collision.mdr`; create `test/typeclass/failure/countable_no_instance.mdr` and its `.expected`

- [ ] **Step 1: Replace the temporary test**

The Task 1 file demonstrated a bug that no longer exists. Replace it with one asserting the correct behaviour: a user type named `SiteMap` with no `Countable` instance gives a clear error naming `Countable`.

- [ ] **Step 2: Add a `.expected` snapshot**

Several typeclass tests assert only exit code, which would let a wrong dispatch pass silently. Pin the error text and line.

- [ ] **Step 3: Verify the snapshot bites**

Corrupt it, confirm the test fails, restore it. Do not skip this — a snapshot that does not bite is worse than no snapshot, because it looks like coverage.

- [ ] **Step 4: Add a positive test**

`test/typeclass/success/countable_all_types.mdr` plus `.expected`, using `#` on an `Array`, a `Map`, a `Set`, a `List` and a `Text` in one file and printing all five lengths. This is the regression guard for the whole change.

- [ ] **Step 5: Commit**

```bash
git add test/typeclass/
git commit -m "test: cover # resolving through Countable for every type"
```

---

## Done when

1. `#` has exactly one lowering path: the `Array` opcode, then `Countable`.
2. `HasNameSuffix` is gone from `CodeGenerator.cpp`, or its remaining callers are named in the report.
3. A user type whose name ends in `Map`, `Set` or `List` gets a clean error, not a wrong call.
4. `#` works on `Array`, `Map`, `Set`, `List` and `Text`, covered by a snapshot-pinned test.
5. No regressions against the 263/267 baseline.
