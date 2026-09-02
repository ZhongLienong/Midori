# Make `[]` Resolve Through `Indexable`

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `x[i]` resolves through an `Indexable` typeclass instead of being hard-wired to `Array`, completing the operator work `Countable` started.

**Architecture:** Unlike `#`, `[]` has no name-based dispatch to delete. `TypeChecker.cpp:5982` simply rejects anything that is not an `ArrayType`, and codegen emits `GET_ARRAY`. This is therefore a pure *addition* — the operator becomes extensible — rather than a removal of special cases. `Array` keeps the opcode behind its instance, exactly as `Array` does for `Countable`.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` test suites run by the Midori CLI.

**Why now:** the spec lists `Indexable` under section 4's additions, and `Countable` has just established the pattern end to end — one-line type-checker change through `FindMatchingInstance`, plus `ResolveInstanceNameForTypeArgs` in codegen. Doing it while that shape is fresh is cheaper than doing it later.

---

## Scope: read-only

`Indexable` covers `x[i]` only. **There is no `Set` method**, because `x[i] = v` is being deleted with the rest of assignment (spec section 4). `IndexAssignment` stays as it is and is removed later by the grammar work, not by this plan.

## Precedent to copy — verified working

The `Countable` change is the template:

- `TypeChecker.cpp` — the exact-key `m_instances.find` became `FindMatchingInstance`, one line (`6ae6d52`).
- `CodeGenerator.cpp` — a raw `m_global_variables.find(mangled_name)` became `ResolveInstanceNameForTypeArgs(...)` plus `EmitResolvedNameGetGlobal`, eight lines.
- `MidoriPrelude/Countable.mdr` — an opcode-backed instance for `Array`.

Two traps that bit `Countable` and will bite here:

1. **Generic instances need `FindMatchingInstance`, not an exact-key lookup.** An exact key cannot match `Indexable<Bag<T>>` against a request for `Bag<Int>`.
2. **`ResolveInstanceName` searches `base_name + "@module"`.** A raw global lookup misses instances that arrive via import, which is how `#` on an imported `Text` had silently never worked.

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `MidoriPrelude/Indexable.mdr` | `Indexable` class; `Array` instance | Create |
| `src/Compiler/TypeChecker/TypeChecker.cpp` | `IndexAccess` (`:5954`) resolves via `Indexable` | Modify |
| `src/Compiler/CodeGenerator/CodeGenerator.cpp` | emit the instance call when the operand is not an `Array` | Modify |
| `test/typeclass/success/indexable_*.mdr` + `.expected` | coverage | Create |

**Build and test — PowerShell, NOT Git Bash** (Git Bash mangles the vcvars64 quoting and hangs):

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
```

Baseline **273/273, fully green**. Do not regress it. Do **not** use `build/out/Midori.exe` — a stale April build that silently gives wrong results.

This checkout is shared with other sessions. Run `git status` first; stage by explicit pathspec, never `git add -A`.

---

## Task 1: Establish the current behaviour

- [ ] **Step 1: Confirm what `[]` rejects today**

```
def t : Text = "abc";
IO::PrintLine(t[0] as Text);
```

gives `Array get expression type error: expected array type` at `TypeChecker.cpp:5982`. Confirm, and record the exact text.

- [ ] **Step 2: Confirm arrays still work**

Run any existing test using `arr[i]` and confirm it passes. `test/array_comprehension/` and `test/expression/` have several.

- [ ] **Step 3: Decide the class shape and report before implementing**

The obvious shape is:

```
class Indexable < C > {
    type Element;
    Get: fn(container: C, index: Int) -> Element;
};
```

An associated `Element` type mirrors `Iterable`'s `Item`. But **check whether an `Int` index is right** before committing to it — a `Map<K,V>` would want `K`, not `Int`, which would mean a second type parameter:

```
class Indexable < C, I > {
    type Element;
    Get: fn(container: C, index: I) -> Element;
};
```

Do not guess. Report which shape you recommend and why, and **stop for confirmation**. This decides whether `Map` can ever support `m[key]`, so it is a language-design call, not an implementation detail.

---

## Task 2: Add the class and the `Array` instance

Only after Task 1 Step 3 is confirmed.

- [ ] **Step 1: Create `MidoriPrelude/Indexable.mdr`**

Match the prelude's machine-formatted spacing — spaces inside angle brackets, as in `Countable.mdr`.

- [ ] **Step 2: Add the `Array` instance backed by the existing opcode**

Check `MidoriFFIRegistry.h` for an array-get FFI. If none exists, the instance may need a new entry, or codegen may keep the opcode fast path for `Array` and use the instance only for other types — `Countable` kept an `Array` opcode branch for exactly this reason. Report which you found.

- [ ] **Step 3: Build and run the suite**

Nothing should change yet — the type checker still rejects non-arrays. This proves the class and instance type-check.

- [ ] **Step 4: Commit**

```bash
git add MidoriPrelude/Indexable.mdr
git commit -m "feat(prelude): add the Indexable class with an Array instance"
```

---

## Task 3: Resolve `[]` through the instance

- [ ] **Step 1: Type checker**

At `TypeChecker.cpp:5982`, instead of rejecting non-array operands outright, fall through to `FindMatchingInstance` for `Indexable`. Keep the `ArrayType` fast path. Follow `Countable`'s post-`6ae6d52` shape.

- [ ] **Step 2: Codegen**

Emit the instance call for non-array operands, using `ResolveInstanceNameForTypeArgs` plus `EmitResolvedNameGetGlobal` — **not** a raw `m_global_variables.find`, which would miss imported instances.

- [ ] **Step 3: Build and run the suite**

Array indexing must be unchanged. If anything regresses, the fast path is being bypassed; report rather than working around it.

- [ ] **Step 4: Prove extensibility**

A user type with an `Indexable` instance must work, generically:

```
struct Bag < T > { items: Array < T > };
instance Indexable < Bag < T >> {
    type Element = T;
    defun Get(container: Bag < T >, index: Int): T => container.items[index];
};
```

`b[1]` on a `Bag<Int>` must return the right element. This is the case that would fail with an exact-key lookup, so it is the real test of the change.

- [ ] **Step 5: Commit**

```bash
git add src/Compiler/TypeChecker/TypeChecker.cpp src/Compiler/CodeGenerator/CodeGenerator.cpp
git commit -m "refactor(typechecker,codegen): resolve [] through Indexable"
```

---

## Task 4: Tests

- [ ] **Step 1: Add coverage with `.expected` snapshots**

- `indexable_array.mdr` — arrays still work
- `indexable_generic_user_type.mdr` — `Bag<Int>` and `Bag<Text>`
- `failure/indexable_no_instance.mdr` — a type with no instance gives a clean error naming `Indexable`

- [ ] **Step 2: Verify every snapshot bites**

Corrupt each one, confirm `[FAIL]`, restore, confirm `[OK]`. A snapshot that does not bite is worse than none, because it looks like coverage.

- [ ] **Step 3: Check both spellings agree**

`b[1]` and `Indexable::Get(b, 1)` must give the same answer. The equivalent divergence for `Countable` was a real bug, fixed in `b4703d1` — confirm it has not recurred here.

- [ ] **Step 4: Commit**

```bash
git add test/typeclass/
git commit -m "test: cover [] resolving through Indexable"
```

---

## Done when

1. `x[i]` resolves through `Indexable` for any type with an instance, arrays included.
2. A generic user type works at multiple instantiations.
3. A type with no instance gives a clean error naming `Indexable`.
4. `b[1]` and `Indexable::Get(b, 1)` agree.
5. Suite still 273/273 plus the new tests.
