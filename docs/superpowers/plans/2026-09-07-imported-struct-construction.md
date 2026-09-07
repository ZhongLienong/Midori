# Constructing an Imported Struct Crashes the Type Checker

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `use Lib.{ Point }` followed by `Point(1, 2)` compiles, instead of
killing the type checker with `std::bad_variant_access`.

**Status:** reduced and localised to a stage; **not** diagnosed to a line. Repro
in `docs/superpowers/repros/imported-struct-construction*.mdr`.

---

## The reproduction

Two files, six lines of substance:

```
module TinyLib2
public export { Flat, Wrap }
type Flat = { a: Int, b: Int };
type Wrap < T > = { v: T };
```

```
module TFlat
import { "./tiny_lib2.mdr", "../MidoriPrelude/IO.mdr" }
use TinyLib2.{ Flat }
def f : Flat = Flat (1, 2);
IO::PrintLine(f.a as Text);
```

```
internal failure in the type checker stage: bad variant access
```

## What was established, by probe

- **The typeclass is irrelevant.** This was found while chasing a typeclass
  dispatch failure. Removing the class, the instance, the associated type, the
  generic parameter and the `Option` wrapper one at a time left the crash intact.
- **Genericity is irrelevant.** `Flat` — a plain non-generic struct — crashes
  exactly as `Wrap<T>` does.
- **The construction site is what matters.** Constructing inside the defining
  module and exporting a factory (`MinLib::MakePair()`) works and prints
  correctly. Only construction in the *consumer* fails.
- **It is not new.** The same crash occurs with the pre-2026-09-06 `Iterable`
  signature, and reverting `src/` to `e854ea2` reproduces the family.
- **Qualified construction is a clean error, not a crash.**
  `TinyLib2::Flat(1, 2)` reports *"Call expression type error: not a callable"*,
  so `use` is the only route to the failing path.

## Why 374 tests pass over it

No test constructs an imported struct. Every `use X.{...}` in `test/` imports a
*function* (overwhelmingly `IO.{PrintLine}`). Imported **unions** work because
their constructors are qualified by the type — `Option::Some(...)` — which is a
different path. The prelude never hits it either: `Set`, `Map` and `Entry` are
only ever constructed inside their own modules, behind factories like `SetNew`.

This is a coverage hole in an extremely ordinary operation: any library exporting
a record type is unusable from a consumer module.

## Where to look

`std::bad_variant_access` means an unchecked `GetType<T>()` on a `MidoriType`
holding a different alternative. The stage is the **type checker**, so the
candidate is the path that resolves a `use`-imported name to a constructor:
whatever binds `Flat` in the consumer's scope is probably storing the struct
*type* where the constructor's `FunctionType` is expected, and the `Construct` or
`Call` visitor then reads it as the wrong alternative.

Compare against the union path, which works, and against
`Parser::ParseConstruct`'s handling of a locally declared struct.

## Tasks

- [ ] **Task 1: Find the line.** The exception is caught at
  `Compiler.cpp`'s stage loop, so a temporary `try`/`catch` narrowed further —
  around the type checker's name-resolution and `Construct` paths — will name it
  quickly. Do not guess; the two previous guesses in this project were both wrong.
- [ ] **Task 2: Fix it, with a diagnostic where a diagnostic belongs.** If the
  imported name genuinely cannot be constructed, that must be a clean error, not
  an unchecked `GetType`.
- [ ] **Task 3: Close the coverage hole.** Add cross-module tests that construct
  an imported struct, both generic and not, and read a field back. Snapshots
  verified to bite.

## Done when

1. The repro compiles and prints `1`.
2. The generic case `Wrap<Int>` works too.
3. Cross-module struct construction is covered by tests that bite.
4. Suite still green at 374/374 plus the new cases.
