# `type` and `alias` — One Form Per Concept

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `type` introduces a distinct type — record or sum — and `alias` abbreviates a type expression. `struct` and `union` become deletable.

**Architecture:** Purely additive. `struct` and `union` keep working; nothing is deleted here. The new forms map onto the **same AST nodes** the old ones produce, so the type checker, codegen and VM should need no changes at all — if they do, that is a finding worth reporting rather than working around.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` test suites run by the Midori CLI.

**Why now:** it gates the `struct`/`union` deletion, which is **131 declaration sites** across the prelude and tests. It is also the last piece of the type-declaration story: spec §4 collapses three statement nodes (`Struct`, `Union`, `TypeAlias`) into one `TypeDefinition`, taking statements from 11 to 5.

---

## The forms

```
type Point        = { x: Int, y: Int };              // record
type Option<T>    = Some(T) | None;                  // sum
type MapSlot<K,V> = Empty | Deleted | Occupied(K, V);
alias IntMap<V>   = Map<Int, V>;                     // transparent abbreviation
```

The right-hand side decides which kind it is — `{ … }` is a record, `A | B` is a sum. No keyword needs to distinguish them, which is why `struct` and `union` can both go.

A single-variant record does **not** repeat its name: `type Point = { x: Int, y: Int }`, not `type Point = Point { … }`. The type name is the constructor.

## Why `alias` is a separate keyword rather than more `type`

These are two concepts, not one keyword doing two jobs:

- `type Meters = Int` introduces a **distinct** type. `Meters` and `Int` are not interchangeable.
- `alias Meters = Int` introduces a **name for** a type. They are interchangeable.

Today `type X = Y` is transparent (`docs/type-system.md`: aliases are "fully interchangeable with the underlying type"). Under the merged form, `type Point = { … }` would be nominal while `type Meters = Int` stayed transparent — the same keyword meaning two different things depending on the shape of its right-hand side. That is exactly the ambiguity this redesign removes elsewhere.

It also unblocks spec §5: `Text` as a **nominal** newtype over `Array<Byte>`. Transparent would collapse `Hashable<Text>` into `Hashable<Array<Byte>>` and make every byte array a valid map key.

## Scope

**In:** parsing `type` in its three new shapes and `alias`, mapping onto existing AST nodes.

**Out:**
- Deleting `struct`, `union`, or the old transparent `type X = Y`. A later plan, once the prelude has migrated.
- Merging the `Struct`, `Union` and `TypeAlias` statement nodes into one. That is a refactor with no user-visible effect and belongs with the deletion.
- Field access or `{ with }` on multi-variant types.

## Known traps in this codebase

Established the hard way. Do not rediscover them:

1. **`SubstituteTypeParams` with an empty map is not the identity** — since `e730762` it rebuilds a `StructType` with `m_generic_params` cleared.
2. **Two analysis files fall through silently** rather than failing to compile when a new AST variant is missed: `Analysis/SemanticFacts.cpp` and `Analysis/SharedAnalysis.cpp`. Only relevant if new nodes are introduced — this plan should introduce none.
3. **There are two `{` dispatch sites** in the parser (`ParsePrimary` and the function-body fast path). `type X = { … }` puts a `{` in a new position; check it does not collide with the record-update probe added in `ee5eb20`.
4. **`ProbeArrayComprehension` precedent** — bounded lookahead scans are an established pattern here if disambiguation is needed.

**Build and test — PowerShell, NOT Git Bash** (Git Bash mangles the vcvars64 quoting and hangs):

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **304/304** and **821 assertions / 147 cases**, both green. Do not regress either. Do **not** use `build/out/Midori.exe` — a stale April build.

This checkout is shared with other sessions. Run `git status` first; stage by explicit pathspec, never `git add -A`.

---

## Task 1: Trace, then stop

Several plans in this project asserted premises that only survived until someone traced them — including, most recently, an opcode sequence that was approved without being executed and turned out to be wrong. Measure. If something looks obvious, prove it with a probe.

- [ ] **Step 1: Map what `type` does today**

`ParseTypeAliasDeclaration` is at `Parser.cpp:4015`, reached from `:5527`. `TYPE` is also matched at `:3482` and `:3758` — establish what those two are (associated types in classes and instances, most likely) and confirm the new forms will not disturb them.

- [ ] **Step 2: Map what `struct` and `union` produce**

Where each is parsed, and exactly which AST node and `MidoriType` each produces. The new `type` forms must produce **identical** nodes — report the constructors involved so the mapping can be verified rather than assumed.

- [ ] **Step 3: Establish the disambiguation cost**

`type Point = { x: Int, y: Int }` puts `{` after `=`. `type Option<T> = Some(T) | None` puts an identifier there. Report whether one token of lookahead after `=` distinguishes record from sum from alias, or whether more is needed.

Check specifically that this does not collide with the record-update brace probe added in `ee5eb20`.

- [ ] **Step 4: Decide whether `alias` needs a new token**

`ALIAS` is not currently in `Token.h`. Report what adding a reserved word costs — lexer table, any keyword-count assertions, and whether any existing `.mdr` file in the repo uses `alias` as an identifier. That last one matters: adding a reserved word is a breaking change for any code using it as a name.

- [ ] **Step 5: Size it and report**

Line estimate by layer, plus any design fork. **Stop and report before implementing.**

Flag particularly:
- Whether a nominal `type Meters = Int` is even representable today. The existing alias is transparent by construction; a *nominal* newtype over a primitive may need type-system work rather than parser work, and if so that is a fork worth naming before starting.
- Whether generic parameters work identically in all three shapes.

---

## Task 2 onward

Written after Task 1 reports.

## Done when

1. `type Point = { x: Int, y: Int }` declares a record usable exactly like the `struct` form, including construction and field access.
2. `type Option<T> = Some(T) | None` declares a sum usable exactly like the `union` form, including `match`.
3. `alias IntMap<V> = Map<Int, V>` is transparent — an `IntMap<Text>` and a `Map<Int, Text>` are interchangeable.
4. `type Meters = Int` is **nominal** — passing a `Meters` where an `Int` is expected is an error. If this needs type-system work beyond the parser, it is reported as a fork rather than silently made transparent.
5. Generic parameters work in all three `type` shapes.
6. `struct`, `union` and the existing `type X = Y` are unchanged and every existing test passes.
7. Tests carry `.expected` snapshots, each verified to bite by corrupting it before restoring.
