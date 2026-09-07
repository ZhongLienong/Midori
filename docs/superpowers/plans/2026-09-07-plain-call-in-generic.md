# A Generic Function Calling a Non-Generic Helper Crashes Across Modules

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** a generic function may call an ordinary non-generic function from its own
module, and still work when specialized for a caller in another module.

**Repro:** `docs/superpowers/repros/plain-call-in-generic*.mdr`. Panics with
`MemoryAccessViolation`; should print `42`.

---

## Reduced form

```
// library module
def Plain = fn(n: Int) -> Int => n + 1;
def CallsPlain = fn < T >(x: T) -> Int => Plain(41);
```

```
// consumer module
IO::PrintLine(Lib::CallsPlain(0) as Text);
```

The identical code in a **single module** prints `42`. Only the cross-module case
crashes.

## Established by probe

| variation | result |
|---|---|
| generic function calling a **generic** helper, cross-module | works |
| generic function containing an array comprehension, cross-module | works |
| comprehension **and** generic helper, cross-module | works |
| generic function calling a **non-generic** helper, **same** module | works |
| generic function calling a **non-generic** helper, **cross-module** | **crashes** |

So the trigger is precisely: specializing a generic function whose body calls a
non-generic function defined in the same module, for a caller in another module.
No struct, typeclass, instance, comprehension or recursion is required.

## Pre-existing, not introduced by `d0d0cf4`

Checked, because `d0d0cf4` changed how procedures are named and this is adjacent.
Reverting `src/Compiler/CodeGenerator/CodeGenerator.cpp` to `a281843`, rebuilding
and running the repro crashes identically. The procedure-naming fix neither caused
nor masks it.

## Why the suite never caught it

The prelude had no generic function calling a non-generic helper — every helper it
reaches is either generic (`ArrayUtil::Length`) or a typeclass method. Writing
`ArrayUtil::Slice` with an obvious `Clamp(value, low, high)` helper is what
surfaced it. `Slice` now inlines the clamps to avoid the crash, which is a
workaround, not a preference.

## Where to look

The specialization path copies a generic body into a new procedure and re-emits
its calls. A call to a *generic* callee is re-resolved through specialization; a
call to a *non-generic* callee presumably keeps whatever procedure or global index
the original body carried, which is a module-local index that means something
different once the specialization is emitted for another module. Compare against
`CodeGenerator::SpecializeGenericFunction` and how it rewrites call targets.

**That is a hypothesis and nothing more.** Five mechanism hypotheses were formed
and discarded in the neighbouring defect before instrumentation found the real
cause; the two experiments that worked there were (1) suppressing each candidate
emission in turn and (2) printing procedure names against procedure **sizes**.
Reach for those before reasoning.

## Tasks

- [ ] **Task 1: Instrument.** Dump procedures for the repro and for the working
  single-module version and diff. `HandleRun` suppresses disassembly via a
  `ScopedTestModeOverride(true)` at `CLI.cpp:1626`; flip it, rebuild, flip back.
- [ ] **Task 2: Fix.**
- [ ] **Task 3: Cover it.** A three-module test where a generic function calls a
  non-generic helper, plus a second call in the same expression to catch stack
  damage. Snapshot verified to bite.
- [ ] **Task 4: Restore the `Clamp` helper in `ArrayUtil::Slice`.**

## Done when

1. The repro prints `42`.
2. `ArrayUtil::Slice` reads through a `Clamp` helper again.
3. Covered by tests that bite; suite green at 379/379 plus the new cases.
