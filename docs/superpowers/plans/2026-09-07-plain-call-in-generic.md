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


---

## Mostly fixed — 2026-09-07, `47275f9`

Suite **381/381**, unit tests **1024 assertions / 176 cases**. Tasks 1-4 done;
the defect is **not fully closed** — see the limitation below.

### The cause

A generic function is specialized into the module that *calls* it, but its body
still names globals from the module that *declared* it. The `NameAccess` emitter
sends a qualified name (`Mod::sym`) through `GetImportPlaceholder` and an
unqualified one to the local global table — and in a foreign body every name is
unqualified. So `Plain(41)` became a `GET_GLOBAL` against a slot in the caller's
table that nothing ever defines.

The disassembly showed it plainly once dumped: the library emits
`DEFINE_GLOBAL 3 // Plain`, the specialization emits `GET_GLOBAL 45 // Plain`.
Two slots, one name, and 45 is never written. Calling it panics.

`GenericFunctionInfo` now carries the declaring module, and
`SpecializeGenericFunction` sets `m_specialization_source_module` while generating
a foreign body, which routes those names through `GetImportPlaceholder` against
the declaring module.

### Limitation: the helper must be exported

The guard is `m_specialization_source_module.has_value() && !m_global_variables.contains(name)`.
The second half is load-bearing — dropping it makes a **private** helper work but
breaks two tests, so some names legitimately resolve locally and the condition
does not yet distinguish them.

Consequence: `ArrayUtil::Clamp` is exported, which it should not need to be, and a
private helper called from an exported generic **still panics**. That is the same
crash, narrowed rather than eliminated.

Worth noting the minimal repro passes *without* exporting `Plain` while
`ArrayUtil` needed the export — an unexplained asymmetry, and the thread to pull
next. The difference is probably that `Slice` also calls the generic `Length`,
nesting one specialization inside another.

### Next

- [ ] Find a discriminator that admits private helpers without breaking the two
  tests the relaxed guard fails. Identify those two first; they say exactly which
  names must stay local.
- [ ] Make an unresolvable specialization reference a clean diagnostic rather than
  a crash, so this class of bug can never again present as
  `MemoryAccessViolation`.
- [ ] Un-export `ArrayUtil::Clamp` once private helpers work.
