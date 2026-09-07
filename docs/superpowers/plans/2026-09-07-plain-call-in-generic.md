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


---

## Correction and sharper findings — 2026-09-07

### The "relaxed guard breaks two tests" claim above is wrong

Recorded in the previous section and disproved on the next attempt. The two
failures were `prelude/success/array_util.mdr` and
`prelude/success/documentation_examples.mdr` — **both of which are the private
`Clamp` case itself**, not unrelated collateral. The earlier measurement changed
two things at once (relaxing the guard *and* un-exporting `Clamp`) and attributed
the result to the wrong one.

With `Clamp` exported, the relaxed guard passes **381/381**. With `Clamp` private,
both guards fail identically. So:

- The `!m_global_variables.contains(name)` half is **not** load-bearing for
  correctness on the current suite.
- Relaxing it does **not** fix private helpers either.

The real constraint is elsewhere: the import cannot be resolved to a non-exported
global.

### Why it presents as a crash rather than an error

`BytecodeLinker::ResolveImports` (`:660`) falls back to `0uz` when a symbol
resolves to nothing:

```cpp
const std::optional<size_t> global_result = FindSymbolInGlobals(*imported_module, import.m_name, base_offset);
return global_result.value_or(0uz);
```

An unresolved import therefore silently becomes global index 0, and calling it
panics with `MemoryAccessViolation`. **This is the single highest-value thing to
fix next**: every defect in this family has cost hours precisely because it
presents as memory corruption rather than as a message.

### The remaining puzzle

`FindSymbolInGlobals` searches `module.m_global_variables` by name, not just
exports, so a private `Clamp` *should* resolve through it. It does not. Something
upstream — `ValidateImport`, or the earlier `ResolveImportsAndPatch` pass, which
runs before `ConcatenateBytecode` and is a separate path — either rejects or
bypasses the import first. **Not reduced.** Do not guess: the `0uz` fallback
should be turned into a diagnostic first, and it will then say plainly which
symbol failed and where.

### Current state

`ArrayUtil::Clamp` stays exported. Suite **381/381**.


---

## Root cause of the private-helper case, proven — 2026-09-07

### The two resolution paths disagree

`BytecodeLinker::ResolveImports` tries exports first, then falls back to a search
over all of the module's globals:

- `FindSymbolInExports` returns `base_offset + it->m_global_index` — the index
  **recorded** on the export record.
- `FindSymbolInGlobals` returns `base_offset + local_index` — the symbol's
  **position** in `module.m_global_variables`.

These are not the same number. Instrumented on `ArrayUtil::Clamp` with both paths
computed for the same symbol:

```
INDEX-MISMATCH Clamp export=19 globals=26
```

An exported symbol takes the first path and is correct. A **private** symbol has
no export record, falls to the second, and gets an index seven slots off — which
is then called, and panics. That is the whole private-helper defect.

This is the same class as `d0d0cf4`: a **recorded** index and a **positional**
index that agree until something inserts entries, and then silently do not.

### Not an unresolved-import problem

`6167a86` turned the `value_or(0uz)` fallback into a diagnostic, and it does
**not** fire for `Clamp`. The import resolves; it resolves to the wrong slot. The
diagnostic is still worth having — it is why this family kept surfacing as
`MemoryAccessViolation` — but it was not the cause here.

### The fix, not yet made

`FindSymbolInGlobals` must return the symbol's real global index rather than its
position. Establish first **why** `m_global_variables` is not index-aligned — the
seven-slot gap is a fact to explain, not to paper over with an offset. Whatever is
in those seven entries (imported placeholders, most likely) determines whether the
right fix is a name-to-index map carried on `BytecodeModule`, or making the vector
index-aligned at construction.

Until then `ArrayUtil::Clamp` stays exported, which is why the suite is green.


---

## The seven-slot gap, explained — 2026-09-07

The previous section asked why `m_global_variables` is not index-aligned and said
to explain the gap before correcting it. Measured, and the answer is worse than a
gap.

### Every global in `ArrayUtil` is recorded with index 0

`BytecodeModule::m_global_variables` is built by sorting the codegen map
`m_global_variables` (`unordered_map<string,int>`) by value and emitting names in
that order (`CodeGenerator.cpp:2192-2199`). Instrumenting that loop for
`ArrayUtil`:

```
rank=0 index=0 name=Reverse    rank=4 index=0 name=Append
rank=1 index=0 name=Length     rank=5 index=0 name=Prepend
rank=2 index=0 name=Slice      rank=6 index=0 name=Contains
rank=3 index=0 name=Extend     rank=7 index=0 name=Clamp
```

**All eight are index 0.** The sort is therefore arbitrary and the resulting
positions are meaningless, which is why `FindSymbolInGlobals` returned
`base + 7 = 26` for `Clamp` while its export record said `base + 0 = 19`. There is
no seven-slot gap to explain; the ranks are noise.

### Where the zeros come from

`m_global_variables` is read with `operator[]` in three places — 
`CodeGenerator.cpp:2078`, `:2121`, `:2138` — and `operator[]` **inserts a
default-constructed 0** when the key is absent. `:2078` is the export-tracking
path:

```cpp
const size_t global_index = static_cast<size_t>(m_self->m_global_variables[function_name]);
m_self->m_tracked_exports.emplace_back(function_name, procedure_index, global_index, ...);
```

Every function in `ArrayUtil` is generic, so none of them owns a global. Recording
their exports therefore inserts a bogus zero into the map *and* stamps
`m_global_index = 0` onto the export record. Both sides of the later comparison
are built on that.

### Why this is not a two-line fix

Replacing `operator[]` with `find()` only moves the question: what *should* a
generic function's export record say? It has no global of its own — it is
specialized per consumer, which is the whole reason this plan exists. The honest
options are to give generic exports an explicit "no global" marker that
`FindSymbolInGlobals` and `FindSymbolInExports` both respect, or to stop routing
specialization references through the global table at all.

That is a design decision about how a generic crosses a module boundary, not a
patch. It should be made deliberately, with the `operator[]` insertions fixed at
the same time so the map stops accumulating phantom zero entries.

### Current state

`ArrayUtil::Clamp` stays exported; suite **381/381**. The unresolved-import
diagnostic from `6167a86` stands and is unrelated to this — it correctly does not
fire, because the import resolves, just against meaningless indices.
