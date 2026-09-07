# A Generic Call Inside an Instance Method Loses Its Arguments

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** a generic free function called from a typeclass instance method receives
its arguments, instead of reading them as zero.

**Severity: highest of the known defects.** This is a **silent wrong answer**, not
a crash. The two robustness fixes from 2026-09-06 (`5d07252`, `a1ce32d`) turn
internal failures into diagnostics; they do nothing here, because nothing fails.

**Repro:** `docs/superpowers/repros/generic-call-in-instance*.mdr`. Prints `0`,
should print `4`.

---

## Reduced form

**Three modules are required.** The class must be in one, the instance and the
generic function in a second, and the caller in a third.

```
// class module
class G < S > { Gg: fn(s: S) -> Int; };
```

```
// instance module
type H < T > = { v: T };
def MakeH = fn() -> H < Int > => H (9);
def GenericT = fn < T >(x: T, n: Int) -> Int => n;
instance G < H < Int >> {
    def Gg = fn(h: H < Int >) -> Int => GenericT(1, 4);
};
```

```
// consumer module
def h = Lib::MakeH();
IO::PrintLine(G::Gg(h) as Text);   // prints 0, expected 4
```

## Established by probe, one axis at a time

| variation | result |
|---|---|
| instance body is the literal `42` | **42** — dispatch itself is fine |
| body calls a **non-generic** helper, `NoGeneric(1, 2)` | **2** — correct |
| body calls a **generic** helper, `GenericT(1, 4)` | **0** — wrong |
| same generic call, all three parts in **one module** | **4** — correct |
| class imported, but instance and caller in the same module | **4** — correct |
| generic instance + non-generic helper | correct |
| non-generic instance + generic helper | **wrong** |

So: the generic **helper** is the trigger, not a generic instance; and the caller
must live in a third module. Neither the struct, the recursion, the `Option`, the
tuple nor the associated type from the original symptom is required — all were
stripped away.

## The likely mechanism, unconfirmed

A helper returning a **literal** returns it correctly while a helper returning a
**parameter** returns 0, which says the call happens and the body runs but the
parameters are not bound. Combining several calls in one arithmetic expression
gave 0 for *all* of them, including the non-generic call that works in isolation —
that points at **operand-stack corruption**, i.e. the specialized function is
being called with an arity that does not match what the call site pushes.

Start at `CodeGenerator::SpecializeGenericFunction` and the call-emission path for
a generic callee reached from an instance method body, comparing the arity used at
the call site against the specialized definition. `8ff02cc` fixed cross-module
specialization of *constrained* instances and is the nearest precedent.

**Do not trust this paragraph.** Every mechanism hypothesis in this project so far
has been wrong — the `Unify` site, the tuple-payload diagnosis, and the
`Freshen`/`SubstituteTypeParams` theory were all disproved by instrumentation.
Instrument first.

## How this was missed

The prelude never hits it: `Map` and `Set` recurse through `Iterable::Next`
instead of calling a generic helper, which is what the 2026-09-06 work switched
them to after the symptom appeared. Restoring `Set.mdr`'s `SetIterStep` helper
reproduces it immediately — iteration silently visits nothing.

## Tasks

- [ ] **Task 1: Instrument, do not reason.** Disassemble the specialized helper
  and the instance method (`Disassembler.cpp` exists) for the three-module repro
  and for the single-module version that works, and diff them. The arity mismatch
  should be visible directly.
- [ ] **Task 2: Fix, and check the stack discipline** — a wrong-arity call
  corrupts everything after it in the same expression, so the fix must be
  verified with several calls combined arithmetically, not just one in isolation.
- [ ] **Task 3: Close the coverage hole.** Three-module tests: a generic helper
  called from an instance method, with a non-generic call in the same expression
  to catch stack corruption. Snapshots verified to bite.
- [ ] **Task 4: Simplify `Map` and `Set`.** Once fixed, `SetIterStep` and
  `MapIterStep` can come back, which is the clearer way to write them.

## Done when

1. The repro prints `4`.
2. Several calls combined in one expression all give correct values.
3. `Set.mdr` works with the `SetIterStep` helper restored.
4. Covered by tests that bite. Suite green at 376/376 plus the new cases.


---

## Task 1 findings — 2026-09-07, instrumented

Disassembly is already built into `x64-development` (`MIDORI_DEBUG_LEVEL 2`), but
`HandleRun` installs a `ScopedTestModeOverride(true)` that suppresses it
(`CLI.cpp:1626`). Flipping that to `false`, rebuilding, and running the repro
dumps every procedure. Flip it back afterwards.

### The instance-method global is defined twice, from two different procedures

Broken (three modules):

```
$main$@GenericCallInInstanceLib
  MAKE_FUNCTION 7   DEFINE_GLOBAL 11  // MakeH
  MAKE_FUNCTION 8   DEFINE_GLOBAL 12  // $Gg_G_H_Int_

$module_bootstrap$@GenericCallInInstance
  MAKE_FUNCTION 9   DEFINE_GLOBAL 12  // $Gg_G_H_Int_
```

Working (one module) has the identical shape, with procedures 28 and 29:

```
$main$@DisSingle          MAKE_FUNCTION 28  DEFINE_GLOBAL 43  // $Gg_G_H_Int_
$module_bootstrap$        MAKE_FUNCTION 29  DEFINE_GLOBAL 43  // $Gg_G_H_Int_
```

So **both** builds bind one global to two different procedures. The difference is
which definition executes last: in the single-module build the correct one wins,
and across modules the module `$main$` and the bootstrap run in the other order,
so the global ends up holding the wrong procedure. Calling it with one argument
then lands in a body that reads local 1, which was never pushed — hence `0`, and
hence the operand-stack corruption when several calls share an expression.

### Two traps for whoever picks this up

- **The disassembler's procedure names are offset from the bodies they label.**
  `GenericT<Int,Int>` prints the body `INT_1; INT_4; CALL_PROC_2` — which is
  `Gg`'s body — and `$Gg_G_H_Int_` prints `GET_LOCAL 1; RETURN`, which is
  `GenericT`'s. This looks exactly like the bug and **is not**: the working build
  shows the same offset. Diff working against broken before concluding anything
  from a single dump. This nearly became the fourth wrong mechanism guess in this
  project.
- **Procedure indices are positional.** `DisassembleBytecodeStream` is called in
  a loop over `m_procedure_names`, so the Nth printed procedure is index N-1,
  ignoring the `=== Optimization Pass ===` lines.

### What is still unconfirmed

That the *ordering* of the two definitions is what differs, rather than one of
the two procedures itself being wrong, is **inferred from the two dumps, not
observed**. Confirm it — printing the global's value at each define, or tracing
execution — before changing emission. The fix is presumably to stop emitting the
duplicate rather than to reorder anything, but which of the two sites is the
spurious one has not been established.


---

## Ruled out — 2026-09-07

Read from source, each one a hypothesis that looked right and is not. Recorded so
the next attempt does not re-walk them.

- **`PatchBootstrapOffsets` failing to shift references.** It does shift, by +1,
  over `MAKE_CLOSURE`, `MAKE_FUNCTION` and every `CALL_PROC*` in procedures
  `1..n`, which is correct for a bootstrap inserted at index 0.
- **`PatchProcedure` failing to add the module base offset.** It adds
  `proc_base_offset` to the same opcode set, so a module-local procedure index is
  rebased before the +1 is applied.
- **`BuildInstanceGlobalInits` using pre-insert indices.** It is called from
  `BuildBootstrapProcedure`, which runs *after*
  `m_global_procedure_names.insert(begin, bootstrap_name)`, so its `proc_idx` is
  already the final index.

### The one solid observation

The instance method's global is written twice: once by the defining module's own
`$main$` (`CodeGenerator.cpp:2364-2367`, the ordinary `is_global` path in
`operator()(MidoriStatement::FunctionDefinition&)`) and once by the bootstrap
(`BuildInstanceGlobalInits`). One of those two is redundant by construction. The
next step is to establish **which of them writes the wrong procedure**, and that
must be *observed* — a trace on `DEFINE_GLOBAL` in the VM, or a print of the
resolved procedure at each site — not read off the disassembly, whose procedure
labels are offset from the bodies they print.

A generic call is required to trigger it, so the specialization procedure that
codegen appends for `GenericT<Int,Int>` is the remaining suspect: something about
where a specialization lands in the module's procedure list relative to the
instance method. That is a **suspicion, not a finding**.
