# Type Parameters and `where` Clauses on `fn`

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `fn<T>(x: T) -> T where C<T> => e` parses and works, so a lambda can express everything `defun` can.

**Architecture:** Purely additive. `defun` keeps working throughout; nothing is deleted by this plan. `ParseFunctionExpression` currently consumes `(` immediately (`Parser.cpp`), so there is no slot for type parameters. `defun` already parses both type parameters and `where` clauses, so the machinery exists and needs routing to a second caller.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` test suites run by the Midori CLI.

**Why this first:** it is the deepest prerequisite in the redesign. `defun` cannot be deleted (spec §4) until `fn` can express generics and constraints, and `def F = fn(...)` cannot become the single definition form until then either. Everything in plans 4–6 sits behind it.

---

## Scope

**In:** type parameters and `where` clauses on `fn` expressions.

**Out, deliberately:**
- Deleting `defun` — a later plan, once the prelude has migrated.
- `->` in return position — `fn<T>(x: T) : T` keeps the current `:` for now. Changing the separator is a separate additive step, and bundling them makes a parser failure ambiguous between two causes.
- Making `spawn` accept a `def`-bound lambda. `spawn` requires a `MidoriStatement::FunctionDefinition` (`TypeChecker.cpp`, the named-top-level-function check). That stays true here and is lifted when `defun` goes.

## Known traps in this codebase

Established the hard way during earlier plans. Read before starting:

1. **`SubstituteTypeParams` with an empty map is not the identity.** Since `e730762` it rebuilds a `StructType` with `m_generic_params` cleared. Anything assuming "substituting nothing changes nothing" is wrong.
2. **A one-parameter class cannot have a heterogeneous method** (`CodeGenerator.cpp`, the `candidate_args.size() == 1u` branch matches the single argument against every actual argument). Not directly in scope, but it shapes what constraints on a generic lambda can look like.
3. **Exact-key instance lookups miss generic instances.** Use `FindMatchingInstance`, which does the `MatchInstanceTypeArg` scan.
4. **Raw `m_global_variables.find` misses imported instances.** `ResolveInstanceName` appends `@module`; use `ResolveInstanceNameForTypeArgs`.

**Build and test — PowerShell, NOT Git Bash** (Git Bash mangles the vcvars64 quoting and hangs):

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **280/280** and **759 assertions / 145 cases**, both fully green. Do not regress either. Do **not** use `build/out/Midori.exe` — a stale April build.

This checkout is shared with other sessions. Run `git status` first; stage by explicit pathspec, never `git add -A`.

---

## Task 1: Trace, then stop

Three of the earlier plans in this project had premises that turned out wrong when someone finally traced them. Measure rather than reason from structure.

- [ ] **Step 1: Confirm the current failure**

```
def Id = fn<T>(x: T) : T => x;
```

gives `Expected '(' before function parameters.` Confirm and record the exact text.

- [ ] **Step 2: Map how `defun` does it**

Report, with file:line, where `defun` parses its type parameters, where it registers them into scope so the body can refer to `T`, where it parses `where` clauses (`ParseClassConstraints`), and where it pushes constraints onto `m_state.m_active_constraints` under an `ActiveConstraintGuard`.

- [ ] **Step 3: Establish what the type checker and codegen need**

The parser is likely the easy half. Report:

- Where a generic `defun` is registered so it can be specialized — `m_generic_functions` in `CodeGenerator.cpp` — and what that registration keys on. Does it require a `FunctionDefinition` statement, or would a lambda bound to a `def` reach it?
- How `TypeChecker` freshens a generic function's parameter types, and whether the lambda path has an equivalent. `MakeLambdaFresheningContext` exists — establish what it currently does and whether it is the right hook.
- Whether a generic lambda would need to be monomorphised at all, or whether the existing constrained-instance specialization machinery already covers it.

- [ ] **Step 4: Size it and report**

Give a line estimate split by layer (parser / type checker / codegen) and name any design fork you find. **Stop and report before implementing.**

Particular things to flag if you see them:
- Whether `fn<T>` creates a parsing ambiguity with `<` as less-than. It should not — `fn` is a keyword — but confirm.
- Whether a generic lambda can be recursive. A `def`-bound lambda already can be (`def Fact = fn(n: Int) : Int => ... Fact(n-1)` works). Establish whether that survives type parameters.
- Whether the anonymous case (`Iter::Map(fn<T>(x: T) : T => x)`) needs anything the named case does not.

---

## Task 2 onward

Written after Task 1 reports. Do not proceed past Task 1 without confirmation.

Expected shape, subject to what the trace finds: parse type parameters and `where` into the existing `MidoriExpression::Function` node, reusing `ParseClassConstraints` and `ActiveConstraintGuard`; route the type checker's generic handling to the lambda path; register generic lambdas for specialization the way generic `defun`s are.

## Done when

1. `def Id = fn<T>(x: T) : T => x;` compiles and `Id(5)` returns `5`, `Id("a")` returns `"a"`.
2. A constrained generic lambda works: `def Show = fn<T>(x: T) : Text where Convertable<T, Text> => x as Text;`
3. An anonymous generic lambda works as a call argument.
4. A generic lambda can be recursive.
5. `defun` is unchanged and every existing test still passes.
6. Tests carry `.expected` snapshots, each verified to bite by corrupting it before restoring.

---

## Trace findings — 2026-09-02

Measured, not reasoned. Three of these are about `defun` as it stands today, not
about the new work.

**The AST slot already exists, and there is a live rejection waiting.**
`MidoriExpression::Function` has `m_generic_params` (`AbstractSyntaxTree.h:510`);
the parser passes an empty vector at `Parser.cpp:4241`. `TypeChecker.cpp:5659-5663`
explicitly errors with *"lambda expressions cannot have generic parameters. Use
'defun' instead."* — unreachable today, live the moment the parser populates the
field. The lambda path also **already has the constraint half**:
`Parser.cpp:4230-4233` does `CollectSignatureConstraints` + `PushActiveConstraints`
+ `ActiveConstraintGuard`. What is missing is the `<T>` slot and the `where`
keyword.

**Binding a generic `defun` to a name loses its genericity — today.**

```
defun identity<T>(x: T) : T => x;
def alias = identity;
alias(42);      // ok
alias("hi");    // Type Checker Error: Expected type 'Int' but got 'Text'
```

Direct calls to `identity` at both types work. `TypeChecker.cpp:5462-5473` picks
`Freshen` over `ApplySubstitution` based on `m_generic_functions.contains(name)`,
and that map is populated only from the `defun` path at `:3235`.

**Passing a generic function as a value crashes.** `apply(identity, 5)` compiles,
generates code, and dies with `panic[MemoryAccessViolation]`. Pre-existing;
tracked separately. Specialization is reachable only through a name-keyed call
path (`CodeGenerator.cpp:3596` requires a `NameAccess` callee), so an argument
position never creates one and the VM jumps into a slot that was never filled.

**Monomorphisation is required for constrained generics, not optional.** A
top-level `defun show<T>(x: T) : Text where Convertable<T, Text>` works; the same
`defun` nested inside a function body fails at codegen with *"Cannot resolve
Convertable instance for type variables outside of specialization context"* — the
`is_global` gate at `CodeGenerator.cpp:2303-2311`. An *unconstrained* local
generic runs fine, because the bytecode is untyped and erasure happens to work.

### Scope decided: parity with `defun`, nothing more

The purpose of this work is to let `defun` be deleted, so a generic lambda needs
to do exactly what `defun` does. Every open fork resolved the same way:

- **Anonymous** generic lambdas: rejected with a clear error. They are the
  unsolved value-form, not an increment on the named case.
- **Capturing** generic lambdas: rejected. `defun` is always capture-free, so
  parity does not require it, and rejecting keeps the work on the `ClosureLifting`
  path instead of needing a second codegen registration route.
- **Local** generic lambdas: rejected, top-level `def` only. A nested constrained
  `defun` already fails, so top-level-only *is* parity; lifting the `is_global`
  gate would be fixing a `defun` bug under cover of this plan.
- **Explicit `m_constraints`** on `MidoriExpression::Function`: added. A `where`
  clause can name constraints the signature never mentions, so signature
  propagation alone loses them.

### Highest-risk item

`ClosureLifting.cpp:172-185` constructs `FunctionDefinition` with the defaulted
empty `constraints` argument, so a `where` clause on a lambda would be **silently
dropped** — miscompiled dispatch, not an error. It already forwards
`m_generic_params` at `:177`, which is why the rest of codegen may need very
little.

### Two type-checker paths, not one

Beyond the `Function` visitor, `def X = fn(...)` has its own branch at
`TypeChecker.cpp:2968-3078` that binds the name at `:3028` *before* evaluating the
lambda — which is how recursion works today. It needs the same generic handling,
and `:3055-3067` hard-errors on `HasTypeVariables`, which a generic lambda's type
always satisfies.

### Inherited, not introduced

Per-use `Freshen` on a recursive generic permits polymorphic recursion, which
monomorphisation cannot always terminate on. `defun` has this hazard today; v1
inherits it rather than creating it.
