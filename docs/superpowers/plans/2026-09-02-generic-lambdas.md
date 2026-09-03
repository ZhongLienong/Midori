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
