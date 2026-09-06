# Associated-Type Equality Constraints — `where Stepper::Item<S> ~ Int`

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** a `where` clause can state that an associated-type projection equals a
type, so a combinator that *transforms* element types can be written.

**Architecture:** purely additive. `~` already lexes as `Token::Name::TILDE`
(`Lexer.cpp:729`), used today only as unary bitwise-NOT (`Parser.cpp:1433`); types
and expressions are separate grammars, so infix `~` between two types is
unambiguous and needs **no lexer work**. The *resolution* half already exists —
`ResolveAssociatedType` (`TypeChecker.cpp:1221`) reduces `Stepper::Item<S>` once
`S` is known. What is missing is a way to say the equation while `S` is still
abstract.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` suites run by the Midori CLI.

**Decision this implements:** spec §13 option **A**, chosen 2026-09-06. Option B
was measured not to work. Option C was declined because it relies on a functional
dependency `S -> A` that nothing enforces.

---

## Why this and nothing else

This is the **only** item on the critical path. Assignment removal — the last
deletion and the only semantic one — needs the immutable prelude, which needs
`Iter::Map` and `Iter::Filter`, which need this. Every other open item in the spec
either sits behind it or was measured to be unreachable (see §4's corrected counts
table, commit `e90e4df`).

## The program that must work

From spec §13. It fails today, and the failure is a *language* gap, not a bug:

```
class Stepper<S> {
    type Item;
    Step: fn(state: S) -> Option<Item>;
};

struct Doubled<S> { inner: S };

instance Stepper<Doubled<S>> where Stepper<S>, Stepper::Item<S> ~ Int {
    type Item = Int;
    Step: fn(state: Doubled<S>) -> Option<Int> =>
        match Stepper::Step(state.inner) with
            case Option::Some(v) => Option::Some<Int>(v * 2)
            case Option::None => Option::None<Int>;
};
```

Without the `~ Int`, `v` has type `Stepper::Item<S>` with `S` abstract and `v * 2`
cannot type. Haskell rejects the identical program for the identical reason.

Note the surface syntax throughout this plan is **post-deletion v2**: no `defun`,
no `new`, `->` in return position. The spec §13 snippet predates those deletions.

## Representation: widen in place, then reject explicitly

`ClassConstraint` (`Type.h:67-75`) is:

```cpp
struct ClassConstraint
{
    std::string m_class_name;
    std::vector<std::shared_ptr<MidoriType>> m_type_args;

    ClassConstraint() = default;
    ClassConstraint(const std::string& typeclass_name, std::vector<std::shared_ptr<MidoriType>>&& type_args);
    bool operator==(const ClassConstraint& other) const;
};
```

**196 sites** reference `ClassConstraint` across `src/`, 95 of them in
`TypeChecker.cpp` and 30 around `m_state.m_active_constraints`. Replacing the
element type of every constraint list with a new `std::variant` would touch all of
them at once.

So: **widen `ClassConstraint` in place** with a discriminator, keeping the 196
sites compiling. That buys a small change and creates one specific hazard — an
equality constraint silently flowing into a path that assumes a class constraint
and trying to resolve an instance for it. Task 1 Step 4 exists to enumerate those
paths, and Task 5 exists to close them. **Do not skip Task 5.** A silently
misrouted constraint is a miscompile, not an error.

## Known traps in this codebase

Established the hard way in earlier plans. Do not rediscover them:

1. **`SubstituteTypeParams` with an empty map is not the identity** — since
   `e730762` it rebuilds a `StructType` with `m_generic_params` cleared.
2. **Exact-key instance lookups miss generic instances.** Use
   `FindMatchingInstance` (`TypeChecker.cpp:1191`), which does the
   `MatchInstanceTypeArg` scan.
3. **Raw `m_global_variables.find` misses imported instances.**
   `ResolveInstanceName` appends `@module`; use `ResolveInstanceNameForTypeArgs`.
4. **`ClosureLifting.cpp:172-185` constructs `FunctionDefinition` with the
   defaulted empty `constraints` argument**, so constraints on a lifted lambda are
   **silently dropped**. Directly in scope here: a dropped equality constraint is a
   miscompile.
5. **`SemanticFacts.cpp` and `SharedAnalysis.cpp` use `if constexpr` chains that
   silently fall through to `else`.** Read them rather than assuming a missed case
   fails to compile.

**Build and test — PowerShell, NOT Git Bash** (Git Bash mangles the vcvars64
quoting and hangs):

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **367/367** and **1024 assertions / 176 cases**, both green as of
2026-09-06. Do not regress either. Do **not** use `build/out/Midori.exe` — a stale
April build that silently gives wrong results.

This checkout is shared with other sessions. Run `git status` first; stage by
explicit pathspec, never `git add -A`. Branch: `v2-expression-oriented`.

---

## Task 1: Trace, then stop

Four earlier plans in this project asserted premises that only survived until
someone traced them. **Probe rather than reason.** Report findings and stop before
implementing.

**Files:** none modified. Scratch `.mdr` probes only.

- [ ] **Step 1: Confirm the failure and record its exact text**

Write the program from "The program that must work" above, minus the `, Stepper::Item<S> ~ Int`
(which does not parse yet). Run it. Record the verbatim error text and the
`TypeChecker.cpp` line that emits it.

Then delete the `* 2` — making the body a pure pass-through — and confirm it
**passes**. That difference is the whole feature. If the pass-through version also
fails, the premise is wrong; **stop and report**.

- [ ] **Step 2: Map the three `ParseClassConstraints` callers**

`Parser::ParseClassConstraints` is called at `Parser.cpp:2923`, `:3720` and
`:4351`. Report what construct each serves (expected: type header, `instance`,
function/lambda signature) and whether all three should accept equality
constraints. State a recommendation.

- [ ] **Step 3: Establish how a constraint reaches the type checker**

Report, with file:line:

- where parsed constraints are pushed onto `m_state.m_active_constraints`
  (`ActiveConstraintGuard`),
- where the type checker *consumes* an active constraint to discharge a projection —
  specifically how `ResolveAssociatedType` (`TypeChecker.cpp:1221`) is reached and
  what it does when the type argument is still a `TypeVariable` or `GenericParam`,
- whether there is an existing substitution/equation map an equality could be
  seeded into, or whether one must be added.

- [ ] **Step 4: Enumerate every site that would misread an equality constraint**

This is the point of the whole task. Widening `ClassConstraint` in place means an
equality constraint can reach code that assumes `m_class_name` names a real
typeclass. Find every such site — instance resolution, mangled-name construction
in `CodeGenerator.cpp`, constraint equality/hashing, `Type.cpp`'s `ToString`.
Report the list. Task 5 closes it.

- [ ] **Step 5: Size it and report**

Line estimate split by layer (parser / type checker / codegen), plus any design
fork. **Stop and report before implementing.**

Flag specifically:
- Whether `ParseType()` already parses `Stepper::Item<S>` into a
  `MidoriType::AssociatedType` (`Parser.cpp:5198` calls `MakeAssociatedType`), so
  the left-hand side needs no new type parsing.
- Whether an equality constraint should be permitted with a non-projection
  left-hand side (`T ~ Int`). Recommendation: **reject it** — it is either a
  tautology or a contradiction, never useful, and accepting it invites confusion
  with a type annotation.
- Whether `ClosureLifting` (trap 4) can carry a lambda bearing one.

---

## Task 2: Widen the constraint representation

Only after Task 1 reports and its findings are confirmed.

**Files:**
- Modify: `src/Compiler/AbstractSyntaxTree/Type.h:67-75`
- Modify: `src/Compiler/AbstractSyntaxTree/Type.cpp` (`operator==`, `ToString`)

- [ ] **Step 1: Add the discriminator**

```cpp
struct ClassConstraint
{
    enum class Kind
    {
        Class,
        Equality
    };

    std::string m_class_name;
    std::vector<std::shared_ptr<MidoriType>> m_type_args;
    Kind m_kind = Kind::Class;
    std::shared_ptr<MidoriType> m_equality_lhs;
    std::shared_ptr<MidoriType> m_equality_rhs;

    ClassConstraint() = default;
    ClassConstraint(const std::string& typeclass_name, std::vector<std::shared_ptr<MidoriType>>&& type_args);
    ClassConstraint(std::shared_ptr<MidoriType>&& lhs, std::shared_ptr<MidoriType>&& rhs);
    bool operator==(const ClassConstraint& other) const;
    bool IsEquality() const;
};
```

The existing constructor keeps `m_kind` at `Kind::Class`, so all 196 sites and
every existing test are unaffected by this step alone.

- [ ] **Step 2: Implement the new members in `Type.cpp`**

`IsEquality()` returns `m_kind == Kind::Equality`. Extend `operator==` to compare
`m_kind` first and then the equality operands when the kind is `Equality` — two
constraints of different kinds are never equal. Extend `ToString` so an equality
constraint renders as `Lhs ~ Rhs`, not as a class name with no type arguments.

- [ ] **Step 3: Build and run both suites**

Nothing should change. This step proves the widening is inert.

Expected: **367/367**, **1024 assertions / 176 cases**.

- [ ] **Step 4: Commit**

```bash
git add src/Compiler/AbstractSyntaxTree/Type.h src/Compiler/AbstractSyntaxTree/Type.cpp
git commit -m "refactor(type): widen ClassConstraint to carry an equality form"
```

---

## Task 3: Parse `Lhs ~ Rhs` in `where` clauses

**Files:**
- Modify: `src/Compiler/Parser/Parser.cpp` (`Parser::ParseClassConstraints`)
- Test: `test/typeclass/failure/equality_constraint_non_projection.mdr` + `.expected`

- [ ] **Step 1: Write the failing test first**

`test/typeclass/success/equality_constraint_parses.mdr` — the smallest program
whose *only* new element is the syntax:

```
class Stepper<S> {
    type Item;
    Step: fn(state: S) -> Option<Item>;
};

struct Doubled<S> { inner: S };

instance Stepper<Doubled<S>> where Stepper<S>, Stepper::Item<S> ~ Int {
    type Item = Int;
    Step: fn(state: Doubled<S>) -> Option<Int> =>
        match Stepper::Step(state.inner) with
            case Option::Some(v) => Option::Some<Int>(v)
            case Option::None => Option::None<Int>;
};

IO::PrintLine("parsed");
```

The body deliberately does **not** use `* 2` yet — Task 3 is parsing only. Run it
and confirm it fails in the parser, not the type checker.

- [ ] **Step 2: Dispatch on `::` inside `parse_constraint`**

In the `parse_constraint` lambda, after consuming the leading
`IDENTIFIER_LITERAL`, one token of lookahead distinguishes the two forms: `::`
means an associated-type projection and therefore an equality constraint; `<`
means a class constraint. Route the projection case through the existing type
parser so the left-hand side becomes a `MidoriType::AssociatedType`
(`Parser.cpp:5198`), then consume `Token::Name::TILDE` and `ParseType()` for the
right-hand side, returning the equality constructor added in Task 2.

Keep the existing class-constraint path byte-for-byte unchanged.

- [ ] **Step 3: Reject a non-projection left-hand side**

`where T ~ Int` must be a clean parser error naming the rule: an equality
constraint's left side must be an associated-type projection such as
`Stepper::Item<S>`. Add
`test/typeclass/failure/equality_constraint_non_projection.mdr` and its
`.expected`.

- [ ] **Step 4: Build and confirm the parse test passes**

`equality_constraint_parses.mdr` must now reach the type checker. Both suites must
still be green at **367/367** plus the new cases.

- [ ] **Step 5: Commit**

```bash
git add src/Compiler/Parser/Parser.cpp test/typeclass/
git commit -m "feat(parser): parse associated-type equality constraints in where clauses"
```

---

## Task 4: Discharge the equation in the type checker

**Files:**
- Modify: `src/Compiler/TypeChecker/TypeChecker.cpp`
- Test: `test/typeclass/success/equality_constraint_transforms.mdr` + `.expected`

- [ ] **Step 1: Write the failing test**

`equality_constraint_transforms.mdr` — the Task 3 program with the body changed to
`Option::Some<Int>(v * 2)` and a driver that steps a `Doubled<...>` and prints the
result. This is the program from spec §13. Run it, confirm it fails, record the
error.

- [ ] **Step 2: Seed the equation where active constraints are consulted**

Using Task 1 Step 3's findings: when an active constraint is an equality, register
its left-hand projection as resolving to its right-hand type, so that a projection
encountered while `S` is abstract reduces to the stated type instead of remaining
opaque. Reuse `ResolveAssociatedType` (`TypeChecker.cpp:1221`) rather than adding
a second reduction path.

- [ ] **Step 3: Build and confirm the transform test passes**

The stepped value must come back doubled.

- [ ] **Step 4: Verify an unsatisfied equality is caught**

Add `test/typeclass/failure/equality_constraint_unsatisfied.mdr`: an instance
promising `Stepper::Item<S> ~ Int` used at an `S` whose `Item` is `Text`. This must
be a clean type error naming both types, **not** a miscompile. If it compiles, the
equation is being assumed rather than checked; **stop and report**.

- [ ] **Step 5: Commit**

```bash
git add src/Compiler/TypeChecker/TypeChecker.cpp test/typeclass/
git commit -m "feat(typechecker): discharge associated-type equality constraints"
```

---

## Task 5: Close the misrouting hazard

The one created by widening in place. Task 1 Step 4 produced the list.

**Files:**
- Modify: each site named in the Task 1 Step 4 report — expected to include
  `src/Compiler/TypeChecker/TypeChecker.cpp` (instance resolution) and
  `src/Compiler/CodeGenerator/CodeGenerator.cpp` (mangled-name construction).

- [ ] **Step 1: Guard every site that assumes `m_class_name` names a typeclass**

At each, skip or reject equality constraints explicitly. An equality constraint
must never reach `FindMatchingInstance` or contribute to a mangled instance name.

- [ ] **Step 2: Prove each guard bites**

For each guard, temporarily remove it, rebuild, and confirm a test fails or an
assertion fires. Restore. A guard that cannot be observed to matter should be
reported rather than kept on faith.

- [ ] **Step 3: Check trap 4 — `ClosureLifting`**

`ClosureLifting.cpp:172-185` defaults `constraints` to empty. Write a lambda
carrying an equality constraint, confirm it either works or is a clean error, and
**not** that the constraint is silently dropped.

- [ ] **Step 4: Build, run both suites, commit**

```bash
git add src/Compiler/TypeChecker/TypeChecker.cpp src/Compiler/CodeGenerator/CodeGenerator.cpp
git commit -m "fix(typechecker,codegen): keep equality constraints out of instance resolution"
```

---

## Task 6: `Iter::Map` and `Iter::Filter`

The reason the feature exists. Until these compile, the feature is unproven.

**Files:**
- Test: `test/typeclass/success/iter_map_filter.mdr` + `.expected`

- [ ] **Step 1: Write `Map` over a stepper**

A `Map<S, A, B>` wrapper whose `Stepper` instance carries
`where Stepper<S>, Stepper::Item<S> ~ A` and whose `Step` applies
`f : fn(A) -> B`. Drive it over a concrete source stepper and print the results.

- [ ] **Step 2: Write `Filter` over a stepper**

Same shape, predicate `fn(A) -> Bool`, looping until an element passes or the
source is exhausted.

- [ ] **Step 3: Compose them**

`Map` over a `Filter` over a source. This is the case that proves the constraint
composes through two layers rather than only one.

- [ ] **Step 4: Verify every snapshot bites**

Corrupt each `.expected`, confirm `[FAIL]`, restore, confirm `[OK]`. A snapshot
that does not bite is worse than none, because it looks like coverage.

- [ ] **Step 5: Commit**

```bash
git add test/typeclass/
git commit -m "test: Iter::Map and Iter::Filter via equality constraints"
```

---

## Task 7: Record the outcome

- [ ] **Step 1: Update spec §13**

Mark option A implemented, with the commit range. Record anything the trace
contradicted — this project's specs carry their corrections inline, and §4's
counts table (`e90e4df`) is the model.

- [ ] **Step 2: Note what this unblocks**

Spec §11's library rewrite and, behind it, assignment removal.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/specs/2026-09-01-expression-oriented-midori-design.md
git commit -m "docs(spec): record equality constraints as implemented"
```

---

## Done when

1. `where Stepper::Item<S> ~ Int` parses in all constraint positions Task 1 Step 2
   recommends.
2. A non-projection left-hand side is a clean parser error.
3. The spec §13 program — the one with `* 2` — compiles and runs.
4. An instance whose promised equality does not hold is a clean type error, not a
   miscompile.
5. No equality constraint reaches instance resolution or a mangled name, and each
   guard was observed to bite.
6. `Map`, `Filter`, and `Map` composed over `Filter` all work.
7. Both suites green: **367/367** plus new cases, and **1024 assertions / 176
   cases** plus any added.

---

## Trace findings — 2026-09-06

Measured, not reasoned. Run inline rather than by subagent (the dispatched one
died on a rate limit before doing any work).

### The premise holds, and the error is exactly as §13 predicted

The pass-through case already passes — it is
`test/typeclass/success/constrained_instance_assoc_type.mdr`, live in the green
suite. Adding `* 2` and `type Item = Int` gives:

```
Type mismatch between 'Stepper::Item<T2>' and 'Int'
```

at the `* 2` site, plus the known cascade at the use site
(`no matching concrete instance for 'Stepper::Step'`). Emitted from
`TypeChecker::MakeUnificationError` (`TypeChecker.cpp:1387`, message at `:1411`).

### This plan's own code samples were written in the wrong syntax

Corrected below; **do not copy the samples from the earlier sections of this
plan**. An instance member is a `def` binding, not a signature:

```
type Counter = { current: Int, limit: Int };
type Doubled<S> = { inner: S };

instance Stepper<Doubled<S>> where Stepper<S>, Stepper::Item<S> ~ Int {
    type Item = Int;
    def Step = fn(state: Doubled<S>) -> Option<Int> =>
        match Stepper::Step(state.inner) with
            case Option::Some(v) => Option::Some(v * 2)
            case Option::None() => Option::None();
};
```

A class body still uses the `Name: fn(...) -> T;` signature form. Record types are
`type N = { field: T };`. Model probes on
`test/typeclass/success/constrained_instance_assoc_type.mdr`.

### `Unify` has no `AssociatedType` case at all — this is the insertion point

`TypeChecker::Unify` spans `:1437-1711` and contains **zero** occurrences of
`AssociatedType`. A projection meeting a concrete type therefore falls straight
through to the error path. Task 4 does not need a new equation map seeded
somewhere subtle; it needs an `AssociatedType` branch in `Unify` that consults the
active equality constraints before failing.

`ResolveAssociatedType` (`:1251`) is the right reducer and already degrades
correctly: when `FindMatchingInstance` cannot resolve — which is exactly the
abstract-`S` case — it returns the projection unchanged rather than erroring. So
the branch is "reduce, and if still a projection, look for a stated equality".

### Widening in place needs no plumbing, which settles the representation

`m_active_constraints` is declared `std::vector<MidoriType::ClassConstraint>`
(`TypeChecker.h:105`). Widening `ClassConstraint` therefore makes equality
constraints visible inside `Unify` with no new member, no new parameter, and no
threading. This is a stronger argument for widening in place than the
196-call-site one the plan opened with.

### The three parse callers

| line | function | verdict |
|---|---|---|
| `:2923` | `ParseTypeDeclarationHeader` (`:2873`) | allow |
| `:3720` | `ParseInstanceDeclaration` (`:3630`) | **required** |
| `:4351` | `ParseFunctionExpression` (`:4258`) | allow — generic functions consuming a stepper need it |

All three route through the one `ParseClassConstraints`, so allowing everywhere is
zero extra work while restricting would need a mode flag. Allow all three.

### Task 5's list, enumerated

Every site iterating `m_active_constraints` treats `m_class_name` as a real
typeclass. Each needs an `IsEquality()` skip.

`TypeChecker.cpp` — **16 read sites**: `:894`, `:1165`, `:2338`, `:3157`, `:3506`,
`:4306`, `:4507`, `:4623`, `:4771`, `:4825`, `:4865`, `:4976`, `:5337`, `:5816`,
`:5842`, `:5875`, `:6146`. Plus the free function `ContainsConstraint` (`:430`),
which `:3157`, `:3506` and `:6146` call.

`CodeGenerator.cpp` — constraint substitution loops at `:5866`, `:6389`, `:6433`,
`:6457`, `:6508`, and three `MidoriType` visitor overloads for `ClassConstraint`
at `:5621`, `:5786`, `:6542` that already no-op.

Roughly 22 guards, each one line. Tractable, but this is the bulk of the work and
the plan's warning was justified.

### Trap 4 probably does not apply — still verify

`ClosureLifting` early-returns before lifting in both shapes that can carry a
constraint: a top-level `def` bound to a lambda (`ClosureLifting.cpp:171-175`) and
any lambda with generic parameters (`:181-187`). Both carry comments explaining
why. So the defaulted-empty-`constraints` construction is likely unreachable for
constraint-bearing lambdas. **This was read, not probed** — Task 5 Step 3 still
runs the probe.

### Sizing

Parser ~40 lines (one dispatch in `parse_constraint`, one error). Type
representation ~35 lines. Type checker ~60 lines, of which the `Unify` branch is
maybe 25 and the rest are guards. Codegen ~15 lines of guards. Tests ~150 lines of
`.mdr`. No lexer work, no VM work, no new opcode.

### Verdict

**Task 2 proceeds as written.** The `Kind` discriminator and the second
constructor are unchanged by the trace. Task 4's step 2 is now concrete — add the
`AssociatedType` branch to `Unify` — and should be rewritten from "seed the
equation where active constraints are consulted" to that.
