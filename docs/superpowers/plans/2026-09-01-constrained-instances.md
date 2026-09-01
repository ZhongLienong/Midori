# Constrained Instances Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Allow `instance C<T> where D<T> { … }`, so an instance can require constraints on its own type parameters.

**Architecture:** The AST and type-checker data structures already carry constraints — `MidoriStatement::Instance::m_constraints` and `TypeChecker::InstanceInfo::m_constraints` both exist. Neither is ever populated: the parser hardcodes an empty vector at construction, and instance resolution ignores the field. This plan wires the existing path end to end: parse `where` using the existing `ParseClassConstraints`, carry the result into `InstanceInfo`, and check it during `FindMatchingInstance`.

**Tech Stack:** C++23, CMake + Ninja presets, Catch2-style `.mdr` suites run by the Midori CLI.

**Coding style — follow exactly (from `CLAUDE.md`):**
- Do **not** use `auto`. Spell out every type, including iterator and lambda return types.
- Use C++23 features. Prefer `std::ranges` algorithms and function chaining over hand-written loops with mutable flags.
- `m_` prefix for members, `s_` for statics. Pascal case for classes, structs and functions.
- Define and use real constructors — no aggregate initialisation of guard types.
- Braces on every `if` / `for`, never a one-liner body.
- Implementations go in `.cpp` files; headers declare.
- No comments on self-explanatory code, and no new docs unless asked.

**Why first:** Nothing else in the v2 spec works without it. Iterator combinators need `instance Iterable<Mapped<I, A, B>> where Iterable<I>`, and that is unparseable today.

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `src/Compiler/Parser/Parser.cpp` | `ParseInstanceDeclaration` (from :3491), `Instance` construction (:3849) | Modify |
| `src/Compiler/TypeChecker/TypeChecker.cpp` | Instance registration (:3352–3380), `FindMatchingInstance` (:1083) | Modify |
| `test/typeclass/success/constrained_instance.mdr` | Constrained instance resolves when the constraint holds | Create |
| `test/typeclass/success/constrained_instance_nested.mdr` | Constraint propagates through a wrapper type | Create |
| `test/typeclass/failure/constrained_instance_unsatisfied.mdr` | Constraint violation is a compile error | Create |

**Build and test commands used throughout — verified working 2026-09-01:**

The MSVC environment must be loaded first or `cl.exe` cannot find the standard
library headers. A bare `cmake --build` fails with
`fatal error C1083: Cannot open include file: 'cinttypes'`.

**Run every build command in PowerShell, not Git Bash.** Git Bash mangles the
quoting around the `vcvars64.bat` path and the command hangs rather than failing.

Build:

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

Test — note the binary lives under the preset's output directory. Do **not** use
`build/out/Midori.exe`; that tree is a stale April build and will silently give
wrong results:

```bash
./out/build/ninja/x64-development/out/Midori.exe test typeclass
```

Baseline on this branch, measured 2026-09-01:

- `test typeclass` → `Total: 23/23 passed`
- full `test` → `253/258`

**The typeclass denominator grows as you add test files.** Discovery is a
`recursive_directory_iterator` over the suite directory
(`src/Utility/TestRunner/TestRunner.cpp:610`), so adding a file makes the target
24, then 25, and so on. The bar is *no regression in the 23 pre-existing tests*,
not a literal count. (The 23rd is `prelude/success/builtin_type_operations_typeclasses.mdr`,
matched by filename rather than living under `test/typeclass/`.)

**Four failures are pre-existing on this branch and unrelated to this plan.** Do
not chase them:

- `concurrency/worker_cancel_blocked_receive`
- `concurrency/worker_cancel_spin`
- `static_analyzer/warning_then_codegen_failure`
- `static_analyzer/unused_local_warning`

---

## Task 1: Parse `where` on instance declarations

**Files:**
- Modify: `src/Compiler/Parser/Parser.cpp:3491` (`ParseInstanceDeclaration`) and `:3849` (`Instance` construction)
- Test: `test/typeclass/success/constrained_instance.mdr`

- [ ] **Step 1: Write the failing test**

Create `test/typeclass/success/constrained_instance.mdr`:

```
module ConstrainedInstance

import { "../../../MidoriPrelude/IO.mdr", }

class Show<T> {
    show: fn(value: T) -> Text;
};

struct Boxed<T> { item: T };

instance Show<Int> {
    defun show(value: Int) : Text => "Int(" ++ (value as Text) ++ ")";
};

instance Show<Boxed<T>> where Show<T> {
    defun show(value: Boxed<T>) : Text => "Boxed(" ++ Show::show(value.item) ++ ")";
};

def inner = new Boxed<Int>(7);
IO::PrintLine(Show::show(inner));
```

- [ ] **Step 2: Run it to confirm it fails**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance.mdr
```

Expected: a **Parser Error** at the `where` on the `instance Show<Boxed<T>>` line — `ParseInstanceDeclaration` has no `WHERE` branch, so it fails at the token after the type-argument list.

- [ ] **Step 3: Parse the clause**

In `ParseInstanceDeclaration`, after the type-argument list is consumed and before the opening brace is expected, add the same clause the function parser uses at `Parser.cpp:2815`:

```cpp
std::vector<MidoriType::ClassConstraint> constraints;
if (Match(Token::Name::WHERE))
{
    std::expected<std::vector<MidoriType::ClassConstraint>, CompilerError> constraints_result =
        ParseClassConstraints(typeclass_name);
    if (!constraints_result.has_value())
    {
        return std::unexpected(constraints_result.error());
    }

    constraints = std::move(constraints_result.value());
}
```

`ParseClassConstraints` is declared at `Parser.h:618` and already handles the comma-separated `C<T>, D<U>` form. The instance's type parameters are in scope here because `ParseInstanceDeclaration` calls `BeginScope()` before parsing type arguments.

- [ ] **Step 4: Pass the constraints into the AST node**

At `Parser.cpp:3849`, replace the hardcoded empty vector:

```cpp
MidoriStatement::Instance(std::move(typeclass_name), std::move(type_args), std::vector<MidoriType::ClassConstraint>(), std::move(associated_types), std::move(methods))
```

with:

```cpp
MidoriStatement::Instance(std::move(typeclass_name), std::move(type_args), std::move(constraints), std::move(associated_types), std::move(methods))
```

Leave the derive-generated sites passing `{}` — derived instances carry no user-written constraints. There are **four**, not two: `Parser.cpp:5710`, `:5774`, `:5888`, `:6022` (line numbers after the Task 1 insertion). None of them should be touched.

- [ ] **Step 4b: Make the constraints active while parsing method bodies**

Parsing alone is not enough. Inside a method of `instance Show<Boxed<T>> where Show<T>`, the body calls `Show::show(value.item)` on an abstract `T` — that only resolves if `Show<T>` is an *assumed* constraint for the duration of the body.

The function parser already does this: it pushes parsed constraints onto `m_state.m_active_constraints` under an `ActiveConstraintGuard` at `Parser.cpp:2832-2840`. Mirror that here. The two cases are exactly analogous — in `defun f<T>(...) where Equatable<T>` the parameter is abstract and the constraint assumed, and the same holds inside a constrained instance's methods.

The guard's lifetime must cover method-body parsing and end before `ParseInstanceDeclaration` returns, so constraints do not leak into declarations that follow. An instance with no `where` clause must push nothing, leaving the guard a no-op.

**What this step does and does not fix.** It does *not* clear the type-checker error inside the method body. Traced 2026-09-01: the parser's only consumers of `m_active_constraints` are the name-resolution sites at `Parser.cpp:543` (unqualified) and `:1831` (qualified), and a qualified `Show::show` already resolves through the concrete-typeclass fallback just below `:1844`. Both the method-body error and the call-site error are emitted by the type checker and are cleared by Tasks 2 and 3 together.

What it does fix is consistency: instance method bodies now treat where-clause constraints as assumed during name resolution, exactly as function bodies do. Demonstrable on an unqualified call inside a constrained instance method — `Undefined name.` before, `Unqualified class method 'show'. Use qualified syntax like 'Show::show'.` after.

- [ ] **Step 5: Build and re-run**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance.mdr
```

Expected: the parser error is gone. The program may still fail later — Task 2 and Task 3 complete the path. Record which error you now get; it should be a type-checker error, not a parser error.

- [ ] **Step 6: Confirm nothing regressed**

```bash
./out/build/ninja/x64-development/out/Midori.exe test typeclass
```

Expected: the pre-existing suite passes exactly as before. Unconstrained instances take the `Match(WHERE)` false branch and are unaffected.

- [ ] **Step 7: Commit**

```bash
git add src/Compiler/Parser/Parser.cpp test/typeclass/success/constrained_instance.mdr
git commit -m "feat(parser): parse where clauses on instance declarations"
```

---

## Task 2: Carry constraints into `InstanceInfo`

**Files:**
- Modify: `src/Compiler/TypeChecker/TypeChecker.cpp:3380` (the `m_instances.emplace` in instance registration)

- [ ] **Step 1: Confirm the field is currently dropped**

```bash
grep -n "m_instances.emplace" src/Compiler/TypeChecker/TypeChecker.cpp
```

Expected: two sites, `:2669` and `:3380`, both passing `std::vector<MidoriType::ClassConstraint>{}` as the third `InstanceInfo` argument. `:2669` is the derive path and stays empty; `:3380` is the user-declaration path and is the one to change.

- [ ] **Step 2: Write the failing test**

Create `test/typeclass/success/constrained_instance_nested.mdr`:

```
module ConstrainedInstanceNested

import { "../../../MidoriPrelude/IO.mdr", }

class Show<T> {
    show: fn(value: T) -> Text;
};

struct Boxed<T> { item: T };

instance Show<Int> {
    defun show(value: Int) : Text => "Int(" ++ (value as Text) ++ ")";
};

instance Show<Boxed<T>> where Show<T> {
    defun show(value: Boxed<T>) : Text => "Boxed(" ++ Show::show(value.item) ++ ")";
};

def twice = new Boxed<Boxed<Int>>(new Boxed<Int>(3));
IO::PrintLine(Show::show(twice));
```

This is the case that only works if the constraint is stored: resolving `Show<Boxed<Boxed<Int>>>` requires satisfying `Show<Boxed<Int>>`, which in turn requires `Show<Int>`.

- [ ] **Step 3: Run it to confirm it fails**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance_nested.mdr
```

Expected: FAIL. The constraint was parsed but discarded at registration, so the recursive requirement is never checked or propagated.

- [ ] **Step 4: Store the constraints**

At `TypeChecker.cpp:3380`, change the third argument of the `InstanceInfo` construction from `std::vector<MidoriType::ClassConstraint>{}` to the statement's constraints:

```cpp
m_instances.emplace
(
    std::move(instance_key),
    InstanceInfo
    (
        instance_stmt.m_class_name.m_lexeme,
        std::move(type_args_copy),
        std::vector<MidoriType::ClassConstraint>(instance_stmt.m_constraints),
        std::move(associated_type_bindings),
        std::move(method_impls)
    )
);
```

Copy rather than move from `instance_stmt.m_constraints` — the statement is still owned by the program tree and may be visited again during codegen.

- [ ] **Step 5: Build and confirm the stored value**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance.mdr
```

Expected: the single-level case from Task 1 now runs and prints `Boxed(Int(7))`. The nested case from Step 2 may still fail until Task 3.

- [ ] **Step 6: Commit**

```bash
git add src/Compiler/TypeChecker/TypeChecker.cpp test/typeclass/success/constrained_instance_nested.mdr
git commit -m "feat(typecheck): store instance where-constraints in InstanceInfo"
```

---

## Task 3: Check constraints during instance resolution

**Files:**
- Modify: `src/Compiler/TypeChecker/TypeChecker.cpp:1083` (`FindMatchingInstance`)

- [ ] **Step 1: Read the current matcher**

```bash
sed -n '1083,1125p' src/Compiler/TypeChecker/TypeChecker.cpp
```

It walks `m_instances`, unifies each candidate's type arguments against the requested ones, and on success builds `ResolvedInstanceMatch{ .m_instance = &instance_info, .m_substitutions = std::move(substitutions) }` at `:1116`. It never consults `instance_info.m_constraints`.

- [ ] **Step 2: Run the nested test to confirm it still fails**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance_nested.mdr
```

Expected: FAIL. Nothing yet recurses into the stored constraints.

- [ ] **Step 3: Write the substitution helper it needs**

There is no existing helper that applies a `TypeEnvironment` to a type. `ApplySubstitution` (`TypeChecker.h:155`) is non-const and applies the checker's own `m_type_substitution`, so it cannot be called from the `const` `FindMatchingInstance`.

Add a free function in the same anonymous namespace as `MatchInstanceTypeArg`, directly above it at `TypeChecker.cpp:546`. `TypeEnvironment` is `std::unordered_map<std::string, std::shared_ptr<MidoriType>>` (`TypeChecker.h:23`), keyed by `MidoriType::GenericParam::m_name`:

```cpp
std::shared_ptr<MidoriType> SubstituteWithEnvironment(const std::shared_ptr<MidoriType>& type, const std::unordered_map<std::string, std::shared_ptr<MidoriType>>& env)
{
    if (!type)
    {
        return type;
    }

    if (type->IsType<MidoriType::GenericParam>())
    {
        const std::string& param_name = type->GetType<MidoriType::GenericParam>().m_name;
        std::unordered_map<std::string, std::shared_ptr<MidoriType>>::const_iterator it = env.find(param_name);
        return it != env.end() ? it->second : type;
    }

    if (type->IsType<MidoriType::ArrayType>())
    {
        std::shared_ptr<MidoriType> element = SubstituteWithEnvironment(type->GetType<MidoriType::ArrayType>().m_element_type, env);
        return MidoriType::MakeArrayType(std::move(element));
    }

    return type;
}
```

Handle only `GenericParam` and `ArrayType` for now. A constraint's type arguments are the instance's own type parameters, possibly wrapped — `where Iterable<I>` and `where Show<T>` are both bare params, and `Array<T>` is the one wrapper the prelude needs. If a later plan requires deeper nesting, extend this function then rather than speculatively now.

Confirm the exact constructor name for arrays before building:

```bash
grep -n "MakeArrayType\|MakeArray" src/Compiler/AbstractSyntaxTree/Type.h
```

Use whatever that returns; if arrays are constructed differently, mirror the pattern used at `TypeChecker.cpp:578`.

- [ ] **Step 4: Add the constraint check, in the right place**

Placement matters. The loop in `FindMatchingInstance` runs: match type args → `if (!matched) continue;` → `if (resolved_match.has_value()) return std::nullopt;` (ambiguity) → assign `resolved_match`.

The check goes **after** `if (!matched) continue;` and **before** the ambiguity check, so a candidate whose constraints fail is skipped entirely and never counts as a competing match:

```cpp
const bool all_constraints_satisfied = std::ranges::all_of
(
    instance_info.m_constraints,
    [this, &substitutions](const MidoriType::ClassConstraint& constraint) -> bool
    {
        std::vector<std::shared_ptr<MidoriType>> constraint_args;
        constraint_args.reserve(constraint.m_type_args.size());
        std::ranges::transform
        (
            constraint.m_type_args,
            std::back_inserter(constraint_args),
            [&substitutions](const std::shared_ptr<MidoriType>& constraint_arg) -> std::shared_ptr<MidoriType>
            {
                return SubstituteWithEnvironment(constraint_arg, substitutions);
            }
        );

        return FindMatchingInstance(constraint.m_class_name, constraint_args).has_value();
    }
);

if (!all_constraints_satisfied)
{
    continue;
}
```

`std::ranges::all_of` short-circuits, so this keeps the early-exit behaviour without a mutable flag. Confirm `<algorithm>` and `<iterator>` are included at the top of `TypeChecker.cpp`; add them if not.

Putting it after the ambiguity check instead would make an unsatisfiable instance collide with a valid one and report a spurious ambiguity error.

The recursive call is what makes the nested case work: `Show<Boxed<Boxed<Int>>>` requires `Show<Boxed<Int>>`, which requires `Show<Int>`, which is concrete, has no constraints, and terminates the recursion.

Check the field name on `ClassConstraint` before building:

```bash
grep -n "struct ClassConstraint" -A 8 src/Compiler/AbstractSyntaxTree/Type.h
```

Use its actual member names in place of `m_class_name` and `m_type_args` if they differ.

- [ ] **Step 5: Guard against runaway recursion**

A malformed instance such as `instance Show<Boxed<T>> where Show<Boxed<T>>` would recurse forever. Add a depth counter as a member on `TypeChecker`:

In `TypeChecker.h`, beside `m_instances` at `:97`:

```cpp
mutable int m_instance_resolution_depth = 0;
```

At the top of `FindMatchingInstance`:

Add the guard type in the same anonymous namespace as `SubstituteWithEnvironment`, following the `ScopeGuard` pattern already used at `Parser.cpp:3512`:

```cpp
class InstanceDepthGuard
{
private:
    int* m_depth;

public:
    explicit InstanceDepthGuard(int* depth) noexcept
        : m_depth(depth)
    {
        *m_depth += 1;
    }

    ~InstanceDepthGuard() noexcept
    {
        *m_depth -= 1;
    }

    InstanceDepthGuard(const InstanceDepthGuard&) = delete;
    InstanceDepthGuard& operator=(const InstanceDepthGuard&) = delete;
};
```

Then at the top of `FindMatchingInstance`:

```cpp
static constexpr int s_max_instance_resolution_depth = 64;
if (m_instance_resolution_depth >= s_max_instance_resolution_depth)
{
    return std::nullopt;
}

const InstanceDepthGuard depth_guard(&m_instance_resolution_depth);
```

- [ ] **Step 6: Build and run both success tests**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance.mdr
```

Expected: `Boxed(Int(7))`

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance_nested.mdr
```

Expected: `Boxed(Boxed(Int(3)))`

- [ ] **Step 7: Run the full suite**

```bash
./out/build/ninja/x64-development/out/Midori.exe test typeclass
```

Expected: all pre-existing tests still pass. Instances with no constraints skip the loop entirely.

- [ ] **Step 8: Commit**

```bash
git add src/Compiler/TypeChecker/TypeChecker.cpp src/Compiler/TypeChecker/TypeChecker.h
git commit -m "feat(typecheck): enforce instance where-constraints during resolution"
```

---

## Task 4: Reject unsatisfied constraints with a clear error

**Files:**
- Create: `test/typeclass/failure/constrained_instance_unsatisfied.mdr`
- Modify: `src/Compiler/TypeChecker/TypeChecker.cpp` (the call site that reports a missing instance)

- [ ] **Step 1: Write the failing test**

Create `test/typeclass/failure/constrained_instance_unsatisfied.mdr`:

```
module ConstrainedInstanceUnsatisfied

// Test: using a constrained instance whose constraint is not satisfied
// This should produce a compile-time error

import { "../../../MidoriPrelude/IO.mdr", }

class Show<T> {
    show: fn(value: T) -> Text;
};

struct Boxed<T> { item: T };
struct Opaque { tag: Int };

instance Show<Boxed<T>> where Show<T> {
    defun show(value: Boxed<T>) : Text => "Boxed(" ++ Show::show(value.item) ++ ")";
};

// No instance Show<Opaque> exists, so Show<Boxed<Opaque>> must not resolve.
def bad = new Boxed<Opaque>(new Opaque(1));
IO::PrintLine(Show::show(bad));
```

- [ ] **Step 2: Run it and inspect the current message**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/failure/constrained_instance_unsatisfied.mdr
```

Expected after Task 3: compilation fails, which is correct — but the message will say the instance was not found rather than naming the unmet constraint. Record the exact text.

- [ ] **Step 3: Name the unmet constraint**

Have `FindMatchingInstance` record why the last candidate was rejected. Add beside the depth counter in `TypeChecker.h`:

```cpp
mutable std::string m_last_unsatisfied_constraint;
```

In the constraint loop from Task 3, before `break`:

```cpp
m_last_unsatisfied_constraint = constraint.m_class_name + "<" + constraint_args[0]->ToString() + ">";
```

Then, at the site that reports a missing instance, append it when non-empty:

```cpp
std::string detail = m_last_unsatisfied_constraint.empty()
    ? std::string()
    : " (unsatisfied constraint: " + m_last_unsatisfied_constraint + ")";
```

Clear `m_last_unsatisfied_constraint` at the start of each top-level resolution so a stale value from an earlier successful lookup cannot leak into an unrelated error.

- [ ] **Step 4: Re-run and check the message**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/failure/constrained_instance_unsatisfied.mdr
```

Expected: the error now names `Show<Opaque>` as the unsatisfied constraint rather than only reporting that `Show<Boxed<Opaque>>` was not found.

- [ ] **Step 5: Run the full suite**

```bash
./out/build/ninja/x64-development/out/Midori.exe test typeclass
```

Expected: all success tests pass; all four pre-existing failure tests still fail with their original error codes.

- [ ] **Step 6: Commit**

```bash
git add src/Compiler/TypeChecker/TypeChecker.cpp src/Compiler/TypeChecker/TypeChecker.h test/typeclass/failure/constrained_instance_unsatisfied.mdr
git commit -m "feat(typecheck): name the unsatisfied constraint when an instance fails to resolve"
```

---

## Task 5: Prove the target case — a generic combinator instance

**Files:**
- Create: `test/typeclass/success/constrained_instance_combinator.mdr`

This is the shape the whole v2 library rewrite depends on. It is worth its own test so a regression here is caught immediately rather than during the `Iter` rewrite.

- [ ] **Step 1: Write the test**

Create `test/typeclass/success/constrained_instance_combinator.mdr`:

```
module ConstrainedInstanceCombinator

import { "../../../MidoriPrelude/IO.mdr", "../../../MidoriPrelude/Prelude/Option.mdr" }

use Option.{ Option }

class Stepper<S> {
    type Item;
    Step: fn(state: S) -> Option<Item>;
};

struct Counter { current: Int, limit: Int };
struct Doubled<S> { inner: S };

instance Stepper<Counter> {
    type Item = Int;
    defun Step(state: Counter) : Option<Int> =>
        if state.current >= state.limit
        then new Option::None<Int>()
        else new Option::Some<Int>(state.current);
};

instance Stepper<Doubled<S>> where Stepper<S> {
    type Item = Int;
    defun Step(state: Doubled<S>) : Option<Int> =>
        match Stepper::Step(state.inner) with
            case Option::Some(v) => new Option::Some<Int>(v * 2)
            case Option::None() => new Option::None<Int>();
};

def wrapped = new Doubled<Counter>(new Counter(3, 10));
match Stepper::Step(wrapped) with
    case Option::Some(v) => IO::PrintLine(v as Text)
    case Option::None() => IO::PrintLine("none");
```

- [ ] **Step 2: Run it**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/constrained_instance_combinator.mdr
```

Expected: prints `6` — `Counter(3, 10)` steps to `3`, and the `Doubled` wrapper doubles it.

If this fails while Tasks 1–4 pass, the gap is associated-type resolution through a constrained instance rather than the constraint check itself. Check `TypeChecker.cpp:1191`, where `FindMatchingInstance` is called for associated-type lookup, and confirm the substitution built there reaches the constraint check added in Task 3.

- [ ] **Step 3: Run the full test suite, not just typeclass**

```bash
./out/build/ninja/x64-development/out/Midori.exe test
```

Expected: every suite passes. This change touches instance resolution, which `Equatable`, `Orderable`, `Countable`, `Concatenable` and `Convertable` all depend on — `TypeChecker.cpp:4130`, `:4273`, `:4327` and `:4499` are all call sites.

- [ ] **Step 4: Commit**

```bash
git add test/typeclass/success/constrained_instance_combinator.mdr
git commit -m "test: cover constrained instances on a generic wrapper type"
```

---

## Done when

1. `instance C<T> where D<T> { … }` parses, registers, and resolves.
2. Nested wrapper types resolve recursively — `Show<Boxed<Boxed<Int>>>` works.
3. An unsatisfied constraint is a compile error naming the constraint that failed.
4. Every existing suite passes, including the five typeclass failure tests.
5. `test/typeclass/success/constrained_instance_combinator.mdr` prints `6`.

---

## Trace findings — 2026-09-01, after Tasks 1 and 2

Three of this plan's premises were wrong. Recorded here so Task 3 is not written
against them again.

**Task 2 was already implemented.** `TypeChecker.cpp:3380` has passed
`std::vector<MidoriType::ClassConstraint>(instance_stmt.m_constraints)` since
commit `63d76d7` ("Implement associated type"), and `InstanceInfo` has always
stored it (`TypeChecker.h:77`, constructor at `TypeChecker.cpp:2575`). The vector
was empty only because the *parser* never populated
`instance_stmt.m_constraints`. Task 1 made the existing storage live on its own,
so Task 2 required no C++ at all — only its regression test, committed as
`682d078`.

**`TypeChecker.cpp:2669` is not the `deriving` path.** It is the
*imported-instance* path (`imported_instance_associated_type_bindings`), and it
still passes an empty constraint vector. **Constraints on instances imported
across module boundaries are therefore dropped.** Not exercised by the current
tests, which declare instances in the same module. This needs its own task before
the library rewrite, where `Iter` combinators will be imported everywhere.

**`FindMatchingInstance` is not on the dispatch path for a class-method call.**
Its only callers are `Transferable` (`:896`), `ValidateFunctionConstraints`
(`:1165`), associated types (`:1191`), `Iterable` (`:3780`, `:3981`) and
`Concatenable` (`:4370`). A call like `Show::show(x)` instead goes through a
separate inline candidate loop at `TypeChecker.cpp:4946-5004` that iterates
`m_instances` directly and matches on *method parameter types* rather than
instance type arguments. **Adding a constraint check inside `FindMatchingInstance`
— which is what Task 3 above specifies — would not affect the failing test.**

### What actually blocks the test

Instance-head matching already works. `MatchInstanceTypeArg` (`:636-657`) compares
`m_name` then recurses pairwise over `m_member_types`, so `Boxed<T>` against
`Boxed<Int>` matches and binds `T = Int`. Verified empirically: an
`instance Show<Boxed<T>>` whose body makes no constrained call compiles and runs.

The line-20 error is a pure cascade from line 16. The single blocker is the
recursive `Show::show(value.item)` inside the method body, and it has two layers:

1. **The type checker's `m_active_constraints` is never populated for instance
   method bodies.** Writes occur only at `:3096-3102` (from `defun.m_constraints`)
   and `:5537-5542`; the Instance visitor's method-body loop at `:3499` pushes
   nothing. Task 1 pushed onto the *parser's* separately-named member, which is a
   different thing and affects name resolution only.

2. **Even once populated, constraint selection fails on a type mismatch.**
   At `:4855-4880` the constraint's first type argument is substituted through
   `env_substitutions` — keyed by *value-binding* names such as `value`, not
   type-parameter names, so `T` passes through unchanged — and then compared with
   strict equality:

   ```cpp
   if (*resolved_first == *first_arg_type)
   ```

   `resolved_first` is `GenericParam T`; `first_arg_type` is the fresh unification
   variable `T0` created at `:1936` (`Type.cpp:181-183` confirms `TypeVariable`
   prints as `"T" + id`). `GenericParam T != TypeVariable T0`, so no constraint is
   ever selected.

   Confirmed by experiment: pushing the constraints around the `:3499` loop moves
   execution into the abstract branch and changes the error to
   `no matching class constraint for 'Show::show' and argument type 'T0'`.

### Task 3, rewritten scope

1. Push the instance's `m_constraints` onto the **type checker's**
   `m_active_constraints` around the method-body loop at `:3499`, under a guard.
2. Bridge the `GenericParam` / `TypeVariable` gap at `:4866`. **Open design
   choice** — see below.
3. Probably mirror both in `CodeGenerator.cpp:5651` for monomorphisation.

`FindMatchingInstance` does not need to change for this test, though it may still
need the constraint check for the `Iterable` and `Concatenable` paths that do go
through it.

### The open design choice in step 2

- **Freshen the instance's generic params** to the same type variables the method
  signature uses, so `T` and `T0` are literally the same type before comparison.
  Keeps the strict `==`. Narrower, but requires getting the freshening context
  right at the instance-visit boundary.
- **Relax the comparison** at `:4866` to a `MatchInstanceTypeArg`-style structural
  match instead of `==`. Smaller diff and reuses a tested function, but loosens a
  check that other constraint paths also rely on, so it needs a careful look at
  what else reaches that line.

---

## Task 3 (rewritten): make constrained instance methods dispatch

**Decision taken:** bridge the `GenericParam` / `TypeVariable` gap by **freshening**,
not by relaxing the comparison at `:4866`. Loosening `==` would change behaviour
for `Transferable` (`:896`), `ValidateFunctionConstraints` (`:1165`), associated
types (`:1191`), `Iterable` (`:3780`, `:3981`) and `Concatenable` (`:4370`), all
of which reach constraint comparison. A blast radius across five paths is the
wrong trade for a smaller diff in a type checker.

**Files:** `src/Compiler/TypeChecker/TypeChecker.cpp`, possibly
`src/Compiler/CodeGenerator/CodeGenerator.cpp:5651`.

**Target:** `test/typeclass/success/constrained_instance.mdr` prints
`Boxed(Int(7))` and `constrained_instance_nested.mdr` prints
`Boxed(Boxed(Int(3)))`.

### Step 1: Trace before writing code — checkpoint

Three of this plan's earlier premises were wrong because they were reasoned from
structure rather than traced. Do not repeat that. Establish and report, with file
and line for each:

1. Where an instance method's parameter types are freshened during
   `Evaluate(method)` from the loop at `:3499`. Which `FresheningContext` is used,
   and is it reachable from the instance visitor?
2. Whether the instance's `GenericParam T` can be freshened through that *same*
   context, so `Show<T>` becomes `Show<T0>` and matches `first_arg_type` at `:4866`.
3. Whether the sharing is better done by the instance visitor stashing a context
   for the method visitor, or by the method visitor freshening the enclosing
   instance's constraints with the context it already has.

Report the mechanism and your recommended approach **before implementing**. Stop
and wait for confirmation.

### Step 2: Push instance constraints onto the type checker's active set

Mirror the defun path at `:3096-3104` — the same `prev_constraints_size` capture,
the same `ContainsConstraint` dedup — around the method-body loop at `:3499`.
Restore on every exit path, including the `return result` inside the loop.

Note this is the *type checker's* `m_active_constraints`, a different member from
the parser's identically-named one that Task 1 touched.

### Step 3: Freshen so the comparison can succeed

Per the traced mechanism from Step 1.

### Step 4: Verify both tests, then the suites

`constrained_instance.mdr` → `Boxed(Int(7))`;
`constrained_instance_nested.mdr` → `Boxed(Boxed(Int(3)))`.

Then full `test`. Four failures are pre-existing and unrelated:
`concurrency/worker_cancel_blocked_receive`, `concurrency/worker_cancel_spin`,
`static_analyzer/warning_then_codegen_failure`,
`static_analyzer/unused_local_warning`.

If codegen fails after the type checker passes, `CodeGenerator.cpp:5651` needs the
same treatment for monomorphisation.

### Deferred, not in this task

`TypeChecker.cpp:2669` — the imported-instance path — still drops constraints, so
a constrained instance imported across a module boundary loses them. No test
covers it because every current test declares instances in the same module. This
needs its own task before the library rewrite, where `Iter` combinators are
imported everywhere.
