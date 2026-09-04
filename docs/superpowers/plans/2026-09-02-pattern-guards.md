# Pattern Guards — `case P if cond => e`

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `case Ok(n) if n > 100 => "large"` — a match arm that applies only when its condition holds, falling through to the next arm when it does not.

**Architecture:** Purely additive. `MidoriExpression::Case` (`AbstractSyntaxTree.h:695`) gains an optional guard expression; nothing is removed. `IF` is already a keyword, so no lexer work.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` test suites run by the Midori CLI.

**Why it is not sugar:** a failed guard falls through to the **next case**, which a nested `if` inside the arm cannot do. Without guards, this needs the `Err` arm duplicated inside a nested `if`:

```
match r with
    case Ok(n) if n > 100 => "large"
    case Ok(n) if n > 0   => "small"
    case Ok(_)            => "zero or below"
    case Err(e)           => e
```

---

## The one design decision, already made

**A guarded case does not count toward exhaustiveness.**

The type checker rejects non-exhaustive matches (`TypeChecker.cpp:4034`). A guard is evaluated at runtime, so the compiler cannot know it will ever succeed. `case Ok(n) if n > 100 => …` therefore does **not** discharge the `Ok` obligation — an unguarded `Ok` arm or a `default` is still required.

This is what Rust, Haskell and Scala all do, and the alternative is unsound: a match made of only guarded arms could fall off the end at runtime with nothing to return.

Do not re-open this. Implement it, and make sure a test pins it.

## Scope

**In:** an optional guard on `case` arms in `match` expressions.

**Out:**
- Guards on `default`. A `default` with a guard would be a fourth thing and is not needed.
- Pattern binding visibility beyond the guard's own arm — a guard sees the bindings its pattern introduced, and nothing more.
- Or-patterns (`case A | B =>`). Separate, and lower value.

## Known traps in this codebase

Established the hard way. Do not rediscover them:

1. **`Analysis/SemanticFacts.cpp` and `Analysis/SharedAnalysis.cpp` fall through silently** rather than failing to compile when a new AST shape is missed. The purity chain matters here: a `Case` with a guard is pure only if the guard is pure as well as the body. Getting it wrong lets dead-code elimination drop an arm.
2. **`src/Utility/Formatter/Formatter.cpp` has three `default:`-terminated switches** with the same hazard. A guard adds tokens to a `case` line.
3. **`SubstituteTypeParams` with an empty map is not the identity** — since `e730762` it rebuilds a `StructType` with `m_generic_params` cleared.
4. Several plans here asserted premises that only survived until someone traced them, most recently an opcode sequence approved without being executed that turned out wrong. **Probe rather than reason.**

**Build and test — PowerShell, NOT Git Bash** (Git Bash mangles the vcvars64 quoting and hangs):

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **326/326**, both suites green. Do not regress either. Do **not** use `build/out/Midori.exe` — a stale April build.

This checkout is shared with other sessions. Run `git status` first; stage by explicit pathspec, never `git add -A`.

---

## Task 1: Trace before editing

- [ ] **Step 1: Map the match pipeline**

Case arms are parsed from `Parser.cpp:4351`. Report where a `Case` is type-checked, where its pattern bindings are introduced into scope, where the arm is lowered, and how the VM dispatches between arms. The last one decides how a failed guard falls through.

- [ ] **Step 2: Establish where exhaustiveness is computed**

`TypeChecker.cpp:4034` emits the error. Find where the set of covered constructors is accumulated — that is the single place a guarded arm must not contribute to.

- [ ] **Step 3: Check the fall-through mechanism**

Report how the generated code moves to the next arm on a pattern mismatch. A guard needs the same path: evaluate the guard after the pattern binds, and on false take the existing mismatch route. If that route is not reusable, say so before building an alternative.

- [ ] **Step 4: Report**

Sizes by layer and anything surprising. If you find a genuine design fork beyond the exhaustiveness decision above, **stop and report it** rather than choosing.

Otherwise continue straight into implementation — this plan is small enough not to need a second confirmation round.

---

## Tasks 2+: Implement

Add the optional guard to `MidoriExpression::Case`, parse it, type-check it as `Bool` with the pattern's bindings in scope, exclude guarded arms from exhaustiveness, and lower it onto the existing mismatch path.

## Done when

1. `case Ok(n) if n > 100 => …` compiles and selects correctly.
2. A failed guard falls through to the next arm, including to a later arm with the **same** constructor.
3. A guard sees its pattern's bindings — `case Ok(n) if n > 100` can refer to `n`.
4. A guard that is not `Bool` is a compile error.
5. **A match whose only `Ok` arm is guarded is rejected as non-exhaustive**, with the existing error.
6. Guards work inside a `match` in a function body and at statement level.
7. `midori fmt` round-trips a guarded case and is idempotent.
8. Tests carry `.expected` snapshots, each verified to bite by corrupting it before restoring.
