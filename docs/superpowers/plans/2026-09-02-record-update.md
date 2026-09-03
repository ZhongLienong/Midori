# Record Update — `{ s with f = v, g = w }`

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `{ s with field = value, other = value }` produces a copy of `s` with those fields replaced.

**Architecture:** Purely additive — a new expression form. Nothing is deleted. `WITH` already exists as a token (`Token.h:98`) with exactly one use in the parser (`match x with`), so the keyword is available without lexer work.

**Tech Stack:** C++23, CMake + Ninja presets, `.mdr` test suites run by the Midori CLI.

**Why it matters:** today there is **no way to produce a modified copy of a struct**. A struct therefore cannot appear in a pipeline at all — no builder style, no `cfg |> WithPort(8080)`. That is a hole in a language whose stated character is expression-oriented and piping-oriented (spec §1), and it is a prerequisite for the prelude rewrite, where value-returning updates replace field mutation.

---

## Semantics — decided, do not re-open

Three rules, settled during design:

1. **Simultaneous, not sequential.** Every right-hand side evaluates against the *original* record, so `{ r with a = r.b, b = r.a }` swaps.
2. **Duplicate fields are an error.** `{ r with a = 1, a = 2 }` does not take the last one; it fails to compile.
3. **No nested paths.** `{ cfg with server.port = 8080 }` is lens territory and out of scope. Nest explicitly: `{ cfg with server = { cfg.server with port = 8080 } }`.

Multi-field update is not only convenience. Where a type has a cross-field invariant — say `start <= end` — single-field updates force a path through an invalid intermediate state. Simultaneous multi-field update lets a value move between valid states atomically.

## Scope

**In:** the expression form, for single-variant record types.

**Out:**
- Multi-variant types. `{ u with … }` on a union requires the variant to be statically known, which needs the `type`/`alias` merge landing first. Reject with a clear error naming the reason.
- Nested paths (rule 3 above).
- Any change to struct construction or field access.

## Known traps in this codebase

Established the hard way in earlier plans. Do not rediscover them:

1. **`SubstituteTypeParams` with an empty map is not the identity** — since `e730762` it rebuilds a `StructType` with `m_generic_params` cleared.
2. **Exact-key instance lookups miss generic instances.** Use `FindMatchingInstance`.
3. **Raw `m_global_variables.find` misses imported instances.** Use `ResolveInstanceNameForTypeArgs`.
4. **`ClosureLifting` silently drops constraints** when constructing a `FunctionDefinition` — relevant if any new expression form ends up lifted.

**Build and test — PowerShell, NOT Git Bash** (Git Bash mangles the vcvars64 quoting and hangs):

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **297/297** and **759 assertions / 145 cases**, both green. Do not regress either. Do **not** use `build/out/Midori.exe` — a stale April build.

This checkout is shared with other sessions. Run `git status` first; stage by explicit pathspec, never `git add -A`.

---

## Task 1: Trace, then stop

Several earlier plans in this project asserted premises that only survived until someone traced them. Measure rather than reason from structure.

- [ ] **Step 1: Establish the parsing situation**

`{` currently begins a block expression. `{ s with … }` therefore needs a decision at the parser: how far ahead must it look to distinguish a record update from a block whose first statement is an expression?

Report what `{` currently dispatches to, and whether distinguishing them needs one token of lookahead, arbitrary lookahead, or a different syntax. **If it needs arbitrary lookahead, stop and report** — that is a syntax decision, not an implementation detail, and an alternative spelling may be better than an unbounded scan.

- [ ] **Step 2: Establish how a struct is constructed and copied today**

Where does `new Point(1, 2)` lower? Is there an existing "copy this struct" operation at the VM level, or does a record update need a new opcode? Report the opcode and its shape.

- [ ] **Step 3: Establish how field order is determined**

A record update must produce fields in the type's declared order regardless of the order written in the update. Report where the declared order lives (`StructType::m_member_names` and `m_member_types`) and how construction maps names to slots.

- [ ] **Step 4: Size it and report**

Line estimate split by layer, plus any design fork. **Stop and report before implementing.**

---

## Task 2 onward

Written after Task 1 reports.

Expected shape, subject to the trace: a new `MidoriExpression::RecordUpdate` node holding the source expression and a vector of (field token, value expression); type checker validates the source is a single-variant record, every named field exists, no duplicates, and each value matches its field's type; codegen evaluates the source once, then emits the field values in declared order.

## Done when

1. `{ cfg with port = 8080 }` produces a copy with one field changed and the original unmodified.
2. Multi-field works and is simultaneous — `{ r with a = r.b, b = r.a }` swaps.
3. A duplicate field is a compile error.
4. An unknown field name is a compile error naming the field and the type.
5. A type mismatch on a field value is a compile error.
6. A multi-variant type gives a clear error explaining it is not yet supported.
7. Builder-style piping works end to end:
   `Config("localhost", 80, false) |> WithPort(8080) |> WithTls(true)`
8. Tests carry `.expected` snapshots, each verified to bite by corrupting it before restoring.
