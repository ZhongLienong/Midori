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

---

## Trace findings — 2026-09-02

**The `{` ambiguity is bounded in practice, measured not argued.** Distinguishing
`{ s with … }` from a block needs a scan from `{` at depth 0, stopping at the
first of `;` or `}` (block), `WITH` (record update), or EOF. One extra rule is
required: track an unconsumed `MATCH` at depth 0, because `{ match o with case … }`
puts `WITH` at depth 0 immediately inside the brace.

Validated by token-level simulation over all 352 `.mdr` files in the repo:

| variant | `{` misclassified as a record update |
|---|---|
| with the `match` guard | **0** |
| without it | **51** — all in `MidoriPrelude/Prelude/Result.mdr`, `Option.mdr` and similar |

So the guard is a demonstrated requirement, not defensive padding. Scan cost over
the same corpus: 1493 braces probed, mean 14.2 tokens, median 8, p95 43, max 212.
Precedent exists — `ProbeArrayComprehension` (`Parser.cpp:2412-2495`) already does
this shape of scan.

`TryParser` (`Parser.h:192-205`) is **not** a safe fallback: it restores
`ParseState` only, not `m_warnings`, `m_pending_statements`, or the
`s_match_counter` / `s_comp_counter` statics, so backtracking over a failed block
parse has side effects.

**There are two `{` dispatch sites, and the second is load-bearing.**
`Parser.cpp:1729` in `ParsePrimary`, and `Parser.cpp:4291`, the function-body fast
path that deliberately bypasses `ParseCall` to stop `fn() => {}()` eating the
`()`. The plan's headline example —
`defun WithPort(c: Config, p: Int) : Config => { c with port = p }` — goes through
`:4291`, so patching only `ParsePrimary` would leave it parsing as a block.

**No new opcode is needed.** Existing operations compose: evaluate the source,
then `DUP`/`GET_MEMBER i` per preserved field, the value expressions for replaced
fields, `CONSTRUCT_STRUCT n`, then `SWAP`/`POP`. Zero change to the VM, the
bytecode linker, or the disassembler. The value stack is a GC root set, so holding
the source there across allocating field expressions is safe. `m_operand_depth`
must be incremented for the live source, exactly as `Construct` does at
`CodeGenerator.cpp:4388`.

**A record update must not lower to a `Construct` node** — and the reason is a
language-level fact worth keeping:

```
defun Retag<T>(b: Bag<T>, t: Int) : Bag<T> => new Bag(b.items, t);
  -> Construct expression type error: could not infer all type arguments for 'Bag'
```

`Construct` rejects a generic struct built inside a generic function
(`TypeChecker.cpp:6064-6074`, the `HasTypeVariables` guard). A record update typed
from the *source's own type* has no such problem, so
`defun WithTag<T>(b: Bag<T>, t: Int) : Bag<T> => { b with tag = t }` **works where
`new Bag(...)` does not.** Record update is therefore strictly more expressive
than construction inside generic code, not merely more convenient.

It also sidesteps three of the four known traps: typing from the source's own type
needs no `Freshen`, no constructor lookup, no `SubstituteTypeParams`, and no
instance resolution.

**Multi-variant detection is trivial.** `struct` and `union` are separate
`MidoriType` variants, and a `union` is always `UnionType` even with one variant.
The scoped-out error is a two-line mirror of `TypeChecker.cpp:5523-5526`. The
`type`/`alias` merge is needed to *support* multi-variant updates, not to *detect*
them.

**Size:** roughly 350 production lines across ~15 files. The visitor boilerplate is
a real tax — a new `ExpressionUnion` variant touches 11 files. Most fail to compile
when missed, but `Analysis/SemanticFacts.cpp` and `Analysis/SharedAnalysis.cpp` use
`if constexpr` chains that **silently fall through to `else`**. Those two need
reading rather than extending; the purity chain especially, since a record update
is pure exactly when its source and every value expression are, and getting it
wrong would let dead-code elimination drop one.
