# Delete Assignment

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `x = e` is not grammar. Four AST nodes and the compound-assignment
tokens go with it.

**Architecture:** this is the last semantic deletion in spec §4 and the only one
that changes what programs can express. The prelude has already been rewritten
without it (`f86cac5`), so nothing in the library depends on it; what remains is
the grammar, the nodes behind it, and 35 test files.

**Tech Stack:** C++23 compiler; `.mdr` corpus migration.

---

## What is actually being deleted

| | count | where |
|---|---|---|
| Expression nodes | 4 | `Assignment`, `CompoundAssign`, `MemberAssignment`, `IndexAssignment` |
| Compound-assignment tokens | 11 | `PLUS_EQUAL`, `MINUS_EQUAL`, `STAR_EQUAL`, `SLASH_EQUAL`, `PERCENT_EQUAL`, `AMPERSAND_EQUAL`, `PIPE_EQUAL`, `CARET_EQUAL`, `LEFT_SHIFT_EQUAL`, `RIGHT_SHIFT_EQUAL`, `PLUS_PLUS_EQUAL` |
| Corpus assignments | 76 | 35 files under `test/` |

`SINGLE_EQUAL` stays — it binds in `def x = e` and in `{ s with f = v }`.

## The consequence to state plainly before starting

Without assignment, **`for x in it { … }` can only perform effects**. Its body can
call `IO::PrintLine` or another effectful function, but it cannot accumulate.
Every accumulating loop in the corpus has to become a comprehension, a fold, or a
recursive helper.

That is not a side effect of the change; it is the change. Spec §3 keeps `for`
deliberately as the consume-an-iterable form at the IO edge, and §2's first
constraint — no construct exists purely for effect — is what `for` is the
exception to. Anyone executing this plan should be comfortable with that reading
before touching the parser, because roughly half the migration is rewriting loops
that count things.

## `loop`, `break` and `continue` go at the same time

Spec §4 lists them as removed, and they are unusable without assignment: a `loop`
whose body cannot mutate can only spin or `break` immediately. Deleting assignment
and leaving them would ship a construct with no correct use.

`for` stays. `break` inside `for` is the one open question — see "Open" below.

## Order of work, and why

Delete the *uses* before the *grammar*, in this order:

1. Corpus migration, while assignment still parses. Each file's behaviour is
   verified against its existing `.expected` before the grammar changes, so a
   migration bug cannot hide behind a parse error.
2. Parser: reject the forms.
3. AST and downstream: remove the nodes.

Reversing 1 and 2 means 35 files break at once with no way to tell a bad rewrite
from a missing feature.

## Known traps

1. **Counting assignments with a regex matches `default =>` arms.** Earlier
   figures of 62, 59 and 52 were inflated by exactly this. Use
   `'^[[:space:]]*[A-Za-z_][A-Za-z0-9_]*([.]\\w+|\\[[^]]*\\])*[[:space:]]*([-+*/%|&^]|<<|>>)?=[^=>]'`
   and sanity-check the total against a manual count of one file.
2. **A `.expected` snapshot captures compiler warnings, including their line
   numbers.** Adding a line to a prelude file has twice broken unrelated tests
   this way. Keep migrated files warning-free rather than re-baselining.
3. **Removing an `ExpressionUnion` variant touches ~11 files.** Most fail to
   compile when missed, but `Analysis/SemanticFacts.cpp` and
   `Analysis/SharedAnalysis.cpp` use `if constexpr` chains that **silently fall
   through to `else`**. Read those two rather than relying on the compiler.
4. **The generated `for` loop stores into locals with `SET_LOCAL`.** That is a VM
   operation and stays. Do not remove it while removing the language construct.

**Build and test — PowerShell, NOT Git Bash:**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **380/380** and **1024 assertions / 176 cases**, both green as of
2026-09-11. Do **not** use `build/out/Midori.exe` — a stale April build.

Checkout is shared. `git status` first; stage by explicit pathspec.

---

## Task 1: Survey, then stop

- [ ] **Step 1: Classify all 76 assignments**

Into: loop counters, accumulators, field mutation, array-element mutation, and
anything that fits none of those. Report the counts. The last bucket is the one
that decides whether this plan is complete or needs a new form.

- [ ] **Step 2: Report the files that will need real thought**

Most will be mechanical. Name the ones that are not, and say why. **Stop and
report before migrating.**

---

## Task 2: Migrate the corpus

- [ ] **Step 1: Loop counters and accumulators to comprehensions or folds**

Verify each file against its existing `.expected` after rewriting, while
assignment still parses.

- [ ] **Step 2: Field and element mutation to value-returning forms**

`{ s with f = v }` for records. For arrays, `ArrayUtil::WithReplaced`.

- [ ] **Step 3: Commit per directory**, so a regression bisects to a small change.

---

## Task 3: Reject the grammar

- [ ] **Step 1: Parser rejects `=` in expression position** with a message naming
  the replacement, in the manner of the five earlier deletions.
- [ ] **Step 2: Delete the compound-assignment tokens** from the lexer.
- [ ] **Step 3: Failure tests** — one per deleted form, each asserting the
  message, snapshots verified to bite.
- [ ] **Step 4: Commit**

---

## Task 4: Delete the nodes

- [ ] **Step 1: Remove the four expression variants** and every visitor arm.
- [ ] **Step 2: Read `SemanticFacts.cpp` and `SharedAnalysis.cpp`** rather than
  trusting the compiler — trap 3.
- [ ] **Step 3: Remove `loop`, `break`, `continue`** and the `Continue` statement
  node.
- [ ] **Step 4: Recount** expression and statement nodes and reserved words, and
  update spec §4's table with the measured figures.
- [ ] **Step 5: Commit**

---

## Open, and not decided by this plan

- **Does `break` survive inside `for`?** It carries a value and is the only way
  out of an iteration early. Spec §4 lists `break` as removed alongside `loop`,
  but that entry was written when `break` existed to serve `loop`. Keeping it for
  `for` is defensible and is a separate call.
- **Do `Appendable`, `Prependable` and `Extendable` survive?** They mutate an
  array in place. Nothing in the prelude calls them since `8cee027`, but they stay
  exported — so this deletion removes the assignment *operator* without removing
  mutation from the language. Spec §4 lists the three `ArrayUtil` forwarders as
  removed but says nothing about the classes themselves.

## Done when

1. `x = e`, `x += e` and friends are parse errors naming their replacement.
2. The four expression nodes and the `Continue` statement node are gone.
3. No `.mdr` file in `test/` or `MidoriPrelude/` contains an assignment.
4. Suite green at 380/380 plus the new failure tests.
5. Spec §4's counts table carries measured figures, not targets.
