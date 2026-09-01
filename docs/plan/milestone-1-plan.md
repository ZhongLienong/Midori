# Milestone 1 Dev Plan: Make The Language Surface Trustworthy

## Objective

Ensure that docs, parser, typechecker, and runtime all describe the same language. A new user should be able to answer "is this feature real?" without reading C++ source.

## Phase 1: Feature Audit

See `docs/plan/milestone-1-phase-1-audit.md` for the completed audit and the
Phase 2 discrepancy list.

### 1.1 Audit README.md Claims Against Implementation

Go through every feature claim in README.md and verify each one against the actual token set, AST nodes, typechecker logic, codegen, and runtime behavior.

**Checklist:**

| Area | Claim | Verify Against |
|------|-------|----------------|
| Primitive types | Int, Float, Byte, Word, Bool, Text, Unit | Token.h keywords, TypeChecker evaluation, Runtime opcodes |
| Composite types | Array, Tuple, Function, Struct, Union | AST node types, TypeChecker handlers |
| Operators | arithmetic, comparison, logical, bitwise, pipe, concat, length, compound assignment | Current Token.h symbol/operator surface, Parser precedence, CodeGenerator |
| Pattern matching | union, Bool, tuple, array, literal, wildcard, binding | Pattern variant (6 types), exhaustiveness checker |
| Generics | parametric polymorphism, constraints, where clauses | TypeChecker generic specialization, AST Struct/Union/FunctionDefinition |
| Typeclasses | class, instance, associated types, deriving | Class/Instance AST nodes, TypeChecker class/instance registry |
| Deriving | Equatable, Hashable, Map, Bind, Unwrap | DerivingExpander or equivalent, test/deriving/ |
| Module system | import, export, visibility, qualified access | ModuleManager, ImportResolver, test/module/ |
| FFI | foreign declarations, type marshalling | ForeignDefinition AST, PackageManager, test/ffi/ |
| Closures | lexical scoping, capture | Function AST, CaptureEscapeDiagnostic, test/closure/ |
| Ranges | binary (start..end), ternary (start..step..end) | RangeBinary, RangeTernary AST, test/range/ |
| Pipe operator | `|>` with inferred lambdas | Token PIPE, Parser, test/pipe/ |
| Array comprehension | `[expr for x in range]` | ArrayComprehension AST, test/array_comprehension/ |
| Control flow | if/else, for/in, loop, break, continue, return | AST nodes, test coverage |
| async/await | historical concurrency surface references | Token keywords, AST, test/concurrency/ |

**Action items:**
- [x] For each row: confirm token exists, AST node exists, typechecker handles it, codegen emits it, at least one regression test passes
- [x] Flag any claim that is partially implemented or only works in limited cases
- [x] Flag async/await specifically: determine if it is truly functional or only scaffolded

### 1.2 Audit docs/ Against Implementation

Review each of the 9 documentation files for stale or inaccurate claims.

| Document | Key claims to verify |
|----------|---------------------|
| `docs/type-system.md` | Inference algorithm, constraint solving, associated types, deriving targets, exhaustiveness scope |
| `docs/prelude.md` | All MidoriPrelude modules exist and export the listed functions |
| `docs/compilation-workflow.md` | All 8 pipeline phases match current code structure |
| `docs/module-system.md` | Namespace isolation, dependency resolution, visibility rules, circular import handling |
| `docs/testing.md` | Test commands, directory layout, helper API all current |
| `docs/runtime-architecture.md` | VM model, closure representation, GC behavior |
| `docs/package-system.md` | Package structure, FFI integration steps, manifest fields |
| `docs/project-standard.md` | Project layout, manifest schema |
| `docs/error-reporting.md` | Error/warning codes, formatting, machine-readable output |

**Action items:**
- [x] For each doc: read it line by line and confirm claims against source
- [x] Mark claims that reference features not yet in the AST/token surface
- [x] Mark claims that describe behavior differently from what the code does
- [x] Collect all discrepancies into a single tracking list

### 1.3 Reverse Audit: Implemented But Undocumented

Scan the implementation for features that exist in code but have no user-facing documentation.

- [x] Walk all current expression AST variants (36 at audit time): is each one documented somewhere?
- [x] Walk all 11 statement AST variants: is each one documented?
- [x] Walk all 6 pattern types: is each one documented?
- [x] Walk all current keyword/type lexemes (43 at audit time): is each one explained in at least one doc?
- [x] Walk the current symbol/operator token surface (51 symbol tokens at audit time): is each user-visible operator covered?
- [x] Check StaticAnalyzer diagnostics: are all 4 passes documented for users?
- [x] Check optimizer passes: are these documented for users who care about codegen?

## Phase 2: Fix Documentation Drift

### 2.1 Remove or Correct Stale Claims

Based on Phase 1 findings:

- [ ] Remove any feature claim that is not implemented
- [ ] Downgrade partially-implemented features to "experimental" or "limited" with clear scope
- [ ] Fix any behavioral description that does not match actual behavior
- [ ] Update code examples that no longer compile or produce different output

### 2.2 Clarify async/await Status

Phase 1 found that `async` / `await` are not implemented in the current token, AST,
parser, typechecker, codegen, or runtime surface. The remaining references are in
planning docs only, and `test/concurrency/` is currently empty.

- [ ] Determine exact implementation status: tokenized? parsed? typechecked? codegen? runtime?
- [ ] If partially implemented: mark as experimental with clear boundaries
- [ ] If only scaffolded: remove from user-facing docs, keep only in internal notes
- [ ] Update README to reflect actual status

### 2.3 Document Undocumented Features

For any implemented feature found in Phase 1.3 that lacks documentation:

- [ ] Add it to the feature matrix (Phase 3)
- [ ] If stable and tested: add brief documentation or reference
- [ ] If unstable: mark as experimental in the feature matrix

## Phase 3: Create Feature Matrix

### 3.1 Design the Feature Matrix

Create `docs/feature-matrix.md` as the single source of truth for what Midori supports.

**Structure:**

```
# Midori Feature Matrix

## Stability Levels
- **Stable**: Implemented, tested, documented. Safe to rely on.
- **Experimental**: Implemented but may change. Use with caution.
- **Planned**: Not yet implemented. Do not use.

## Type System
| Feature | Status | Tests | Notes |
|---------|--------|-------|-------|
| Int / Float / Byte / Word / Bool / Text / Unit | Stable | test/literal/ | ... |
| Array<T> | Stable | test/expression/ | ... |
| Tuple types | Stable | test/expression/ | ... |
| ...

## Control Flow
...

## Data Types
...

## Pattern Matching
...

## Generics and Typeclasses
...

## Module System
...

## FFI
...

## Operators
...

## Standard Library
...
```

**Action items:**
- [ ] Create the matrix covering every user-visible feature
- [ ] Assign stability levels based on Phase 1 findings
- [ ] Link each feature to its primary test directory
- [ ] Add notes for known limitations or edge cases

### 3.2 Define Stability Criteria

Document what it means for a feature to be "stable" vs "experimental":

- **Stable**: parser handles it, typechecker validates it, codegen emits it, at least one regression test exists, documented
- **Experimental**: some pipeline stages handle it but coverage or behavior may change without notice
- **Planned**: appears in design docs or roadmap but not in the implementation

## Phase 4: Versioning Policy

### 4.1 Create Versioning Policy Document

Create `docs/versioning-policy.md` covering:

- [ ] What constitutes a breaking change (syntax removal, type system behavior change, runtime semantic change)
- [ ] How breaking changes are communicated (changelog, deprecation warnings)
- [ ] How experimental features graduate to stable
- [ ] How stable features get deprecated
- [ ] Version numbering scheme (semantic versioning or equivalent)

### 4.2 Define the Experimental-to-Stable Graduation Path

A feature moves from experimental to stable when:

1. It has full pipeline coverage (lexer through runtime)
2. It has regression tests covering normal and failure cases
3. It has user-facing documentation
4. It has been available for at least one minor version without breaking changes
5. It has no known semantic bugs

## Phase 5: Documentation Test Suite

### 5.1 Extract Compilable Examples from Docs

- [ ] Scan README.md for all code blocks with Midori syntax
- [ ] Scan each docs/*.md file for all code blocks
- [ ] Create a list of every example that should compile and run
- [ ] Create a list of every example that should fail (error examples)

### 5.2 Build a Doc Example Test Harness

Add a test category or script that compiles all documented examples as part of normal CI:

- [ ] Create `test/doc_examples/` directory
- [ ] For each compilable example in docs: create a `.mdr` test file
- [ ] For each error example in docs: create a failure test
- [ ] Add these to the CMake test target or test runner script
- [ ] Ensure the test runner treats doc example failures as blocking

### 5.3 Automate Doc Freshness Checks

- [ ] Add a CI step or script that extracts code blocks from docs and compiles them
- [ ] Fail the build if any documented example does not compile
- [ ] Consider a marker syntax in docs (e.g. `midori-test` fence) to distinguish runnable examples from pseudocode

## Phase 6: Gap Closure

### 6.1 Add Missing Tests for Stable Features

After the feature matrix is built, identify any feature marked "stable" that lacks regression tests:

- [ ] For each gap: write at least one success test and one failure test
- [ ] Focus on features that appear in multiple docs but have thin test coverage
- [ ] Prioritize: pattern matching edge cases, generic constraint failures, deriving behavior, module visibility rules

### 6.2 Add Missing Parser Coverage for Documented Syntax

If any documented syntax is not handled by the parser:

- [ ] Add parser support, or
- [ ] Remove the syntax from docs and mark it as planned in the feature matrix

### 6.3 Align Error Messages with Documentation

- [ ] Verify that error messages reference the same terminology as docs
- [ ] Check that error codes in `docs/error-reporting.md` match actual emitted codes
- [ ] Update either docs or code to resolve any terminology drift

## Execution Order

```
Phase 1 (Audit)
  1.1 README audit ──────────┐
  1.2 docs/ audit ───────────┤── can run in parallel
  1.3 Reverse audit ─────────┘
         │
Phase 2 (Fix drift) ← depends on Phase 1 findings
  2.1 Remove/correct stale claims
  2.2 Clarify async/await
  2.3 Document undocumented features
         │
Phase 3 (Feature matrix) ← depends on Phase 2
  3.1 Build feature-matrix.md
  3.2 Define stability criteria
         │
Phase 4 (Versioning) ← can overlap with Phase 3
  4.1 versioning-policy.md
  4.2 Graduation path
         │
Phase 5 (Doc tests) ← can overlap with Phase 3/4
  5.1 Extract examples
  5.2 Build test harness
  5.3 Automate freshness
         │
Phase 6 (Gap closure) ← depends on Phase 3 matrix
  6.1 Missing tests
  6.2 Missing parser coverage
  6.3 Error message alignment
```

## Exit Criteria Checklist

- [ ] Every feature in user-facing docs is implemented, tested, or explicitly marked experimental
- [ ] No documented syntax exists without parser coverage
- [ ] No stable feature lacks at least one regression test
- [ ] A feature matrix document exists as single source of truth
- [ ] Stale documentation has been removed or corrected
- [ ] A versioning policy document exists
- [ ] All documented code examples compile as part of normal testing
- [ ] A new user can determine feature status without reading C++ source

## Estimated Effort

| Phase | Effort | Notes |
|-------|--------|-------|
| 1. Audit | Medium | Mostly reading and cross-referencing, but thorough |
| 2. Fix drift | Low-Medium | Depends on how much drift Phase 1 finds |
| 3. Feature matrix | Medium | One-time creation, ongoing maintenance |
| 4. Versioning policy | Low | Policy document, not implementation |
| 5. Doc test suite | Medium | Harness setup + extracting all examples |
| 6. Gap closure | Medium-High | Depends on how many gaps exist |

## Key Risks

- **async/await ambiguity**: If partially implemented, deciding whether to finish or remove it could expand scope significantly. Recommendation: mark experimental and defer completion to a later milestone.
- **Deriving limitations**: README documents 5 deriving targets with noted shape limitations. These need clear documentation of what works and what does not.
- **Prelude completeness**: `docs/prelude.md` lists many functions across 6+ modules. Verifying every listed function exists and behaves as documented is the most labor-intensive single audit task.
- **Scope creep**: The audit may reveal bugs or missing features that are tempting to fix immediately. Resist: log them in the feature matrix and address in later milestones unless they are documentation-only fixes.
