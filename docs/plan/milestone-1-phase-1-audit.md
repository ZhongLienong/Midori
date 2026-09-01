# Milestone 1 Phase 1 Audit

Date: 2026-04-04

## Scope

This audit cross-checks the user-facing language claims in `README.md` and `docs/`
against the current implementation in:

- `src/Compiler/Token/`
- `src/Compiler/Lexer/`
- `src/Compiler/AbstractSyntaxTree/`
- `src/Compiler/Parser/`
- `src/Compiler/TypeChecker/`
- `src/Compiler/CodeGenerator/`
- `src/Compiler/ModuleManager/`
- `src/Compiler/OptimizerManager/`
- `src/Interpreter/VirtualMachine/`
- `src/Utility/Project/`
- `src/Library/`
- `MidoriPrelude/`
- `test/`
- `tests/`

Focused verification also ran on 2026-04-04 with:

```powershell
python scripts/run_tests.py --build Development --category literal
python scripts/run_tests.py --build Development --category generics
python scripts/run_tests.py --build Development --category match
python scripts/run_tests.py --build Development --category pipe
python scripts/run_tests.py --build Development --category typeclass
python scripts/run_tests.py --build Development --category module
python scripts/run_tests.py --build Development --category prelude
python scripts/run_tests.py --build Development --category deriving
python scripts/run_tests.py --build Development --category ffi
python scripts/run_tests.py --build Development --category closure
python scripts/run_tests.py --build Development --category range
python scripts/run_tests.py --build Development --category array_comprehension
python scripts/run_tests.py --build Development --category expression
python scripts/run_tests.py --build Development --category for_loop
python scripts/run_tests.py --build Development --category as_operator
python scripts/run_tests.py --build Development --category hashmap
python scripts/run_tests.py --build Development --category hashset
ctest --test-dir out/build/ninja/x64-development -N
```

All listed regression categories passed. `ctest -N` reported 74 registered unit
tests.

## Headline Findings

- The core language surface documented in `README.md` is mostly real and covered:
  literals, ADTs, generics, typeclasses, deriving, modules, closures, ranges,
  pipes, array comprehensions, pattern matching, and control flow all exist in
  the lexer/parser/typechecker/codegen/runtime and have regression coverage.
- The largest user-facing drift is in documentation detail, not in the README
  feature list.
- `docs/compilation-workflow.md` is materially stale in multiple sections.
- `docs/module-system.md` is mostly correct on semantics but stale on scheduling
  and implementation narrative.
- `docs/package-system.md` and the README FFI section both overstate or misstate
  parts of the current FFI ABI and memory flow.
- `docs/error-reporting.md` does not exist even though the implementation has
  structured diagnostics and machine-readable warning/error coverage in `tests/`.
- The milestone plan itself contains outdated inventory assumptions:
  current code recognizes 43 keyword/type lexemes, 51 symbol tokens, 11
  statement variants, 36 expression variants, and 6 pattern variants.
- `async` / `await` are not implemented. They are absent from the token set,
  AST, parser, typechecker, code generator, runtime, README, and regression
  suite. `test/concurrency/` exists as an empty directory only.

## README Audit

| Area | Status | Evidence | Test Evidence | Notes |
|------|--------|----------|---------------|-------|
| Primitive types | Verified | `Lexer.cpp`, `Type.h`, `TypeChecker.cpp`, `Executable.h`, `VirtualMachine.cpp` | `test/literal/`, `test/ffi/`, `tests/unit/runtime/` | `Int`, `Float`, `Byte`, `Word`, `Bool`, `Text`, `Unit` are all present end-to-end. |
| Composite types | Verified | `AbstractSyntaxTree.h`, `Type.h`, `TypeChecker.cpp` | `test/literal/`, `test/struct/`, `test/union/`, `test/generics/`, `test/type_alias/` | Arrays, tuples, functions, structs, unions, and aliases are implemented. |
| Operators | Partially documented | `Token.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp`, `VirtualMachine.cpp` | `test/expression/`, `test/for_loop/`, `test/typeclass/`, `test/pipe/`, `test/as_operator/`, `tests/unit/lexer/` | Arithmetic/comparison/logical/bitwise/pipe/concat/length operators are real. Docs understate generic operator support and bitwise compound assignments. |
| Pattern matching | Verified with stated limitation | `AbstractSyntaxTree.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp` | `test/match/`, `tests/unit/parser/`, `tests/unit/typechecker/` | Pattern variants are `Binding`, `Wildcard`, `Literal`, `Tuple`, `Array`, `Constructor`. Exhaustiveness is top-level only, which docs already note in `docs/type-system.md`. |
| Generics | Verified | `AbstractSyntaxTree.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp` | `test/generics/`, `test/type_alias/` | Functions, structs, unions, aliases, constructor inference, and type-definition constraints are real. |
| Typeclasses | Verified | `AbstractSyntaxTree.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp`, `CompiledModule.h` | `test/typeclass/`, `test/module/`, `tests/unit/typechecker/` | Classes, instances, associated types, cross-module metadata, and constrained dispatch are implemented. |
| Deriving | Verified with limits | `Parser.cpp` deriving queues, `TypeChecker.cpp`, `CodeGenerator.cpp` | `test/deriving/` | No separate `DerivingExpander` class exists; deriving is synthesized inside the parser. Supported targets and shape limits match the current docs. |
| Module system | Verified | `ModuleManager.cpp`, `ImportResolver.cpp`, `Parser.cpp`, `BytecodeLinker.cpp` | `test/module/`, `tests/unit/module/`, `tests/unit/compiler/` | Imports/exports/visibility/qualified access are implemented and well covered. |
| FFI | Implemented, docs drift present | `AbstractSyntaxTree.h`, `Parser.cpp`, `CodeGenerator.cpp`, `VirtualMachine.cpp`, `PackageManifest.cpp`, `DynamicFFIRegistry.cpp` | `test/ffi/`, `test/prelude/`, `tests/unit/runtime/` | Foreign declarations work. Builtin FFI is well covered. Package loading exists, but package-specific regression coverage is thin and ABI docs need correction. |
| Closures | Verified | `AbstractSyntaxTree.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp`, `VirtualMachine.cpp`, `CaptureEscapeDiagnostic.cpp` | `test/closure/`, `tests/unit/runtime/`, `tests/unit/static_analyzer/` | Lexical capture and mutable cell boxing are implemented. |
| Ranges | Verified | `AbstractSyntaxTree.h`, `Type.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp`, `VirtualMachine.cpp` | `test/range/`, `test/for_loop/`, `test/generics/success/generic_range.mdr`, `tests/unit/runtime/` | Binary and ternary ranges are both real. |
| Pipe operator | Verified | `Lexer.cpp`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp` | `test/pipe/` | `|>` and `|> match with` are implemented. |
| Array comprehension | Verified | `AbstractSyntaxTree.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp` | `test/array_comprehension/`, `tests/unit/parser/` | Range, array, and `Iterable`-based comprehensions are implemented. |
| Control flow | Verified | `AbstractSyntaxTree.h`, `Parser.cpp`, `TypeChecker.cpp`, `CodeGenerator.cpp` | `test/expression/`, `test/for_loop/`, `test/match/` | `if/else`, `for/in`, `loop`, `break`, `continue`, and `return` all exist. |
| async/await | Not implemented | Token set, AST, parser, typechecker, codegen, runtime all lack async surface | `test/concurrency/` contains no files | The current README no longer claims this feature. The stale reference now lives in the milestone plan, not in user-facing docs. |

## docs/ Audit

| Document | Status | Findings |
|----------|--------|----------|
| `docs/type-system.md` | Mostly accurate | The main claims about inference, associated types, deriving limits, constrained type definitions, and exhaustiveness match current code. Gap: it does not connect operator behavior to the implemented `Convertable`, `Concatenable`, `Countable`, and `Orderable` typeclass-backed operator paths. |
| `docs/prelude.md` | Mostly accurate | The covered `IO`, `System`, `DateTime`, `TextUtil`, `ArrayUtil`, `Option`, `Result`, `List`, `Map`, `Set`, and `Math` APIs exist and are exercised by `test/prelude/`, `test/hashmap/`, and `test/hashset/`. Gap: several public helper/typeclass modules are omitted from the document. |
| `docs/compilation-workflow.md` | Needs correction | The pipeline shape is current, but several details are stale: nested block comments are not implemented; the AST node inventory uses old names; `Async` / `Await` are listed but do not exist; the optimizer list is incomplete; and the optimizer now iterates up to `OptimizerManager::s_max_iterations` instead of running a single fixed pass each. |
| `docs/module-system.md` | Mostly accurate, partially stale | Module declaration rules, visibility, duplicate-name handling, and import semantics match the code. The stale parts are the build narrative: compilation no longer waits strictly by tier, and there is no `AsyncThreadPool` implementation in the current compiler. |
| `docs/testing.md` | Accurate | The `tests/` vs `test/` split, helper headers, command matrix, and warning snapshot support all match the repository. `ctest -N` confirms the unit target is registered. |
| `docs/runtime-architecture.md` | Accurate | VM ownership, closure capture model, GC-managed object classes, and global-variable model match the current runtime. |
| `docs/package-system.md` | Needs correction | Manifest parsing and dynamic loading are real, but the FFI ABI and memory-flow text is not fully accurate for the current VM. Raw-value C examples should read from `&args[i]`, and long `Text` / `Array` returns are adopted rather than always copied and immediately freed. Package support also has little automated regression coverage today. |
| `docs/project-standard.md` | Mostly accurate | `project.midori` handling, package fallback, `MIDORI_PATH` layering, and `Midori.exe init` all match `ProjectManifest.cpp` and `Midori.cpp`. Coverage is source-based rather than regression-based. |
| `docs/error-reporting.md` | Missing | The file does not exist. This is a real doc gap because the implementation now has structured compiler reports, warning/error codes, machine-readable warning output, and unit coverage for machine-readable diagnostics. |

## Reverse Audit: Implemented But Underdocumented

The following user-visible features exist in code but are not well represented in
the current docs:

- Generic operator-backed conversions through `Convertable<From, To>` and the
  `as` operator.
- Generic concatenation through `Concatenable<T>` and `++`, beyond just `Text`
  and `Array<T>`.
- Generic counting through `Countable<T>` and `#`, beyond just arrays.
- Operator-based ordering/equality hooks through `Orderable<T>` and
  `Equatable<T>`.
- Bitwise compound assignments: `&=`, `|=`, `^=`, `<<=`, `>>=`.
- Tuple destructuring statements (`TupleDefinition`), which are implemented but
  have no README/docs example and no obvious file-based regression coverage.
- Public prelude/helper modules not called out in `docs/prelude.md`:
  `Appendable`, `Concatenable`, `Convertable`, `Countable`, `Equatable`,
  `Extendable`, `Hashable`, `Iterable`, `Orderable`, `Prependable`,
  `Prelude/Panic`.
- Structured machine-readable diagnostics and stable diagnostic codes, which are
  covered by unit tests but lack a dedicated user doc.

## Inventory Corrections

Current implementation inventory:

- 43 recognized keyword/type lexemes in `Lexer::s_keywords`
- 51 symbol tokens in `Token::Name`
- 11 `MidoriStatement` variants
- 36 `MidoriExpression` variants
- 6 `MidoriPattern` variants

This means the milestone plan's "32 keywords" and "26 operators" references are
outdated starting assumptions rather than current facts.

## Discrepancy Tracking List

1. Create `docs/error-reporting.md` from the existing compiler report and test surface.
2. Rewrite stale sections in `docs/compilation-workflow.md`:
   remove nested-comment claim, remove async/await, update AST names, update optimizer list, document iterative optimization.
3. Update `docs/module-system.md` to describe ready-queue scheduling and remove `AsyncThreadPool` references.
4. Clarify operator docs in `README.md` and `docs/type-system.md`:
   `as`, `++`, `#`, and bitwise compound assignments are more general than currently described.
5. Correct the FFI ABI examples in the README and `docs/package-system.md`.
6. Decide whether tuple destructuring should be documented, tested, or treated as internal/experimental surface.
7. Decide whether the omitted public prelude/typeclass modules should be added to `docs/prelude.md` or intentionally left as internal-adjacent surface.

## Phase 1 Exit Status

Phase 1 is complete as an audit:

- README claims were checked against implementation and current tests.
- Existing docs were reviewed against the current source tree.
- Missing and stale docs were identified.
- Implemented-but-underdocumented surface was collected.
- The async/await question was resolved: not implemented.
