# Milestone 2 Dev Plan: Ship A Real Developer Workflow

## Objective

A new user can initialize, format, check, run, and test a project without learning the internal CMake/test harness structure. Editor integration can show errors and warnings from a stable machine-readable interface.

Status note: checklist reconciled against the current repository state on 2026-04-05. Open boxes represent remaining cleanup, incomplete coverage, or intentionally deferred stretch work.

## Current State

The CLI today has 5 command variants: bare `<file>`, `check`, `init`, `init --package`, and `--help`. There is no formatter, no test command, no LSP, no syntax highlighting, and no `--version`. Diagnostics support JSON output via `check --format json`, and machine-readable warnings via the `MIDORI_TEST_WARNING_FORMAT=machine` env var, but the two systems are not unified. All test orchestration lives in Python scripts (`run_tests.py`, `test_project.py`, `check_cli_contracts.py`) that are developer-facing, not user-facing.

## Phase 1: Expand the CLI

### 1.1 Add Missing Core Commands

The CLI (currently in `src/Midori.cpp` `main()` with manual argument parsing) needs these additions:

| Command | Purpose | Exists Today |
|---------|---------|--------------|
| `midori run <file>` | Compile and execute (explicit subcommand form) | Only bare `<file>` form |
| `midori check <file>` | Type-check without execution | Yes |
| `midori fmt <file\|dir>` | Format source files | No |
| `midori test [filter]` | Run project tests | No |
| `midori init [path]` | Initialize project | Yes |
| `midori init --package [path]` | Initialize package | Yes |
| `midori build <file>` | Compile without execution (emit bytecode) | No |
| `midori --version` | Print version string | No |

**Action items:**
- [x] Add explicit `run` subcommand (keep bare `<file>` as shorthand)
- [x] Add `fmt` subcommand (stub initially, implemented in Phase 2)
- [x] Add `test` subcommand that invokes project test discovery
- [x] Add `build` subcommand that compiles and reports but does not execute
- [x] Add `--version` flag with version string from CMake or a version header
- [x] Keep backward compatibility: bare `<file>` still works as implicit `run`

### 1.2 Unify CLI Argument Parsing

The current parsing is inline in `main()` with manual string checks. This will not scale.

- [x] Extract CLI parsing into a dedicated module (e.g., `src/Utility/CLI/CLI.h`)
- [x] Define a command dispatch table mapping subcommand strings to handler functions
- [x] Support global flags (`--help`, `--version`, `--format json`) and per-command flags
- [x] Validate flag combinations and provide clear error messages for invalid usage

### 1.3 Improve Help Output

Current help is a single `PrintUsage()` function with hardcoded text.

- [x] Add per-command help: `midori fmt --help` shows formatter-specific options
- [x] Add command listing with short descriptions
- [x] Add examples section in help output for common workflows
- [x] Include project manifest file names (`project.midori`, `package.midori`) in relevant help text

## Phase 2: Build the Formatter

### 2.1 Design Formatter Architecture

No formatter exists today. The `AbstractSyntaxTreePrinter` is debug-only and not suitable for user-facing formatting.

**Design decisions:**
- [x] Decide input: re-parse source to AST then pretty-print, or operate on token stream with whitespace normalization
- [x] Decide style: define the canonical Midori style (indentation, brace placement, line width, spacing rules)
- [x] Decide scope: format entire files, or support range formatting for editor integration

**Recommended approach:** Parse to AST, then pretty-print. This guarantees idempotency and normalizes all style differences. Token-stream formatting is simpler but cannot fix structural formatting (e.g., long lines, argument wrapping).

### 2.2 Define the Canonical Style

Document the style rules before implementing:

- [x] Indentation: spaces (4) or tabs
- [x] Brace placement for `if`, `for`, `loop`, `match`, `defun`, `struct`, `union`, `class`, `instance`
- [x] Line width limit (e.g., 100 or 120 characters)
- [x] Trailing commas in multi-line argument lists and array literals
- [x] Blank line rules between top-level definitions
- [x] Spacing around operators, after commas, inside brackets
- [x] Import ordering and grouping
- [x] Pattern alignment in `match` arms

### 2.3 Implement the Formatter

- [x] Create `src/Utility/Formatter/Formatter.h` and `Formatter.cpp`
- [x] Implement formatter that emits formatted source text (token-stream approach chosen over AST visitor; see 2.1)
- [x] Handle all 11 statement types and all 36 expression types
- [x] Handle comments (requires lexer changes to preserve comment tokens)
- [x] Write to stdout by default, `--write` / `-w` flag to modify in place
- [x] `--check` flag to exit non-zero if file would change (for CI)

### 2.4 Verify Idempotency

- [x] Format every file in `test/` and verify formatting a second time produces identical output
- [x] Add a CI step that runs `midori fmt --check` on the test corpus (see `scripts/check_format.py` idempotency check; `--enforce-clean` available for user-facing roots)
- [x] Add unit tests for edge cases: empty files, deeply nested expressions, long lines, comments

### 2.5 Comment Preservation

The current lexer discards comments. The formatter must preserve them.

- [x] Extend the lexer to optionally emit comment tokens (or attach comments to adjacent AST nodes)
- [x] Decide comment attachment rules: leading comments attach to the next node, trailing comments attach to the current line
- [x] Ensure round-trip: parse then format preserves all comments in the correct position

## Phase 3: Project-Aware Test Command

### 3.1 Design the Test Discovery System

Today, tests are run via Python scripts that scan directories. The `midori test` command should make this a first-class workflow.

- [x] Define test discovery rules: scan `test/` directory in project root for `.mdr` files
- [x] Support success tests (compile + run + match expected output) and failure tests (compile should fail)
- [x] Support `.expected` files for output matching (reuse existing convention)
- [x] Support `.warnings.json` files for structured warning checks (reuse existing convention)
- [x] Support `failure/` subdirectory convention for expected-failure tests

### 3.2 Implement the Test Runner

- [x] Create `src/Utility/TestRunner/TestRunner.h` and `TestRunner.cpp`
- [x] Discover test files from project root based on manifest or convention
- [x] Compile and run each test, comparing output against `.expected` files
- [x] Report pass/fail with colored output and summary statistics
- [x] Support filtering: `midori test closure` runs only tests in `test/closure/`
- [x] Support pattern matching: `midori test --pattern loop` runs tests matching "loop"
- [x] Exit non-zero if any test fails

### 3.3 Integrate with Project Manifest

- [x] Respect `source_dir` from `project.midori` for test file discovery
- [x] Support a `[test]` section in `project.midori` for test configuration (test directory, timeout, etc.)
- [x] Fall back to convention (`test/` in project root) when no manifest config exists

## Phase 4: Stabilize Machine-Readable Diagnostics

### 4.1 Unify Diagnostic Output

Today there are two separate machine-readable paths:
1. `check --format json` returns a JSON object with `warnings` and `errors` arrays
2. `MIDORI_TEST_WARNING_FORMAT=machine` emits `MIDORI_WARNING\t<JSON>` lines

These should converge into a single stable format.

- [x] Define a versioned diagnostic JSON schema (e.g., `{"version": 1, "diagnostics": [...]}`)
- [x] Each diagnostic includes: `severity` (error/warning), `stage`, `code`, `message`, `file`, `line`, `column`, `endColumn`, `suggestion`
- [x] Deprecate the `MIDORI_WARNING\t` format in favor of structured JSON
- [x] Make `--format json` available on all commands, not just `check`

### 4.2 Document the Diagnostic Schema

- [x] Create `docs/diagnostic-format.md` with the full JSON schema
- [x] Include examples for each severity level and diagnostic code
- [x] Document all error codes (`CompilerErrorCode` enum) and warning codes (`CompilerWarningCode` enum)
- [x] Specify stability guarantees: which fields are stable, which may change

### 4.3 Add Diagnostic Metadata

Enrich diagnostics with information editors need:

- [x] Add `endLine` and `endColumn` for multi-line spans (currently only start position + caret length)
- [x] Add `relatedInformation` for diagnostics that reference multiple locations (e.g., "defined here" + "used here")
- [x] Add `source` field (always `"midori"`) for filtering in multi-language editors
- [x] Ensure all diagnostics include file path (some internal errors may lack location; added `CompilerError::WithFile` helper and migrated `BytecodeLinker` and `Compiler` fallback paths)

## Phase 5: Editor Integration

### 5.1 Syntax Highlighting Grammar

No syntax highlighting exists for any editor.

- [x] Create a TextMate grammar (`midori.tmLanguage.json`) covering:
  - All 32 keywords and 7 primitive type names
  - String literals, number literals, boolean literals
  - Comments (once comment syntax is formalized)
  - Operators and punctuation
  - Function definitions and calls
  - Type annotations
  - Module/import syntax
- [x] Package as a minimal VSCode extension (`midori-lang`) with the grammar
- [x] Include file association for `.mdr` files

### 5.2 Diagnostic Integration via JSON

Rather than building a full LSP immediately, provide editor integration through the stable JSON diagnostic format:

- [x] Document a workflow: editor runs `midori check <file> --format json` on save
- [x] Map JSON diagnostics to editor problem markers (VSCode diagnostics API, etc.)
- [x] Provide a sample VSCode extension that:
  - Runs `midori check` on save
  - Parses JSON output
  - Displays errors/warnings inline
  - Shows suggestions as quick-fix actions

### 5.3 LSP Foundation (Stretch Goal)

A full LSP is a large effort. If time permits, lay the foundation:

- [ ] Create a minimal Language Server that wraps `midori check --format json`
- [ ] Support `textDocument/didSave` to trigger diagnostics
- [ ] Support `textDocument/publishDiagnostics` to push results to the editor
- [ ] Defer hover, completion, go-to-definition to a later milestone

## Phase 6: Polish and Documentation

### 6.1 Command Discoverability

- [x] `midori` with no arguments shows a brief overview of available commands
- [x] Each command supports `--help` with usage, flags, and examples
- [x] Add a `midori help <command>` alias for `midori <command> --help`
- [x] Error messages for invalid commands suggest the closest valid command

### 6.2 Workflow Documentation

- [x] Update `docs/testing.md` to document the `midori test` command alongside existing Python scripts
- [x] Create `docs/formatting.md` with style rules and `midori fmt` usage
- [x] Update `docs/project-standard.md` with any new manifest fields
- [x] Add a "Getting Started" section to README that uses only `midori` CLI commands (no CMake or Python)

### 6.3 CLI Contract Tests

Extend `scripts/check_cli_contracts.py` to cover new commands:

- [x] Test `midori run <file>` produces expected output
- [x] Test `midori fmt --check` returns correct exit codes
- [x] Test `midori test` discovers and runs tests
- [x] Test `midori build` compiles without executing
- [x] Test `midori --version` output format
- [x] Test `midori help` and per-command `--help`
- [x] Test `--format json` on all supporting commands

## Execution Order

```
Phase 1 (CLI expansion)
  1.1 Core commands ─────────┐
  1.2 Argument parsing ──────┤── sequential (parsing refactor first, then commands)
  1.3 Help output ───────────┘
         │
Phase 2 (Formatter) ──────────── can start after 1.2 (needs CLI wiring)
  2.1 Architecture
  2.2 Style definition
  2.3 Implementation
  2.4 Idempotency tests
  2.5 Comment preservation
         │
Phase 3 (Test command) ──────── can start in parallel with Phase 2
  3.1 Discovery design
  3.2 Runner implementation
  3.3 Manifest integration
         │
Phase 4 (Diagnostics) ──────── can start in parallel with Phases 2/3
  4.1 Unify output
  4.2 Document schema
  4.3 Diagnostic metadata
         │
Phase 5 (Editor integration) ← depends on Phase 4 (stable diagnostic format)
  5.1 Syntax highlighting ──── can start immediately (no deps)
  5.2 Diagnostic integration
  5.3 LSP foundation (stretch)
         │
Phase 6 (Polish) ← depends on all prior phases
  6.1 Discoverability
  6.2 Workflow docs
  6.3 CLI contract tests
```

## Exit Criteria Checklist

- [x] A new user can `midori init`, `midori fmt`, `midori check`, `midori run`, and `midori test` a project without CMake or Python knowledge
- [x] Editor integration can show errors and warnings from a stable, documented JSON interface
- [x] A first-party formatter exists with an idempotent, documented style
- [x] Formatting stops being a source of style drift in examples and packages
- [x] `--version` reports the current Midori version
- [x] Per-command help is available for every subcommand
- [x] CLI contract tests cover all new commands
- [x] At least one editor (VSCode) has syntax highlighting and diagnostic display

## Estimated Effort

| Phase | Effort | Notes |
|-------|--------|-------|
| 1. CLI expansion | Medium | Refactor parsing, add subcommands, improve help |
| 2. Formatter | High | Largest single deliverable; comment preservation is the hard part |
| 3. Test command | Medium | Reuses existing test conventions; mostly plumbing |
| 4. Diagnostics | Medium | Schema design + unification; enriching spans |
| 5. Editor integration | Medium-High | Grammar is straightforward; VSCode extension is new territory |
| 6. Polish | Low-Medium | Docs, help text, contract tests |

## Key Risks

- **Comment preservation in formatter**: The lexer currently discards comments entirely. Adding comment retention touches the lexer, parser, and AST — this is the deepest cross-cutting change in this milestone. Consider shipping the formatter without comment support initially and adding it as a fast follow.
- **Formatter style debates**: Style is subjective. Define the style early (Phase 2.2), get alignment, and do not revisit during implementation. One opinionated style is better than a configurable one at this stage.
- **LSP scope creep**: A full LSP is a multi-milestone effort. Phase 5.3 is explicitly a stretch goal. Do not block the milestone on it — the `check --format json` integration is sufficient for initial editor support.
- **Test runner vs existing scripts**: The new `midori test` command should not break existing Python-based test workflows. Keep both paths working during the transition; deprecate the Python scripts only after the native runner is proven.
- **Backward compatibility**: Adding explicit `run` subcommand must not break bare `<file>` usage. The CLI refactor must handle both forms.

## Dependencies on Milestone 1

- The formatter should format according to the stable language surface defined by the Milestone 1 feature matrix. If Milestone 1 is not complete, the formatter may need to handle syntax that is later removed.
- Diagnostic codes documented in Phase 4.2 should reference the same terminology established in Milestone 1's error message alignment (Phase 6.3).
- The "Getting Started" workflow docs (Phase 6.2) should reference only stable features as classified by Milestone 1.

Per the roadmap: Milestones 1 and 2 should begin immediately and overlap. The CLI expansion and diagnostics work (Phases 1, 3, 4) can proceed independently of Milestone 1. The formatter and editor integration benefit from Milestone 1 being further along.
