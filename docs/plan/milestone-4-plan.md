# Milestone 4 Dev Plan: Harden Runtime, Debugging, And Release Diagnostics

## Objective

A runtime crash or panic produces actionable output for normal users, not only
for compiler developers. FFI package authors have a stable contract to target.

## Zero Runtime Overhead Constraint

Nothing in this milestone may add instructions, branches, or memory writes to
the steady-state execution path. Improvements must stay in one of these buckets:

1. Error-path only
2. Compile-time only
3. Load-time only
4. Hardware-trapped
5. Separate binary

## Status Summary

Completed on this branch:

- always-on runtime stack traces in all builds
- recursive frame collapsing
- file-backed and embedded source lines in runtime diagnostics
- cross-platform guard-page stack overflow detection
- structured `RuntimeErrorCode`, `RuntimeDiagnosticKind`, `RuntimeError`, and
  `RuntimeStackFrame`
- machine-readable runtime diagnostics for `run --format json`
- panic vs error distinction with exit codes `2` and `1`
- hardware-trapped division-by-zero handling
- load-time FFI symbol validation
- versioned FFI ABI documentation and manifest validation via
  `[ffi].abi_version`
- first-pass integer overflow static analysis warnings
- targeted runtime, formatting, static-analyzer, and package tests for the new
  behavior

Still open:

- column-aware runtime source mapping and caret rendering
- procedure span metadata / "defined at" context
- FFI crash-specific wrapping that identifies the native function/package
- profiling-only GC/runtime metrics
- broader runtime and FFI stress coverage

## Current State

The runtime is a register-based stack VM (`src/Interpreter/VirtualMachine/`)
executing ~202 opcodes. It has a mark-and-sweep GC
(`src/Interpreter/GarbageCollector/`), a block allocator
(`src/Interpreter/Allocator/`), and a two-tier FFI registry (builtins in
`MidoriFFIRegistry`, dynamic packages via `DynamicFFIRegistry`).

What works today:

- runtime failures render through structured `RuntimeError` objects instead of
  ad hoc print paths
- stack traces include procedure names, module names, file paths, and line
  numbers in every build configuration
- runtime diagnostics use embedded source lines when present and fall back to
  reading the original source file on the error path
- stack overflow is trapped on Windows and Unix/macOS with guard pages
- division by zero is trapped through platform exception/signal handling
- `run --format json` emits runtime diagnostics in the same report envelope as
  compiler diagnostics, with `"source": "midori-runtime"` and structured
  `stack` data
- dynamic FFI packages now fail early when a declared symbol is missing or when
  `[ffi].abi_version` does not match the runtime ABI
- the static analyzer warns on obvious literal integer overflow patterns

Remaining gaps:

- no column threading through bytecode/source mapping yet
- no procedure span metadata for closures or "defined at" context
- no package/function-specific panic wrapper around native crashes yet
- no profiling-only build or metrics collection yet
- runtime/FFI stress coverage is still partial

## Phase Status

## Phase 1: Stack Traces In All Builds

- [x] Remove the old stack-trace build gate from runtime diagnostics
- [x] Keep execution visualization gated separately behind
  `MIDORI_ENABLE_EXECUTION_TRACE`
- [x] Include module names in stack frames
- [x] Include source line text when available
- [ ] Add a caret indicator once column info exists
- [x] Collapse recursive frames
- [x] Catch stack overflow on Windows and Unix/macOS
- [x] Add stack-overflow runtime coverage

## Phase 2: Source Mapping Improvements

- [ ] Thread column information through bytecode emission
- [ ] Update runtime diagnostics to display columns/carets
- [x] Preserve source lines in the linked executable for runtime display
- [x] Fall back to reading the original `.mdr` file on the error path when
  embedded lines are unavailable
- [ ] Store procedure span metadata for richer context

## Phase 3: Structured Runtime Error Codes

- [x] Define `RuntimeErrorCode`
- [x] Map existing VM runtime error sites to structured codes
- [x] Catch hardware-trapped division by zero and tag it as
  `DivisionByZero`
- [x] Define `RuntimeError` and `RuntimeStackFrame`
- [x] Replace ad hoc runtime rendering with structured runtime errors
- [x] Serialize runtime diagnostics in machine-readable JSON
- [x] Include `"source": "midori-runtime"` in runtime JSON
- [x] Include structured `stack` frames in runtime JSON
- [x] Add `IntegerOverflowDiagnostic`
- [x] Register the pass in `StaticAnalyzerManager`
- [x] Emit it as `CompilerWarningCode::IntegerOverflow`
- [x] Add static-analyzer coverage for detected and undetected literal patterns

## Phase 4: Stabilize Panic And Fatal Error Presentation

- [x] Distinguish recoverable runtime errors from panics
- [x] Recoverable runtime errors exit with code `1`
- [x] Panics exit with code `2`
- [x] Use consistent `error[...]` / `panic[...]` rendering backed by the
  structured runtime type
- [x] Render stack traces from structured frame data
- [x] Restore/reset platform exception or signal handlers after emitting panic
  diagnostics
- [ ] Close or unwind package-defined native resources on panic

## Phase 5: FFI Boundary Hardening

- [x] Validate declared native symbols at library load time
- [x] Emit clear missing-symbol errors during load
- [ ] Wrap native calls with package/function-specific crash diagnostics
- [x] Create `docs/ffi-abi.md`
- [x] Version the ABI as `FFI ABI v1`
- [x] Document calling convention, ownership rules, and error behavior
- [x] Add `[ffi].abi_version` validation at load time

## Phase 6: GC And Runtime Observability

- [ ] Add a separate Profiling build configuration
- [ ] Add profiling-only GC metrics
- [ ] Add profiling-only runtime performance counters
- [ ] Support JSON output for profiling data

## Phase 7: Runtime Test Hardening

- [x] Index bounds: out-of-bounds read
- [ ] Index bounds: out-of-bounds write
- [ ] Index bounds: negative index
- [ ] Array operations: pop from empty, oversized allocation, comprehension edge
  cases
- [x] Stack overflow: deep recursion
- [ ] Stack overflow: mutual recursion
- [x] Division by zero: clean hardware-trapped runtime error
- [ ] FFI errors: runtime call-site missing function
- [ ] FFI errors: successful dynamic-library load and call
- [ ] GC stress coverage
- [ ] Closure capture coverage across GC cycles
- [ ] Large data stress coverage
- [x] Runtime source-line formatting tests
- [x] Runtime JSON serialization tests
- [x] Panic vs error exit-code coverage
- [ ] Verify rendered messages for every `RuntimeErrorCode`
- [ ] FFI boundary tests for all `FFIReturnKind` variants
- [ ] FFI crash panic tests

## Exit Criteria Checklist

- [x] Stack traces with file, module, procedure, and line info appear in all
  build configurations including Release
- [ ] Runtime errors display the source line and caret when source and column
  data are available
- [x] Runtime errors have structured codes (`RuntimeErrorCode`)
- [x] Runtime errors are available in machine-readable JSON via `--format json`
- [x] Panic and recoverable error are visually distinct with different exit
  codes
- [x] Stack overflow is caught on both Windows and Unix via hardware guard pages
- [x] Division by zero is caught via hardware trap (`SIGFPE` / structured
  exception)
- [x] Integer overflow is detected at compile time by a best-effort static
  analysis pass
- [x] FFI symbols are validated at library load time
- [ ] FFI crashes are caught with package/function-specific panic diagnostics
- [x] A versioned FFI ABI document exists for package authors
- [ ] GC metrics and performance counters are available in a separate Profiling
  build
- [ ] No phase in this milestone adds any instructions, branches, or memory
  writes to the steady-state execution path of the Release binary

## Remaining Execution Order

1. Finish Phase 2 column/procedure metadata.
2. Add Phase 5 native-crash-specific wrapping.
3. Add Phase 6 profiling-only observability.
4. Expand Phase 7 stress and FFI boundary coverage.

## Key Risks

- Column threading is a broad compile-time-only change touching many codegen
  sites.
- Native crash wrapping is platform-specific and easy to get subtly wrong.
- Profiling must stay fully compiled out of the standard Release binary.
- Integer overflow warnings remain intentionally incomplete; false negatives are
  acceptable, false positives are not.
