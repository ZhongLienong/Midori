# Error Reporting

Midori's diagnostic pipeline is structured. Compiler stages produce `CompilerError` and `CompilerWarning` values, aggregate them into `CompilerReport`, and defer rendering until the driver boundary.

## Core Types

Source:

- `src/Common/Error/Error.h`
- `src/Common/Error/Error.cpp`
- `src/Compiler/Result/Result.h`
- `src/Utility/Driver/MidoriDriver.cpp`

Main types:

- `CompilerError`
- `CompilerWarning`
- `CompilerErrorLocation`
- `MidoriResult::CompilerWarnings`
- `MidoriResult::CompilerDiagnostics`
- `MidoriResult::CompilerReport`

## Stages

Diagnostics carry a `CompilerStage`:

- `Lexer`
- `Parser`
- `TypeChecker`
- `StaticAnalyzer`
- `CodeGenerator`
- `Module`
- `Optimizer`
- `BytecodeLinker`
- `Compiler`
- `Runtime`
- `Unknown`

These stage names are also used in machine-readable output.

## Codes

### Error Codes

Current `CompilerErrorCode` values:

- `None`
- `NoMatch`
- `ModuleImportResolutionFailed`
- `ModuleImportFileOpenFailed`
- `ModuleCircularDependency`
- `ModuleDeclarationMissing`
- `ModuleDeclarationDuplicate`
- `ModuleMissingExportedSymbol`
- `TypeUndefinedName`
- `TypeUnsatisfiedConstraint`
- `TypeNotCallable`
- `TypeIncorrectArity`
- `TypeMismatch`
- `TypeNonExhaustiveMatch`
- `CodeGeneratorLimitExceeded`
- `CodeGeneratorUnresolvedMethodResolution`
- `CodeGeneratorAmbiguousMethodResolution`
- `CodeGeneratorUnsupportedLowering`
- `BytecodeLinkerNoModulesToLink`
- `BytecodeLinkerDuplicateExportedSymbol`
- `BytecodeLinkerUnresolvedImport`
- `CompilerNoModulesReadyToCompile`
- `CompilerIncompleteCompilationSchedule`
- `CompilerMissingCompiledModule`

### Warning Codes

Current `CompilerWarningCode` values:

- `None`
- `NameShadowing`
- `UnusedLocal`
- `UnreachableCode`
- `CaptureEscape`

## Location Model

`CompilerErrorLocation` can carry:

- `file_name`
- `line`
- `column`
- `caret_length`
- `source_line`

Not every diagnostic includes all fields:

- `WithToken(...)` derives line, column, caret length, and source line from a `Token`
- `WithContext(...)` accepts them explicitly
- `Simple(...)` creates an unlocated diagnostic that renders as a plain message

## Human-Readable Rendering

Located diagnostics render with:

- a stage/severity header
- file and line
- source line context
- caret highlighting
- an optional suggestion line

Example shape:

```text
Parser Error at Format.mdr:2
  |
2 | def value = ;
  |     ^^^ Expected expression
  |
  | Try adding a literal
```

Warnings render the same way, but with a warning header instead of an error header.

If a diagnostic has no location, `Rendered()` is just the message text.

## Report Aggregation

`CompilerReport` combines warnings and errors in one object:

- warnings are stored in `CompilerWarnings`
- errors are stored in `CompilerDiagnostics`
- `RenderedWarnings()` renders grouped warning summaries followed by the warning bodies
- `RenderedErrors()` renders the error bodies
- `Rendered()` concatenates warnings first, then errors

Important behavior from the current pipeline:

- successful compilations can still return warnings
- warnings from earlier stages survive later failures
- warnings are not printed immediately when produced
- the driver prints warnings once, in build-schedule order

That ordering is tested in:

- `tests/unit/common/ErrorFormattingTests.cpp`
- `tests/unit/compiler/CompilerWarningAggregationTests.cpp`

## Driver Behavior

`MidoriDriver` uses the report like this:

- on successful compile-and-run, warnings are emitted before execution starts
- on compilation failure, warnings are rendered before the final errors
- the `"Compilation failed :( "` banner is only used for compilation failures
- runtime diagnostics are rendered without the compilation banner

## Machine-Readable Output

There are two machine-readable surfaces.

### Line-Oriented Warning Output

`SerializeMachineReadableWarning(...)` emits:

```text
MIDORI_WARNING\t{"stage":"StaticAnalyzer", ...}
```

This path exists mainly for legacy test harnesses that still want warnings as a
line stream.

### JSON Payloads

The stable schema is documented in [Diagnostic Format](diagnostic-format.md).
At a high level, `--format json` now returns a command envelope with a nested
`report` object containing:

- `diagnostics`
- `warnings`
- `errors`

Each diagnostic carries fields such as:

- `stage`
- `code`
- `file_path`
- `line`
- `column`
- `caret_length`
- `message`
- `suggestion`

Whole-report JSON:

```json
{
  "version": 1,
  "source": "midori",
  "command": "check",
  "success": true,
  "exitCode": 0,
  "report": {
    "version": 1,
    "source": "midori",
    "diagnostics": [...],
    "warnings": [...],
    "errors": [...]
  }
}
```

APIs:

- `SerializeMachineReadableError(...)`
- `SerializeMachineReadableWarningPayload(...)`
- `MidoriResult::CompilerWarnings::MachineReadable()`
- `MidoriResult::CompilerWarnings::MachineReadableJson()`
- `MidoriResult::CompilerDiagnostics::MachineReadableJson()`
- `MidoriResult::CompilerReport::MachineReadableJson()`

User-facing entry point:

- `Midori.exe check <source_file_path> --format json` prints the command JSON envelope to stdout
- `Midori.exe build <source_file_path> --format json` and `Midori.exe run <source_file_path> --format json` use the same nested `report` shape

## Test Harness Toggle

If `MIDORI_TEST_WARNING_FORMAT=machine` is present in the environment, the
driver prints machine-readable warning lines alongside the normal human-readable
warning output.

This is deprecated in favor of `--format json`, but remains available for the
legacy Python test runner.

## Practical Guidance

When adding new diagnostics:

- prefer a specific `CompilerStage`
- assign a stable error or warning code when the diagnostic should be regression-tested
- use `WithToken(...)` whenever a token span exists
- use `WithContext(...)` when the location is known but not anchored to one token
- keep rendering and machine-readable serialization derived from the structured diagnostic, not from ad hoc print statements
