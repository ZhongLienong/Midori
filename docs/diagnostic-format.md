# Diagnostic Format

Midori's machine-readable diagnostic contract is exposed through `--format json`
on CLI commands such as:

```powershell
Midori.exe check src/Main.mdr --format json
Midori.exe build src/Main.mdr --format json
Midori.exe run src/Main.mdr --format json
```

## Top-Level Envelope

`check`, `build`, and `run` return a command envelope with a nested `report`:

```json
{
  "version": 1,
  "source": "midori",
  "command": "check",
  "success": true,
  "exitCode": 0,
  "stdout": "",
  "stderr": "",
  "report": {
    "version": 1,
    "source": "midori",
    "diagnostics": [],
    "warnings": [],
    "errors": []
  }
}
```

Stable fields:

- `version`: schema version. Current value is `1`.
- `source`: always `"midori"`.
- `command`: CLI command name.
- `success`: `true` when the command succeeded.
- `exitCode`: CLI exit code.
- `report`: compiler/runtime diagnostics for the command.

`run --format json` also captures program `stdout` and `stderr` in the envelope.

## Diagnostic Object

Each entry in `report.diagnostics`, `report.warnings`, and `report.errors` has
this shape:

```json
{
  "source": "midori",
  "severity": "warning",
  "stage": "StaticAnalyzer",
  "code": "UnusedLocal",
  "message": "Binding 'unused' is never read.",
  "file": "test/static_analyzer/success/unused_local_warning.mdr",
  "file_path": "test/static_analyzer/success/unused_local_warning.mdr",
  "line": 4,
  "column": 4,
  "endLine": 4,
  "endColumn": 10,
  "caret_length": 6,
  "suggestion": "Prefix with '_' if intentional.",
  "relatedInformation": []
}
```

Stable fields:

- `source`
- `severity`
- `stage`
- `code`
- `message`
- `file`
- `line`
- `column`
- `endLine`
- `endColumn`
- `suggestion`

Compatibility alias:

- `file_path`: legacy alias for `file`
- `caret_length`: retained for existing tooling; editors should prefer
  `endLine` / `endColumn`

`relatedInformation` is reserved for secondary spans such as “defined here”.
It is currently emitted as an empty array when no related locations exist.

## Stages

Current `stage` values:

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

## Error Codes

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

## Warning Codes

Current `CompilerWarningCode` values:

- `None`
- `NameShadowing`
- `UnusedLocal`
- `UnreachableCode`
- `CaptureEscape`

## Stability

Version `1` is intended for editor and automation integration.

Stability guarantees:

- existing fields listed above are stable within schema version `1`
- new fields may be added in future versions without removing existing fields
- `file_path` and `caret_length` are compatibility fields and may be removed in
  a future schema version after a deprecation window
