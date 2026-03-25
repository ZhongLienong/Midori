# Testing Guide

Midori has two complementary test layers:

- `tests/` contains in-process implementation tests built with Catch2 and linked against `MidoriCore`.
- `test/` contains file-based language regression tests run through `scripts/run_tests.py` and `Midori.exe`.

Use the smallest layer that proves the behavior you are changing. If a regression is important at both the subsystem and CLI level, add both.

## Choose the Right Test Layer

Add a test under `tests/` when the behavior is best validated in-process:

- lexer, parser, type checker, static analyzer, module graph, or VM behavior
- assertions can target tokens, AST shape, warnings, errors, exit codes, or captured output
- the fixture can be created inline or with `TempDir` / `TempProject`
- a failing test should point at a specific implementation seam instead of a black-box executable run

Add a test under `test/` when the behavior is best validated as a user-visible program run:

- the scenario naturally lives as one or more `.mdr` files on disk
- the regression depends on the executable boundary, startup behavior, or file layout
- the assertion is exit status, `.expected` output, or `.warnings.json`
- the test should exercise the same path that `Midori.exe` users take

Default rule:

- if the subsystem is directly reachable through `MidoriCore` or `tests/support`, prefer `tests/`
- if the value comes from a full-program fixture and black-box execution, prefer `test/`

## Layout and Conventions

Implementation tests:

- place files under `tests/unit/<area>/`
- name files `<Subsystem>Tests.cpp`
- use behavior-focused `TEST_CASE` names
- tag by area first, then by narrower slice when useful, for example `[module][import]` or `[runtime][vm][error]`
- keep each `TEST_CASE` focused on one regression or semantic rule

Regression tests:

- place programs under `test/<category>/`
- use `failure/` directories for compile-fail scenarios
- add `<name>.expected` when stdout/stderr must match a snapshot
- add `<name>.warnings.json` when warnings need structured assertions

## Support Helpers

The helpers in `tests/support/` exist to keep new tests short and deterministic.

`CompileHelpers` in [`tests/support/CompileHelpers.h`](../tests/support/CompileHelpers.h):

- `LexSnippet(source, file_name)` returns `std::expected<LexedSnippet, CompilerError>`
- `ParseSnippet(source, file_name)` returns parsed statements, module declaration metadata, and collected `use` imports
- `TypeCheckSnippet(source, file_name)` returns the typed program tree and warnings
- `AnalyzeSnippet(source, file_name)` returns warnings and static-analyzer errors
- `CompileSnippet(source, file_name)` compiles through the driver layer without launching the CLI
- `ExecuteSnippet(source, file_name)` compiles and runs a snippet in-process and captures stdout/stderr
- `CollectTokenNames(tokens)` turns a token stream into a concise sequence for lexer assertions

`CompileSnippet` and `ExecuteSnippet` already force Midori test mode, so most unit tests do not need to set `MIDORI_TEST_MODE` manually.

Filesystem and environment helpers:

- [`tests/support/TempDir.h`](../tests/support/TempDir.h) creates an isolated temporary directory and removes it on scope exit
- [`tests/support/TempProject.h`](../tests/support/TempProject.h) builds small module trees for import and build-graph tests
- [`tests/support/ScopedEnvVar.h`](../tests/support/ScopedEnvVar.h) sets and restores environment variables such as `MIDORI_PATH`
- [`tests/support/OutputCapture.h`](../tests/support/OutputCapture.h) captures native stdout/stderr when a test cannot use `ExecuteSnippet`

Diagnostic helpers:

- [`tests/support/DiagnosticMatchers.h`](../tests/support/DiagnosticMatchers.h) matches warnings and errors by stage, code, line, and message fragments
- prefer these matchers over exact full-render snapshots when only part of the diagnostic matters

Example patterns:

```cpp
const std::expected<MidoriTest::LexedSnippet, CompilerError> lex_result =
	MidoriTest::LexSnippet("def value = 1;\n", "Value.mdr");

REQUIRE(lex_result.has_value());
REQUIRE(MidoriTest::CollectTokenNames(lex_result->m_tokens) == std::vector<Token::Name>
{
	Token::Name::DEF,
	Token::Name::IDENTIFIER_LITERAL,
	Token::Name::SINGLE_EQUAL,
	Token::Name::INTEGER_LITERAL,
	Token::Name::SINGLE_SEMICOLON
});
```

```cpp
const MidoriTest::TempProject project
({
	MidoriTest::TempProjectFile("Main.mdr", "module Main\nimport { \"./Lib.mdr\" }\ndefun main(): Int => 0;\n"),
	MidoriTest::TempProjectFile("Lib.mdr", "module Lib\ndef value = 1;\n")
});
```

```cpp
const CompilerWarning* warning = MidoriTest::FindWarning(analyze_result->m_warnings, CompilerWarningCode::UnusedLocal);
REQUIRE(warning != nullptr);

std::string mismatch;
REQUIRE(MidoriTest::Matches(
	*warning,
	MidoriTest::WarningExpectation
	{
		.m_stage = CompilerStage::StaticAnalyzer,
		.m_line = 4,
		.m_message_substrings = { "never read" }
	},
	&mismatch));
```

## Command Matrix

One-command entry point from the repo root:

```powershell
python scripts/test_project.py
python scripts/test_project.py --mode unit --build Debug
python scripts/test_project.py --mode unit --unit-tag "[runtime]"
python scripts/test_project.py --mode regression --category closure
```

Configure and build implementation tests on Windows:

```powershell
cmake --preset x64-debug
cmake --build --preset x64-debug --target MidoriUnitTests

cmake --preset x64-development
cmake --build --preset x64-development --target MidoriUnitTests
```

Configure and build implementation tests on Linux:

```bash
cmake --preset linux-debug
cmake --build --preset linux-debug --target MidoriUnitTests
```

Release builds leave `MIDORI_BUILD_TESTS` off by default. Opt in explicitly when needed:

```powershell
cmake --preset x64-release -DMIDORI_BUILD_TESTS=ON
cmake --build --preset x64-release --target MidoriUnitTests
```

Run all registered Catch2 suites through CTest:

```powershell
ctest --test-dir out/build/ninja/x64-debug --output-on-failure
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

```bash
ctest --test-dir out/build/ninja/linux-debug --output-on-failure
```

Filter implementation tests by discovered test name. For area tags such as `[runtime]`, running the Catch2 executable directly is usually simpler:

```powershell
ctest --test-dir out/build/ninja/x64-debug --output-on-failure -R TypeChecker
ctest --test-dir out/build/ninja/x64-debug --output-on-failure -R ImportResolver
```

Run the Catch2 executable directly when you want tag filtering:

```powershell
.\out\build\ninja\x64-debug\out\MidoriUnitTests.exe [runtime]
.\out\build\ninja\x64-debug\out\MidoriUnitTests.exe [module][import]
```

```bash
./out/build/ninja/linux-debug/out/MidoriUnitTests [runtime]
```

Run the file-based regression suite:

```powershell
python scripts/run_tests.py --build Development
python scripts/run_tests.py --build Debug
```

The unified script above reuses `scripts/run_tests.py`; it does not replace it.

Filter regression tests:

```powershell
python scripts/run_tests.py --category closure --build Development
python scripts/run_tests.py --category static_analyzer --build Development
python scripts/run_tests.py --pattern recursive --build Development
python scripts/run_tests.py --test closure/simple.mdr --build Development
```

## Authoring Checklist

- choose `tests/` unless the regression specifically needs black-box executable coverage
- use the narrowest helper that reaches the subsystem you are changing
- prefer structured assertions over full rendered-output snapshots
- keep temp filesystem state inside `TempDir` or `TempProject`
- keep environment changes scoped with `ScopedEnvVar`
- add a `test/` fixture in addition to a unit test when the behavior is critical at the language level
