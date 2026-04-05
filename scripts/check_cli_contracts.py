#!/usr/bin/env python3
"""
Run CLI-facing contract checks that are not represented as plain .mdr fixtures.

Covers:
- `Midori.exe check <file> --format json`
- `Midori.exe run <file>`
- `Midori.exe build <file>`
- `Midori.exe fmt --check`
- `Midori.exe test`
- `Midori.exe --version`
- `Midori.exe help <command>`
- `project.midori` lookup and source-dir precedence
- `[project]` fallback inside `package.midori`
- `Midori.exe init` project/package scaffolding
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import re
import sys
import tempfile
from pathlib import Path
from typing import Any

from run_tests import TestRunner


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8", newline="\n")


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def run_midori(
    runner: TestRunner,
    args: list[str],
    env_overrides: dict[str, str | None] | None = None,
    cwd: Path | None = None,
) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    if env_overrides is not None:
        for key, value in env_overrides.items():
            if value is None:
                env.pop(key, None)
            else:
                env[key] = value

    return subprocess.run(
        [str(runner.midori_exe), *args],
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        timeout=30,
        cwd=cwd or repo_root(),
        env=env,
        check=False,
    )


def parse_command_json(name: str, completed: subprocess.CompletedProcess[str]) -> dict[str, Any]:
    if completed.stderr.strip() != "":
        raise AssertionError(f"{name}: expected empty stderr, got:\n{completed.stderr}")

    output = completed.stdout.strip()
    if output == "":
        raise AssertionError(f"{name}: expected JSON on stdout, got empty output.")

    try:
        payload = json.loads(output)
    except json.JSONDecodeError as exc:
        raise AssertionError(f"{name}: stdout was not valid JSON.\n{output}") from exc

    if not isinstance(payload, dict):
        raise AssertionError(f"{name}: expected top-level JSON object, got {type(payload).__name__}.")
    return payload


def require_report(payload: dict[str, Any], name: str) -> dict[str, Any]:
    report = payload.get("report")
    if not isinstance(report, dict):
        raise AssertionError(f"{name}: expected nested report object, got {type(report).__name__}.")
    if "warnings" not in report or "errors" not in report:
        raise AssertionError(f"{name}: report payload is missing warnings/errors keys.")
    return report


def assert_condition(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def scenario_check_json_success_uses_project_manifest(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-success-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        external_dir = temp_dir / "external"
        project_dir = temp_dir / "Project"
        source_dir = project_dir / "src"

        external_dir.mkdir(parents=True)
        source_dir.mkdir(parents=True)

        # If MIDORI_PATH wins over the project manifest, Support::Value() becomes a
        # non-callable variable and compilation fails.
        write_text(
            external_dir / "Support.mdr",
            "module Support\n"
            "public export { Value }\n"
            "def Value = 7;\n",
        )
        write_text(
            project_dir / "project.midori",
            "[project]\n"
            "name = \"CliCheck\"\n"
            "source_dir = \"src\"\n",
        )
        write_text(
            source_dir / "Support.mdr",
            "module Support\n"
            "public export { Value }\n"
            "defun Value(): Int => 41;\n",
        )
        main_path = source_dir / "Main.mdr"
        write_text(
            main_path,
            "module Main\n"
            "import { <Support> }\n"
            "defun main(): Int => {\n"
            "    def unused = 1;\n"
            "    Support::Value()\n"
            "};\n",
        )

        completed = run_midori(
            runner,
            ["check", str(main_path), "--format", "json"],
            env_overrides={"MIDORI_PATH": str(external_dir)},
        )
        payload = parse_command_json("check_json_success_uses_project_manifest", completed)
        report = require_report(payload, "check_json_success_uses_project_manifest")

        assert_condition(
            completed.returncode == 0,
            f"check_json_success_uses_project_manifest: expected exit code 0, got {completed.returncode}.",
        )
        assert_condition(payload.get("command") == "check", f"Unexpected command payload: {payload}")
        warnings = report["warnings"]
        errors = report["errors"]
        assert_condition(isinstance(warnings, list), "Expected warnings to be a JSON array.")
        assert_condition(isinstance(errors, list), "Expected errors to be a JSON array.")
        assert_condition(len(errors) == 0, f"Expected no errors, got: {errors}")
        assert_condition(len(warnings) == 1, f"Expected exactly one warning, got: {warnings}")
        warning = warnings[0]
        assert_condition(warning["stage"] == "StaticAnalyzer", f"Unexpected warning stage: {warning}")
        assert_condition(warning["code"] == "UnusedLocal", f"Unexpected warning code: {warning}")
        assert_condition(str(warning["file_path"]).endswith("Main.mdr"), f"Unexpected warning file_path: {warning}")


def scenario_check_json_failure_reports_parser_errors(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-failure-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        source_path = temp_dir / "Broken.mdr"
        write_text(
            source_path,
            "module Broken\n"
            "def value = ;\n",
        )

        completed = run_midori(
            runner,
            ["check", str(source_path), "--format", "json"],
            env_overrides={"MIDORI_PATH": None},
        )
        payload = parse_command_json("check_json_failure_reports_parser_errors", completed)
        report = require_report(payload, "check_json_failure_reports_parser_errors")

        assert_condition(
            completed.returncode != 0,
            "check_json_failure_reports_parser_errors: expected non-zero exit code.",
        )
        warnings = report["warnings"]
        errors = report["errors"]
        assert_condition(warnings == [], f"Expected no warnings, got: {warnings}")
        assert_condition(isinstance(errors, list) and len(errors) == 1, f"Expected one error, got: {errors}")
        error = errors[0]
        assert_condition(error["stage"] == "Parser", f"Unexpected parser error payload: {error}")
        assert_condition(
            "Expected expression" in str(error["message"]),
            f"Expected parser message to mention 'Expected expression', got: {error}",
        )
        assert_condition(str(error["file_path"]).endswith("Broken.mdr"), f"Unexpected file_path: {error}")


def scenario_package_manifest_project_fallback(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-package-fallback-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        external_dir = temp_dir / "external"
        project_dir = temp_dir / "Fallback"
        source_dir = project_dir / "libsrc"

        external_dir.mkdir(parents=True)
        source_dir.mkdir(parents=True)

        write_text(
            external_dir / "Support.mdr",
            "module Support\n"
            "public export { Value }\n"
            "def Value = 0;\n",
        )
        write_text(
            project_dir / "package.midori",
            "[project]\n"
            "source_dir = \"libsrc\"\n",
        )
        write_text(
            source_dir / "Support.mdr",
            "module Support\n"
            "public export { Value }\n"
            "defun Value(): Int => 99;\n",
        )
        main_path = source_dir / "Main.mdr"
        write_text(
            main_path,
            "module Main\n"
            "import { <Support> }\n"
            "defun main(): Int => Support::Value();\n",
        )

        completed = run_midori(
            runner,
            ["check", str(main_path), "--format", "json"],
            env_overrides={"MIDORI_PATH": str(external_dir)},
        )
        payload = parse_command_json("package_manifest_project_fallback", completed)
        report = require_report(payload, "package_manifest_project_fallback")

        assert_condition(
            completed.returncode == 0,
            f"package_manifest_project_fallback: expected exit code 0, got {completed.returncode}.",
        )
        assert_condition(report["warnings"] == [], f"Expected no warnings, got: {report['warnings']}")
        assert_condition(report["errors"] == [], f"Expected no errors, got: {report['errors']}")


def scenario_project_manifest_takes_precedence_over_package_fallback(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-project-precedence-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        project_dir = temp_dir / "Precedence"
        preferred_source_dir = project_dir / "src"
        package_source_dir = project_dir / "pkgsrc"

        preferred_source_dir.mkdir(parents=True)
        package_source_dir.mkdir(parents=True)

        write_text(
            project_dir / "project.midori",
            "[project]\n"
            "source_dir = \"src\"\n",
        )
        write_text(
            project_dir / "package.midori",
            "[project]\n"
            "source_dir = \"pkgsrc\"\n",
        )
        write_text(
            preferred_source_dir / "Support.mdr",
            "module Support\n"
            "public export { Value }\n"
            "defun Value(): Int => 5;\n",
        )
        write_text(
            package_source_dir / "Support.mdr",
            "module Support\n"
            "public export { Value }\n"
            "def Value = 5;\n",
        )
        main_path = preferred_source_dir / "Main.mdr"
        write_text(
            main_path,
            "module Main\n"
            "import { <Support> }\n"
            "defun main(): Int => Support::Value();\n",
        )

        completed = run_midori(
            runner,
            ["check", str(main_path), "--format", "json"],
            env_overrides={"MIDORI_PATH": None},
        )
        payload = parse_command_json("project_manifest_takes_precedence_over_package_fallback", completed)
        report = require_report(payload, "project_manifest_takes_precedence_over_package_fallback")

        assert_condition(
            completed.returncode == 0,
            "project_manifest_takes_precedence_over_package_fallback: expected exit code 0.",
        )
        assert_condition(report["warnings"] == [], f"Expected no warnings, got: {report['warnings']}")
        assert_condition(report["errors"] == [], f"Expected no errors, got: {report['errors']}")


def scenario_init_project_scaffolds_files(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-init-project-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        project_root = temp_dir / "CliProject"

        completed = run_midori(
            runner,
            ["init", str(project_root), "--name", "CliProject"],
            env_overrides={"MIDORI_PATH": None},
        )

        assert_condition(
            completed.returncode == 0,
            f"init_project_scaffolds_files: expected exit code 0, got {completed.returncode}.",
        )
        assert_condition(completed.stderr.strip() == "", f"Expected empty stderr, got:\n{completed.stderr}")
        assert_condition("Initialized Midori project" in completed.stdout, f"Unexpected stdout:\n{completed.stdout}")
        assert_condition((project_root / "project.midori").exists(), "Expected project.midori to be created.")
        assert_condition((project_root / "src" / "Main.mdr").exists(), "Expected src/Main.mdr to be created.")
        assert_condition((project_root / "packages").is_dir(), "Expected packages/ directory to be created.")

        manifest_contents = read_text(project_root / "project.midori")
        assert_condition("name = \"CliProject\"" in manifest_contents, "Expected project name in project.midori.")
        assert_condition("entry = \"src/Main.mdr\"" in manifest_contents, "Expected entry in project.midori.")


def scenario_init_package_scaffolds_files(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-init-package-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        package_root = temp_dir / "CliPackage"

        completed = run_midori(
            runner,
            ["init", "--package", str(package_root), "--name", "123-demo"],
            env_overrides={"MIDORI_PATH": None},
        )

        assert_condition(
            completed.returncode == 0,
            f"init_package_scaffolds_files: expected exit code 0, got {completed.returncode}.",
        )
        assert_condition(completed.stderr.strip() == "", f"Expected empty stderr, got:\n{completed.stderr}")
        assert_condition("Initialized Midori package" in completed.stdout, f"Unexpected stdout:\n{completed.stdout}")
        assert_condition((package_root / "package.midori").exists(), "Expected package.midori to be created.")
        assert_condition((package_root / "Package123_demo.mdr").exists(), "Expected sanitized package module file.")

        manifest_contents = read_text(package_root / "package.midori")
        assert_condition("main = \"Package123_demo.mdr\"" in manifest_contents, "Expected main module entry in package.midori.")
        assert_condition("exports = [\"Package123_demo\"]" in manifest_contents, "Expected sanitized export entry in package.midori.")


def scenario_version_output_format(runner: TestRunner) -> None:
    completed = run_midori(runner, ["--version"], env_overrides={"MIDORI_PATH": None})
    assert_condition(completed.returncode == 0, f"version_output_format: expected exit code 0, got {completed.returncode}.")
    assert_condition(completed.stderr.strip() == "", f"Expected empty stderr, got:\n{completed.stderr}")
    assert_condition(
        re.fullmatch(r"midori \d+\.\d+\.\d+\s*", completed.stdout) is not None,
        f"Unexpected version output:\n{completed.stdout}",
    )


def scenario_help_lists_new_commands(runner: TestRunner) -> None:
    completed = run_midori(runner, [], env_overrides={"MIDORI_PATH": None})
    assert_condition(completed.returncode == 0, f"help_lists_new_commands: expected exit code 0, got {completed.returncode}.")
    assert_condition("fmt" in completed.stdout and "test" in completed.stdout and "build" in completed.stdout, f"Unexpected help output:\n{completed.stdout}")

    per_command = run_midori(runner, ["help", "test"], env_overrides={"MIDORI_PATH": None})
    assert_condition(per_command.returncode == 0, f"help_test: expected exit code 0, got {per_command.returncode}.")
    assert_condition("--pattern" in per_command.stdout and "--test" in per_command.stdout, f"Unexpected test help output:\n{per_command.stdout}")


def scenario_run_command_executes_program(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-run-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        source_path = temp_dir / "Main.mdr"
        write_text(
            source_path,
            "module Main\n"
            "defun main(): Int => 0;\n",
        )

        completed = run_midori(
            runner,
            ["run", str(source_path), "--format", "json"],
            env_overrides={"MIDORI_PATH": None},
        )
        payload = parse_command_json("run_command_executes_program", completed)
        report = require_report(payload, "run_command_executes_program")

        assert_condition(completed.returncode == 0, f"run_command_executes_program: expected exit code 0, got {completed.returncode}.")
        assert_condition(payload.get("command") == "run", f"Unexpected command payload: {payload}")
        assert_condition(payload.get("success") is True, f"Expected run success payload, got {payload}")
        assert_condition(report["errors"] == [], f"Expected no run errors, got: {report['errors']}")


def scenario_build_command_compiles_without_running(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-build-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        source_path = temp_dir / "Main.mdr"
        write_text(
            source_path,
            "module Main\n"
            "defun main(): Int => 13;\n",
        )

        completed = run_midori(
            runner,
            ["build", str(source_path), "--format", "json"],
            env_overrides={"MIDORI_PATH": None},
        )
        payload = parse_command_json("build_command_compiles_without_running", completed)
        report = require_report(payload, "build_command_compiles_without_running")

        assert_condition(completed.returncode == 0, f"build_command_compiles_without_running: expected exit code 0, got {completed.returncode}.")
        assert_condition(payload.get("command") == "build", f"Unexpected command payload: {payload}")
        assert_condition(payload.get("success") is True, f"Expected build success payload, got {payload}")
        assert_condition(report["errors"] == [], f"Expected no build errors, got: {report['errors']}")
        artifact = payload.get("artifact")
        assert_condition(isinstance(artifact, dict), f"Expected artifact object, got: {artifact}")
        assert_condition(artifact.get("procedureCount", 0) >= 1, f"Expected procedure count in artifact, got: {artifact}")
        artifact_path = artifact.get("path")
        assert_condition(isinstance(artifact_path, str) and artifact_path != "", f"Expected artifact path, got: {artifact}")
        artifact_file = Path(artifact_path)
        assert_condition(artifact_file.exists(), f"Expected artifact file to exist: {artifact_file}")
        artifact_payload = json.loads(read_text(artifact_file))
        assert_condition(isinstance(artifact_payload.get("procedures"), list), f"Expected serialized procedures in artifact: {artifact_payload}")
        assert_condition(artifact_payload.get("entryFile", "").endswith("Main.mdr"), f"Unexpected artifact entry file: {artifact_payload}")


def scenario_fmt_check_and_write(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-fmt-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        source_path = temp_dir / "Main.mdr"
        write_text(
            source_path,
            "module Main\n"
            "// comment\n"
            "defun main():Int=>0; // trailing\n",
        )

        check_before = run_midori(runner, ["fmt", str(source_path), "--check", "--format", "json"], env_overrides={"MIDORI_PATH": None})
        check_before_payload = parse_command_json("fmt_check_before_write", check_before)
        assert_condition(check_before.returncode != 0, "fmt_check_before_write: expected non-zero exit code.")
        assert_condition(check_before_payload.get("changedCount") == 1, f"Expected one changed file before write, got: {check_before_payload}")

        write_completed = run_midori(runner, ["fmt", str(source_path), "--write", "--format", "json"], env_overrides={"MIDORI_PATH": None})
        write_payload = parse_command_json("fmt_write", write_completed)
        assert_condition(write_completed.returncode == 0, f"fmt_write: expected exit code 0, got {write_completed.returncode}.")
        assert_condition(write_payload.get("changedCount") == 1, f"Expected one changed file during write, got: {write_payload}")
        formatted_text = read_text(source_path)
        assert_condition("// comment" in formatted_text, f"Expected leading comment to be preserved, got:\n{formatted_text}")
        assert_condition("// trailing" in formatted_text, f"Expected trailing comment to be preserved, got:\n{formatted_text}")

        check_after = run_midori(runner, ["fmt", str(source_path), "--check", "--format", "json"], env_overrides={"MIDORI_PATH": None})
        check_after_payload = parse_command_json("fmt_check_after_write", check_after)
        assert_condition(check_after.returncode == 0, f"fmt_check_after_write: expected exit code 0, got {check_after.returncode}.")
        assert_condition(check_after_payload.get("changedCount") == 0, f"Expected no changed files after write, got: {check_after_payload}")


def scenario_test_command_discovers_project_tests(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-test-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        project_dir = temp_dir / "Project"
        test_dir = project_dir / "test"
        test_dir.mkdir(parents=True)

        write_text(
            project_dir / "project.midori",
            "[project]\n"
            "name = \"CliTest\"\n"
            "source_dir = \"src\"\n"
            "\n"
            "[test]\n"
            "dir = \"test\"\n"
            "timeout_ms = 30000\n",
        )
        write_text(
            test_dir / "smoke.mdr",
            "module Smoke\n"
            "defun main(): Int => 0;\n",
        )

        completed = run_midori(
            runner,
            ["test", "--format", "json"],
            env_overrides={"MIDORI_PATH": None},
            cwd=project_dir,
        )
        payload = parse_command_json("test_command_discovers_project_tests", completed)
        summary = payload.get("summary")

        assert_condition(completed.returncode == 0, f"test_command_discovers_project_tests: expected exit code 0, got {completed.returncode}.")
        assert_condition(payload.get("command") == "test", f"Unexpected command payload: {payload}")
        assert_condition(isinstance(summary, dict), f"Expected summary object, got: {summary}")
        assert_condition(summary.get("total") == 1, f"Expected one discovered test, got: {payload}")
        assert_condition(summary.get("passed") == 1, f"Expected one passing test, got: {payload}")


def scenario_test_command_enforces_timeout(runner: TestRunner) -> None:
    with tempfile.TemporaryDirectory(prefix="midori-cli-test-timeout-") as temp_dir_raw:
        temp_dir = Path(temp_dir_raw)
        project_dir = temp_dir / "Project"
        test_dir = project_dir / "test"
        test_dir.mkdir(parents=True)

        write_text(
            project_dir / "project.midori",
            "[project]\n"
            "name = \"CliTimeout\"\n"
            "source_dir = \"src\"\n"
            "\n"
            "[test]\n"
            "dir = \"test\"\n"
            "timeout_ms = 50\n",
        )
        write_text(
            test_dir / "hang.mdr",
            "module Hang\n"
            "loop {\n"
            "};\n",
        )

        completed = run_midori(
            runner,
            ["test", "--format", "json"],
            env_overrides={"MIDORI_PATH": None},
            cwd=project_dir,
        )
        payload = parse_command_json("test_command_enforces_timeout", completed)
        summary = payload.get("summary")
        results = payload.get("results")

        assert_condition(completed.returncode != 0, "test_command_enforces_timeout: expected non-zero exit code.")
        assert_condition(isinstance(summary, dict), f"Expected summary object, got: {payload}")
        assert_condition(summary.get("total") == 1, f"Expected one discovered test, got: {payload}")
        assert_condition(summary.get("timedOut") == 1, f"Expected one timed out test, got: {payload}")
        assert_condition(isinstance(results, list) and len(results) == 1, f"Expected one result entry, got: {payload}")
        assert_condition(results[0].get("timedOut") is True, f"Expected timedOut=true on the test result, got: {results[0]}")

SCENARIOS: list[tuple[str, Any]] = [
    ("check_json_success_uses_project_manifest", scenario_check_json_success_uses_project_manifest),
    ("check_json_failure_reports_parser_errors", scenario_check_json_failure_reports_parser_errors),
    ("package_manifest_project_fallback", scenario_package_manifest_project_fallback),
    ("project_manifest_takes_precedence_over_package_fallback", scenario_project_manifest_takes_precedence_over_package_fallback),
    ("init_project_scaffolds_files", scenario_init_project_scaffolds_files),
    ("init_package_scaffolds_files", scenario_init_package_scaffolds_files),
    ("version_output_format", scenario_version_output_format),
    ("help_lists_new_commands", scenario_help_lists_new_commands),
    ("run_command_executes_program", scenario_run_command_executes_program),
    ("build_command_compiles_without_running", scenario_build_command_compiles_without_running),
    ("fmt_check_and_write", scenario_fmt_check_and_write),
    ("test_command_discovers_project_tests", scenario_test_command_discovers_project_tests),
    ("test_command_enforces_timeout", scenario_test_command_enforces_timeout),
]


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Run Midori CLI contract checks.")
    parser.add_argument(
        "--build",
        default="Development",
        choices=["Debug", "Development", "Release"],
        help="Build configuration used to locate Midori.exe (default: Development).",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show failure details.",
    )
    args = parser.parse_args(argv)

    runner = TestRunner(build_config=args.build, verbose=args.verbose)
    print(f"Executable: {runner.midori_exe}")
    print(f"Build: {runner.build_config}")
    if runner.executable_notice:
        print(f"Notice: {runner.executable_notice}")

    failures: list[tuple[str, str]] = []
    for name, scenario in SCENARIOS:
        try:
            scenario(runner)
            print(f"[OK] {name}")
        except AssertionError as exc:
            print(f"[FAIL] {name}")
            if args.verbose:
                print(str(exc))
            failures.append((name, str(exc)))

    if failures:
        print(f"\n[FAILED] {len(failures)} CLI contract check(s) failed.")
        if not args.verbose:
            for name, message in failures:
                print(f"- {name}: {message}")
        return 1

    print(f"\n[SUCCESS] {len(SCENARIOS)} CLI contract check(s) passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
