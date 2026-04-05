#!/usr/bin/env python3
"""
Run CLI-facing contract checks that are not represented as plain .mdr fixtures.

Covers:
- `Midori.exe check <file> --format json`
- `project.midori` lookup and source-dir precedence
- `[project]` fallback inside `package.midori`
- `Midori.exe init` project/package scaffolding
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
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


def parse_report_json(name: str, completed: subprocess.CompletedProcess[str]) -> dict[str, Any]:
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
    if "warnings" not in payload or "errors" not in payload:
        raise AssertionError(f"{name}: JSON payload is missing warnings/errors keys.")

    return payload


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
        payload = parse_report_json("check_json_success_uses_project_manifest", completed)

        assert_condition(
            completed.returncode == 0,
            f"check_json_success_uses_project_manifest: expected exit code 0, got {completed.returncode}.",
        )
        warnings = payload["warnings"]
        errors = payload["errors"]
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
        payload = parse_report_json("check_json_failure_reports_parser_errors", completed)

        assert_condition(
            completed.returncode != 0,
            "check_json_failure_reports_parser_errors: expected non-zero exit code.",
        )
        warnings = payload["warnings"]
        errors = payload["errors"]
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
        payload = parse_report_json("package_manifest_project_fallback", completed)

        assert_condition(
            completed.returncode == 0,
            f"package_manifest_project_fallback: expected exit code 0, got {completed.returncode}.",
        )
        assert_condition(payload["warnings"] == [], f"Expected no warnings, got: {payload['warnings']}")
        assert_condition(payload["errors"] == [], f"Expected no errors, got: {payload['errors']}")


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
        payload = parse_report_json("project_manifest_takes_precedence_over_package_fallback", completed)

        assert_condition(
            completed.returncode == 0,
            "project_manifest_takes_precedence_over_package_fallback: expected exit code 0.",
        )
        assert_condition(payload["warnings"] == [], f"Expected no warnings, got: {payload['warnings']}")
        assert_condition(payload["errors"] == [], f"Expected no errors, got: {payload['errors']}")


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


SCENARIOS: list[tuple[str, Any]] = [
    ("check_json_success_uses_project_manifest", scenario_check_json_success_uses_project_manifest),
    ("check_json_failure_reports_parser_errors", scenario_check_json_failure_reports_parser_errors),
    ("package_manifest_project_fallback", scenario_package_manifest_project_fallback),
    ("project_manifest_takes_precedence_over_package_fallback", scenario_project_manifest_takes_precedence_over_package_fallback),
    ("init_project_scaffolds_files", scenario_init_project_scaffolds_files),
    ("init_package_scaffolds_files", scenario_init_package_scaffolds_files),
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
