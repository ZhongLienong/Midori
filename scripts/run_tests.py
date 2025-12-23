#!/usr/bin/env python3
"""
Midori Test Runner

Runs all tests in the test/ directory and reports results.
Supports:
- Expected output verification (.expected files)
- Failure tests (tests in failure/ directories should fail compilation)
- Success tests (tests in success/ directories should succeed)
- Colored output with detailed reporting
- Test filtering by category, pattern, or specific test file
- Automatic detailed output for single/few tests

Usage:
    python scripts/run_tests.py                          # Run all tests
    python scripts/run_tests.py --category closure       # Run only closure tests
    python scripts/run_tests.py --pattern loop           # Run tests matching 'loop'
    python scripts/run_tests.py --test closure/simple    # Run specific test
    python scripts/run_tests.py --verbose                # Show detailed output
    python scripts/run_tests.py --build Debug            # Use Debug build
"""

import os
import sys
import subprocess
import argparse
from pathlib import Path
from dataclasses import dataclass
from typing import List, Optional
import re

# Color codes
class Color:
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    MAGENTA = '\033[95m'
    CYAN = '\033[96m'
    WHITE = '\033[97m'
    GRAY = '\033[90m'
    BOLD = '\033[1m'
    RESET = '\033[0m'

@dataclass
class TestResult:
    """Result of running a single test."""
    name: str
    path: Path
    passed: bool
    expected_to_fail: bool
    output: str
    exit_code: int = 0
    error: Optional[str] = None
    duration_ms: float = 0.0

class TestRunner:
    def __init__(self, build_config: str = "Development", verbose: bool = False):
        self.root_dir = Path(__file__).parent.parent
        self.test_dir = self.root_dir / "test"
        self.build_config = build_config
        self.verbose = verbose

        # Find the Midori executable
        self.midori_exe = self.find_executable()
        if not self.midori_exe:
            print(f"{Color.RED}Error: Could not find Midori executable{Color.RESET}")
            sys.exit(1)

        # Test results
        self.results: List[TestResult] = []

    def find_executable(self) -> Optional[Path]:
        """Find the Midori executable based on build configuration."""
        possible_paths = [
            self.root_dir / f"out/build/x64-{self.build_config.lower()}/Midori.exe",
        ]

        for path in possible_paths:
            if path.exists():
                return path

        return None

    def is_failure_test(self, test_path: Path) -> bool:
        """Check if test is expected to fail based on directory name."""
        return 'failure' in str(test_path.parent).lower()

    def get_expected_output(self, test_path: Path) -> Optional[str]:
        """Get expected output from .expected file if it exists."""
        expected_file = test_path.with_suffix('.expected')
        if expected_file.exists():
            return expected_file.read_text(encoding='utf-8')
        return None

    def run_test(self, test_path: Path) -> TestResult:
        """Run a single test file."""
        relative_path = test_path.relative_to(self.test_dir)
        test_name = str(relative_path)

        expected_to_fail = self.is_failure_test(test_path)
        expected_output = self.get_expected_output(test_path)

        try:
            import time
            start = time.time()

            result = subprocess.run(
                [str(self.midori_exe), str(test_path)],
                capture_output=True,
                text=True,
                encoding='utf-8',
                errors='replace',
                timeout=30
            )

            duration_ms = (time.time() - start) * 1000

            output = result.stdout + result.stderr

            # Determine if test passed
            if expected_to_fail:
                # Failure tests should have non-zero exit code
                passed = result.returncode != 0
            else:
                # Success tests should have zero exit code
                passed = result.returncode == 0

                # If there's expected output, verify it matches
                if passed and expected_output is not None:
                    # Remove ANSI color codes for comparison
                    clean_output = re.sub(r'\x1b\[[0-9;]*m', '', output)
                    passed = clean_output.strip() == expected_output.strip()

            return TestResult(
                name=test_name,
                path=test_path,
                passed=passed,
                expected_to_fail=expected_to_fail,
                output=output,
                exit_code=result.returncode,
                duration_ms=duration_ms
            )

        except subprocess.TimeoutExpired:
            return TestResult(
                name=test_name,
                path=test_path,
                passed=False,
                expected_to_fail=expected_to_fail,
                output="",
                error="Test timed out (30s)",
                duration_ms=30000
            )
        except Exception as e:
            return TestResult(
                name=test_name,
                path=test_path,
                passed=False,
                expected_to_fail=expected_to_fail,
                output="",
                error=str(e),
                duration_ms=0
            )

    def find_tests(self, category: Optional[str] = None, pattern: Optional[str] = None, test_file: Optional[str] = None) -> List[Path]:
        """Find all test files matching the filter criteria."""
        tests = []

        # If specific test file is provided, try to find it
        if test_file:
            # Try as absolute path first
            test_path = Path(test_file)
            if not test_path.exists():
                # Try relative to test directory
                test_path = self.test_dir / test_file
                if not test_path.exists():
                    # Try with .mdr extension
                    test_path = self.test_dir / f"{test_file}.mdr"
                    if not test_path.exists():
                        # Try finding by name pattern
                        for candidate in self.test_dir.rglob("*.mdr"):
                            if candidate.name == test_file or candidate.name == f"{test_file}.mdr":
                                return [candidate]
                        return []
            return [test_path]

        for test_file in self.test_dir.rglob("*.mdr"):
            # Skip non-test files
            if test_file.name in ['minimal_test.mdr', 'test.mdr', 'simple_test.mdr', 'test_backup.mdr']:
                if test_file.parent == self.test_dir:
                    continue

            # Apply category filter
            if category:
                if category not in str(test_file.relative_to(self.test_dir)):
                    continue

            # Apply pattern filter
            if pattern:
                if pattern.lower() not in test_file.name.lower():
                    continue

            tests.append(test_file)

        return sorted(tests)

    def print_result(self, result: TestResult, show_output: bool = False):
        """Print a single test result."""
        status_icon = f"{Color.GREEN}[OK]{Color.RESET}" if result.passed else f"{Color.RED}[FAIL]{Color.RESET}"
        test_type = f"{Color.YELLOW}[SHOULD-FAIL]{Color.RESET}" if result.expected_to_fail else f"{Color.CYAN}[SUCCESS]{Color.RESET}"

        print(f"{status_icon} {test_type} {result.name} {Color.GRAY}({result.duration_ms:.0f}ms){Color.RESET}")

        if not result.passed and (self.verbose or show_output):
            print(f"  {Color.YELLOW}Exit code: {result.exit_code}{Color.RESET}")
            if result.error:
                print(f"  {Color.RED}Error: {result.error}{Color.RESET}")
            else:
                print(f"  {Color.GRAY}Output:{Color.RESET}")
                # Show all lines if single test, otherwise first 10 lines
                max_lines = None if show_output else 10
                for line in result.output.split('\n')[:max_lines]:
                    if line:  # Skip empty lines
                        print(f"    {Color.GRAY}{line}{Color.RESET}")

    def run_all_tests(self, category: Optional[str] = None, pattern: Optional[str] = None, test_file: Optional[str] = None):
        """Run all tests and print results."""
        tests = self.find_tests(category, pattern, test_file)

        if not tests:
            print(f"{Color.YELLOW}No tests found matching criteria{Color.RESET}")
            return

        # Auto-enable verbose output for single/few tests
        show_full_output = len(tests) <= 3

        print(f"{Color.BOLD}Midori Test Suite{Color.RESET}")
        print(f"{Color.GRAY}{'=' * 60}{Color.RESET}")
        print(f"Executable: {Color.CYAN}{self.midori_exe}{Color.RESET}")
        print(f"Build: {Color.CYAN}{self.build_config}{Color.RESET}")
        print(f"Tests: {Color.CYAN}{len(tests)}{Color.RESET}")
        if show_full_output:
            print(f"Mode: {Color.CYAN}Detailed output enabled{Color.RESET}")
        print(f"{Color.GRAY}{'=' * 60}{Color.RESET}\n")

        # Group tests by category
        categories = {}
        for test in tests:
            category_name = str(test.relative_to(self.test_dir).parts[0])
            if category_name not in categories:
                categories[category_name] = []
            categories[category_name].append(test)

        # Run tests by category
        for cat_name in sorted(categories.keys()):
            print(f"\n{Color.BOLD}{Color.BLUE}[{cat_name}]{Color.RESET}")

            for test_path in categories[cat_name]:
                result = self.run_test(test_path)
                self.results.append(result)
                self.print_result(result, show_output=show_full_output)

        # Print summary
        self.print_summary()

    def print_summary(self):
        """Print test summary statistics."""
        total = len(self.results)
        passed = sum(1 for r in self.results if r.passed)
        failed = total - passed

        success_tests = [r for r in self.results if not r.expected_to_fail]
        failure_tests = [r for r in self.results if r.expected_to_fail]

        success_passed = sum(1 for r in success_tests if r.passed)
        failure_passed = sum(1 for r in failure_tests if r.passed)

        total_time = sum(r.duration_ms for r in self.results)

        print(f"\n{Color.GRAY}{'=' * 60}{Color.RESET}")
        print(f"{Color.BOLD}Test Summary{Color.RESET}\n")

        if failed == 0:
            print(f"{Color.GREEN}{Color.BOLD}[SUCCESS] All tests passed!{Color.RESET}")
        else:
            print(f"{Color.RED}{Color.BOLD}[FAILED] Some tests failed{Color.RESET}")

        print(f"\n{Color.CYAN}Total:{Color.RESET}     {passed}/{total} passed")
        print(f"{Color.CYAN}Success:{Color.RESET}   {success_passed}/{len(success_tests)} passed")
        print(f"{Color.CYAN}Failure:{Color.RESET}   {failure_passed}/{len(failure_tests)} passed (should fail)")
        print(f"{Color.CYAN}Duration:{Color.RESET}  {total_time:.0f}ms")

        if failed > 0:
            print(f"\n{Color.RED}Failed tests:{Color.RESET}")
            for result in self.results:
                if not result.passed:
                    print(f"  {Color.RED}[X]{Color.RESET} {result.name}")

        print(f"{Color.GRAY}{'=' * 60}{Color.RESET}")

        # Exit with appropriate code
        sys.exit(0 if failed == 0 else 1)

def main():
    parser = argparse.ArgumentParser(description='Run Midori test suite')
    parser.add_argument('--build', default='Development',
                        choices=['Debug', 'Development', 'Release'],
                        help='Build configuration to use (default: Development)')
    parser.add_argument('--category', help='Run only tests in specified category (e.g., closure)')
    parser.add_argument('--pattern', help='Run only tests matching pattern')
    parser.add_argument('--test', help='Run specific test file (e.g., closure/simple.mdr or just simple)')
    parser.add_argument('--verbose', '-v', action='store_true', help='Show detailed output')

    args = parser.parse_args()

    runner = TestRunner(build_config=args.build, verbose=args.verbose)
    runner.run_all_tests(category=args.category, pattern=args.pattern, test_file=args.test)

if __name__ == '__main__':
    main()
