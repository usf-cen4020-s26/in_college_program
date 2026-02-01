#!/usr/bin/env python3
"""
InCollege COBOL Test Runner

This module provides functionality to test COBOL executables against expected outputs.
It supports single test execution and batch testing with multi-part test support.
"""

import argparse
import difflib
import json
import shutil
import subprocess
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import List, Optional, Tuple


class TestStatus(Enum):
    """Enum representing the status of a test execution."""

    PASSED = "PASSED"
    FAILED = "FAILED"
    ERROR = "ERROR"


@dataclass
class TestResult:
    """
    Represents the result of a single test execution.

    Attributes:
        test_name: Name of the test
        status: Status of the test (PASSED/FAILED/ERROR)
        expected_output: Expected output content
        actual_output: Actual output from the COBOL program
        error_message: Error message if status is ERROR
        diff: Unified diff between expected and actual output
    """

    test_name: str
    status: TestStatus
    expected_output: str
    actual_output: str
    error_message: Optional[str] = None
    diff: Optional[str] = None


@dataclass
class TestCase:
    """
    Represents a test case with input and expected output files.

    Attributes:
        name: Name of the test case
        input_file: Path to the input file
        expected_output_file: Path to the expected output file
        part_number: Part number for multi-part tests (None for single-part)
    """

    name: str
    input_file: Path
    expected_output_file: Path
    part_number: Optional[int] = None


class CobolTestRunner:
    """
    Test runner for COBOL executables.

    This class handles execution of COBOL programs with input files and
    comparison of actual output against expected output.
    """

    def __init__(
        self, executable_path: Path, work_dir: Optional[Path] = None, timeout: int = 10
    ):
        """
        Initialize the test runner.

        Args:
            executable_path: Path to the compiled COBOL executable
            work_dir: Working directory where INPUT.TXT, OUTPUT.TXT, and ACCOUNTS.DAT will be stored
            timeout: Maximum execution time in seconds for each test (default: 10)

        Raises:
            FileNotFoundError: If executable doesn't exist
        """
        # Resolve to absolute path to avoid issues when changing working directory
        executable_path = executable_path.resolve()

        if not executable_path.exists():
            raise FileNotFoundError(f"Executable not found: {executable_path}")

        self.executable_path = executable_path
        self.work_dir = work_dir or Path("/tmp/incollege_test_work")
        self.work_dir.mkdir(parents=True, exist_ok=True)
        self.timeout = timeout

        # Define the file paths that COBOL program uses
        self.input_txt = self.work_dir / "INPUT.TXT"
        self.output_txt = self.work_dir / "OUTPUT.TXT"
        self.accounts_dat = self.work_dir / "ACCOUNTS.DAT"
        self.profiles_dat = self.work_dir / "PROFILES.DAT"

    def run_single_test(
        self, input_file: Path, expected_output_file: Path, test_name: str
    ) -> TestResult:
        """
        Run a single test case.

        Args:
            input_file: Path to the input file
            expected_output_file: Path to the expected output file
            test_name: Name of the test

        Returns:
            TestResult object containing the test execution results
        """
        try:
            # Read expected output
            expected_output = expected_output_file.read_text()

            # Run COBOL executable with input
            actual_output = self._execute_cobol(input_file)

            # Compare outputs
            if actual_output.strip() == expected_output.strip():
                return TestResult(
                    test_name=test_name,
                    status=TestStatus.PASSED,
                    expected_output=expected_output,
                    actual_output=actual_output,
                )
            else:
                # Generate diff
                diff = self._generate_diff(expected_output, actual_output, test_name)
                return TestResult(
                    test_name=test_name,
                    status=TestStatus.FAILED,
                    expected_output=expected_output,
                    actual_output=actual_output,
                    diff=diff,
                )

        except subprocess.TimeoutExpired:
            return TestResult(
                test_name=test_name,
                status=TestStatus.ERROR,
                expected_output="",
                actual_output="",
                error_message=f"Test timed out after {self.timeout} seconds. "
                f"Program may be waiting for input or in an infinite loop.",
            )
        except Exception as e:
            return TestResult(
                test_name=test_name,
                status=TestStatus.ERROR,
                expected_output="",
                actual_output="",
                error_message=str(e),
            )

    def _execute_cobol(self, input_file: Path) -> str:
        """
        Execute the COBOL program with the given input file.

        The COBOL program reads from INPUT.TXT and writes to OUTPUT.TXT.
        This method:
        1. Copies the test input file to INPUT.TXT in the work directory
        2. Removes OUTPUT.TXT if it exists from previous runs
        3. Runs the executable in the work directory
        4. Reads and returns the contents of OUTPUT.TXT

        Args:
            input_file: Path to the test input file

        Returns:
            Program output from OUTPUT.TXT as a string

        Raises:
            subprocess.CalledProcessError: If program execution fails
            FileNotFoundError: If OUTPUT.TXT is not created
        """
        # Copy input file to INPUT.TXT in the work directory
        shutil.copy2(input_file, self.input_txt)

        # Remove OUTPUT.TXT if it exists from a previous run
        if self.output_txt.exists():
            self.output_txt.unlink()

        # Run the COBOL executable in the work directory
        result = subprocess.run(
            [str(self.executable_path)],
            capture_output=True,
            text=True,
            cwd=self.work_dir,
            timeout=self.timeout,
        )

        if result.returncode != 0:
            raise subprocess.CalledProcessError(
                result.returncode,
                self.executable_path,
                output=result.stdout,
                stderr=result.stderr,
            )

        # Read the output from OUTPUT.TXT
        if not self.output_txt.exists():
            raise FileNotFoundError(
                f"OUTPUT.TXT was not created by the COBOL program. "
                f"stdout: {result.stdout}, stderr: {result.stderr}"
            )

        return self.output_txt.read_text()

    def _generate_diff(self, expected: str, actual: str, test_name: str) -> str:
        """
        Generate a unified diff between expected and actual output.

        Args:
            expected: Expected output string
            actual: Actual output string
            test_name: Name of the test

        Returns:
            Unified diff as a string
        """
        expected_lines = expected.splitlines(keepends=True)
        actual_lines = actual.splitlines(keepends=True)

        diff = difflib.unified_diff(
            expected_lines,
            actual_lines,
            fromfile=f"{test_name} (expected)",
            tofile=f"{test_name} (actual)",
            lineterm="\n",
        )

        return "".join(diff)

    def clear_persistence(self) -> None:
        """
        Clear persistent storage (ACCOUNTS.DAT) between test groups.

        This should be called before each test group to ensure a clean state.
        For multi-part tests, this is called before the first part only,
        allowing persistence across parts within the same group.
        """
        if self.accounts_dat.exists():
            self.accounts_dat.unlink()

        if self.profiles_dat.exists():
            self.profiles_dat.unlink()

    def cleanup_work_dir(self) -> None:
        """
        Clean up all files in the work directory.

        This removes INPUT.TXT, OUTPUT.TXT, ACCOUNTS.DAT, and PROFILES.DAT.
        Useful for complete cleanup after all tests.
        """
        for file_path in [
            self.input_txt,
            self.output_txt,
            self.accounts_dat,
            self.profiles_dat,
        ]:
            if file_path.exists():
                file_path.unlink()


def discover_tests(test_root: Path) -> List[List[TestCase]]:
    """
    Discover all test cases in the test directory.

    Multi-part tests (files ending with _part_N) are grouped together.

    Args:
        test_root: Root directory for tests (e.g., tests/fixtures)

    Returns:
        List of test case groups. Each group is a list of TestCase objects.
        Single-part tests have groups with one element.
    """
    test_groups: dict[str, List[TestCase]] = {}

    # Recursively find all directories containing 'inputs' and 'expected' subdirectories
    def find_test_dirs(root: Path) -> List[Path]:
        """Find all directories that contain both 'inputs' and 'expected' subdirectories."""
        test_dirs: List[Path] = []
        for item in root.rglob("*"):
            if item.is_dir():
                inputs_dir = item / "inputs"
                expected_dir = item / "expected"
                if inputs_dir.exists() and expected_dir.exists():
                    test_dirs.append(item)

        return test_dirs

    # Discover all test directories
    for category_dir in find_test_dirs(test_root):
        inputs_dir = category_dir / "inputs"
        expected_dir = category_dir / "expected"

        for input_file in sorted(inputs_dir.glob("*.in.txt")):
            test_base_name = input_file.stem.replace(".in", "")

            # Check if this is a multi-part test
            part_number = None
            base_name = test_base_name

            if "_part_" in test_base_name:
                parts = test_base_name.rsplit("_part_", 1)
                if len(parts) == 2 and parts[1].isdigit():
                    base_name = parts[0]
                    part_number = int(parts[1])

            # Construct expected output file path
            expected_file = expected_dir / f"{test_base_name}.out.txt"

            if not expected_file.exists():
                print(
                    f"Warning: Expected output file not found: {expected_file}",
                    file=sys.stderr,
                )
                continue

            test_case = TestCase(
                name=test_base_name,
                input_file=input_file,
                expected_output_file=expected_file,
                part_number=part_number,
            )

            # Group multi-part tests together
            if base_name not in test_groups:
                test_groups[base_name] = []
            test_groups[base_name].append(test_case)

    # Sort multi-part tests by part number
    for group in test_groups.values():
        group.sort(key=lambda tc: tc.part_number if tc.part_number is not None else 0)

    return list(test_groups.values())


def print_test_result(result: TestResult, verbose: bool = False) -> None:
    """
    Print a formatted test result.

    Args:
        result: TestResult object to print
        verbose: If True, print full output; if False, print summary only
    """
    status_symbol = {
        TestStatus.PASSED: "✓",
        TestStatus.FAILED: "✗",
        TestStatus.ERROR: "⚠",
    }

    status_color = {
        TestStatus.PASSED: "\033[92m",  # Green
        TestStatus.FAILED: "\033[91m",  # Red
        TestStatus.ERROR: "\033[93m",  # Yellow
    }

    reset_color = "\033[0m"

    symbol = status_symbol.get(result.status, "?")
    color = status_color.get(result.status, "")

    print(f"{color}{symbol} {result.test_name}: {result.status.value}{reset_color}")

    if result.status == TestStatus.ERROR:
        print(f"  Error: {result.error_message}")
    elif result.status == TestStatus.FAILED:
        print(f"\n  Differences found:")
        if result.diff:
            for line in result.diff.split("\n"):
                if line.startswith("+") and not line.startswith("+++"):
                    print(f"    \033[92m{line}\033[0m")  # Green for additions
                elif line.startswith("-") and not line.startswith("---"):
                    print(f"    \033[91m{line}\033[0m")  # Red for deletions
                elif line.startswith("@@"):
                    print(f"    \033[94m{line}\033[0m")  # Blue for line numbers
                else:
                    print(f"    {line}")

    if verbose and result.status != TestStatus.ERROR:
        print(f"\n  Expected Output:")
        for line in result.expected_output.split("\n")[:10]:
            print(f"    {line}")
        print(f"\n  Actual Output:")
        for line in result.actual_output.split("\n")[:10]:
            print(f"    {line}")


def run_test_suite(
    executable_path: Path, test_root: Path, verbose: bool = False, timeout: int = 10
) -> Tuple[List[TestResult], int, int, int]:
    """
    Run all discovered tests.

    Args:
        executable_path: Path to the COBOL executable
        test_root: Root directory for tests
        verbose: If True, print detailed output
        timeout: Maximum execution time in seconds for each test

    Returns:
        Tuple of (all_results, passed_count, failed_count, error_count)
    """
    runner = CobolTestRunner(executable_path, timeout=timeout)
    test_groups = discover_tests(test_root)

    all_results: List[TestResult] = []
    passed = 0
    failed = 0
    errors = 0

    print(f"\n{'=' * 70}")
    print(f"Running InCollege COBOL Test Suite")
    print(f"{'=' * 70}\n")
    print(f"Executable: {executable_path}")
    print(f"Test Root: {test_root}")
    print(f"Found {len(test_groups)} test groups\n")

    for group in test_groups:
        # Clear persistence before each test group
        runner.clear_persistence()

        if len(group) > 1:
            print(f"\n--- Multi-part test: {group[0].name.rsplit('_part_', 1)[0]} ---")

        for test_case in group:
            result = runner.run_single_test(
                test_case.input_file, test_case.expected_output_file, test_case.name
            )

            all_results.append(result)

            if result.status == TestStatus.PASSED:
                passed += 1
            elif result.status == TestStatus.FAILED:
                failed += 1
            else:
                errors += 1

            print_test_result(result, verbose)

    return all_results, passed, failed, errors


def generate_report(
    results: List[TestResult],
    passed: int,
    failed: int,
    errors: int,
    output_file: Optional[Path] = None,
) -> None:
    """
    Generate a summary report of test results.

    Args:
        results: List of all test results
        passed: Number of passed tests
        failed: Number of failed tests
        errors: Number of tests with errors
        output_file: Optional file to write JSON report to
    """
    total = passed + failed + errors

    print(f"\n{'=' * 70}")
    print(f"Test Summary")
    print(f"{'=' * 70}\n")
    print(f"Total Tests: {total}")
    print(
        f"Passed: \033[92m{passed}\033[0m ({passed / total * 100:.1f}%)"
        if total > 0
        else "Passed: 0"
    )
    print(
        f"Failed: \033[91m{failed}\033[0m ({failed / total * 100:.1f}%)"
        if total > 0
        else "Failed: 0"
    )
    print(
        f"Errors: \033[93m{errors}\033[0m ({errors / total * 100:.1f}%)"
        if total > 0
        else "Errors: 0"
    )
    print(f"\n{'=' * 70}\n")

    if failed > 0:
        print("Failed tests:")
        for result in results:
            if result.status == TestStatus.FAILED:
                print(f"  - {result.test_name}")
        print()

    if errors > 0:
        print("Tests with errors:")
        for result in results:
            if result.status == TestStatus.ERROR:
                print(f"  - {result.test_name}: {result.error_message}")
        print()

    # Generate JSON report if requested
    if output_file:
        report_data: dict[str, object] = {
            "summary": {
                "total": total,
                "passed": passed,
                "failed": failed,
                "errors": errors,
            },
            "results": [
                {
                    "test_name": r.test_name,
                    "status": r.status.value,
                    "error_message": r.error_message,
                    "has_diff": r.diff is not None,
                }
                for r in results
            ],
        }

        output_file.write_text(json.dumps(report_data, indent=2))
        print(f"JSON report written to: {output_file}")


def main() -> int:
    """
    Main entry point for the test runner.

    Returns:
        Exit code (0 for success, 1 for failures)
    """
    parser = argparse.ArgumentParser(
        description="Test runner for InCollege COBOL program",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "executable", type=Path, help="Path to the compiled COBOL executable"
    )

    parser.add_argument(
        "--test-root",
        type=Path,
        default=Path("tests/fixtures"),
        help="Root directory for test fixtures (default: tests/fixtures)",
    )

    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Print verbose output including actual and expected outputs",
    )

    parser.add_argument(
        "--report", type=Path, help="Generate JSON report at specified path"
    )

    parser.add_argument(
        "--timeout",
        type=int,
        default=10,
        help="Maximum execution time in seconds for each test (default: 10)",
    )

    args = parser.parse_args()

    # Validate inputs
    if not args.executable.exists():
        print(f"Error: Executable not found: {args.executable}", file=sys.stderr)
        return 1

    if not args.test_root.exists():
        print(
            f"Error: Test root directory not found: {args.test_root}", file=sys.stderr
        )
        return 1

    try:
        results, passed, failed, errors = run_test_suite(
            args.executable, args.test_root, args.verbose, args.timeout
        )

        generate_report(results, passed, failed, errors, args.report)

        # Return non-zero exit code if any tests failed or had errors
        return 0 if (failed == 0 and errors == 0) else 1

    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
