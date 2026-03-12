"""CLI entry point and test-suite orchestration.

Provides :func:`main` which parses command-line arguments and delegates to
either :func:`run_single_fixture` or :func:`run_test_suite`.
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path
from typing import Optional

from incollege_tests.discovery import (build_dump_output_path,
                                       derive_expected_output_path,
                                       discover_tests)
from incollege_tests.models import TestResult, TestStatus
from incollege_tests.preprocessing import preprocess_input_file
from incollege_tests.reporting import generate_report, print_test_result
from incollege_tests.runner import CobolTestRunner


def run_single_fixture(
    executable_path: Path,
    input_file: Path,
    timeout: int,
    verbose: bool,
    dump_output_dir: Optional[Path],
    dump_only: bool,
    expected_output_file: Optional[Path],
) -> tuple[list[TestResult], int, int, int]:
    """Run exactly one fixture input file.

    Args:
        executable_path: Path to the COBOL executable.
        input_file: Path to the ``.in.txt`` fixture.
        timeout: Execution timeout in seconds.
        verbose: Whether to print verbose output.
        dump_output_dir: Optional directory for actual output dumps.
        dump_only: Skip expected-output comparison when ``True``.
        expected_output_file: Explicit expected file (auto-derived if ``None``).

    Returns:
        ``(results, passed, failed, errors)`` tuple.
    """
    runner = CobolTestRunner(executable_path, timeout=timeout)
    runner.clear_persistence()

    input_text, seed_users = preprocess_input_file(input_file)
    if seed_users:
        runner.seed_users(seed_users)

    test_name = input_file.stem.replace(".in", "")

    print(f"\n{'=' * 70}")
    print("Running Single Fixture")
    print(f"{'=' * 70}\n")
    print(f"Executable: {executable_path}")
    print(f"Input File: {input_file}")

    if dump_output_dir:
        dump_output_dir.mkdir(parents=True, exist_ok=True)
        print(f"Dump Output Dir: {dump_output_dir}")
    if dump_only:
        print("Mode: dump-only (output comparison disabled)")
    print()

    if dump_only:
        try:
            actual_output = runner.execute_with_input_text(input_text)
            result = TestResult(
                test_name=test_name,
                status=TestStatus.PASSED,
                expected_output="",
                actual_output=actual_output,
            )
        except subprocess.TimeoutExpired:
            result = TestResult(
                test_name=test_name,
                status=TestStatus.ERROR,
                expected_output="",
                actual_output="",
                error_message=f"Test timed out after {timeout} seconds. "
                "Program may be waiting for input or in an infinite loop.",
            )
        except Exception as e:
            result = TestResult(
                test_name=test_name,
                status=TestStatus.ERROR,
                expected_output="",
                actual_output="",
                error_message=str(e),
            )
    else:
        if expected_output_file is None:
            expected_output_file = derive_expected_output_path(input_file)

        if not expected_output_file.exists():
            raise FileNotFoundError(
                "Expected output file not found for single fixture run: "
                f"{expected_output_file}. Use --dump-only to run without expected output."
            )

        result = runner.run_single_test_with_input_text(
            input_text, expected_output_file, test_name
        )

    if dump_output_dir and result.status != TestStatus.ERROR:
        dump_file = dump_output_dir / f"{test_name}.actual.out.txt"
        dump_file.write_text(result.actual_output)

    print_test_result(result, verbose)

    passed = 1 if result.status == TestStatus.PASSED else 0
    failed = 1 if result.status == TestStatus.FAILED else 0
    errors = 1 if result.status == TestStatus.ERROR else 0

    return [result], passed, failed, errors


def run_test_suite(
    executable_path: Path,
    test_root: Path,
    verbose: bool = False,
    timeout: int = 10,
    dump_output_dir: Optional[Path] = None,
    dump_only: bool = False,
) -> tuple[list[TestResult], int, int, int]:
    """Run all discovered tests under *test_root*.

    Args:
        executable_path: Path to the COBOL executable.
        test_root: Root directory for test fixtures.
        verbose: Whether to print detailed output.
        timeout: Execution timeout in seconds per test.
        dump_output_dir: Optional directory for actual output dumps.
        dump_only: Skip expected-output comparison when ``True``.

    Returns:
        ``(results, passed, failed, errors)`` tuple.
    """
    runner = CobolTestRunner(executable_path, timeout=timeout)
    test_groups = discover_tests(test_root)

    all_results: list[TestResult] = []
    passed = 0
    failed = 0
    errors = 0

    print(f"\n{'=' * 70}")
    print("Running InCollege COBOL Test Suite")
    print(f"{'=' * 70}\n")
    print(f"Executable: {executable_path}")
    print(f"Test Root: {test_root}")
    print(f"Found {len(test_groups)} test groups\n")

    if dump_output_dir:
        dump_output_dir.mkdir(parents=True, exist_ok=True)
        print(f"Dump Output Dir: {dump_output_dir}")
    if dump_only:
        print("Mode: dump-only (output comparison disabled)")
    if dump_output_dir or dump_only:
        print()

    for group in test_groups:
        runner.clear_persistence()

        if len(group) > 1:
            print(f"\n--- Multi-part test: {group[0].name.rsplit('_part_', 1)[0]} ---")

        for test_case in group:
            input_text, seed_users = preprocess_input_file(test_case.input_file)
            if seed_users:
                runner.seed_users(seed_users)

            if dump_only:
                try:
                    actual_output = runner.execute_with_input_text(input_text)
                    result = TestResult(
                        test_name=test_case.name,
                        status=TestStatus.PASSED,
                        expected_output="",
                        actual_output=actual_output,
                    )
                except subprocess.TimeoutExpired:
                    result = TestResult(
                        test_name=test_case.name,
                        status=TestStatus.ERROR,
                        expected_output="",
                        actual_output="",
                        error_message=f"Test timed out after {timeout} seconds. "
                        "Program may be waiting for input or in an infinite loop.",
                    )
                except Exception as e:
                    result = TestResult(
                        test_name=test_case.name,
                        status=TestStatus.ERROR,
                        expected_output="",
                        actual_output="",
                        error_message=str(e),
                    )
            else:
                result = runner.run_single_test_with_input_text(
                    input_text, test_case.expected_output_file, test_case.name
                )

            if dump_output_dir and result.status != TestStatus.ERROR:
                dump_path = build_dump_output_path(
                    dump_output_dir, test_root, test_case
                )
                dump_path.parent.mkdir(parents=True, exist_ok=True)
                dump_path.write_text(result.actual_output)

            all_results.append(result)

            if result.status == TestStatus.PASSED:
                passed += 1
            elif result.status == TestStatus.FAILED:
                failed += 1
            else:
                errors += 1

            print_test_result(result, verbose)

    return all_results, passed, failed, errors


def main() -> int:
    """CLI entry point for the test runner.

    Returns:
        Exit code: ``0`` for success, ``1`` for failures.
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
        "--input-file",
        type=Path,
        help="Run exactly one fixture input file (*.in.txt) instead of discovering a test root",
    )
    parser.add_argument(
        "--expected-file",
        type=Path,
        help="Expected output file for --input-file mode (optional, auto-derived if omitted)",
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
    parser.add_argument(
        "--dump-output",
        type=Path,
        help="Directory to write actual output dumps (*.actual.out.txt)",
    )
    parser.add_argument(
        "--dump-only",
        action="store_true",
        help="Run tests and dump outputs without expected-output diff comparison",
    )

    args = parser.parse_args()

    if not args.executable.exists():
        print(f"Error: Executable not found: {args.executable}", file=sys.stderr)
        return 1

    if args.input_file and not args.input_file.exists():
        print(
            f"Error: Input fixture file not found: {args.input_file}", file=sys.stderr
        )
        return 1

    if not args.input_file and not args.test_root.exists():
        print(
            f"Error: Test root directory not found: {args.test_root}", file=sys.stderr
        )
        return 1

    dump_output_dir: Optional[Path] = args.dump_output
    if args.dump_only and dump_output_dir is None:
        dump_output_dir = Path("test-dumps")

    try:
        if args.input_file:
            results, passed, failed, errors = run_single_fixture(
                executable_path=args.executable,
                input_file=args.input_file,
                timeout=args.timeout,
                verbose=args.verbose,
                dump_output_dir=dump_output_dir,
                dump_only=args.dump_only,
                expected_output_file=args.expected_file,
            )
        else:
            results, passed, failed, errors = run_test_suite(
                args.executable,
                args.test_root,
                args.verbose,
                args.timeout,
                dump_output_dir,
                args.dump_only,
            )

        generate_report(results, passed, failed, errors, args.report)
        return 0 if (failed == 0 and errors == 0) else 1

    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        return 1
