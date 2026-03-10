"""Terminal output formatting and JSON report generation.

Handles coloured test-result printing and optional JSON report files.
"""
from __future__ import annotations

import json
from pathlib import Path
from typing import Optional

from incollege_tests.models import TestResult, TestStatus

# ANSI colour codes
_GREEN: str = "\033[92m"
_RED: str = "\033[91m"
_YELLOW: str = "\033[93m"
_BLUE: str = "\033[94m"
_RESET: str = "\033[0m"

_STATUS_SYMBOL: dict[TestStatus, str] = {
    TestStatus.PASSED: "\u2713",
    TestStatus.FAILED: "\u2717",
    TestStatus.ERROR: "\u26a0",
}

_STATUS_COLOR: dict[TestStatus, str] = {
    TestStatus.PASSED: _GREEN,
    TestStatus.FAILED: _RED,
    TestStatus.ERROR: _YELLOW,
}


def print_test_result(result: TestResult, verbose: bool = False) -> None:
    """Print a single formatted test result to stdout.

    Args:
        result: The test result to display.
        verbose: If ``True``, also print expected/actual output excerpts.
    """
    symbol = _STATUS_SYMBOL.get(result.status, "?")
    color = _STATUS_COLOR.get(result.status, "")

    print(f"{color}{symbol} {result.test_name}: {result.status.value}{_RESET}")

    if result.status == TestStatus.ERROR:
        print(f"  Error: {result.error_message}")
    elif result.status == TestStatus.FAILED:
        print("\n  Differences found:")
        if result.diff:
            for line in result.diff.split("\n"):
                if line.startswith("+") and not line.startswith("+++"):
                    print(f"    {_GREEN}{line}{_RESET}")
                elif line.startswith("-") and not line.startswith("---"):
                    print(f"    {_RED}{line}{_RESET}")
                elif line.startswith("@@"):
                    print(f"    {_BLUE}{line}{_RESET}")
                else:
                    print(f"    {line}")

    if verbose and result.status != TestStatus.ERROR:
        print("\n  Expected Output:")
        for line in result.expected_output.split("\n")[:10]:
            print(f"    {line}")
        print("\n  Actual Output:")
        for line in result.actual_output.split("\n")[:10]:
            print(f"    {line}")


def generate_report(
    results: list[TestResult],
    passed: int,
    failed: int,
    errors: int,
    output_file: Optional[Path] = None,
) -> None:
    """Print a summary report and optionally write a JSON report file.

    Args:
        results: All test results from the run.
        passed: Count of passed tests.
        failed: Count of failed tests.
        errors: Count of errored tests.
        output_file: Optional path for a JSON report.
    """
    total = passed + failed + errors

    print(f"\n{'=' * 70}")
    print("Test Summary")
    print(f"{'=' * 70}\n")
    print(f"Total Tests: {total}")
    print(
        f"Passed: {_GREEN}{passed}{_RESET} ({passed / total * 100:.1f}%)"
        if total > 0
        else "Passed: 0"
    )
    print(
        f"Failed: {_RED}{failed}{_RESET} ({failed / total * 100:.1f}%)"
        if total > 0
        else "Failed: 0"
    )
    print(
        f"Errors: {_YELLOW}{errors}{_RESET} ({errors / total * 100:.1f}%)"
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
