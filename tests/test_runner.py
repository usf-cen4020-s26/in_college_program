#!/usr/bin/env python3
"""InCollege COBOL Test Runner — backward-compatibility wrapper.

This file delegates to the ``incollege_tests`` package.  All logic now
lives in ``tests/incollege_tests/``.

Legacy imports (``from test_runner import CobolTestRunner``, etc.) are
re-exported so that existing scripts and CI continue to work.
"""

from __future__ import annotations

import sys

# Re-export the full public API so existing ``from test_runner import …``
# statements keep working.
from incollege_tests import (  # noqa: F401
    CobolTestRunner as CobolTestRunner,
    PersistenceManager as PersistenceManager,
    ProfileRecordData as ProfileRecordData,
    SeedUserMacro as SeedUserMacro,
    TestCase as TestCase,
    TestResult as TestResult,
    TestStatus as TestStatus,
    build_dump_output_path as build_dump_output_path,
    cli_main as cli_main,
    derive_expected_output_path as derive_expected_output_path,
    discover_tests as discover_tests,
    expand_macros as expand_macros,
    generate_report as generate_report,
    has_macros as has_macros,
    load_macros as load_macros,
    packaging_main as packaging_main,
    preprocess_input_file as preprocess_input_file,
    print_test_result as print_test_result,
    run_single_fixture as run_single_fixture,
    run_test_suite as run_test_suite,
    validate_no_unexpanded as validate_no_unexpanded,
)


def main() -> int:
    """Entry point — delegates to ``incollege_tests.cli.main()``."""
    return cli_main()


if __name__ == "__main__":
    sys.exit(main())
