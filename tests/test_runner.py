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
from incollege_tests import CobolTestRunner as CobolTestRunner  # noqa: F401
from incollege_tests import PersistenceManager as PersistenceManager
from incollege_tests import ProfileRecordData as ProfileRecordData
from incollege_tests import SeedUserMacro as SeedUserMacro
from incollege_tests import TestCase as TestCase
from incollege_tests import TestResult as TestResult
from incollege_tests import TestStatus as TestStatus
from incollege_tests import build_dump_output_path as build_dump_output_path
from incollege_tests import cli_main as cli_main
from incollege_tests import derive_expected_output_path as derive_expected_output_path
from incollege_tests import discover_tests as discover_tests
from incollege_tests import expand_macros as expand_macros
from incollege_tests import generate_report as generate_report
from incollege_tests import has_macros as has_macros
from incollege_tests import load_macros as load_macros
from incollege_tests import packaging_main as packaging_main
from incollege_tests import preprocess_input_file as preprocess_input_file
from incollege_tests import print_test_result as print_test_result
from incollege_tests import run_single_fixture as run_single_fixture
from incollege_tests import run_test_suite as run_test_suite
from incollege_tests import validate_no_unexpanded as validate_no_unexpanded


def main() -> int:
    """Entry point — delegates to ``incollege_tests.cli.main()``."""
    return cli_main()


if __name__ == "__main__":
    sys.exit(main())
