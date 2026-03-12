"""InCollege COBOL test infrastructure package.

Re-exports the public API so that consumers can do::

    from incollege_tests import CobolTestRunner, expand_macros, discover_tests
"""

from __future__ import annotations

from incollege_tests.cli import main as cli_main
from incollege_tests.cli import run_single_fixture, run_test_suite
from incollege_tests.discovery import (build_dump_output_path,
                                       derive_expected_output_path,
                                       discover_tests)
from incollege_tests.macros import (expand_macros, has_macros, load_macros,
                                    validate_no_unexpanded)
from incollege_tests.models import (ProfileRecordData, SeedUserMacro, TestCase,
                                    TestResult, TestStatus)
from incollege_tests.packaging import main as packaging_main
from incollege_tests.persistence import PersistenceManager
from incollege_tests.preprocessing import preprocess_input_file
from incollege_tests.reporting import generate_report, print_test_result
from incollege_tests.runner import CobolTestRunner

__all__ = [
    "CobolTestRunner",
    "PersistenceManager",
    "ProfileRecordData",
    "SeedUserMacro",
    "TestCase",
    "TestResult",
    "TestStatus",
    "build_dump_output_path",
    "cli_main",
    "derive_expected_output_path",
    "discover_tests",
    "expand_macros",
    "generate_report",
    "has_macros",
    "load_macros",
    "packaging_main",
    "preprocess_input_file",
    "print_test_result",
    "run_single_fixture",
    "run_test_suite",
    "validate_no_unexpanded",
]
