"""Data models for InCollege test infrastructure.

Provides dataclasses for seed-user macros, COBOL persistence records,
test cases, and test results.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Optional


@dataclass
class SeedUserMacro:
    """Top-of-file test macro describing a user to seed before execution.

    Attributes:
        username: Account username (max 20 chars).
        password: Account password (max 12 chars).
        with_profile: Whether to also create a PROFILES.DAT record.
        first_name: Profile first name.
        last_name: Profile last name.
        university: Profile university.
        major: Profile major.
        grad_year: 4-digit graduation year.
        about_me: Profile about-me text.
    """

    username: str
    password: str
    with_profile: bool
    first_name: str = ""
    last_name: str = ""
    university: str = ""
    major: str = ""
    grad_year: str = ""
    about_me: str = ""


@dataclass
class ProfileRecordData:
    """In-memory representation of one PROFILES.DAT record.

    Attributes:
        username: Account username.
        has_profile: ``"1"`` if the user has a profile, ``"0"`` otherwise.
        first_name: Profile first name (fixed-width).
        last_name: Profile last name (fixed-width).
        university: Profile university (fixed-width).
        major: Profile major (fixed-width).
        grad_year: 4-digit graduation year.
        about_me: About-me text (fixed-width).
        exp_count: Number of experience entries as a single digit string.
        exp_entries: List of (title, company, dates, description) tuples.
        edu_count: Number of education entries as a single digit string.
        edu_entries: List of (degree, university, years) tuples.
    """

    username: str
    has_profile: str
    first_name: str
    last_name: str
    university: str
    major: str
    grad_year: str
    about_me: str
    exp_count: str
    exp_entries: list[tuple[str, str, str, str]]
    edu_count: str
    edu_entries: list[tuple[str, str, str]]


class TestStatus(Enum):
    """Enum representing the status of a test execution."""

    PASSED = "PASSED"
    FAILED = "FAILED"
    ERROR = "ERROR"


@dataclass
class TestResult:
    """Result of a single test execution.

    Attributes:
        test_name: Name of the test.
        status: Status of the test (PASSED/FAILED/ERROR).
        expected_output: Expected output content.
        actual_output: Actual output from the COBOL program.
        error_message: Error message if status is ERROR.
        diff: Unified diff between expected and actual output.
    """

    test_name: str
    status: TestStatus
    expected_output: str
    actual_output: str
    error_message: Optional[str] = None
    diff: Optional[str] = None


@dataclass
class TestCase:
    """A test case with input and expected output files.

    Attributes:
        name: Name of the test case.
        input_file: Path to the input file.
        expected_output_file: Path to the expected output file.
        part_number: Part number for multi-part tests (``None`` for single-part).
    """

    name: str
    input_file: Path
    expected_output_file: Path
    part_number: Optional[int] = None
