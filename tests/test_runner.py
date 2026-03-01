#!/usr/bin/env python3
"""
InCollege COBOL Test Runner

This module provides functionality to test COBOL executables against expected outputs.
It supports single test execution and batch testing with multi-part test support.
"""

import argparse
import difflib
import json
import shlex
import subprocess
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple


ACCOUNT_USERNAME_WIDTH = 20
ACCOUNT_PASSWORD_WIDTH = 12
ACCOUNT_RECORD_WIDTH = ACCOUNT_USERNAME_WIDTH + ACCOUNT_PASSWORD_WIDTH

PROFILE_USERNAME_WIDTH = 20
PROFILE_HAS_PROFILE_WIDTH = 1
PROFILE_FIRST_NAME_WIDTH = 30
PROFILE_LAST_NAME_WIDTH = 30
PROFILE_UNIVERSITY_WIDTH = 50
PROFILE_MAJOR_WIDTH = 50
PROFILE_GRAD_YEAR_WIDTH = 4
PROFILE_ABOUT_ME_WIDTH = 200
PROFILE_COUNT_WIDTH = 1

PROFILE_EXP_TITLE_WIDTH = 50
PROFILE_EXP_COMPANY_WIDTH = 50
PROFILE_EXP_DATES_WIDTH = 30
PROFILE_EXP_DESC_WIDTH = 100
PROFILE_EXP_ENTRY_WIDTH = (
    PROFILE_EXP_TITLE_WIDTH
    + PROFILE_EXP_COMPANY_WIDTH
    + PROFILE_EXP_DATES_WIDTH
    + PROFILE_EXP_DESC_WIDTH
)
PROFILE_EXP_MAX = 3

PROFILE_EDU_DEGREE_WIDTH = 50
PROFILE_EDU_UNIVERSITY_WIDTH = 50
PROFILE_EDU_YEARS_WIDTH = 20
PROFILE_EDU_ENTRY_WIDTH = (
    PROFILE_EDU_DEGREE_WIDTH + PROFILE_EDU_UNIVERSITY_WIDTH + PROFILE_EDU_YEARS_WIDTH
)
PROFILE_EDU_MAX = 3

PROFILE_RECORD_WIDTH = (
    PROFILE_USERNAME_WIDTH
    + PROFILE_HAS_PROFILE_WIDTH
    + PROFILE_FIRST_NAME_WIDTH
    + PROFILE_LAST_NAME_WIDTH
    + PROFILE_UNIVERSITY_WIDTH
    + PROFILE_MAJOR_WIDTH
    + PROFILE_GRAD_YEAR_WIDTH
    + PROFILE_ABOUT_ME_WIDTH
    + PROFILE_COUNT_WIDTH
    + (PROFILE_EXP_ENTRY_WIDTH * PROFILE_EXP_MAX)
    + PROFILE_COUNT_WIDTH
    + (PROFILE_EDU_ENTRY_WIDTH * PROFILE_EDU_MAX)
)


@dataclass
class SeedUserMacro:
    """Top-of-file test macro describing a user to seed before execution."""

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
    """In-memory representation of one PROFILES.DAT record."""

    username: str
    has_profile: str
    first_name: str
    last_name: str
    university: str
    major: str
    grad_year: str
    about_me: str
    exp_count: str
    exp_entries: List[Tuple[str, str, str, str]]
    edu_count: str
    edu_entries: List[Tuple[str, str, str]]


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
        self.pending_dat = self.work_dir / "PENDING.DAT"
        self.connections_dat = self.work_dir / "CONNECTIONS.DAT"
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

            return self._build_result(
                test_name=test_name,
                expected_output=expected_output,
                actual_output=actual_output,
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

    def run_single_test_with_input_text(
        self, input_text: str, expected_output_file: Path, test_name: str
    ) -> TestResult:
        """
        Run a single test case using a preprocessed input string.

        Args:
            input_text: Input content to write into INPUT.TXT
            expected_output_file: Path to the expected output file
            test_name: Name of the test

        Returns:
            TestResult object containing the test execution results
        """
        try:
            expected_output = expected_output_file.read_text()
            actual_output = self._execute_cobol_with_input_text(input_text)

            return self._build_result(
                test_name=test_name,
                expected_output=expected_output,
                actual_output=actual_output,
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

    def execute_with_input_text(self, input_text: str) -> str:
        """Execute COBOL with preprocessed input and return OUTPUT.TXT contents."""
        return self._execute_cobol_with_input_text(input_text)

    def _build_result(
        self, test_name: str, expected_output: str, actual_output: str
    ) -> TestResult:
        """Compare expected and actual output and build a test result."""

        # Compare outputs
        if actual_output.strip() == expected_output.strip():
            return TestResult(
                test_name=test_name,
                status=TestStatus.PASSED,
                expected_output=expected_output,
                actual_output=actual_output,
            )
        # Generate diff
        diff = self._generate_diff(expected_output, actual_output, test_name)
        return TestResult(
            test_name=test_name,
            status=TestStatus.FAILED,
            expected_output=expected_output,
            actual_output=actual_output,
            diff=diff,
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
        input_text = input_file.read_text()
        return self._execute_cobol_with_input_text(input_text)

    def _execute_cobol_with_input_text(self, input_text: str) -> str:
        """Execute the COBOL program with raw INPUT.TXT text content."""
        self.input_txt.write_text(input_text)

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

    def seed_users(self, users: List[SeedUserMacro]) -> None:
        """
        Seed account/profile persistence records before a test executes.

        Existing records for the same username are updated.
        """
        if not users:
            return

        accounts = self._load_accounts()
        profiles = self._load_profiles()

        account_password_by_username: Dict[str, str] = {
            username: password for username, password in accounts
        }
        account_order = [username for username, _ in accounts]

        profile_by_username = {profile.username: profile for profile in profiles}

        for user in users:
            if user.username in account_password_by_username:
                account_password_by_username[user.username] = user.password
            else:
                if len(account_order) >= 5:
                    raise ValueError(
                        "Cannot seed more than 5 accounts. "
                        f"Failed while adding username '{user.username}'."
                    )
                account_order.append(user.username)
                account_password_by_username[user.username] = user.password

            if user.with_profile:
                profile_by_username[user.username] = ProfileRecordData(
                    username=user.username,
                    has_profile="1",
                    first_name=user.first_name,
                    last_name=user.last_name,
                    university=user.university,
                    major=user.major,
                    grad_year=user.grad_year,
                    about_me=user.about_me,
                    exp_count="0",
                    exp_entries=[("", "", "", "") for _ in range(PROFILE_EXP_MAX)],
                    edu_count="0",
                    edu_entries=[("", "", "") for _ in range(PROFILE_EDU_MAX)],
                )

        updated_accounts = [
            (username, account_password_by_username[username]) for username in account_order
        ]

        updated_profiles = [
            profile_by_username[username]
            for username in account_order
            if username in profile_by_username
        ]

        self._write_accounts(updated_accounts)
        self._write_profiles(updated_profiles)

    @staticmethod
    def _fit(value: str, width: int) -> str:
        """Trim/pad a string for fixed-width COBOL records."""
        return value[:width].ljust(width)

    def _load_accounts(self) -> List[Tuple[str, str]]:
        """Load existing account records from ACCOUNTS.DAT."""
        if not self.accounts_dat.exists():
            return []

        accounts: List[Tuple[str, str]] = []
        for raw_line in self.accounts_dat.read_text().splitlines():
            line = raw_line[:ACCOUNT_RECORD_WIDTH].ljust(ACCOUNT_RECORD_WIDTH)
            username = line[:ACCOUNT_USERNAME_WIDTH].rstrip()
            password = line[
                ACCOUNT_USERNAME_WIDTH : ACCOUNT_USERNAME_WIDTH + ACCOUNT_PASSWORD_WIDTH
            ].rstrip()
            if username:
                accounts.append((username, password))
        return accounts

    def _write_accounts(self, accounts: List[Tuple[str, str]]) -> None:
        """Write full account dataset to ACCOUNTS.DAT."""
        if not accounts:
            if self.accounts_dat.exists():
                self.accounts_dat.unlink()
            return

        lines: List[str] = []
        for username, password in accounts:
            lines.append(
                self._fit(username, ACCOUNT_USERNAME_WIDTH)
                + self._fit(password, ACCOUNT_PASSWORD_WIDTH)
            )
        self.accounts_dat.write_text("\n".join(lines) + "\n")

    def _load_profiles(self) -> List[ProfileRecordData]:
        """Load existing profile records from PROFILES.DAT."""
        if not self.profiles_dat.exists():
            return []

        profiles: List[ProfileRecordData] = []

        for raw_line in self.profiles_dat.read_text().splitlines():
            line = raw_line[:PROFILE_RECORD_WIDTH].ljust(PROFILE_RECORD_WIDTH)
            cursor = 0

            def take(width: int) -> str:
                nonlocal cursor
                value = line[cursor : cursor + width]
                cursor += width
                return value

            username = take(PROFILE_USERNAME_WIDTH).rstrip()
            has_profile = take(PROFILE_HAS_PROFILE_WIDTH)
            first_name = take(PROFILE_FIRST_NAME_WIDTH)
            last_name = take(PROFILE_LAST_NAME_WIDTH)
            university = take(PROFILE_UNIVERSITY_WIDTH)
            major = take(PROFILE_MAJOR_WIDTH)
            grad_year = take(PROFILE_GRAD_YEAR_WIDTH)
            about_me = take(PROFILE_ABOUT_ME_WIDTH)
            exp_count = take(PROFILE_COUNT_WIDTH)

            exp_entries: List[Tuple[str, str, str, str]] = []
            for _ in range(PROFILE_EXP_MAX):
                exp_entries.append(
                    (
                        take(PROFILE_EXP_TITLE_WIDTH),
                        take(PROFILE_EXP_COMPANY_WIDTH),
                        take(PROFILE_EXP_DATES_WIDTH),
                        take(PROFILE_EXP_DESC_WIDTH),
                    )
                )

            edu_count = take(PROFILE_COUNT_WIDTH)
            edu_entries: List[Tuple[str, str, str]] = []
            for _ in range(PROFILE_EDU_MAX):
                edu_entries.append(
                    (
                        take(PROFILE_EDU_DEGREE_WIDTH),
                        take(PROFILE_EDU_UNIVERSITY_WIDTH),
                        take(PROFILE_EDU_YEARS_WIDTH),
                    )
                )

            if username:
                profiles.append(
                    ProfileRecordData(
                        username=username,
                        has_profile=has_profile,
                        first_name=first_name,
                        last_name=last_name,
                        university=university,
                        major=major,
                        grad_year=grad_year,
                        about_me=about_me,
                        exp_count=exp_count,
                        exp_entries=exp_entries,
                        edu_count=edu_count,
                        edu_entries=edu_entries,
                    )
                )

        return profiles

    def _write_profiles(self, profiles: List[ProfileRecordData]) -> None:
        """Write full profile dataset to PROFILES.DAT."""
        if not profiles:
            if self.profiles_dat.exists():
                self.profiles_dat.unlink()
            return

        def ensure_exp_entries(
            entries: List[Tuple[str, str, str, str]],
        ) -> List[Tuple[str, str, str, str]]:
            safe_entries = list(entries[:PROFILE_EXP_MAX])
            while len(safe_entries) < PROFILE_EXP_MAX:
                safe_entries.append(("", "", "", ""))
            return safe_entries

        def ensure_edu_entries(
            entries: List[Tuple[str, str, str]],
        ) -> List[Tuple[str, str, str]]:
            safe_entries = list(entries[:PROFILE_EDU_MAX])
            while len(safe_entries) < PROFILE_EDU_MAX:
                safe_entries.append(("", "", ""))
            return safe_entries

        lines: List[str] = []
        for profile in profiles:
            exp_entries = ensure_exp_entries(profile.exp_entries)
            edu_entries = ensure_edu_entries(profile.edu_entries)

            line = ""
            line += self._fit(profile.username, PROFILE_USERNAME_WIDTH)
            line += self._fit(profile.has_profile, PROFILE_HAS_PROFILE_WIDTH)
            line += self._fit(profile.first_name, PROFILE_FIRST_NAME_WIDTH)
            line += self._fit(profile.last_name, PROFILE_LAST_NAME_WIDTH)
            line += self._fit(profile.university, PROFILE_UNIVERSITY_WIDTH)
            line += self._fit(profile.major, PROFILE_MAJOR_WIDTH)
            line += self._fit(profile.grad_year, PROFILE_GRAD_YEAR_WIDTH)
            line += self._fit(profile.about_me, PROFILE_ABOUT_ME_WIDTH)
            line += self._fit(profile.exp_count, PROFILE_COUNT_WIDTH)

            for title, company, dates, desc in exp_entries:
                line += self._fit(title, PROFILE_EXP_TITLE_WIDTH)
                line += self._fit(company, PROFILE_EXP_COMPANY_WIDTH)
                line += self._fit(dates, PROFILE_EXP_DATES_WIDTH)
                line += self._fit(desc, PROFILE_EXP_DESC_WIDTH)

            line += self._fit(profile.edu_count, PROFILE_COUNT_WIDTH)

            for degree, university, years in edu_entries:
                line += self._fit(degree, PROFILE_EDU_DEGREE_WIDTH)
                line += self._fit(university, PROFILE_EDU_UNIVERSITY_WIDTH)
                line += self._fit(years, PROFILE_EDU_YEARS_WIDTH)

            lines.append(line)

        self.profiles_dat.write_text("\n".join(lines) + "\n")

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

        if self.pending_dat.exists():
            self.pending_dat.unlink()

        if self.connections_dat.exists():
            self.connections_dat.unlink()

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
            self.pending_dat,
            self.connections_dat,
        ]:
            if file_path.exists():
                file_path.unlink()


def _parse_macro_key_values(macro_args: str) -> Dict[str, str]:
    """Parse `key=value` tokens from a macro line."""
    tokens = shlex.split(macro_args)
    parsed: Dict[str, str] = {}

    for token in tokens:
        if "=" not in token:
            raise ValueError(
                f"Invalid macro token '{token}'. Expected key=value format."
            )
        key, value = token.split("=", 1)
        key = key.strip().lower()
        value = value.strip()
        if not key:
            raise ValueError(f"Invalid macro token '{token}'. Missing key name.")
        parsed[key] = value

    return parsed


def _to_bool(value: str) -> bool:
    """Convert a string to bool for macro parameters."""
    lowered = value.strip().lower()
    if lowered in {"1", "true", "yes", "y", "on"}:
        return True
    if lowered in {"0", "false", "no", "n", "off"}:
        return False
    raise ValueError(
        f"Invalid boolean value '{value}'. Use true/false, yes/no, or 1/0."
    )


def _parse_seed_user_macro(macro_line: str) -> SeedUserMacro:
    """Parse one `@seed_user` macro line into a SeedUserMacro object."""
    stripped = macro_line.strip()
    if stripped.startswith("#"):
        stripped = stripped[1:].strip()

    parts = stripped.split(maxsplit=1)
    command = parts[0].lower()
    if command != "@seed_user":
        raise ValueError(f"Unsupported macro directive '{parts[0]}'.")

    macro_args = parts[1] if len(parts) > 1 else ""
    parsed = _parse_macro_key_values(macro_args)

    username = parsed.get("username", "")
    password = parsed.get("password", "")
    with_profile = _to_bool(parsed.get("with_profile", "true"))

    if not username:
        raise ValueError("@seed_user macro requires 'username'.")
    if not password:
        raise ValueError("@seed_user macro requires 'password'.")

    if len(username) > ACCOUNT_USERNAME_WIDTH:
        raise ValueError(
            f"@seed_user username '{username}' exceeds {ACCOUNT_USERNAME_WIDTH} characters."
        )
    if len(password) > ACCOUNT_PASSWORD_WIDTH:
        raise ValueError(
            f"@seed_user password for '{username}' exceeds {ACCOUNT_PASSWORD_WIDTH} characters."
        )

    first_name = parsed.get("first_name", "")
    last_name = parsed.get("last_name", "")
    university = parsed.get("university", "")
    major = parsed.get("major", "")
    grad_year = parsed.get("grad_year", "")
    about_me = parsed.get("about_me", "")

    if with_profile:
        required_profile_fields = {
            "first_name": first_name,
            "last_name": last_name,
            "university": university,
            "major": major,
            "grad_year": grad_year,
        }
        missing = [name for name, value in required_profile_fields.items() if not value]
        if missing:
            missing_list = ", ".join(missing)
            raise ValueError(
                "@seed_user with_profile=true requires fields: "
                f"{missing_list}. (username='{username}')"
            )

        if not grad_year.isdigit() or len(grad_year) != 4:
            raise ValueError(
                "@seed_user grad_year must be a 4-digit year "
                f"for username '{username}'."
            )

    return SeedUserMacro(
        username=username,
        password=password,
        with_profile=with_profile,
        first_name=first_name,
        last_name=last_name,
        university=university,
        major=major,
        grad_year=grad_year,
        about_me=about_me,
    )


def preprocess_input_file(input_file: Path) -> Tuple[str, List[SeedUserMacro]]:
    """
    Parse seed macros from the top of a test input file and return executable input.

    Macros must appear at the top of the file and are removed from the final INPUT.TXT.
    """
    lines = input_file.read_text().splitlines(keepends=True)
    seed_users: List[SeedUserMacro] = []

    body_start = 0
    in_header = False

    for index, line in enumerate(lines):
        stripped = line.strip()

        is_seed_directive = stripped.startswith("@seed_user") or stripped.startswith(
            "# @seed_user"
        ) or stripped.startswith("#@seed_user")

        if is_seed_directive:
            seed_users.append(_parse_seed_user_macro(stripped))
            body_start = index + 1
            in_header = True
            continue

        if in_header and (stripped == "" or stripped.startswith("#")):
            body_start = index + 1
            continue

        break

    executable_input = "".join(lines[body_start:])
    return executable_input, seed_users


def build_dump_output_path(dump_root: Path, test_root: Path, test_case: TestCase) -> Path:
    """Build a deterministic output dump path for one test case."""
    rel_input = test_case.input_file.relative_to(test_root)
    rel_parts = list(rel_input.parts)

    if "inputs" in rel_parts:
        idx = rel_parts.index("inputs")
        rel_parts[idx] = "actual"

    input_name = rel_parts[-1]
    if input_name.endswith(".in.txt"):
        output_name = f"{input_name[:-7]}.actual.out.txt"
    else:
        output_name = f"{Path(input_name).stem}.actual.out.txt"

    rel_parts[-1] = output_name
    return dump_root / Path(*rel_parts)


def derive_expected_output_path(input_file: Path) -> Path:
    """Derive expected output path from an input fixture path."""
    input_parts = list(input_file.parts)

    if "inputs" not in input_parts:
        raise ValueError(
            "Could not derive expected output path: input file is not under an 'inputs' directory."
        )

    idx = input_parts.index("inputs")
    input_parts[idx] = "expected"

    input_name = input_parts[-1]
    if input_name.endswith(".in.txt"):
        output_name = f"{input_name[:-7]}.out.txt"
    else:
        output_name = f"{Path(input_name).stem}.out.txt"

    input_parts[-1] = output_name
    return Path(*input_parts)


def run_single_fixture(
    executable_path: Path,
    input_file: Path,
    timeout: int,
    verbose: bool,
    dump_output_dir: Optional[Path],
    dump_only: bool,
    expected_output_file: Optional[Path],
) -> Tuple[List[TestResult], int, int, int]:
    """Run exactly one fixture input file."""
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

        # First check if the root itself is a test directory
        if (root / "inputs").exists() and (root / "expected").exists():
            test_dirs.append(root)

        # Then check all subdirectories
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
        print("\n  Differences found:")
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
        print("\n  Expected Output:")
        for line in result.expected_output.split("\n")[:10]:
            print(f"    {line}")
        print("\n  Actual Output:")
        for line in result.actual_output.split("\n")[:10]:
            print(f"    {line}")


def run_test_suite(
    executable_path: Path,
    test_root: Path,
    verbose: bool = False,
    timeout: int = 10,
    dump_output_dir: Optional[Path] = None,
    dump_only: bool = False,
) -> Tuple[List[TestResult], int, int, int]:
    """
    Run all discovered tests.

    Args:
        executable_path: Path to the COBOL executable
        test_root: Root directory for tests
        verbose: If True, print detailed output
        timeout: Maximum execution time in seconds for each test
        dump_output_dir: Optional directory where actual outputs are written
        dump_only: If True, skip expected-output comparison and only dump outputs

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
        # Clear persistence before each test group
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
                dump_path = build_dump_output_path(dump_output_dir, test_root, test_case)
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
    print("Test Summary")
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
        report_data: Dict[str, object] = {
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

    # Validate inputs
    if not args.executable.exists():
        print(f"Error: Executable not found: {args.executable}", file=sys.stderr)
        return 1

    if args.input_file and not args.input_file.exists():
        print(f"Error: Input fixture file not found: {args.input_file}", file=sys.stderr)
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

        # Return non-zero exit code if any tests failed or had errors
        return 0 if (failed == 0 and errors == 0) else 1

    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
