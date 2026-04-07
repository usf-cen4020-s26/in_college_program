"""COBOL test execution engine.

Provides :class:`CobolTestRunner` which executes the InCollege COBOL
executable with test inputs and compares actual output against expected output.
Uses :class:`~incollege_tests.persistence.PersistenceManager` via composition.
"""

from __future__ import annotations

import difflib
import re
import subprocess
from pathlib import Path
from typing import Optional

from incollege_tests.macros import expand_macros, load_macros
from incollege_tests.models import (
    SeedConnectionMacro,
    SeedMessageMacro,
    SeedUserMacro,
    TestResult,
    TestStatus,
)
from incollege_tests.persistence import PersistenceManager

# Normalize volatile "Sent:" lines so runtime timestamps match expected placeholders.
_SENT_TIMESTAMP_LINE: re.Pattern[str] = re.compile(
    r"^Sent: \d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\s*$",
    re.MULTILINE,
)


def _normalize_sent_timestamp_lines(text: str) -> str:
    """Replace ``Sent: YYYY-MM-DD hh:mm:ss`` lines with a stable placeholder."""
    t = _SENT_TIMESTAMP_LINE.sub("Sent: |NORM|", text)
    return t.replace("Sent: {{SENT_TIMESTAMP}}", "Sent: |NORM|")


class CobolTestRunner:
    """Test runner for COBOL executables.

    Handles execution of COBOL programs with input files, comparison of actual
    output against expected output, and persistence management.

    Args:
        executable_path: Path to the compiled COBOL executable.
        work_dir: Working directory for I/O and ``.DAT`` files.
        timeout: Maximum execution time in seconds per test.

    Raises:
        FileNotFoundError: If *executable_path* does not exist.
    """

    def __init__(
        self,
        executable_path: Path,
        work_dir: Optional[Path] = None,
        timeout: int = 10,
    ) -> None:
        executable_path = executable_path.resolve()
        if not executable_path.exists():
            raise FileNotFoundError(f"Executable not found: {executable_path}")

        self.executable_path: Path = executable_path
        self.work_dir: Path = work_dir or Path("/tmp/incollege_test_work")
        self.work_dir.mkdir(parents=True, exist_ok=True)
        self.timeout: int = timeout

        self.input_txt: Path = self.work_dir / "INPUT.TXT"
        self.output_txt: Path = self.work_dir / "OUTPUT.TXT"

        self.persistence: PersistenceManager = PersistenceManager(self.work_dir)

        # Pre-load macros once for lifetime of runner
        self._macros: dict[str, str] = load_macros()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def run_single_test(
        self, input_file: Path, expected_output_file: Path, test_name: str
    ) -> TestResult:
        """Run a single test case from file paths.

        Args:
            input_file: Path to the input file.
            expected_output_file: Path to the expected output file.
            test_name: Human-readable name of the test.

        Returns:
            A :class:`~incollege_tests.models.TestResult`.
        """
        try:
            expected_output = expected_output_file.read_text()
            actual_output = self._execute_cobol(input_file)
            return self._build_result(test_name, expected_output, actual_output)
        except subprocess.TimeoutExpired:
            return self._timeout_result(test_name)
        except Exception as e:
            return self._error_result(test_name, str(e))

    def run_single_test_with_input_text(
        self, input_text: str, expected_output_file: Path, test_name: str
    ) -> TestResult:
        """Run a single test case using pre-processed input text.

        Args:
            input_text: Input content to write into ``INPUT.TXT``.
            expected_output_file: Path to the expected output file.
            test_name: Human-readable name of the test.

        Returns:
            A :class:`~incollege_tests.models.TestResult`.
        """
        try:
            expected_output = expected_output_file.read_text()
            actual_output = self._execute_cobol_with_input_text(input_text)
            return self._build_result(test_name, expected_output, actual_output)
        except subprocess.TimeoutExpired:
            return self._timeout_result(test_name)
        except Exception as e:
            return self._error_result(test_name, str(e))

    def execute_with_input_text(self, input_text: str) -> str:
        """Execute COBOL with pre-processed input and return ``OUTPUT.TXT`` contents."""
        return self._execute_cobol_with_input_text(input_text)

    def seed_users(self, users: list[SeedUserMacro]) -> None:
        """Delegate to persistence manager."""
        self.persistence.seed_users(users)

    def seed_connections(self, connections: list[SeedConnectionMacro]) -> None:
        """Delegate to persistence manager."""
        self.persistence.seed_connections(connections)

    def seed_messages(self, messages: list[SeedMessageMacro]) -> None:
        """Delegate to persistence manager."""
        self.persistence.seed_messages(messages)

    def clear_persistence(self) -> None:
        """Delegate to persistence manager."""
        self.persistence.clear_persistence()

    def cleanup_work_dir(self) -> None:
        """Delegate to persistence manager."""
        self.persistence.cleanup_work_dir()

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _build_result(
        self, test_name: str, expected_output: str, actual_output: str
    ) -> TestResult:
        """Compare expected and actual output and build a test result.

        Expands macros in the expected output before comparison.
        """
        expanded_expected = expand_macros(expected_output, self._macros)
        exp_cmp = _normalize_sent_timestamp_lines(expanded_expected)
        act_cmp = _normalize_sent_timestamp_lines(actual_output)

        if act_cmp.strip() == exp_cmp.strip():
            return TestResult(
                test_name=test_name,
                status=TestStatus.PASSED,
                expected_output=expanded_expected,
                actual_output=actual_output,
            )

        diff = self._generate_diff(exp_cmp, act_cmp, test_name)
        return TestResult(
            test_name=test_name,
            status=TestStatus.FAILED,
            expected_output=expanded_expected,
            actual_output=actual_output,
            diff=diff,
        )

    def _execute_cobol(self, input_file: Path) -> str:
        """Execute the COBOL program with input from a file."""
        input_text = input_file.read_text()
        return self._execute_cobol_with_input_text(input_text)

    def _execute_cobol_with_input_text(self, input_text: str) -> str:
        """Execute the COBOL program with raw ``INPUT.TXT`` text content."""
        self.input_txt.write_text(input_text)

        if self.output_txt.exists():
            self.output_txt.unlink()

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

        if not self.output_txt.exists():
            raise FileNotFoundError(
                f"OUTPUT.TXT was not created by the COBOL program. "
                f"stdout: {result.stdout}, stderr: {result.stderr}"
            )

        return self.output_txt.read_text()

    @staticmethod
    def _generate_diff(expected: str, actual: str, test_name: str) -> str:
        """Generate a unified diff between expected and actual output."""
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

    def _timeout_result(self, test_name: str) -> TestResult:
        """Build an ERROR result for a timed-out test."""
        return TestResult(
            test_name=test_name,
            status=TestStatus.ERROR,
            expected_output="",
            actual_output="",
            error_message=(
                f"Test timed out after {self.timeout} seconds. "
                f"Program may be waiting for input or in an infinite loop."
            ),
        )

    @staticmethod
    def _error_result(test_name: str, message: str) -> TestResult:
        """Build an ERROR result with a message."""
        return TestResult(
            test_name=test_name,
            status=TestStatus.ERROR,
            expected_output="",
            actual_output="",
            error_message=message,
        )
