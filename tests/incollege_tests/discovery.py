"""Test discovery: find fixture files and group multi-part tests.

Scans a test root directory for ``inputs/`` and ``expected/`` sub-directories
and builds :class:`~incollege_tests.models.TestCase` groups.
"""

from __future__ import annotations

import sys
from pathlib import Path

from incollege_tests.models import TestCase


def discover_tests(test_root: Path) -> list[list[TestCase]]:
    """Discover all test cases under *test_root*.

    Multi-part tests (files ending with ``_part_N``) are grouped together and
    sorted by part number.

    Args:
        test_root: Root directory for tests (e.g. ``tests/fixtures``).

    Returns:
        List of test-case groups.  Each group is a list of
        :class:`~incollege_tests.models.TestCase` objects.
        Single-part tests have groups with one element.
    """
    test_groups: dict[str, list[TestCase]] = {}

    for category_dir in _find_test_dirs(test_root):
        inputs_dir = category_dir / "inputs"
        expected_dir = category_dir / "expected"

        for input_file in sorted(inputs_dir.glob("*.in.txt")):
            test_base_name = input_file.stem.replace(".in", "")

            part_number: int | None = None
            base_name = test_base_name

            if "_part_" in test_base_name:
                parts = test_base_name.rsplit("_part_", 1)
                if len(parts) == 2 and parts[1].isdigit():
                    base_name = parts[0]
                    part_number = int(parts[1])

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

            if base_name not in test_groups:
                test_groups[base_name] = []
            test_groups[base_name].append(test_case)

    for group in test_groups.values():
        group.sort(key=lambda tc: tc.part_number if tc.part_number is not None else 0)

    return list(test_groups.values())


def derive_expected_output_path(input_file: Path) -> Path:
    """Derive the expected output path from an input fixture path.

    The convention is that ``inputs/foo.in.txt`` maps to
    ``expected/foo.out.txt``.

    Args:
        input_file: Path to a ``.in.txt`` fixture.

    Returns:
        Corresponding ``.out.txt`` path under ``expected/``.

    Raises:
        ValueError: If the input file is not under an ``inputs`` directory.
    """
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


def build_dump_output_path(
    dump_root: Path, test_root: Path, test_case: TestCase
) -> Path:
    """Build a deterministic output-dump path for one test case.

    Args:
        dump_root: Directory where dumps are written.
        test_root: The test fixture root directory.
        test_case: The test case whose output is being dumped.

    Returns:
        Absolute path for the ``.actual.out.txt`` dump file.
    """
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


# ------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------


def _find_test_dirs(root: Path) -> list[Path]:
    """Find all directories that contain both ``inputs`` and ``expected`` sub-directories."""
    test_dirs: list[Path] = []

    if (root / "inputs").exists() and (root / "expected").exists():
        test_dirs.append(root)

    for item in root.rglob("*"):
        if item.is_dir():
            if (item / "inputs").exists() and (item / "expected").exists():
                test_dirs.append(item)

    return test_dirs
