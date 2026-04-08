"""Zip packaging with macro expansion for test submission.

Absorbs the functionality of ``package_tests.py`` and adds automatic macro
expansion so that submitted zip files contain **no** ``{{MACRO}}`` tags —
only fully-expanded expected output.

``@seed_user`` directives in ``.in.txt`` files are also expanded: each
directive becomes account-creation keypresses in the packaged input file, and
the corresponding account-creation menu output is prepended to the packaged
expected-output file.

``@seed_connection`` and ``@seed_message`` are stripped from packaged inputs (no
extra keypresses). When generating the account-creation output prefix for
``.out.txt`` files, the packager runs the COBOL binary then writes
``CONNECTIONS.DAT`` / ``MESSAGES.DAT`` in the same working directory, matching
the test runner so submission zips reflect the same persisted state.
"""

from __future__ import annotations

import argparse
import subprocess
import sys
import tempfile
import zipfile
from pathlib import Path

from incollege_tests.macros import expand_macros, load_macros, validate_no_unexpanded
from incollege_tests.models import (
    SeedConnectionMacro,
    SeedMessageMacro,
    SeedUserMacro,
)
from incollege_tests.persistence import PersistenceManager
from incollege_tests.preprocessing import preprocess_input_file

# Per-fixture seeds parsed from ``.in.txt`` (same order as :func:`preprocess_input_file`).
FixtureSeeds = tuple[
    list[SeedUserMacro],
    list[SeedConnectionMacro],
    list[SeedMessageMacro],
]


def collect_files(fixtures_dir: Path, extension: str) -> list[Path]:
    """Recursively collect files matching *extension* under *fixtures_dir*.

    Args:
        fixtures_dir: Root fixtures directory to search.
        extension: File extension to match (e.g. ``".in.txt"``).

    Returns:
        Sorted list of matching file paths.
    """
    return sorted(fixtures_dir.rglob(f"*{extension}"))


def strip_comments(content: str) -> str:
    """Remove comments from an input file's content.

    Handles two comment styles:
    - Full-line comments: lines whose first non-whitespace character is ``#``
      are dropped entirely.
    - Inline comments: everything from ``#`` to end-of-line is removed; the
      remaining text is right-stripped of whitespace.

    Args:
        content: Raw text of an ``.in.txt`` file.

    Returns:
        Text with all comment annotations removed.
    """
    result: list[str] = []
    for line in content.splitlines(keepends=True):
        stripped = line.lstrip()
        if stripped.startswith("#"):
            # Drop full-line comment
            continue
        # Remove inline comment and trailing whitespace, preserve line ending
        eol = "\n" if line.endswith("\n") else ""
        bare = line.rstrip("\n").rstrip("\r")
        if "#" in bare:
            bare = bare[: bare.index("#")].rstrip()
        result.append(bare + eol)
    return "".join(result)


def _seed_users_to_creation_input(users: list[SeedUserMacro]) -> str:
    """Generate COBOL account-creation keypresses for a list of seed users.

    For each user the basic sequence is:
    ``2`` (Create new account), username, password — which returns to the
    login screen.

    When ``with_profile`` is ``True`` the sequence continues with:
    ``1`` (Login), username, password, ``1`` (Create/Edit Profile),
    first_name, last_name, university, major, grad_year, about_me,
    ``DONE`` (skip experience), ``DONE`` (skip education), ``8`` (Logout).

    Args:
        users: Seed-user macro objects.

    Returns:
        Multi-line string of input keypresses, one per line, ready to prepend
        to the packaged ``.in.txt`` body.
    """
    lines: list[str] = []
    for user in users:
        # Create the account (returns to login screen)
        lines.extend(["2", user.username, user.password])

        if user.with_profile:
            # Login, create profile, then logout back to login screen
            lines.extend([
                "1", user.username, user.password,  # Login
                "1",  # Create/Edit Profile from main menu
                user.first_name or user.username,
                user.last_name or user.username,
                user.university or "University",
                user.major or "Major",
                user.grad_year or "2025",
                user.about_me or " ",  # blank to skip about-me
                "DONE",  # skip experience
                "DONE",  # skip education
                "8",  # Logout (back to login screen)
            ])
    return "\n".join(lines) + "\n" if lines else ""


def _run_creation_prefix_with_dat_seeds(
    creation_input: str,
    executable: Path,
    connections: list[SeedConnectionMacro],
    messages: list[SeedMessageMacro],
) -> str:
    """Run account-creation input, then seed connections/messages like the test runner.

    The creation run leaves ``ACCOUNTS.DAT`` (and profiles) in the temp dir;
    connection and message rows are then written so packaged artifacts stay
    aligned with :class:`~incollege_tests.persistence.PersistenceManager`
    behavior.

    Returns:
        ``OUTPUT.TXT`` from the **creation** COBOL run only (``.DAT`` writes do
        not append to that file).
    """
    with tempfile.TemporaryDirectory() as tmp:
        tmp_path = Path(tmp)
        (tmp_path / "INPUT.TXT").write_text(creation_input)
        subprocess.run(
            [str(executable)],
            cwd=tmp_path,
            capture_output=True,
            timeout=30,
        )
        output_file = tmp_path / "OUTPUT.TXT"
        prefix = output_file.read_text() if output_file.exists() else ""

        if connections or messages:
            pm = PersistenceManager(tmp_path)
            if connections:
                pm.seed_connections(connections)
            if messages:
                pm.seed_messages(messages)

        return prefix


def preprocess_input_content(
    input_file: Path,
):
    """Parse seed macros and strip comments from *input_file*.

    Returns the same tuple as :func:`~incollege_tests.preprocessing.preprocess_input_file`.
    """
    return preprocess_input_file(input_file)


def build_zip_with_expansion(
    files: list[Path],
    zip_path: Path,
    base_dir: Path,
    flat: bool = False,
    expand: bool = True,
    executable: Path | None = None,
    seed_bundle_map: dict[str, FixtureSeeds] | None = None,
) -> None:
    """Create a zip archive, optionally expanding macros in ``.out.txt`` files.

    When *executable* and *seed_bundle_map* are supplied, ``@seed_user`` directives in
    ``.in.txt`` files are replaced with account-creation keypresses, and the
    corresponding account-creation output is prepended to ``.out.txt`` files.
    ``@seed_connection`` / ``@seed_message`` are applied to the packager temp
    directory after that run (see :func:`_run_creation_prefix_with_dat_seeds`).

    Args:
        files: Files to include in the archive.
        zip_path: Destination path for the zip file.
        base_dir: Base directory for computing archive names.
        flat: If ``True``, flatten the directory structure inside the zip.
        expand: If ``True`` (default), expand ``{{MACRO}}`` tags in
            ``.out.txt`` files and validate that none remain.
        executable: Optional path to the COBOL binary used to generate
            account-creation output prefixes for seeded users.
        seed_bundle_map: Mapping from test stem (e.g. ``"my_test"``) to
            ``(users, connections, messages)`` from the corresponding ``.in.txt``.
            Built by the caller when processing inputs so the output zip can reuse it.
    """
    macros: dict[str, str] | None = None
    if expand:
        macros = load_macros()

    with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as zf:
        for f in files:
            arcname = f.name if flat else str(f.relative_to(base_dir))

            if expand and f.name.endswith(".out.txt"):
                content = f.read_text()
                if macros is None:
                    raise RuntimeError("Macros should have been loaded by this point")

                expanded = expand_macros(content, macros)
                validate_no_unexpanded(expanded)

                # Prepend account-creation output for any seeded users
                stem = _fixture_base_name(f)  # e.g. "my_test" from "my_test.out.txt"
                users: list[SeedUserMacro] = []
                connections: list[SeedConnectionMacro] = []
                messages: list[SeedMessageMacro] = []
                if seed_bundle_map and stem in seed_bundle_map:
                    users, connections, messages = seed_bundle_map[stem]
                if users and executable is not None:
                    creation_input = _seed_users_to_creation_input(users)
                    creation_output = _run_creation_prefix_with_dat_seeds(
                        creation_input, executable, connections, messages
                    )
                    expanded = creation_output + expanded

                zf.writestr(arcname, expanded)

            elif f.name.endswith(".in.txt"):
                # Parse seed users and strip comments
                body_text, users, _, _ = preprocess_input_content(f)
                if users:
                    creation_prefix = _seed_users_to_creation_input(users)
                    packaged_input = creation_prefix + body_text
                else:
                    packaged_input = body_text
                zf.writestr(arcname, packaged_input)

            else:
                zf.write(f, arcname)


def _fixture_base_name(path: Path) -> str:
    """Return the base test name from a fixture path, stripping double suffixes.

    For example, ``my_test.in.txt`` and ``my_test.out.txt`` both return
    ``my_test``.
    """
    name = path.name
    for suffix in (".in.txt", ".out.txt"):
        if name.endswith(suffix):
            return name[: -len(suffix)]
    return path.stem


def _build_seed_bundle_map(input_files: list[Path]) -> dict[str, FixtureSeeds]:
    """Scan *input_files* and return a mapping from test stem to all seed macros.

    Args:
        input_files: List of ``.in.txt`` fixture files.

    Returns:
        Dict mapping each file stem to ``(users, connections, messages)``.
        Only stems with at least one seed directive are included.
    """
    bundle_map: dict[str, FixtureSeeds] = {}
    for f in input_files:
        _, users, connections, messages = preprocess_input_content(f)
        if users or connections or messages:
            bundle_map[_fixture_base_name(f)] = (users, connections, messages)
    return bundle_map


def main() -> int:
    """CLI entry point for test packaging.

    Returns:
        Exit code: ``0`` on success, ``1`` on error.
    """
    parser = argparse.ArgumentParser(
        description="Package test inputs and expected outputs into zip files for submission."
    )
    parser.add_argument(
        "--epic",
        default="EpicX",
        help="Epic label used in zip filenames (default: EpicX)",
    )
    parser.add_argument(
        "--story",
        default="StoryX",
        help="Story label used in zip filenames (default: StoryX)",
    )
    parser.add_argument(
        "--fixtures",
        default="./tests/fixtures",
        help="Path to the fixtures directory (default: ./fixtures)",
    )
    parser.add_argument(
        "--outdir",
        default=".",
        help="Directory to place the output zip files (default: .)",
    )
    parser.add_argument(
        "--flat",
        action="store_true",
        help="Flatten directory structure inside the zip (just filenames, no subdirs)",
    )
    parser.add_argument(
        "--no-expand",
        action="store_true",
        help="Do NOT expand macros in .out.txt files (for debugging)",
    )
    parser.add_argument(
        "--executable",
        default=None,
        help=(
            "Path to the compiled COBOL binary. Required when any test uses "
            "@seed_user (and recommended whenever @seed_connection or "
            "@seed_message appear with @seed_user) so account-creation output "
            "and matching .DAT seed steps can be applied for packaged "
            "expected-output files."
        ),
    )

    args = parser.parse_args()

    fixtures_dir = Path(args.fixtures).resolve()
    outdir = Path(args.outdir).resolve()

    if not fixtures_dir.is_dir():
        print(f"Error: fixtures directory not found: {fixtures_dir}", file=sys.stderr)
        return 1

    executable: Path | None = None
    if args.executable:
        executable = Path(args.executable).resolve()
        if not executable.exists():
            print(f"Error: executable not found: {executable}", file=sys.stderr)
            return 1

    outdir.mkdir(parents=True, exist_ok=True)

    input_files = collect_files(fixtures_dir, ".in.txt")
    output_files = collect_files(fixtures_dir, ".out.txt")

    if not input_files:
        print("Warning: No .in.txt files found.", file=sys.stderr)
    if not output_files:
        print("Warning: No .out.txt files found.", file=sys.stderr)

    # Build seed bundle map once so both zips can use it
    seed_bundle_map = _build_seed_bundle_map(input_files)
    any_users = any(b[0] for b in seed_bundle_map.values())
    any_conn_or_msg = any(b[1] or b[2] for b in seed_bundle_map.values())
    if seed_bundle_map and executable is None:
        if any_users:
            print(
                "Warning: @seed_user directives found but --executable not provided. "
                "Account-creation output will NOT be prepended to expected-output files.",
                file=sys.stderr,
            )
        if any_conn_or_msg:
            print(
                "Warning: @seed_connection / @seed_message directives found but "
                "--executable not provided; packager cannot mirror runner "
                "persistence for those seeds when generating output prefixes.",
                file=sys.stderr,
            )

    input_zip = outdir / f"{args.epic}-{args.story}-Test-Input.zip"
    output_zip = outdir / f"{args.epic}-{args.story}-Test-Output.zip"

    expand = not args.no_expand

    # Input files: expand seed users to creation keypresses, strip comments
    build_zip_with_expansion(
        input_files,
        input_zip,
        fixtures_dir,
        args.flat,
        expand=False,
        executable=executable,
        seed_bundle_map=seed_bundle_map,
    )
    # Output files: expand macros + prepend account-creation output for seeded tests
    build_zip_with_expansion(
        output_files,
        output_zip,
        fixtures_dir,
        args.flat,
        expand=expand,
        executable=executable,
        seed_bundle_map=seed_bundle_map,
    )

    print(f"Created {input_zip.name}")
    print(
        f"  \u2192 {len(input_files)} input file(s)  [{input_zip.stat().st_size / 1024:.1f} KB]"
    )
    print(f"Created {output_zip.name}")
    print(
        f"  \u2192 {len(output_files)} output file(s) [{output_zip.stat().st_size / 1024:.1f} KB]"
    )

    print()
    print("\u2500\u2500 Input files \u2500\u2500")
    for f in input_files:
        print(f"  {f.relative_to(fixtures_dir)}")
    print()
    print("\u2500\u2500 Output files \u2500\u2500")
    for f in output_files:
        print(f"  {f.relative_to(fixtures_dir)}")

    return 0
