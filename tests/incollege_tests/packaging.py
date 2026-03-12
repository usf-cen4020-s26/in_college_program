"""Zip packaging with macro expansion for test submission.

Absorbs the functionality of ``package_tests.py`` and adds automatic macro
expansion so that submitted zip files contain **no** ``{{MACRO}}`` tags —
only fully-expanded expected output.
"""

from __future__ import annotations

import argparse
import sys
import zipfile
from pathlib import Path

from incollege_tests.macros import expand_macros, load_macros, validate_no_unexpanded


def collect_files(fixtures_dir: Path, extension: str) -> list[Path]:
    """Recursively collect files matching *extension* under *fixtures_dir*.

    Args:
        fixtures_dir: Root fixtures directory to search.
        extension: File extension to match (e.g. ``".in.txt"``).

    Returns:
        Sorted list of matching file paths.
    """
    return sorted(fixtures_dir.rglob(f"*{extension}"))


def build_zip_with_expansion(
    files: list[Path],
    zip_path: Path,
    base_dir: Path,
    flat: bool = False,
    expand: bool = True,
) -> None:
    """Create a zip archive, optionally expanding macros in ``.out.txt`` files.

    Args:
        files: Files to include in the archive.
        zip_path: Destination path for the zip file.
        base_dir: Base directory for computing archive names.
        flat: If ``True``, flatten the directory structure inside the zip.
        expand: If ``True`` (default), expand ``{{MACRO}}`` tags in
            ``.out.txt`` files and validate that none remain.
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
                zf.writestr(arcname, expanded)
            else:
                zf.write(f, arcname)


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
        default="./fixtures",
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

    args = parser.parse_args()

    fixtures_dir = Path(args.fixtures).resolve()
    outdir = Path(args.outdir).resolve()

    if not fixtures_dir.is_dir():
        print(f"Error: fixtures directory not found: {fixtures_dir}", file=sys.stderr)
        return 1

    outdir.mkdir(parents=True, exist_ok=True)

    input_files = collect_files(fixtures_dir, ".in.txt")
    output_files = collect_files(fixtures_dir, ".out.txt")

    if not input_files:
        print("Warning: No .in.txt files found.", file=sys.stderr)
    if not output_files:
        print("Warning: No .out.txt files found.", file=sys.stderr)

    input_zip = outdir / f"{args.epic}-{args.story}-Test-Input.zip"
    output_zip = outdir / f"{args.epic}-{args.story}-Test-Output.zip"

    expand = not args.no_expand

    # Input files never need macro expansion
    build_zip_with_expansion(
        input_files, input_zip, fixtures_dir, args.flat, expand=False
    )
    build_zip_with_expansion(
        output_files, output_zip, fixtures_dir, args.flat, expand=expand
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
