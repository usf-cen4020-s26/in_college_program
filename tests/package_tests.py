#!/usr/bin/env python3
"""
Package test fixture files into zip archives for submission.

Usage:
  python3 package_tests.py [OPTIONS]

Options:
  --epic NAME        Epic label for filenames (default: EpicX)
  --story NAME       Story label for filenames (default: StoryX)
  --fixtures DIR     Path to fixtures directory (default: ./fixtures)
  --outdir DIR       Directory to place zip files (default: current directory)
  --flat             Flatten directory structure inside the zip (just filenames)
  -h, --help         Show this help message
"""

import argparse
import sys
import zipfile
from pathlib import Path


def collect_files(fixtures_dir: Path, extension: str) -> list[Path]:
    """Recursively collect files matching the given extension."""
    files = sorted(fixtures_dir.rglob(f"*{extension}"))
    return files


def build_zip(files: list[Path], zip_path: Path, base_dir: Path, flat: bool):
    """Create a zip archive from the list of files."""
    with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as zf:
        for f in files:
            arcname = f.name if flat else str(f.relative_to(base_dir))
            zf.write(f, arcname)


def main():
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
    args = parser.parse_args()

    fixtures_dir = Path(args.fixtures).resolve()
    outdir = Path(args.outdir).resolve()

    if not fixtures_dir.is_dir():
        print(f"Error: fixtures directory not found: {fixtures_dir}", file=sys.stderr)
        sys.exit(1)

    outdir.mkdir(parents=True, exist_ok=True)

    # Collect files
    input_files = collect_files(fixtures_dir, ".in.txt")
    output_files = collect_files(fixtures_dir, ".out.txt")

    if not input_files:
        print("Warning: No .in.txt files found.", file=sys.stderr)
    if not output_files:
        print("Warning: No .out.txt files found.", file=sys.stderr)

    # Build zip names
    input_zip = outdir / f"{args.epic}-{args.story}-Test-Input.zip"
    output_zip = outdir / f"{args.epic}-{args.story}-Test-Output.zip"

    # Create zips
    build_zip(input_files, input_zip, fixtures_dir, args.flat)
    build_zip(output_files, output_zip, fixtures_dir, args.flat)

    # Summary
    print(f"Created {input_zip.name}")
    print(f"  → {len(input_files)} input file(s)  [{input_zip.stat().st_size / 1024:.1f} KB]")
    print(f"Created {output_zip.name}")
    print(f"  → {len(output_files)} output file(s) [{output_zip.stat().st_size / 1024:.1f} KB]")

    # List contents if verbose-ish
    print()
    print("── Input files ──")
    for f in input_files:
        print(f"  {f.relative_to(fixtures_dir)}")
    print()
    print("── Output files ──")
    for f in output_files:
        print(f"  {f.relative_to(fixtures_dir)}")


if __name__ == "__main__":
    main()
