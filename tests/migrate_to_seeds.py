#!/usr/bin/env python3
"""Migrate test input/output files to use @seed_user directives.

Transforms test fixtures by:
1. Replacing manual account creation in input files with @seed_user directives
2. Removing corresponding account/profile creation output blocks from expected files

Usage::

    # Dry-run (show what would change):
    python3 tests/migrate_to_seeds.py --dry-run

    # Apply changes to a specific module:
    python3 tests/migrate_to_seeds.py --module profiles/search

    # Apply changes to all eligible modules:
    python3 tests/migrate_to_seeds.py
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from incollege_tests.macros import load_macros

# ---------------------------------------------------------------------------
# Directories to SKIP (test IS the creation/validation flow)
# ---------------------------------------------------------------------------
SKIP_DIRS = {
    "login",
    "eof_tests",
    "seeding",
    # Profile creation/editing tests
    "create_and_view",
    "edit_existing_profile",
    "basic_profile",
    "full_profile",
    "missing_required_fields",
    "invalid_graduation_year",
    "grad_year",
    "multiple_experiences",
    "no_experience_no_education",
    "profile_with_experience",
    "special_characters",
    # Note: special_сharacters uses cyrillic с
    "special_\u0441haracters",
    "with_education",
    "view_no_profile",
    "view_profile_long_text",
    "view_profile_no_optional_after_edit",
    "view_profile_persistence_across_logins",
    "separate_users_profiles",
}

FIXTURES_ROOT = Path(__file__).resolve().parent / "fixtures"


def _strip_comment(line: str) -> str:
    """Strip inline # comment from a line (same logic as preprocessing)."""
    result = []
    i = 0
    while i < len(line):
        if line[i] == "\\" and i + 1 < len(line) and line[i + 1] == "#":
            result.append("#")
            i += 2
        elif line[i] == "#":
            if i == 0 or line[i - 1] in (" ", "\t"):
                break
            result.append(line[i])
            i += 1
        else:
            result.append(line[i])
            i += 1
    return "".join(result).rstrip()


def _needs_quoting(value: str) -> bool:
    """Check if a seed value needs quoting."""
    return " " in value or '"' in value or "'" in value


def _quote(value: str) -> str:
    """Quote a value for @seed_user directive if needed."""
    if not value:
        return '""'
    if _needs_quoting(value):
        escaped = value.replace('"', '\\"')
        return f'"{escaped}"'
    return value


def find_test_cases(module_filter: str | None = None) -> list[dict]:
    """Discover test cases eligible for migration.

    Returns list of dicts with keys: name, input_path, output_path, module
    """
    cases = []

    for expected_dir in sorted(FIXTURES_ROOT.rglob("expected")):
        inputs_dir = expected_dir.parent / "inputs"
        if not inputs_dir.is_dir():
            continue

        # Determine module path relative to fixtures
        rel = expected_dir.parent.relative_to(FIXTURES_ROOT)
        parts = rel.parts

        # Check skip list against any part of the path
        if any(p in SKIP_DIRS for p in parts):
            continue

        if module_filter and not str(rel).startswith(module_filter):
            continue

        for out_file in sorted(expected_dir.glob("*.out.txt")):
            in_file = inputs_dir / out_file.name.replace(".out.txt", ".in.txt")
            if not in_file.exists():
                continue

            cases.append({
                "name": out_file.stem,
                "input_path": in_file,
                "output_path": out_file,
                "module": str(rel),
            })

    return cases


def parse_input_state_machine(raw_lines: list[str]) -> dict:
    """Parse input file using a state machine to identify creation sequences.

    Returns dict with:
        seeds: list of seed user data dicts
        clean_lines: remaining input lines (with original formatting)
        already_seeded: True if file already uses @seed_user
        has_complex_profile: True if profile has experience/education entries
    """
    # Check if already seeded
    for line in raw_lines:
        stripped = line.strip()
        if "@seed_user" in stripped:
            return {"already_seeded": True, "seeds": [], "clean_lines": raw_lines,
                    "has_complex_profile": False}

    # Strip comments for state tracking, but keep originals for line mapping
    content_lines = [_strip_comment(line) for line in raw_lines]

    state = "LOGIN_SCREEN"
    seeds: list[dict] = []
    current_user: str | None = None
    keep_mask = [True] * len(raw_lines)  # which lines to keep
    has_complex_profile = False
    i = 0

    while i < len(content_lines):
        cl = content_lines[i].strip()

        if state == "LOGIN_SCREEN":
            if cl == "2" and i + 2 < len(content_lines):
                # Account creation: 2, username, password
                username = content_lines[i + 1].strip()
                password = content_lines[i + 2].strip()
                seeds.append({
                    "username": username,
                    "password": password,
                    "with_profile": False,
                })
                keep_mask[i] = False
                keep_mask[i + 1] = False
                keep_mask[i + 2] = False
                i += 3
                # State stays LOGIN_SCREEN
                continue
            elif cl == "1" and i + 2 < len(content_lines):
                # Login: 1, username, password
                current_user = content_lines[i + 1].strip()
                # Keep these lines
                i += 3
                state = "MAIN_MENU"
                continue
            elif cl == "3":
                i += 1
                break
            else:
                i += 1
                continue

        elif state == "MAIN_MENU":
            if cl == "1" and i + 6 < len(content_lines):
                # Might be profile creation — check if user doesn't have profile yet
                seed = next(
                    (s for s in seeds if s["username"] == current_user and not s["with_profile"]),
                    None,
                )
                if seed is not None:
                    # Try to parse profile creation fields
                    first_name = content_lines[i + 1].strip()
                    last_name = content_lines[i + 2].strip()
                    university = content_lines[i + 3].strip()
                    major = content_lines[i + 4].strip()
                    grad_year_str = content_lines[i + 5].strip()

                    # Validate grad_year looks like a year
                    if not (grad_year_str.isdigit() and len(grad_year_str) == 4):
                        # Not profile creation, treat as regular menu choice
                        i += 1
                        continue

                    about_me = content_lines[i + 6].strip()
                    j = i + 7

                    # Experience: read until DONE
                    exp_entries = []
                    while j < len(content_lines):
                        el = content_lines[j].strip()
                        if el.upper() == "DONE":
                            j += 1
                            break
                        exp_entries.append(el)
                        j += 1

                    # Education: read until DONE
                    edu_entries = []
                    while j < len(content_lines):
                        el = content_lines[j].strip()
                        if el.upper() == "DONE":
                            j += 1
                            break
                        edu_entries.append(el)
                        j += 1

                    if exp_entries or edu_entries:
                        # Has experience/education — can't fully seed
                        has_complex_profile = True
                        i += 1
                        continue

                    # Simple profile — can seed it
                    seed["with_profile"] = True
                    seed["first_name"] = first_name
                    seed["last_name"] = last_name
                    seed["university"] = university
                    seed["major"] = major
                    seed["grad_year"] = grad_year_str
                    seed["about_me"] = about_me

                    # Mark all profile creation lines for removal
                    for k in range(i, j):
                        if k < len(keep_mask):
                            keep_mask[k] = False
                    i = j
                    # Stay in MAIN_MENU
                    continue
                else:
                    i += 1
                    continue
            elif cl == "8":
                state = "LOGIN_SCREEN"
                i += 1
                continue
            elif cl == "6":
                state = "SKILLS_MENU"
                i += 1
                continue
            elif cl == "3":
                state = "JOB_MENU"
                i += 1
                continue
            elif cl == "4":
                # Find someone — next line is name, then possibly connection choice
                i += 1
                state = "SEARCH_FLOW"
                continue
            elif cl == "5":
                state = "PENDING_REQUESTS"
                i += 1
                continue
            elif cl == "7":
                state = "MY_NETWORK"
                i += 1
                continue
            elif cl == "2":
                state = "VIEW_PROFILE"
                i += 1
                continue
            else:
                i += 1
                continue

        elif state == "SKILLS_MENU":
            if cl == "6":
                state = "MAIN_MENU"
            i += 1
            continue

        elif state == "JOB_MENU":
            if cl == "4":
                state = "MAIN_MENU"
            elif cl == "1":
                state = "JOB_POSTING"
            elif cl == "2":
                state = "JOB_BROWSING"
            elif cl == "3":
                state = "VIEW_APPS"
            i += 1
            continue

        elif state == "JOB_POSTING":
            # Job posting: title, description, employer, location, salary
            # After posting, back to JOB_MENU
            # We need to skip through the posting fields
            # The posting ends when we see a line that could be a menu choice
            # This is tricky — just advance and let the menu detection handle it
            # For simplicity, check if we return to job menu
            if cl in ("1", "2", "3", "4") and i + 1 < len(content_lines):
                next_cl = content_lines[i + 1].strip()
                # If next line looks like a job menu choice or field, stay in posting
                # When we see a choice that matches JOB_MENU, switch back
                pass
            # Just advance - the state machine doesn't need to be perfect
            # for non-creation states since we only care about account/profile creation
            i += 1
            # Heuristic: if we see a valid JOB_MENU choice pattern, switch state
            if cl == "4":
                state = "MAIN_MENU"
            elif cl in ("1", "2", "3"):
                state = "JOB_MENU"
            continue

        else:
            # For all other states, just advance
            # Return to main menu on "8" from any deep state
            # This is a simplification — we track rough state transitions
            if cl == "8" and state not in ("LOGIN_SCREEN",):
                state = "LOGIN_SCREEN"
            elif state in ("SEARCH_FLOW", "VIEW_PROFILE", "PENDING_REQUESTS",
                           "MY_NETWORK", "VIEW_APPS", "JOB_BROWSING"):
                # These states eventually return to MAIN_MENU
                # Check for main menu choices
                if cl in ("1", "2", "3", "4", "5", "6", "7", "8"):
                    # Could be back at main menu
                    if cl == "8":
                        state = "LOGIN_SCREEN"
                    else:
                        state = "MAIN_MENU"
                        continue  # re-process this line in MAIN_MENU state
            i += 1
            continue

    # Build clean lines
    clean_lines = [raw_lines[idx] for idx in range(len(raw_lines)) if keep_mask[idx]]

    return {
        "seeds": seeds,
        "clean_lines": clean_lines,
        "already_seeded": False,
        "has_complex_profile": has_complex_profile,
    }


def build_seed_directives(seeds: list[dict]) -> list[str]:
    """Build @seed_user directive strings from seed data."""
    directives = []
    for seed in seeds:
        parts = [
            f"username={_quote(seed['username'])}",
            f"password={_quote(seed['password'])}",
        ]
        if seed["with_profile"]:
            parts.append(f"first_name={_quote(seed['first_name'])}")
            parts.append(f"last_name={_quote(seed['last_name'])}")
            parts.append(f"university={_quote(seed['university'])}")
            parts.append(f"major={_quote(seed['major'])}")
            parts.append(f"grad_year={seed['grad_year']}")
            if seed.get("about_me"):
                parts.append(f"about_me={_quote(seed['about_me'])}")
        else:
            parts.append("with_profile=false")

        directives.append("@seed_user " + " ".join(parts))
    return directives


def transform_output(output_text: str, seeds: list[dict]) -> str:
    """Remove account creation and profile creation blocks from output.

    Uses macro markers to identify blocks precisely.
    """
    lines = output_text.split("\n")
    result = []
    i = 0

    while i < len(lines):
        line = lines[i].rstrip()

        # Account creation block: 2 -> CREATE_ACCOUNT_HEADER -> username -> PASSWORD_PROMPT -> ******** -> ACCOUNT_CREATED
        if (
            line == "2"
            and i + 1 < len(lines)
            and lines[i + 1].rstrip() == "{{CREATE_ACCOUNT_HEADER}}"
        ):
            # Verify this is an account creation we're seeding
            username_in_output = lines[i + 2].rstrip() if i + 2 < len(lines) else ""
            seeded_usernames = {s["username"] for s in seeds}
            if username_in_output in seeded_usernames:
                # Skip 6 lines: 2, CREATE_ACCOUNT_HEADER, username, PASSWORD_PROMPT, ********, ACCOUNT_CREATED
                i += 6
                # Skip the following LOGIN_SCREEN (return to menu after creation)
                if i < len(lines) and lines[i].rstrip() == "{{LOGIN_SCREEN}}":
                    i += 1
                continue

        # Profile creation block: 1 -> PROFILE_CREATE_HEADER -> fields -> PROFILE_SAVED
        if (
            line == "1"
            and i + 1 < len(lines)
            and lines[i + 1].rstrip() == "{{PROFILE_CREATE_HEADER}}"
        ):
            # Check if this profile belongs to a seeded user
            # Find the end of the profile block (PROFILE_SAVED)
            j = i + 2
            found_saved = False
            while j < len(lines):
                if lines[j].rstrip() == "{{PROFILE_SAVED}}":
                    found_saved = True
                    j += 1  # include PROFILE_SAVED in skip
                    break
                j += 1

            if found_saved:
                # Also skip the following MAIN_MENU (return to menu after save)
                if j < len(lines) and lines[j].rstrip() == "{{MAIN_MENU}}":
                    j += 1
                i = j
                continue

        result.append(lines[i])
        i += 1

    return "\n".join(result)


def migrate_test_case(
    input_path: Path, output_path: Path, dry_run: bool = False
) -> dict:
    """Migrate a single test case.

    Returns dict with status info.
    """
    raw_input = input_path.read_text()
    raw_output = output_path.read_text()
    raw_lines = raw_input.splitlines(keepends=True)

    # Parse input
    parsed = parse_input_state_machine(raw_lines)

    if parsed["already_seeded"]:
        return {"status": "skip", "reason": "already seeded"}

    if not parsed["seeds"]:
        return {"status": "skip", "reason": "no account creation found"}

    if parsed["has_complex_profile"]:
        return {"status": "skip", "reason": "has experience/education entries"}

    seeds = parsed["seeds"]
    clean_lines = parsed["clean_lines"]
    directives = build_seed_directives(seeds)

    # Build new input
    new_input_parts = []
    for d in directives:
        new_input_parts.append(d + "\n")
    new_input_parts.append("\n")
    new_input_parts.extend(clean_lines)
    new_input = "".join(new_input_parts)

    # Transform output
    new_output = transform_output(raw_output, seeds)

    if dry_run:
        changed = new_input != raw_input or new_output != raw_output
        return {
            "status": "would_modify" if changed else "no_change",
            "seeds": len(seeds),
            "profiles": sum(1 for s in seeds if s["with_profile"]),
        }

    # Write files
    input_path.write_text(new_input)
    output_path.write_text(new_output)

    return {
        "status": "modified",
        "seeds": len(seeds),
        "profiles": sum(1 for s in seeds if s["with_profile"]),
    }


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Migrate test files to use @seed_user directives."
    )
    parser.add_argument(
        "--module",
        type=str,
        default=None,
        help="Only migrate tests under this module path (e.g. 'profiles/search')",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would change without modifying files",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show details for each file",
    )
    args = parser.parse_args()

    cases = find_test_cases(args.module)
    if not cases:
        print(f"No eligible test cases found{f' for module {args.module}' if args.module else ''}.")
        return 0

    modified = 0
    skipped = 0
    errors = 0

    current_module = None
    for case in cases:
        if case["module"] != current_module:
            current_module = case["module"]
            print(f"\n=== {current_module} ===")

        try:
            result = migrate_test_case(
                case["input_path"], case["output_path"], dry_run=args.dry_run
            )
        except Exception as e:
            print(f"  ERROR: {case['name']}: {e}")
            errors += 1
            continue

        status = result["status"]
        if status in ("modified", "would_modify"):
            prefix = "[DRY-RUN] " if args.dry_run else ""
            profile_info = ""
            if result.get("profiles"):
                profile_info = f" ({result['profiles']} with profile)"
            print(f"  {prefix}Migrated: {case['name']} — {result['seeds']} seed(s){profile_info}")
            modified += 1
        elif status == "skip":
            if args.verbose:
                print(f"  Skipped: {case['name']} — {result['reason']}")
            skipped += 1
        elif status == "no_change":
            if args.verbose:
                print(f"  No change: {case['name']}")
            skipped += 1

    action = "Would modify" if args.dry_run else "Modified"
    print(f"\n{action}: {modified} | Skipped: {skipped} | Errors: {errors}")
    return 1 if errors > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
