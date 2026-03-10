"""Input file preprocessing: ``@seed_user`` macros and comment stripping.

Before a test input file is fed to the COBOL executable its header macros
(``@seed_user``) are parsed and removed, and inline ``#`` comments are
stripped from body lines.
"""
from __future__ import annotations

import shlex
from pathlib import Path

from incollege_tests.constants import ACCOUNT_PASSWORD_WIDTH, ACCOUNT_USERNAME_WIDTH
from incollege_tests.models import SeedUserMacro


def preprocess_input_file(input_file: Path) -> tuple[str, list[SeedUserMacro]]:
    """Parse seed macros from the top of a test input file and return executable input.

    Macros must appear at the top of the file and are removed from the final
    ``INPUT.TXT``.  Inline ``#`` comments are stripped from body lines (use
    ``\\#`` for a literal ``#``).  Whole-line comments are removed entirely.

    Args:
        input_file: Path to the ``.in.txt`` test fixture.

    Returns:
        A tuple of ``(executable_input_text, seed_users)``.
    """
    lines = input_file.read_text().splitlines(keepends=True)
    seed_users: list[SeedUserMacro] = []

    body_start = 0
    in_header = False

    for index, line in enumerate(lines):
        stripped = line.strip()

        is_seed_directive = (
            stripped.startswith("@seed_user")
            or stripped.startswith("# @seed_user")
            or stripped.startswith("#@seed_user")
        )

        if is_seed_directive:
            seed_users.append(_parse_seed_user_macro(stripped))
            body_start = index + 1
            in_header = True
            continue

        if in_header and (stripped == "" or stripped.startswith("#")):
            body_start = index + 1
            continue

        break

    # Strip inline comments from body lines and remove whole-line comments
    body_lines: list[str] = []
    for line in lines[body_start:]:
        processed = _strip_inline_comment(line)
        content = processed.rstrip("\n")
        if content == "" and line.strip().startswith("#"):
            continue
        body_lines.append(processed)

    executable_input = "".join(body_lines)
    return executable_input, seed_users


def _parse_macro_key_values(macro_args: str) -> dict[str, str]:
    """Parse ``key=value`` tokens from a macro line.

    Args:
        macro_args: The argument portion of a macro directive.

    Returns:
        Mapping of lowercase keys to their string values.

    Raises:
        ValueError: If a token is not in ``key=value`` format.
    """
    tokens = shlex.split(macro_args)
    parsed: dict[str, str] = {}

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
    """Convert a string to ``bool`` for macro parameters.

    Args:
        value: String value such as ``"true"``, ``"1"``, ``"yes"``.

    Returns:
        The boolean interpretation.

    Raises:
        ValueError: If the value is not a recognised boolean string.
    """
    lowered = value.strip().lower()
    if lowered in {"1", "true", "yes", "y", "on"}:
        return True
    if lowered in {"0", "false", "no", "n", "off"}:
        return False
    raise ValueError(
        f"Invalid boolean value '{value}'. Use true/false, yes/no, or 1/0."
    )


def _parse_seed_user_macro(macro_line: str) -> SeedUserMacro:
    """Parse one ``@seed_user`` macro line into a :class:`SeedUserMacro`.

    Args:
        macro_line: The raw (stripped) macro line text.

    Returns:
        A populated :class:`SeedUserMacro` instance.

    Raises:
        ValueError: On validation failures (missing fields, width overflow, etc.).
    """
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


def _strip_inline_comment(line: str) -> str:
    """Strip an inline ``#`` comment from a line, preserving the line ending.

    Rules:

    * ``#`` at column 0 or preceded by whitespace starts a comment.
    * ``\\#`` is an escape sequence that produces a literal ``#``.
    * Trailing whitespace between the content and the ``#`` is removed.

    Args:
        line: A single line of input text.

    Returns:
        The line with any inline comment removed.
    """
    ending = ""
    if line.endswith("\n"):
        ending = "\n"
        line = line[:-1]

    result: list[str] = []
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

    return "".join(result).rstrip() + ending
