"""Input file preprocessing: seed macros and comment stripping.

Header macros ``@seed_user``, ``@seed_connection``, and ``@seed_message`` are
removed before execution. Inline ``#`` comments are stripped from body lines.
"""

from __future__ import annotations

import re
import shlex
from pathlib import Path

from incollege_tests.constants import ACCOUNT_PASSWORD_WIDTH, ACCOUNT_USERNAME_WIDTH
from incollege_tests.models import (
    SeedConnectionMacro,
    SeedMessageMacro,
    SeedUserMacro,
)

# Fixtures use: sender=… recipient=… content="…" timestamp=YYYY-MM-DD hh:mm:ss
# (timestamp may be unquoted and contain spaces — shlex alone cannot parse that.)
_SEED_MESSAGE_BODY_RE = re.compile(
    r"^sender=(?P<sender>\S+)\s+recipient=(?P<recipient>\S+)\s+"
    r'content="(?P<content>[^"]*)"\s+timestamp=(?P<ts>.+)$'
)

_PASSWORD_SPECIAL_CHARS = "!@#$%^&*"


def preprocess_input_file(
    input_file: Path,
) -> tuple[
    str,
    list[SeedUserMacro],
    list[SeedConnectionMacro],
    list[SeedMessageMacro],
]:
    """Parse seed macros from the top of a test input file and return executable input.

    Macros must appear at the top of the file and are removed from the final
    ``INPUT.TXT``.  Inline ``#`` comments are stripped from body lines (use
    ``\\#`` for a literal ``#``).  Whole-line comments are removed entirely.

    Returns:
        ``(executable_input_text, seed_users, seed_connections, seed_messages)``.
    """
    lines = input_file.read_text().splitlines(keepends=True)
    seed_users: list[SeedUserMacro] = []
    seed_connections: list[SeedConnectionMacro] = []
    seed_messages: list[SeedMessageMacro] = []

    body_start = 0
    saw_any_macro = False

    for index, line in enumerate(lines):
        stripped = line.strip()
        directive = _strip_leading_hash_comment(stripped)

        if directive is not None:
            if directive.startswith("@seed_user"):
                seed_users.append(_parse_seed_user_macro(directive))
                body_start = index + 1
                saw_any_macro = True
                continue
            if directive.startswith("@seed_connection"):
                seed_connections.append(_parse_seed_connection_macro(directive))
                body_start = index + 1
                saw_any_macro = True
                continue
            if directive.startswith("@seed_message"):
                seed_messages.append(_parse_seed_message_macro(directive))
                body_start = index + 1
                saw_any_macro = True
                continue

        if saw_any_macro and (stripped == "" or stripped.startswith("#")):
            body_start = index + 1
            continue

        if not saw_any_macro and stripped == "":
            body_start = index + 1
            continue

        body_start = index
        break

    body_lines: list[str] = []
    for line in lines[body_start:]:
        processed = _strip_inline_comment(line)
        content = processed.rstrip("\n")
        if content == "" and line.strip().startswith("#"):
            continue
        body_lines.append(processed)

    executable_input = "".join(body_lines)
    return executable_input, seed_users, seed_connections, seed_messages


def _strip_leading_hash_comment(stripped: str) -> str | None:
    """Return ``@seed_*`` directive text, or ``None`` if not a seed line."""
    s = stripped
    if s.startswith("#"):
        s = s[1:].lstrip()
    if s.startswith("@seed_user"):
        return s
    if s.startswith("@seed_connection"):
        return s
    if s.startswith("@seed_message"):
        return s
    return None


def _parse_macro_key_values(macro_args: str) -> dict[str, str]:
    """Parse ``key=value`` tokens from a macro line."""
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
    """Convert a string to ``bool`` for macro parameters."""
    lowered = value.strip().lower()
    if lowered in {"1", "true", "yes", "y", "on"}:
        return True
    if lowered in {"0", "false", "no", "n", "off"}:
        return False
    raise ValueError(
        f"Invalid boolean value '{value}'. Use true/false, yes/no, or 1/0."
    )


def _validate_seed_password(password: str, username: str) -> None:
    """Validate password using the same policy as COBOL account creation.

    COBOL requires all of the following:
    - length 8-12 characters
    - at least one uppercase letter
    - at least one digit
    - at least one special character in ``!@#$%^&*``
    """
    trimmed = password.strip()

    if len(trimmed) < 8 or len(trimmed) > ACCOUNT_PASSWORD_WIDTH:
        raise ValueError(
            "@seed_user password for "
            f"'{username}' must be 8-{ACCOUNT_PASSWORD_WIDTH} characters."
        )

    has_upper = any("A" <= ch <= "Z" for ch in trimmed)
    has_digit = any("0" <= ch <= "9" for ch in trimmed)
    has_special = any(ch in _PASSWORD_SPECIAL_CHARS for ch in trimmed)

    if not (has_upper and has_digit and has_special):
        raise ValueError(
            "@seed_user password for "
            f"'{username}' must include at least one uppercase letter, "
            "one digit, and one special character from !@#$%^&*."
        )


def _parse_seed_user_macro(macro_line: str) -> SeedUserMacro:
    """Parse one ``@seed_user`` macro line into a :class:`SeedUserMacro`."""
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

    _validate_seed_password(password, username)

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


def _parse_seed_connection_macro(macro_line: str) -> SeedConnectionMacro:
    """Parse ``@seed_connection user_a=... user_b=...``."""
    stripped = macro_line.strip()
    if stripped.startswith("#"):
        stripped = stripped[1:].strip()

    parts = stripped.split(maxsplit=1)
    if len(parts) < 1 or parts[0].lower() != "@seed_connection":
        raise ValueError("Expected @seed_connection directive.")

    macro_args = parts[1] if len(parts) > 1 else ""
    parsed = _parse_macro_key_values(macro_args)
    user_a = parsed.get("user_a", "").strip()
    user_b = parsed.get("user_b", "").strip()
    if not user_a or not user_b:
        raise ValueError("@seed_connection requires user_a= and user_b=.")
    return SeedConnectionMacro(user_a=user_a, user_b=user_b)


def _parse_seed_message_macro(macro_line: str) -> SeedMessageMacro:
    """Parse ``@seed_message`` with double-quoted ``content`` and ``timestamp`` rest-of-line."""
    stripped = macro_line.strip()
    if stripped.startswith("#"):
        stripped = stripped[1:].strip()

    parts = stripped.split(maxsplit=1)
    if len(parts) < 1 or parts[0].lower() != "@seed_message":
        raise ValueError("Expected @seed_message directive.")

    rest = parts[1].strip() if len(parts) > 1 else ""
    m = _SEED_MESSAGE_BODY_RE.match(rest)
    if not m:
        raise ValueError(
            f"Could not parse @seed_message line. Expected: "
            f'sender=… recipient=… content="…" timestamp=…  Got: {rest!r}'
        )

    timestamp = m.group("ts").strip()

    return SeedMessageMacro(
        sender=m.group("sender").strip(),
        recipient=m.group("recipient").strip(),
        content=m.group("content"),
        timestamp=timestamp,
        msg_id=0,
    )


def _strip_inline_comment(line: str) -> str:
    """Strip an inline ``#`` comment from a line, preserving the line ending."""
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
