"""Output macro expansion for ``.out.txt`` test files.

Macros are ``{{MACRO_NAME}}`` tags that appear as the sole content of a line
in expected-output files.  They expand to known menu blocks, banners, and
other repeated text defined in ``tests/macro_defs/menus.yml``.

Usage::

    from incollege_tests.macros import expand_macros

    expanded = expand_macros(raw_expected_text)
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any, Final, cast

import yaml

_MACRO_RE: Final[re.Pattern[str]] = re.compile(r"^\{\{([A-Z_][A-Z0-9_]*)\}\}$")

_DEFAULT_MACRO_PATH: Final[Path] = (
    Path(__file__).resolve().parent.parent / "macro_defs" / "menus.yml"
)


def load_macros(path: Path | None = None) -> dict[str, str]:
    """Load macro definitions from a YAML file.

    Each YAML key becomes a macro name.  Trailing newlines produced by YAML
    literal-block scalars (``|``) are stripped so that expansion does not
    inject extra blank lines.

    Args:
        path: Path to the YAML definitions file.  Defaults to
            ``tests/macro_defs/menus.yml``.

    Returns:
        Mapping of macro name to expansion text.

    Raises:
        FileNotFoundError: If the YAML file does not exist.
        ValueError: If the YAML file is malformed.
    """
    path = path or _DEFAULT_MACRO_PATH
    if not path.exists():
        raise FileNotFoundError(f"Macro definitions file not found: {path}")

    raw: object = yaml.safe_load(path.read_text())
    if not isinstance(raw, dict):
        raise ValueError(f"Expected a YAML mapping in {path}, got {type(raw).__name__}")

    raw_map = cast(dict[object, Any], raw)
    macros: dict[str, str] = {}
    for key, value in raw_map.items():
        if not isinstance(key, str):
            raise ValueError(
                f"Macro key must be a string, got {type(key).__name__}: {key}"
            )
        if not isinstance(value, str):
            raise ValueError(
                f"Macro value for '{key}' must be a string, got {type(value).__name__}"
            )
        # Strip trailing newline that YAML | blocks add
        macros[key] = value.rstrip("\n")
    return macros


def expand_macros(text: str, macros: dict[str, str] | None = None) -> str:
    """Replace all ``{{MACRO_NAME}}`` lines with their definitions.

    Each ``{{MACRO}}`` must be the **entire content** of its line
    (leading/trailing whitespace is ignored).  Inline usage like
    ``some text {{MACRO}}`` is intentionally **not** expanded.

    Args:
        text: Raw expected-output text potentially containing macro tags.
        macros: Pre-loaded macro mapping.  If ``None``, macros are loaded
            from the default YAML file.

    Returns:
        Text with all macro tags replaced by their expansions.

    Raises:
        ValueError: If an unknown macro name is encountered.
    """
    if macros is None:
        macros = load_macros()

    lines = text.split("\n")
    result: list[str] = []

    for line in lines:
        match = _MACRO_RE.match(line.strip())
        if match:
            name = match.group(1)
            if name not in macros:
                available = ", ".join(sorted(macros.keys()))
                raise ValueError(
                    f"Unknown macro '{{{{{{name}}}}}}'.  Available macros: {available}"
                )
            result.append(macros[name])
        else:
            result.append(line)

    return "\n".join(result)


def has_macros(text: str) -> bool:
    """Check whether *text* contains any ``{{MACRO}}`` tags.

    Args:
        text: Text to inspect.

    Returns:
        ``True`` if at least one macro tag is found on its own line.
    """
    for line in text.split("\n"):
        if _MACRO_RE.match(line.strip()):
            return True
    return False


def validate_no_unexpanded(text: str) -> None:
    """Raise ``ValueError`` if any ``{{MACRO}}`` tags remain in *text*.

    Intended as a post-expansion safety check (e.g. before writing to a zip
    archive).

    Args:
        text: Text to validate.

    Raises:
        ValueError: If unexpanded macro tags are found.
    """
    unexpanded: list[str] = []
    for line in text.split("\n"):
        match = _MACRO_RE.match(line.strip())
        if match:
            unexpanded.append(match.group(1))
    if unexpanded:
        names = ", ".join(unexpanded)
        raise ValueError(f"Unexpanded macros found after expansion: {names}")
