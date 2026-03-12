"""Pytest suite validating the output macro engine.

Run with::

    python3 -m pytest tests/test_macros.py -v
"""

from __future__ import annotations

from pathlib import Path

import pytest

from incollege_tests.macros import (
    expand_macros,
    has_macros,
    load_macros,
    validate_no_unexpanded,
)

MACRO_DEFS_PATH: Path = Path(__file__).resolve().parent / "macro_defs" / "menus.yml"


# ------------------------------------------------------------------
# Fixtures
# ------------------------------------------------------------------


@pytest.fixture(scope="session")
def macros() -> dict[str, str]:
    """Load macro definitions once for the test session."""
    return load_macros(MACRO_DEFS_PATH)


# ------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------


def test_load_macros_returns_all_keys(macros: dict[str, str]) -> None:
    """All expected macro keys are present after loading YAML."""
    expected_keys = {
        "WELCOME_BANNER",
        "LOGIN_SCREEN",
        "CREATE_ACCOUNT_HEADER",
        "PASSWORD_PROMPT",
        "ACCOUNT_CREATED",
        "LOGIN_HEADER",
        "LOGIN_SUCCESS",
        "MAIN_MENU",
        "SKILLS_MENU",
        "SKILL_UNDER_CONSTRUCTION",
        "JOB_MENU",
        "JOB_POST_HEADER",
        "JOB_POST_SUCCESS",
        "BROWSE_UNDER_CONSTRUCTION",
        "INVALID_CHOICE",
        "EXIT_MESSAGE",
        "ACCOUNT_LIMIT_REACHED",
        "PROFILE_CREATE_HEADER",
        "PROFILE_EDIT_HEADER",
        "PROFILE_VIEW_HEADER",
        "NO_PROFILE_MESSAGE",
        "PROFILE_SAVED",
        "SEARCH_PROMPT",
        "USER_NOT_FOUND",
        "PENDING_REQUESTS_HEADER",
        "NO_PENDING_REQUESTS",
        "MY_NETWORK_HEADER",
        "NO_CONNECTIONS",
        "PASSWORD_VALIDATION_ERROR",
    }
    assert expected_keys == set(macros.keys())


def test_expand_single_macro(macros: dict[str, str]) -> None:
    """A single ``{{MAIN_MENU}}`` line expands correctly."""
    text = "Some preamble\n{{MAIN_MENU}}\nSome epilogue"
    result = expand_macros(text, macros)
    assert "{{MAIN_MENU}}" not in result
    assert macros["MAIN_MENU"] in result
    assert result.startswith("Some preamble\n")
    assert result.endswith("\nSome epilogue")


def test_expand_multiple_macros(macros: dict[str, str]) -> None:
    """File with several macros expands all of them in order."""
    text = "{{WELCOME_BANNER}}\n{{LOGIN_SCREEN}}\n{{EXIT_MESSAGE}}"
    result = expand_macros(text, macros)
    assert "{{WELCOME_BANNER}}" not in result
    assert "{{LOGIN_SCREEN}}" not in result
    assert "{{EXIT_MESSAGE}}" not in result
    # Check ordering: WELCOME_BANNER text appears before LOGIN_SCREEN text
    wb_pos = result.index("WELCOME TO INCOLLEGE")
    ls_pos = result.index("Please select an option:")
    assert wb_pos < ls_pos


def test_no_macros_passthrough(macros: dict[str, str]) -> None:
    """Plain text without ``{{...}}`` returns unchanged."""
    text = "Hello world\nNo macros here\nJust plain text"
    result = expand_macros(text, macros)
    assert result == text


def test_unknown_macro_raises(macros: dict[str, str]) -> None:
    """``{{NONEXISTENT}}`` raises ``ValueError`` with helpful message."""
    text = "{{NONEXISTENT}}"
    with pytest.raises(ValueError, match="Unknown macro"):
        expand_macros(text, macros)


def test_macro_must_be_full_line(macros: dict[str, str]) -> None:
    """Inline ``some text {{MAIN_MENU}}`` is NOT expanded."""
    text = "some text {{MAIN_MENU}} more text"
    result = expand_macros(text, macros)
    assert result == text  # unchanged


def test_validate_no_unexpanded_clean(macros: dict[str, str]) -> None:
    """No error when text has no macro tags."""
    validate_no_unexpanded("Clean text with no macros")


def test_validate_no_unexpanded_catches_leftovers() -> None:
    """Catches leftover tags after expansion."""
    with pytest.raises(ValueError, match="Unexpanded macros"):
        validate_no_unexpanded("{{MAIN_MENU}}")


def test_yaml_trailing_newline_stripped(macros: dict[str, str]) -> None:
    """YAML ``|`` block values don't end with a trailing newline."""
    for key, value in macros.items():
        assert not value.endswith("\n"), (
            f"Macro '{key}' has trailing newline — "
            f"the YAML | block trailing newline should be stripped"
        )


def test_has_macros_positive() -> None:
    """``has_macros`` detects macro tags."""
    assert has_macros("line1\n{{MAIN_MENU}}\nline3")


def test_has_macros_negative() -> None:
    """``has_macros`` returns False for plain text."""
    assert not has_macros("no macros here\njust text")


def test_has_macros_inline_ignored() -> None:
    """``has_macros`` ignores inline usage."""
    assert not has_macros("text {{MAIN_MENU}} more text")
