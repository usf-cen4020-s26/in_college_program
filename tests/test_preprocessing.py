"""Pytest suite validating input preprocessing and seed-user checks."""

from __future__ import annotations

from pathlib import Path

import pytest

from incollege_tests.preprocessing import preprocess_input_file


def _write_fixture(tmp_path: Path, content: str) -> Path:
    path = tmp_path / "fixture.in.txt"
    path.write_text(content)
    return path


def test_seed_user_valid_password_passes(tmp_path: Path) -> None:
    fixture = _write_fixture(
        tmp_path,
        "@seed_user username=alice password=Alice12! with_profile=false\n"
        "1\n"
        "alice\n"
        "Alice12!\n",
    )

    input_text, seed_users, seed_connections, seed_messages = preprocess_input_file(
        fixture
    )

    assert seed_connections == []
    assert seed_messages == []
    assert len(seed_users) == 1
    assert seed_users[0].username == "alice"
    assert seed_users[0].password == "Alice12!"
    assert input_text == "1\nalice\nAlice12!\n"


def test_seed_user_short_password_rejected(tmp_path: Path) -> None:
    fixture = _write_fixture(
        tmp_path,
        "@seed_user username=alice password=Alice1! with_profile=false\n1\n",
    )

    with pytest.raises(ValueError, match="must be 8-12 characters"):
        preprocess_input_file(fixture)


def test_seed_user_missing_required_character_classes_rejected(tmp_path: Path) -> None:
    fixture = _write_fixture(
        tmp_path,
        "@seed_user username=alice password=alice123! with_profile=false\n1\n",
    )

    with pytest.raises(
        ValueError,
        match="must include at least one uppercase letter, one digit, and one special character",
    ):
        preprocess_input_file(fixture)


def test_seed_user_invalid_special_character_rejected(tmp_path: Path) -> None:
    fixture = _write_fixture(
        tmp_path,
        "@seed_user username=alice password=Alice12+ with_profile=false\n1\n",
    )

    with pytest.raises(
        ValueError,
        match=r"special character from !@#\$%\^&\*",
    ):
        preprocess_input_file(fixture)
