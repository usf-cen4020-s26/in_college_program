"""Pytest coverage for seed expansion in packaging."""

from __future__ import annotations

import zipfile
from pathlib import Path

import pytest

from incollege_tests.packaging import build_zip_with_expansion


def test_build_zip_with_expansion_prepends_connection_seed_prefix(
    tmp_path: Path,
) -> None:
    fixtures_dir = tmp_path / "fixtures"
    fixtures_dir.mkdir(parents=True)

    input_fixture = fixtures_dir / "connection_seed.in.txt"
    input_fixture.write_text(
        "@seed_user username=alice password=Alice1!1 first_name=Alice last_name=Smith university=USF major=CS grad_year=2027\n"
        "@seed_user username=bob password=Bob1Pass! first_name=Bob last_name=Jones university=USF major=IT grad_year=2027\n"
        "@seed_connection user_a=alice user_b=bob\n"
        "\n"
        "3\n"
    )

    zip_path = tmp_path / "inputs.zip"
    build_zip_with_expansion(
        files=[input_fixture],
        zip_path=zip_path,
        base_dir=fixtures_dir,
        expand=False,
    )

    with zipfile.ZipFile(zip_path, "r") as zf:
        packaged = zf.read("connection_seed.in.txt").decode()

    assert "@seed_user" not in packaged
    assert "@seed_connection" not in packaged
    assert "4\nBob Jones\n1\n9\n" in packaged
    assert "1\nbob\nBob1Pass!\n5\n1\n9\n" in packaged
    assert packaged.endswith("3\n")


def test_build_zip_with_expansion_rejects_unexpandable_connection_seed(
    tmp_path: Path,
) -> None:
    fixtures_dir = tmp_path / "fixtures"
    fixtures_dir.mkdir(parents=True)

    input_fixture = fixtures_dir / "bad_connection_seed.in.txt"
    input_fixture.write_text(
        "@seed_user username=alice password=Alice1!1 first_name=Alice last_name=Smith university=USF major=CS grad_year=2027\n"
        "@seed_user username=bob password=Bob1Pass! with_profile=false\n"
        "@seed_connection user_a=alice user_b=bob\n"
        "\n"
        "3\n"
    )

    zip_path = tmp_path / "inputs.zip"

    with pytest.raises(ValueError, match="Cannot expand @seed_connection"):
        build_zip_with_expansion(
            files=[input_fixture],
            zip_path=zip_path,
            base_dir=fixtures_dir,
            expand=False,
        )
