#!/usr/bin/env python3
"""Compatibility wrapper for interactive COBOL live replay CLI."""

from __future__ import annotations

import sys

import typer
from incollege_tests.live_cli import main

if __name__ == "__main__":
    sys.exit(typer.run(main))
