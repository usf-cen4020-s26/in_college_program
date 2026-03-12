#!/usr/bin/env python3
"""Package test fixtures — backward-compatibility wrapper.

This file delegates to ``incollege_tests.packaging``.  All logic now lives
in ``tests/incollege_tests/packaging.py``.
"""

from __future__ import annotations

import sys

from incollege_tests.packaging import main

if __name__ == "__main__":
    sys.exit(main())
