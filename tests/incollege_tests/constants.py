"""Fixed-width field widths and limits for InCollege COBOL data files.

All constants correspond to the COBOL record layouts used by the InCollege
program for ACCOUNTS.DAT and PROFILES.DAT persistence files.
"""
from __future__ import annotations

from typing import Final

# ---------------------------------------------------------------------------
# ACCOUNTS.DAT record layout
# ---------------------------------------------------------------------------
ACCOUNT_USERNAME_WIDTH: Final[int] = 20
ACCOUNT_PASSWORD_WIDTH: Final[int] = 12
ACCOUNT_RECORD_WIDTH: Final[int] = ACCOUNT_USERNAME_WIDTH + ACCOUNT_PASSWORD_WIDTH

# ---------------------------------------------------------------------------
# PROFILES.DAT field widths
# ---------------------------------------------------------------------------
PROFILE_USERNAME_WIDTH: Final[int] = 20
PROFILE_HAS_PROFILE_WIDTH: Final[int] = 1
PROFILE_FIRST_NAME_WIDTH: Final[int] = 30
PROFILE_LAST_NAME_WIDTH: Final[int] = 30
PROFILE_UNIVERSITY_WIDTH: Final[int] = 50
PROFILE_MAJOR_WIDTH: Final[int] = 50
PROFILE_GRAD_YEAR_WIDTH: Final[int] = 4
PROFILE_ABOUT_ME_WIDTH: Final[int] = 200
PROFILE_COUNT_WIDTH: Final[int] = 1

# Experience sub-record
PROFILE_EXP_TITLE_WIDTH: Final[int] = 50
PROFILE_EXP_COMPANY_WIDTH: Final[int] = 50
PROFILE_EXP_DATES_WIDTH: Final[int] = 30
PROFILE_EXP_DESC_WIDTH: Final[int] = 100
PROFILE_EXP_ENTRY_WIDTH: Final[int] = (
    PROFILE_EXP_TITLE_WIDTH
    + PROFILE_EXP_COMPANY_WIDTH
    + PROFILE_EXP_DATES_WIDTH
    + PROFILE_EXP_DESC_WIDTH
)
PROFILE_EXP_MAX: Final[int] = 3

# Education sub-record
PROFILE_EDU_DEGREE_WIDTH: Final[int] = 50
PROFILE_EDU_UNIVERSITY_WIDTH: Final[int] = 50
PROFILE_EDU_YEARS_WIDTH: Final[int] = 20
PROFILE_EDU_ENTRY_WIDTH: Final[int] = (
    PROFILE_EDU_DEGREE_WIDTH + PROFILE_EDU_UNIVERSITY_WIDTH + PROFILE_EDU_YEARS_WIDTH
)
PROFILE_EDU_MAX: Final[int] = 3

# Full profile record width
PROFILE_RECORD_WIDTH: Final[int] = (
    PROFILE_USERNAME_WIDTH
    + PROFILE_HAS_PROFILE_WIDTH
    + PROFILE_FIRST_NAME_WIDTH
    + PROFILE_LAST_NAME_WIDTH
    + PROFILE_UNIVERSITY_WIDTH
    + PROFILE_MAJOR_WIDTH
    + PROFILE_GRAD_YEAR_WIDTH
    + PROFILE_ABOUT_ME_WIDTH
    + PROFILE_COUNT_WIDTH
    + (PROFILE_EXP_ENTRY_WIDTH * PROFILE_EXP_MAX)
    + PROFILE_COUNT_WIDTH
    + (PROFILE_EDU_ENTRY_WIDTH * PROFILE_EDU_MAX)
)

# ---------------------------------------------------------------------------
# Job posting field widths
# ---------------------------------------------------------------------------
JOB_TITLE_WIDTH: Final[int] = 50
JOB_DESC_WIDTH: Final[int] = 250
JOB_EMPLOYER_WIDTH: Final[int] = 50
JOB_LOCATION_WIDTH: Final[int] = 50
JOB_SALARY_WIDTH: Final[int] = 20
JOB_POSTER_WIDTH: Final[int] = 20
JOB_RECORD_WIDTH: Final[int] = (
    JOB_TITLE_WIDTH
    + JOB_DESC_WIDTH
    + JOB_EMPLOYER_WIDTH
    + JOB_LOCATION_WIDTH
    + JOB_SALARY_WIDTH
    + JOB_POSTER_WIDTH
)
JOB_MAX: Final[int] = 5
