# Jira Tasks → Test Cases Mapping

This document maps Jira tasks to the automated test fixtures in `tests/fixtures/`. Each test is a folder with `inputs/*.in.txt` and `expected/*.out.txt` (project rule).

---

## Build automated input files for each scenario (view profile, found search, not found search)

| Jira task | Test(s) that satisfy it | Location |
|-----------|-------------------------|----------|
| **View profile** | `view_profile_long_text`, `view_profile_no_optional_after_edit`, `view_profile_persistence_across_logins`, `create_and_view`, `basic_profile`, `view_no_profile`, `full_profile`, `profile_with_experience`, `no_experience_no_education`, `with_education`, etc. | `tests/fixtures/profiles/` |
| **Found search** | `search_found_after_profile_edit` (part 2: "Alice Green" found), `search_exact_name_match` | `tests/fixtures/main_menu/user_search/`, `tests/fixtures/profiles/search/` |
| **Not found search** | `search_found_after_profile_edit` (part 2: "Alice Brown" not found), `search_nonexistent_user_message`, `search_partial_name_not_found` | `tests/fixtures/main_menu/user_search/`, `tests/fixtures/profiles/search/` |

---

## EOF test: end file right after the search prompt; verify graceful termination

| Jira task | Test that satisfies it | Location |
|-----------|------------------------|----------|
| **EOF after search prompt** | `eof_after_search_prompt` | `tests/fixtures/eof_tests/` |

- **Input:** Login, choose "Find someone you know" (4), then end of file (no name entered).
- **Expected:** Program shows "Enter the full name of the person you are looking for:" then exits with "Thank you for using InCollege!" (no crash).

---

## Other Jira tasks (not test-case tasks)

| Jira task | Type | Notes |
|-----------|------|------|
| Audit code paths: no direct DISPLAY for profile/search | **Code** | Programmers: ensure all output uses the shared output path (e.g. WS-OUTPUT-LINE + 8000-WRITE-OUTPUT). |
| Ensure all search-related lines use WS-OUTPUT-LINE + PERFORM 8000-WRITE-OUTPUT | **Code** | Same as above; no test case covers this. |
| Standardize whitespace (blank lines/separators) for deterministic output | **Code** | Affects program output; expected `.out.txt` files may need updates after code change. |
| Package deliverables | **Process** | Zip/release, README, etc. |

---

## Test folder names (no week/epic/additional)

All test folders use only the test name:

- `view_profile_long_text/`
- `view_profile_no_optional_after_edit/`
- `view_profile_persistence_across_logins/`
- `search_found_after_profile_edit` (under `main_menu/user_search/`)
- `eof_after_search_prompt` (under `eof_tests/`)

Input and expected files follow project rule: `inputs/<name>.in.txt`, `expected/<name>.out.txt` (with `_part_1` / `_part_2` for multi-part tests).
