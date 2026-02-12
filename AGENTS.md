# AGENTS.md - InCollege COBOL Project Guidelines

## Table of Contents
1. [Project Overview](#project-overview)
2. [COBOL Programming Standards](#cobol-programming-standards)
3. [Architecture & Design Patterns](#architecture--design-patterns)
4. [File I/O Requirements](#file-io-requirements)
5. [Testing System](#testing-system)
6. [Development Workflow](#development-workflow)
7. [Epic Requirements Summary](#epic-requirements-summary)
8. [Agent Response Guidelines](#agent-response-guidelines)

---

## Project Overview

InCollege is a career networking platform implemented in COBOL (GNU COBOL/GnuCOBOL). The application provides:
- User authentication (login and account creation)
- User profile management (creation, editing, viewing)
- Basic search functionality (find users by name)
- ... and more as it expands

### Key Constraints
- Maximum 5 user accounts
- All input is read from `INPUT.TXT`
- All output is written to both screen (stdout) and `OUTPUT.TXT`
- Data persists across program executions via `ACCOUNTS.DAT`, `PROFILES.DAT`, and others as the program expands.
- Uses free-format COBOL (`-free` compilation flag) and `-x` flag.

---

## COBOL Programming Standards

### 1. File Structure

Every COBOL program follows this structure (module/subprogram pattern):

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SOME-MODULE.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
    [... variables and data structures ...]

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION.
    [... program logic using LS-CONTEXT ...]
```

### 2. Naming Conventions

**Variables:**
- Working storage variables: `WS-VARIABLE-NAME`
- File descriptors: `FD-FILE-NAME`
- Account fields: `ACCT-FIELD-NAME`
- Profile fields: `PROF-FIELD-NAME`
- Temporary variables: `WS-TEMP-VARIABLE-NAME`

**Paragraphs (Functions):**
- Use numeric prefixes: `1000-INITIALIZE`, `2000-PROCESS-APPLICATION`
- Sub-paragraphs increment: `3100-ATTEMPT-LOGIN`, `3200-VALIDATE-LOGIN`
- Descriptive names in UPPER-KEBAB-CASE

**Example:**
```cobol
3000-LOGIN-PROCESS.
    PERFORM 3100-ATTEMPT-LOGIN.

3100-ATTEMPT-LOGIN.
    PERFORM 3200-VALIDATE-LOGIN.

3200-VALIDATE-LOGIN.
    [validation logic]
```

### 3. Data Structure Patterns

**Arrays (OCCURS clause):**
```cobol
01  WS-USER-ACCOUNTS.
    05  WS-ACCOUNT OCCURS 5 TIMES.
        10  WS-USERNAME         PIC X(20).
        10  WS-PASSWORD         PIC X(12).
```

**Nested arrays (2D structures):**
```cobol
01  WS-USER-PROFILES.
    05  WS-PROFILE OCCURS 5 TIMES.
        10  WS-EXPERIENCE OCCURS 3 TIMES.
            15  WS-EXP-TITLE    PIC X(50).
            15  WS-EXP-COMPANY  PIC X(50).
```

**Flags:**
```cobol
01  WS-LOGIN-SUCCESS            PIC 9 VALUE 0.
01  WS-EOF-FLAG                 PIC 9 VALUE 0.
01  WS-PROGRAM-RUNNING          PIC 9 VALUE 1.
```

### 4. Common COBOL Patterns

**Reading from file (centralized I/O):**
```cobol
CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
IF WS-EOF-FLAG = 1
    MOVE 0 TO WS-PROGRAM-RUNNING
    EXIT PARAGRAPH
END-IF.
MOVE WS-INPUT-LINE TO WS-VARIABLE-NAME.
```

**Writing output (dual screen + file):**
```cobol
MOVE "Your message here" TO WS-OUTPUT-LINE.
CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
```

**Looping with PERFORM VARYING:**
```cobol
PERFORM VARYING WS-INDEX FROM 1 BY 1
    UNTIL WS-INDEX > WS-COUNT
    [... loop body ...]
END-PERFORM.
```

**Conditional loops:**
```cobol
PERFORM UNTIL WS-CONDITION = 1
    [... loop body ...]
END-PERFORM.
```

**String concatenation:**
```cobol
MOVE SPACES TO WS-OUTPUT-LINE.
STRING "Name: "
    FUNCTION TRIM(WS-FIRST-NAME)
    " "
    FUNCTION TRIM(WS-LAST-NAME)
    DELIMITED BY SIZE INTO WS-OUTPUT-LINE
END-STRING.
CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
```

### 5. Input Validation

**Password requirements (Epic 1):**
- 8-12 characters
- At least one uppercase letter
- At least one digit (0-9)
- At least one special character (!@#$%^&*)

**Graduation year validation (Epic 2):**
- Must be exactly 4 digits
- Must be numeric
- Must be between 1950 and 2050

**Required vs Optional Fields:**
- Required fields: First Name, Last Name, University, Major, Graduation Year
- Optional fields: About Me, Experience (up to 3), Education (up to 3)
- Blank optional fields are accepted with "(skipped)" message

---

## Architecture & Design Patterns

### 1. Modular Design

The program is organized into logical sections with clear separation of concerns:

**Program Flow (multi-file):**
```
INCOLLEGE (src/main.cob)
├── CALL APP-INIT (src/app_init.cob)
├── 2000-PROCESS-APPLICATION (main menu loop)
│   ├── CALL LOGIN-MODULE (src/login_module.cob)
│   └── CALL ACCOUNT-MODULE (src/account_module.cob)
└── CALL APP-TERM (src/app_term.cob)
```

**Post-Login Menu (subprogram):**
```
POST-LOGIN-MENU (src/post_login_menu.cob)
├── CALL PROFILE-MODULE (create/edit) / PROFILE-VIEW (view)
├── CALL SEARCH-MODULE
├── CALL SKILLS-MENU
└── Job search placeholder message
```

**Shared Services:**
- `IO-SERVICE` (src/io_service.cob): Centralized input/output (INIT/READ/WRITE/CLOSE)
- `DATA-STORE` (src/data_store.cob): Load/save accounts and profiles (LOAD/SAVE-ACCT/SAVE-PROF)
- `PROFILE-DISPLAY` (src/profile_display.cob): Render profile sections for view/search
- `context.cpy` (src/copybooks/context.cpy): Shared working-storage context

IO-SERVICE opcodes are 5 characters: use `"INIT "`, `"READ "`, `"WRITE"`, `"CLOSE"`.

**File Layout (src/):**
- `src/main.cob`: Main menu loop only (dispatches to modules)
- `src/app_init.cob`: Startup initialization + welcome banner
- `src/app_term.cob`: Shutdown message + close IO
- `src/io_service.cob`: Input/output service (INPUT.TXT / OUTPUT.TXT)
- `src/data_store.cob`: Accounts/profiles load/save
- `src/login_module.cob`: Login flow
- `src/account_module.cob`: Account creation + password validation
- `src/post_login_menu.cob`: Post-login menu and routing
- `src/skills_menu.cob`: Skills submenu
- `src/profile_module.cob`: Create/edit profile; entry `PROFILE-VIEW` for viewing
- `src/profile_display.cob`: Profile rendering sections
- `src/search_module.cob`: “Find someone you know” search

### 2. File Persistence

**Account Storage (`ACCOUNTS.DAT`):**
- Line sequential file
- Each line contains: username (20 chars) + password (12 chars)
- Loaded on initialization, written after account creation

**Profile Storage (`PROFILES.DAT`):**
- Line sequential file
- Complex record structure with nested arrays
- Loaded on initialization, written after profile creation/edit

**Loading Pattern (DATA-STORE):**
```cobol
MOVE "LOAD" TO WS-DATA-ACTION.
CALL "DATA-STORE" USING WS-DATA-ACTION LS-CONTEXT.
```

**Saving Pattern (DATA-STORE):**
```cobol
MOVE "SAVE-ACCT" TO WS-DATA-ACTION.
CALL "DATA-STORE" USING WS-DATA-ACTION LS-CONTEXT.

MOVE "SAVE-PROF" TO WS-DATA-ACTION.
CALL "DATA-STORE" USING WS-DATA-ACTION LS-CONTEXT.
```

### 3. User Session Management

The program tracks the logged-in user using:
- `WS-CURRENT-USER-INDEX`: Index of logged-in user in accounts array
- `WS-CURRENT-PROFILE-INDEX`: Index of user's profile in profiles array
- `WS-PROFILE-FOUND`: Flag indicating if profile exists

**Linking accounts to profiles:**
```cobol
3210-FIND-USER-PROFILE.
    PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
        UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT
            OR WS-PROFILE-FOUND = 1
        IF WS-PROF-USERNAME(WS-ACCOUNT-INDEX) =
            WS-USERNAME(WS-CURRENT-USER-INDEX)
            MOVE WS-ACCOUNT-INDEX TO WS-CURRENT-PROFILE-INDEX
            MOVE 1 TO WS-PROFILE-FOUND
        END-IF
    END-PERFORM.
```

### 4. EOF (End-of-File) Handling

**Critical pattern for graceful termination:**
```cobol
CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.

IF WS-EOF-FLAG = 1
    MOVE 0 TO WS-PROGRAM-RUNNING
    EXIT PARAGRAPH
END-IF
```

**All input-reading sections MUST check EOF:**
- Prevents infinite loops
- Allows test runner to complete execution
- Enables automated testing

---

## File I/O Requirements

### 1. Dual Output Requirement

**All output must go to BOTH screen and file:**

```cobol
CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
```

**This means:**
- Every prompt is in the output file
- Every user input echo is in the output file
- Every message is in the output file
- Screen and file must be IDENTICAL

Use `CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.` for all output.

### 2. Input File Format

The `INPUT.TXT` file contains line-by-line user inputs:

```
1          # Choice: Login
testuser   # Username
Pass1!     # Password
2          # Choice: View Profile
6          # Choice: Logout
```

### 3. Output File Format

The `OUTPUT.TXT` file contains all program output:

```
========================================
     WELCOME TO INCOLLEGE
========================================

Please select an option:
1. Login with existing account
2. Create new account
3. Exit
Enter choice (1-3):
1
=== LOGIN ===
Enter username:
testuser
Enter password:
********
You have successfully logged in
...
```

---

## Testing System

### 1. Test Structure

Tests are located in `tests/fixtures/` with subdirectories for each feature:

```
tests/fixtures/
├── eof_tests/          # End-of-file handling
├── login/              # Login functionality
│   ├── 1_existing_account/
│   ├── 2_new_account/
│   └── login_operations/
├── main_menu/          # Post-login menu
├── profiles/           # Profile creation/viewing
└── ...
```

Each test directory contains:
- `inputs/*.in.txt` - Input files
- `expected/*.out.txt` - Expected output files

### 2. Running Tests

**⚠️ CRITICAL: Always compile before testing!**

```bash
# Step 1: Compile the COBOL program
mkdir -p bin
cobc -x -free -I src/copybooks -o bin/main src/*.cob

# Step 2: Run tests
./run_tests.sh

# Or run directly with Python
python3 tests/test_runner.py bin/main
```

**Why compilation is required:**
- Tests run the compiled binary in `bin/main`
- Changes to any `src/*.cob` or `src/copybooks/*.cpy` don't affect tests until recompiled
- Stale binaries produce incorrect test results

### 3. Test Types

**Single-Part Tests:**
- One input file, one output file
- Example: `valid_account.in.txt` → `valid_account.out.txt`

**Multi-Part Tests (Persistence):**
- Multiple executions with shared persistent storage
- Named with `_part_N` suffix
- Example:
  - `persistence_input_part_1.in.txt` - Create account
  - `persistence_input_part_2.in.txt` - Login with created account

**Test Runner Behavior:**
- Groups multi-part tests automatically
- Runs parts sequentially in same persistence context
- Clears `ACCOUNTS.DAT` and `PROFILES.DAT` between test groups
- Maintains storage across parts of same test

### 4. Test Output Interpretation

**✓ PASSED:**
```
✓ valid_account: PASSED
```

**✗ FAILED (with diff):**
```
✗ password_too_short: FAILED

  Differences found:
    --- password_too_short (expected)
    +++ password_too_short (actual)
    @@ -5,7 +5,7 @@
     Please enter your password:
     Pass1!
    -Password must be between 8 and 12 characters...
    +Invalid password
```

**⚠ ERROR:**
```
⚠ broken_test: ERROR
  Error: Test timed out after 10 seconds.
  Program may be waiting for input or in an infinite loop.
```

---

## Development Workflow

### 1. Before Making Changes

```bash
# 1. Ensure current code compiles
mkdir -p bin
cobc -x -free -I src/copybooks -o bin/main src/*.cob

# 2. Run existing tests to establish baseline
./run_tests.sh
```

### 2. Making Changes

1. Edit the `cob` source files.
2. Follow COBOL naming conventions
3. Add inline comments explaining logic
4. Handle EOF in all input sections

### 3. After Making Changes

```bash
# 1. Compile
cobc -x -free -I src/copybooks -o bin/main src/*.cob

# 2. Test manually (optional)
echo "3" > INPUT.TXT
./bin/main
cat OUTPUT.TXT

# 3. Run automated tests
./run_tests.sh

# 4. If tests fail, examine differences
./run_tests.sh --verbose

# 5. Update tests if behavior intentionally changed
```

---

## Agent Response Guidelines

### 1. What Constitutes an Acceptable Response

**For Code Generation:**
- Follow COBOL naming conventions strictly
- Include EOF handling in all input sections
- Maintain dual output (screen + file)
- Add inline comments with user story references
- Use modular paragraph structure (numbered prefixes)
- Validate all user input according to requirements

**For Code Changes:**
- Preserve existing structure and patterns
- Don't break existing functionality
- Update related paragraphs consistently
- Add new data structures in appropriate sections

**For Bug Fixes:**
- Identify root cause
- Fix at source, not symptom
- Update tests if behavior intentionally changes
- Recompile and test after fix

### 2. What is NOT Acceptable

**Do Not:**
- Change working code without reason
- Ignore EOF handling
- Break dual output (screen + file) requirement
- Skip input validation
- Use inconsistent naming
- Forget to compile before testing
- Modify test files to match broken code
- Remove error messages or validation

**Common Pitfalls to Avoid:**
- Missing `EXIT PARAGRAPH` after EOF check
- Not echoing input to output file
- Changing output format (breaks tests)
- Removing whitespace/newlines (breaks diff)
- Adding features without updating persistence

### 3. Testing Expectations

**Before Claiming Success:**
1. Code compiles without errors
2. All existing tests still pass
3. New functionality has tests
4. Manual testing confirms behavior
5. No infinite loops or hangs

**Test Failure Response:**
1. Examine diff output carefully
2. Identify whether:
   - Code is wrong (fix code)
   - Test is wrong (update test)
   - Specification changed (update both)
3. Never modify tests to pass without understanding why they failed

### 4. Communication Standards

**When Explaining Changes:**
- Reference specific epic/user story
- Identify affected paragraphs by number
- Explain why change was necessary
- List potential side effects
- Provide testing steps

**When Reporting Issues:**
- Describe expected vs actual behavior
- Provide specific line numbers
- Include relevant error messages
- Suggest fix if known

### 5. Documentation Requirements

**Inline Comments:**
```cobol
*> *      *>*****************************************************************
*> *      *> 3000-LOGIN-PROCESS: User Login Handler                        *
*> *      *> USER STORY 2: Login functionality                             *
*> *      *> TASK 4: Unlimited login attempts                              *
*> *      *>*****************************************************************
```

**User-Facing Messages:**
- Clear and concise
- Match specification exactly
- Consistent tone
- No technical jargon

---

## Quick Reference

### Compile Command
```bash
mkdir -p bin && cobc -x -free -I src/copybooks -o bin/main src/main.cob src/app_init.cob src/app_term.cob src/io_service.cob src/data_store.cob src/login_module.cob src/account_module.cob src/post_login_menu.cob src/skills_menu.cob src/profile_module.cob src/profile_display.cob src/search_module.cob
```

**Alternative (Dynamic discovery - useful for CI/CD):**
```bash
mkdir -p bin && OTHER_SOURCES=$(find src -name "*.cob" -not -name "main.cob" | sort | tr '\n' ' ') && cobc -x -free -I src/copybooks -o bin/main src/main.cob $OTHER_SOURCES
```

### Run Tests
```bash
# After compiling!
./run_tests.sh
```

### Verbose Testing
```bash
./run_tests.sh --verbose
```

### Test Specific Category
```bash
python3 tests/test_runner.py bin/main --test-root tests/fixtures/login
```

### Manual Test
```bash
cp tests/fixtures/path/to/test.in.txt INPUT.TXT
./bin/main
cat OUTPUT.TXT
```

### Clean Build
```bash
rm -f bin/main ACCOUNTS.DAT PROFILES.DAT INPUT.TXT OUTPUT.TXT
mkdir -p bin
cobc -x -free -I src/copybooks -o bin/main src/main.cob src/app_init.cob src/app_term.cob src/io_service.cob src/data_store.cob src/login_module.cob src/account_module.cob src/post_login_menu.cob src/skills_menu.cob src/profile_module.cob src/profile_display.cob src/search_module.cob
```

---

## Summary

The InCollege COBOL project requires:
1. **Strict adherence to COBOL conventions** - naming, structure, formatting
2. **Comprehensive EOF handling** - every input must check for end-of-file
3. **Dual output** - all output to both screen and `OUTPUT.TXT`
4. **Data persistence** - accounts and profiles saved across executions
5. **Input validation** - passwords, years, required fields
6. **Modular design** - numbered paragraphs, clear separation of concerns
7. **Compile before testing** - stale binaries produce incorrect results
8. **Test-driven development** - tests define correct behavior

**Remember:** The test suite is the source of truth. If tests pass, the implementation is correct. If tests fail, either the code or tests need updating based on the specification.

For detailed information on specific topics, refer to the linked sections in the table of contents.
