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

Every COBOL program follows this structure:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. INCOLLEGE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "INPUT.TXT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-INPUT-STATUS.
    [... other files ...]

DATA DIVISION.
FILE SECTION.
    [... file record structures ...]

WORKING-STORAGE SECTION.
    [... variables and data structures ...]

PROCEDURE DIVISION.
    [... program logic ...]
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

**Reading from file:**
```cobol
PERFORM 8100-READ-INPUT.
IF WS-EOF-FLAG = 1
    MOVE 0 TO WS-PROGRAM-RUNNING
    EXIT PARAGRAPH
END-IF.
MOVE INPUT-RECORD TO WS-VARIABLE-NAME.
```

**Writing output (dual screen + file):**
```cobol
MOVE "Your message here" TO WS-OUTPUT-LINE.
PERFORM 8000-WRITE-OUTPUT.
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
PERFORM 8000-WRITE-OUTPUT.
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

**Main Program Flow:**
```
0000-MAIN-PROGRAM
├── 1000-INITIALIZE (load files)
├── 2000-PROCESS-APPLICATION (main menu loop)
│   ├── 3000-LOGIN-PROCESS
│   └── 4000-CREATE-ACCOUNT
└── 9000-TERMINATE (close files)
```

**Post-Login Menu:**
```
5000-POST-LOGIN-MENU
├── 6000-SKILLS-MENU
├── 7000-CREATE-EDIT-PROFILE
│   ├── 7200-GET-REQUIRED-FIELDS
│   ├── 7300-GET-OPTIONAL-FIELDS
│   └── 7400-SAVE-PROFILE-DATA
└── 7100-VIEW-PROFILE
    ├── 7110-DISPLAY-BASIC-INFO
    ├── 7120-DISPLAY-ABOUT-ME
    ├── 7130-DISPLAY-EXPERIENCE
    └── 7140-DISPLAY-EDUCATION
```

### 2. File Persistence

**Account Storage (`ACCOUNTS.DAT`):**
- Line sequential file
- Each line contains: username (20 chars) + password (12 chars)
- Loaded on initialization, written after account creation

**Profile Storage (`PROFILES.DAT`):**
- Line sequential file
- Complex record structure with nested arrays
- Loaded on initialization, written after profile creation/edit

**Loading Pattern:**
```cobol
1000-INITIALIZE.
    PERFORM 1100-LOAD-ACCOUNTS.
    PERFORM 1150-LOAD-PROFILES.

1100-LOAD-ACCOUNTS.
    OPEN INPUT ACCOUNTS-FILE.
    PERFORM 1110-READ-ACCOUNT-LOOP.
    CLOSE ACCOUNTS-FILE.
```

**Saving Pattern:**
```cobol
4600-WRITE-ACCOUNTS-FILE.
    OPEN OUTPUT ACCOUNTS-FILE.
    PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
        UNTIL WS-ACCOUNT-INDEX > WS-ACCOUNT-COUNT
        [... write each account ...]
    END-PERFORM.
    CLOSE ACCOUNTS-FILE.
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
PERFORM 8100-READ-INPUT.

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
8000-WRITE-OUTPUT.
    DISPLAY WS-OUTPUT-LINE.
    WRITE OUTPUT-RECORD FROM WS-OUTPUT-LINE.
```

**This means:**
- Every prompt is in the output file
- Every user input echo is in the output file
- Every message is in the output file
- Screen and file must be IDENTICAL

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
cobc -x -free -o bin/main src/main.cob

# Step 2: Run tests
./run_tests.sh

# Or run directly with Python
python3 tests/test_runner.py bin/main
```

**Why compilation is required:**
- Tests run the compiled binary in `bin/main`
- Changes to `src/main.cob` don't affect tests until recompiled
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
cobc -x -free -o bin/main src/main.cob

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
cobc -x -free -o bin/main src/main.cob

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
mkdir -p bin && cobc -x -free -o bin/main src/main.cob
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
cobc -x -free -o bin/main src/main.cob
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
