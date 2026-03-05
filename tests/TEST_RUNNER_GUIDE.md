# Test Runner Documentation

## Overview

The InCollege test infrastructure provides automated testing for the COBOL application with support for:

- Single-part tests (one execution)
- Multi-part tests (multiple executions with persistent state)
- Detailed diff reporting
- CI/CD integration

## Quick Start

### Run All Tests

The easiest way to run all tests is using the provided shell script:

```bash
# Simple run - runs all test categories (login, main_menu, etc.)
./run_tests.sh

# With verbose output - shows full expected/actual outputs
./run_tests.sh --verbose

# Generate JSON report
./run_tests.sh --report
```

The script will:

1. Compile the COBOL program if needed
2. Discover and run all test categories in `tests/fixtures/`
3. Display results for each category separately
4. Show an overall summary

### Run Tests Manually

You can also run tests directly with Python:

```bash
# Run all tests
python3 tests/test_runner.py bin/main

# Run tests for a specific category
python3 tests/test_runner.py bin/main --test-root tests/fixtures/login

# Run with options
python3 tests/test_runner.py bin/main --verbose --report test-report.json
```

### Test Files

Each test consists of two files:

1. **Input file** (`inputs/test_name.in.txt`): Contains the user inputs
2. **Expected output file** (`expected/test_name.out.txt`): Contains the expected program output

## Seed Macros (Pre-Made Users)

You can define test setup macros at the top of an input file to pre-create users before the test runs.
This lets test inputs focus on the behavior being tested instead of repeating account/profile setup flows.

### Macro Format

Use one `@seed_user` directive per user at the top of `*.in.txt`:

```text
@seed_user username=alice password=Alice1! first_name=Alice last_name=Smith university=USF major=CS grad_year=2027
@seed_user username=bob password=BobPass1! first_name=Bob last_name=Jones university=UF major=Math grad_year=2026 about_me="Enjoys systems programming"

1
alice
Alice1!
8
```

You can also write directives as comments:

```text
# @seed_user username=alice password=Alice1! first_name=Alice last_name=Smith university=USF major=CS grad_year=2027
```

### Supported Parameters

- `username` (required, max 20 chars)
- `password` (required, max 12 chars)
- `with_profile` (optional, default `true`)
- `first_name` (required when `with_profile=true`)
- `last_name` (required when `with_profile=true`)
- `university` (required when `with_profile=true`)
- `major` (required when `with_profile=true`)
- `grad_year` (required when `with_profile=true`, must be 4 digits)
- `about_me` (optional)

### Behavior Notes

- Directives are only parsed at the **top of the input file**.
- Macro lines are removed before the test input is sent to the COBOL executable.
- Seeded users are persisted in `ACCOUNTS.DAT` and (if profile enabled) `PROFILES.DAT` before each test part runs.
- Existing seeded usernames are updated (upsert behavior).
- Seeding still respects the 5-account limit and will fail with a clear error if exceeded.

### 10 Blank Template Users (Copy/Paste)

Use these as blank templates. Replace placeholder values inside `<...>` with your test data.

```text
@seed_user username=<USER01> password=<PASS01> first_name=<FIRST01> last_name=<LAST01> university=<UNIV01> major=<MAJOR01> grad_year=<YEAR01> about_me="<ABOUT01>"
@seed_user username=<USER02> password=<PASS02> first_name=<FIRST02> last_name=<LAST02> university=<UNIV02> major=<MAJOR02> grad_year=<YEAR02> about_me="<ABOUT02>"
@seed_user username=<USER03> password=<PASS03> first_name=<FIRST03> last_name=<LAST03> university=<UNIV03> major=<MAJOR03> grad_year=<YEAR03> about_me="<ABOUT03>"
@seed_user username=<USER04> password=<PASS04> first_name=<FIRST04> last_name=<LAST04> university=<UNIV04> major=<MAJOR04> grad_year=<YEAR04> about_me="<ABOUT04>"
@seed_user username=<USER05> password=<PASS05> first_name=<FIRST05> last_name=<LAST05> university=<UNIV05> major=<MAJOR05> grad_year=<YEAR05> about_me="<ABOUT05>"
@seed_user username=<USER06> password=<PASS06> first_name=<FIRST06> last_name=<LAST06> university=<UNIV06> major=<MAJOR06> grad_year=<YEAR06> about_me="<ABOUT06>"
@seed_user username=<USER07> password=<PASS07> first_name=<FIRST07> last_name=<LAST07> university=<UNIV07> major=<MAJOR07> grad_year=<YEAR07> about_me="<ABOUT07>"
@seed_user username=<USER08> password=<PASS08> first_name=<FIRST08> last_name=<LAST08> university=<UNIV08> major=<MAJOR08> grad_year=<YEAR08> about_me="<ABOUT08>"
@seed_user username=<USER09> password=<PASS09> first_name=<FIRST09> last_name=<LAST09> university=<UNIV09> major=<MAJOR09> grad_year=<YEAR09> about_me="<ABOUT09>"
@seed_user username=<USER10> password=<PASS10> first_name=<FIRST10> last_name=<LAST10> university=<UNIV10> major=<MAJOR10> grad_year=<YEAR10> about_me="<ABOUT10>"
```

Notes:

- You can keep this template in a scratch file and copy only the users you need.
- Because the app allows max 5 accounts, only seed up to 5 distinct usernames in one persistence context.
- Keep `username` <= 20 chars and `password` <= 12 chars.

### Macro Usage Examples in Real Tests

#### Example A: Login test with pre-seeded account

```text
@seed_user username=alice password=Alice1! first_name=Alice last_name=Smith university=USF major=CS grad_year=2027

1
alice
Alice1!
8
```

#### Example B: Search test with two discoverable profiles

```text
@seed_user username=anna1 password=AnnaPass1! first_name=Anna last_name=Park university=USF major=IT grad_year=2026
@seed_user username=brian1 password=Brian1! first_name=Brian last_name=Cook university=UF major=Math grad_year=2025

1
anna1
AnnaPass1!
4
Brian Cook
2
8
```

#### Example C: User without profile (force "create profile first" flows)

```text
@seed_user username=justacct password=Acct1! with_profile=false

1
justacct
Acct1!
2
8
```

#### Example D: Multi-part test where only part 1 seeds data

`connection_flow_part_1.in.txt`

```text
@seed_user username=req1 password=ReqPass1! first_name=Request last_name=Sender university=USF major=CS grad_year=2027
@seed_user username=rec1 password=RecPass1! first_name=Request last_name=Receiver university=USF major=CS grad_year=2027

1
req1
ReqPass1!
...
8
```

`connection_flow_part_2.in.txt`

```text
1
rec1
RecPass1!
...
8
```

#### Example E: Upsert/update seeded user in a later part

`profile_refresh_part_1.in.txt`

```text
@seed_user username=sam1 password=SamPass1! first_name=Sam last_name=Lee university=USF major=CS grad_year=2026 about_me="Initial bio"

1
sam1
SamPass1!
8
```

`profile_refresh_part_2.in.txt`

```text
@seed_user username=sam1 password=SamPass1! first_name=Sam last_name=Lee university=USF major=CS grad_year=2026 about_me="Updated bio"

1
sam1
SamPass1!
2
8
```

## Inline Comments

You can add `#` comments to input files to document what each line does. Comments are stripped before the input is fed to the program.

### Syntax

```text
@seed_user username=alice password=Alice1! first_name=Alice last_name=Smith university=USF major=CS grad_year=2027

1        # Log in
alice    # Username
Alice1!  # Password
4        # Search for user
Bob Jones # Search query
8        # Log out
```

- `#` preceded by whitespace (or at column 0) starts a comment — everything after it is removed.
- Whole-line comments (lines with only a comment) are removed entirely.
- Use `\#` to produce a literal `#` in the input if needed.

### VS Code Highlighting

The `.vscode/settings.json` maps `*.in.txt` to `shellscript`, so `#` comments render with your theme's comment color automatically.

## Multi-Part Tests

### What Are Multi-Part Tests?

Multi-part tests verify persistence across multiple program executions. They test that data saved in one run is available in subsequent runs.

### Naming Convention

Multi-part test files end with `_part_N` where N is the part number:

- `test_name_part_1.in.txt` → First execution
- `test_name_part_2.in.txt` → Second execution
- `test_name_part_3.in.txt` → Third execution (if needed)

### How They Work

1. **Part 1** executes with `test_name_part_1.in.txt`
   - Program saves data to persistent storage
   - Output compared to `test_name_part_1.out.txt`

2. **Part 2** executes with `test_name_part_2.in.txt`
   - Program reads previously saved data
   - Output compared to `test_name_part_2.out.txt`

3. Parts execute in order within the same persistence context

### Example: Persistence Test

**Part 1** (`persistence_input_part_1.in.txt`):

```
1          # Create new account
PersistUser1
Persist1!
4          # Logout
```

**Part 2** (`persistence_input_part_2.in.txt`):

```
2          # Log in (account should exist from Part 1)
PersistUser1
Persist1!
4          # Logout
```

## Test Runner Usage

### Command Line Options

**For run_tests.sh:**

```bash
./run_tests.sh [options]
```

**Options:**

- `--verbose`, `-v`: Print detailed output including full expected/actual outputs
- `--report`, `-r`: Generate JSON report at `test-report.json`

**For direct Python usage:**

```bash
python3 tests/test_runner.py <executable> [options]
```

**Arguments:**

- `executable`: Path to compiled COBOL program (e.g., `bin/main`)

**Options:**

- `--test-root PATH`: Root directory for tests. Can be `tests/fixtures` for all tests or a specific category like `tests/fixtures/login`
- `--input-file PATH`: Run exactly one input fixture file (`*.in.txt`)
- `--expected-file PATH`: Expected output for `--input-file` mode (optional, auto-derived if omitted)
- `--verbose`, `-v`: Print detailed output including full expected/actual outputs
- `--report PATH`: Generate JSON report at specified path
- `--timeout SECONDS`: Maximum execution time per test (default: 10)
- `--dump-output PATH`: Write actual outputs to PATH as `*.actual.out.txt`
- `--dump-only`: Skip expected-output diff checks and only execute + dump outputs

### Examples

```bash
# Run all tests using the shell script (recommended)
./run_tests.sh

# Run all tests with verbose output
./run_tests.sh --verbose

# Run all tests directly with Python
python3 tests/test_runner.py bin/main

# Run only login tests
python3 tests/test_runner.py bin/main --test-root tests/fixtures/login

# Run only main menu tests with verbose output
python3 tests/test_runner.py bin/main --test-root tests/fixtures/main_menu --verbose

# Generate JSON report
python3 tests/test_runner.py bin/main --report results.json

# Dump actual outputs while still comparing diffs
python3 tests/test_runner.py bin/main --test-root tests/fixtures/profiles --dump-output test-dumps

# Dump-only mode (no diff checking), useful when creating new expected outputs
python3 tests/test_runner.py bin/main --test-root tests/fixtures/seeding --dump-only --dump-output test-dumps

# Run exactly one fixture (auto-derive expected path)
python3 tests/test_runner.py bin/main --input-file tests/fixtures/login/1_existing_account/inputs/successful_login.in.txt

# Run exactly one fixture without expected file and dump actual output
python3 tests/test_runner.py bin/main --input-file tests/fixtures/profiles/accept_reject_connection_request/inputs/reject_multiple_requests_part_1.in.txt --dump-only --dump-output test-dumps
```

### Dump Output Behavior

- Dump files preserve fixture structure relative to `--test-root`.
- Input path segment `inputs/` is replaced with `actual/` in the dump folder.
- Output names use `.actual.out.txt` suffix.

Example:

- Input: `tests/fixtures/profiles/search/inputs/find_user.in.txt`
- Dump: `test-dumps/profiles/search/actual/find_user.actual.out.txt`

### Exit Codes

- `0`: All tests passed
- `1`: One or more tests failed or had errors

## Understanding Test Output

### Success Example

```
✓ valid_account: PASSED
```

### Failure Example

```
✗ password_too_short: FAILED

  Differences found:
    --- password_too_short (expected)
    +++ password_too_short (actual)
    @@ -5,7 +5,7 @@
     Please enter your password:
     Pass1!
    -Password must be between 8 and 12 characters, contain at least one capital letter, one digit, and one special character.
    +Invalid password
```

**Color Coding:**

- **Green lines** (starting with `+`): Present in actual but not expected
- **Red lines** (starting with `-`): Present in expected but not actual
- **Blue lines** (starting with `@@`): Line number context

### Error Example

```
⚠ broken_test: ERROR
  Error: [Errno 2] No such file or directory: 'expected/broken_test.out.txt'
```

### Summary Report

```
======================================================================
Test Summary
======================================================================

Total Tests: 16
Passed: ✅ 14 (87.5%)
Failed: ❌ 2 (12.5%)
Errors: ⚠️ 0 (0.0%)

======================================================================
```

## Adding New Tests

### Step 1: Create Input File

Create a file in the appropriate `inputs/` directory:

```bash
# For new account tests
nano tests/fixtures/login/2_new_account/inputs/my_new_test.in.txt
```

Add the test inputs:

```
2
TestUser
TestPass1!
4
```

### Step 2: Run the Program Manually

Execute the program with your input to capture the expected output. Note that the COBOL program reads from and writes to `INPUT.TXT` and `OUTPUT.TXT`, so you need to:

```bash
# Copy your test input to INPUT.TXT
cp tests/fixtures/login/2_new_account/inputs/my_new_test.in.txt INPUT.TXT

# Run the program
./bin/main

# Copy the output to your expected output file
cp OUTPUT.TXT tests/fixtures/login/2_new_account/expected/my_new_test.out.txt

# Clean up
rm INPUT.TXT OUTPUT.TXT
```

Alternatively, let the test fail first to see the actual output, then create the expected file if the output looks correct.

### Step 3: Verify the Test

Run the tests to ensure your new test works:

```bash
# Run all tests
./run_tests.sh

# Or run just the specific category
python3 tests/test_runner.py bin/main --test-root tests/fixtures/login
```

### Adding Multi-Part Tests

For tests requiring persistence:

1. Create `test_name_part_1.in.txt` and `test_name_part_1.out.txt`
2. Create `test_name_part_2.in.txt` and `test_name_part_2.out.txt`
3. The test runner automatically groups and runs them in order

## CI/CD Integration

The test runner integrates with GitHub Actions for automated testing.

### Workflow Triggers

- Push to `main` or `develop` branches
- Pull requests to `main` or `develop`
- Manual workflow dispatch

### Workflow Steps

1. **Checkout**: Gets the repository code
2. **Install COBOL**: Installs GNU COBOL compiler
3. **Setup Python**: Installs Python 3.11
4. **Compile**: Builds the COBOL program
5. **Run Tests**: Executes all tests with the test runner
6. **Upload Report**: Saves test report as artifact
7. **Generate Summary**: Creates GitHub Actions summary

### Viewing Results

After workflow execution:

1. Go to **Actions** tab in GitHub
2. Click on the workflow run
3. View **Summary** for quick results
4. Download **test-report** artifact for detailed JSON report
5. Check **Run all tests** step for full logs

### JSON Report Format

```json
{
  "summary": {
    "total": 16,
    "passed": 14,
    "failed": 2,
    "errors": 0
  },
  "results": [
    {
      "test_name": "valid_account",
      "status": "PASSED",
      "error_message": null,
      "has_diff": false
    },
    {
      "test_name": "password_too_short",
      "status": "FAILED",
      "error_message": null,
      "has_diff": true
    }
  ]
}
```

## Troubleshooting

### Test Runner Not Found

```bash
# Make sure it's executable
chmod +x tests/test_runner.py

# Or run with python
python3 tests/test_runner.py bin/main
```

### Executable Not Found

```bash
# Compile the program first
mkdir -p bin
cobc -x -free -o bin/main src/main.cob
```

### All Tests Failing

1. Verify the COBOL program compiles and runs manually
2. Check that output format matches expected files exactly
3. Run a single test with `--verbose` to see differences
4. Ensure persistence directory is writable

### Multi-Part Tests Not Working

1. Verify `_part_N` naming is correct
2. Check that both input and output files exist for each part
3. Ensure persistence directory permissions are correct
4. Parts must be numbered sequentially starting from 1

### Python Dependencies

If you encounter import errors:

```bash
pip install -r requirements.txt
```

## Best Practices

1. **Test Names**: Use descriptive names that explain what is being tested
2. **Expected Outputs**: Ensure expected outputs match program output exactly (including whitespace)
3. **Input Files**: Keep inputs focused on testing one specific scenario
4. **Multi-Part**: Use multi-part tests only when testing persistence
5. **Organization**: Group related tests in appropriate directories
6. **Documentation**: Update this file when adding new test categories

## Technical Details

### Persistence Management

The test runner manages persistence by:

- Creating a temporary directory for each test run
- Setting `INCOLLEGE_DATA_DIR` environment variable
- Running COBOL program in this directory
- Clearing storage between different test groups
- Maintaining storage across parts of the same test

### Diff Algorithm

Uses Python's `difflib.unified_diff` to generate diffs:

- Shows line-by-line differences
- Provides context around changes
- Uses unified diff format (same as `diff -u`)
- Color-codes additions (green) and deletions (red)

### Test Discovery

The test runner automatically:

1. Scans the test root directory recursively
2. Finds all `*.in.txt` files in `inputs/` directories
3. Matches them with corresponding files in `expected/`
4. Groups multi-part tests by base name
5. Sorts parts by number

### Type Safety

The test runner uses Python type hints throughout:

- All functions have type annotations
- Uses `dataclass` for structured data
- Employs `Enum` for status values
- Passes strict `mypy` type checking

## Support

For issues or questions:

1. Check this documentation
2. Review the test runner source code (`tests/test_runner.py`)
3. Check GitHub Actions logs for CI failures
4. Create an issue in the repository
