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
- `--verbose`, `-v`: Print detailed output including full expected/actual outputs
- `--report PATH`: Generate JSON report at specified path
- `--timeout SECONDS`: Maximum execution time per test (default: 10)

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
```

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
