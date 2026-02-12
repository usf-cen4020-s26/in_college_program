# InCollege COBOL Project

This repository contains the InCollege application - a COBOL-based career networking platform with user authentication, job search, and skill learning features. The project includes a comprehensive test suite and CI/CD pipeline.

## Getting Started

### Prerequisites

- [Docker](https://www.docker.com/) (for devcontainer usage)
- [Visual Studio Code](https://code.visualstudio.com/) with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
- Alternatively, a local installation of GNU COBOL and Python 3.11+

### Opening in VS Code Dev Container

1. Open this repository in VS Code.
2. When prompted, reopen in the dev container, or use the Command Palette:
   `Dev Containers: Reopen in Container`
3. The environment will automatically install GNU COBOL, Python, and required extensions.

### Compiling the COBOL Program

The main COBOL source file is [`src/main.cob`](src/main.cob).

To compile:

#### Using VS Code Tasks

- Press <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd> and select **COBOL: Build active file**.
- The compiled executable will be placed in the `bin/` directory (e.g., `bin/main`).

#### Using the Command Line

From the project root, run:

```sh
mkdir -p bin
cobc -x -free -I src/copybooks -o bin/main src/main.cob src/app_init.cob src/app_term.cob src/io_service.cob src/data_store.cob src/login_module.cob src/account_module.cob src/post_login_menu.cob src/skills_menu.cob src/profile_module.cob src/profile_display.cob src/search_module.cob
```

**Alternative (automatically discovers all modules):**
```sh
mkdir -p bin
OTHER_SOURCES=$(find src -name "*.cob" -not -name "main.cob" | sort | tr '\n' ' ')
cobc -x -free -I src/copybooks -o bin/main src/main.cob $OTHER_SOURCES
```

### Running the Program

```sh
./bin/main
```

The program reads input from stdin and writes output to stdout. For automated testing, input can be provided via file redirection:

```sh
./bin/main < tests/fixtures/login/2_new_account/inputs/valid_account.in.txt
```

## Testing

This project includes a comprehensive Python-based testing system that automatically validates the COBOL program against expected outputs.

### Test Structure

Tests are organized in the `tests/fixtures` directory:

Each test category contains:
- `inputs/`: Input files (`.in.txt`) containing user interactions
- `expected/`: Expected output files (`.out.txt`) for validation

### Multi-Part Tests

Some tests require multiple program executions to test persistence (e.g., creating an account in one run and logging in during another). These tests are identified by the `_part_N` suffix (e.g., `persistence_input_part_1.in.txt`, `persistence_input_part_2.in.txt`).

The test runner automatically:
- Detects multi-part tests
- Executes them in order
- Maintains persistent storage between parts
- Clears storage between different test groups

### Running Tests

#### Run All Tests

```sh
python3 tests/test_runner.py bin/main
```

#### Run Tests with Verbose Output

```sh
python3 tests/test_runner.py bin/main --verbose
```

#### Generate JSON Report

```sh
python3 tests/test_runner.py bin/main --report test-report.json
```

#### Specify Custom Test Directory

```sh
python3 tests/test_runner.py bin/main --test-root tests/fixtures/login
```

### Test Runner Features

The test runner ([tests/test_runner.py](tests/test_runner.py)) provides:

- **Automatic Test Discovery**: Scans test directories and discovers all test cases
- **Multi-Part Test Support**: Handles tests requiring persistent state across multiple runs
- **Detailed Diff Output**: Shows exact differences between expected and actual output with colored highlighting
- **Summary Reports**: Provides comprehensive pass/fail statistics
- **Strict Type Checking**: Written with Python type hints for maintainability
- **CI/CD Integration**: Generates JSON reports for automated workflows

### Test Output

The test runner provides color-coded results:
- ✓ **Green**: Test passed
- ✗ **Red**: Test failed (with detailed diff)
- ⚠ **Yellow**: Test error (execution failure)

Example output:
```
======================================================================
Running InCollege COBOL Test Suite
======================================================================

Executable: bin/main
Test Root: tests/fixtures/login
Found 16 test groups

✓ valid_account: PASSED
✗ password_too_short: FAILED

  Differences found:
    --- password_too_short (expected)
    +++ password_too_short (actual)
    @@ -3,7 +3,7 @@
     Please enter your username:
     TestUser2
     Please enter your password:
    -Password must be between 8 and 12 characters...
    +Password requirements not met

======================================================================
Test Summary
======================================================================

Total Tests: 16
Passed: 14 (87.5%)
Failed: 2 (12.5%)
Errors: 0 (0.0%)
```

## Continuous Integration

This project uses GitHub Actions for continuous integration. The workflow:

1. **Builds** the COBOL program
2. **Runs** the complete test suite
3. **Generates** test reports
4. **Uploads** artifacts for review
5. **Checks** Python code quality (formatting, type checking, linting)

The CI pipeline runs on:
- Push to `main` or `develop` branches
- Pull requests to `main` or `develop`
- Manual workflow dispatch

View the workflow configuration in [.github/workflows/test.yml](.github/workflows/test.yml).

### CI Workflow Status

After each run, the workflow provides:
- ✅ Build status
- 📊 Test summary in GitHub Actions summary
- 📁 Downloadable test report artifact
- 🔍 Detailed logs for failures

## Development

### Code Quality

Python code follows strict typing and formatting standards:

```sh
# Format code
black tests/test_runner.py

# Sort imports
isort tests/test_runner.py

# Type checking
mypy tests/test_runner.py --strict

# Linting
pylint tests/test_runner.py
```

### Adding New Tests

1. Create input file in appropriate `inputs/` directory:
   ```
   tests/fixtures/login/2_new_account/inputs/my_test.in.txt
   ```

2. Create expected output file:
   ```
   tests/fixtures/login/2_new_account/expected/my_test.out.txt
   ```

3. For multi-part tests, use the `_part_N` naming convention:
   ```
   my_test_part_1.in.txt / my_test_part_1.out.txt
   my_test_part_2.in.txt / my_test_part_2.out.txt
   ```

4. Run tests to validate:
   ```sh
   python3 tests/test_runner.py bin/main
   ```

## License

See [LICENSE](LICENSE) for details.
