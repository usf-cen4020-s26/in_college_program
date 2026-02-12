# InCollege COBOL Project

InCollege is a career networking platform implemented in COBOL using GNU COBOL. The application allows students and professionals to create profiles, connect with others, and explore career development opportunities. This project demonstrates file-based persistence, menu-driven interfaces, and comprehensive input validation in COBOL.

## Features

### User Authentication (Epic 1)
- Create new user accounts with secure password validation
- Login with existing credentials
- Account limit enforcement (maximum 5 accounts)
- Unlimited login attempts until successful
- Persistent storage of user credentials across program runs

**Password Requirements:**
- 8-12 characters in length
- At least one uppercase letter
- At least one digit (0-9)
- At least one special character (!@#$%^&*)

### User Profile Management (Epic 2)
- Create and edit personal profiles with comprehensive information
- View your own complete profile
- Profile data persists across sessions and logins

**Profile Fields:**
- Required: First Name, Last Name, University/College, Major, Graduation Year
- Optional: About Me section
- Up to 3 work experience entries (title, employer, dates, description)
- Up to 3 education entries (school name, degree, years attended)

**Profile Validations:**
- Graduation year must be 4 digits between 1950 and 2050
- All required fields must be filled before saving
- Optional fields can be skipped

### User Search (Epic 3)
- Search for other InCollege users by their full name
- View complete profiles of found users
- Exact name matching with informative feedback
- Search results display all profile information

### Additional Features
- Skills menu framework (placeholder for future expansion)
- Job search framework (placeholder for future expansion)
- Logout functionality
- Graceful handling of end-of-file conditions

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
cobc -x -free -o bin/main src/main.cob
```

### Running the Program

The program operates on a file-based input/output system designed for automated testing and scripted interactions.

**Input:** All user input is read from `INPUT.TXT` line by line. Each line represents one user interaction (menu choice, username, password, search query, etc.).

**Output:** All program output is written to two destinations:
1. Standard output (your terminal screen)
2. `OUTPUT.TXT` file (identical copy for verification)

To run the program:

```sh
./bin/main
```

The program will read from `INPUT.TXT` and write to both the screen and `OUTPUT.TXT`. When the input file is exhausted (end-of-file), the program terminates gracefully.

**Example INPUT.TXT:**
```
1
testuser
MyPass123!
2
6
3
```

This input would:
1. Select "Login" (1)
2. Enter username "testuser"
3. Enter password "MyPass123!"
4. Select "View My Profile" (2)
5. Select "Logout" (6)
6. Select "Exit" (3) from the main welcome screen

**Data Persistence:**
User accounts are stored in `ACCOUNTS.DAT` and profiles in `PROFILES.DAT`. These files persist between program executions, allowing users to log in across multiple runs.

## Testing

The project includes an automated testing system written in Python that validates program behavior against expected outputs. Tests are comprehensive, covering login flows, profile management, user search, error handling, and edge cases.

### Important: Compile Before Testing

Always compile the COBOL program before running tests. The test runner executes the compiled binary, so changes to source code won't be reflected until you recompile:

```sh
mkdir -p bin
cobc -x -free -o bin/main src/main.cob
```

### Test Organization

Tests are organized by feature category under `tests/fixtures/`:

- `login/` - Authentication and account creation tests
- `profiles/` - Profile creation, editing, and viewing tests
- `main_menu/` - Post-login menu navigation tests
- `eof_tests/` - End-of-file handling validation

Each test consists of:
- An input file (`.in.txt`) containing line-by-line user interactions
- An expected output file (`.out.txt`) containing the exact expected program output

### Multi-Part Tests (Persistence Testing)

Some features require testing persistence across multiple program executions. For example, creating an account in one run and then logging in with that account in a subsequent run. These tests use the `_part_N` naming convention:

- `create_and_login_part_1.in.txt` - Creates a new account
- `create_and_login_part_2.in.txt` - Logs in with the created account

The test runner automatically detects these multi-part tests and:
- Executes parts sequentially in numerical order
- Maintains `ACCOUNTS.DAT` and `PROFILES.DAT` between parts
- Clears persistent storage between different test groups

### Running Tests

Run all tests:

```sh
./run_tests.sh
```

Or use the Python test runner directly:

```sh
python3 tests/test_runner.py bin/main
```

Show detailed differences for failed tests:

```sh
python3 tests/test_runner.py bin/main --verbose
```

Test a specific feature category:

```sh
python3 tests/test_runner.py bin/main --test-root tests/fixtures/login
```

Generate a JSON report for CI/CD:

```sh
python3 tests/test_runner.py bin/main --report test-report.json
```

### Understanding Test Results

The test runner provides color-coded output:

- Green checkmark: Test passed
- Red X: Test failed (shows differences between expected and actual output)
- Yellow warning: Test error (program crash, timeout, or execution failure)

Failed tests display a unified diff showing exactly what differs:

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

After all tests complete, a summary shows total counts and percentages for passed, failed, and error cases.

### Writing New Tests

To add a new test:

1. Create an input file in the appropriate subdirectory under `tests/fixtures/`:
   ```
   tests/fixtures/login/2_new_account/inputs/my_new_test.in.txt
   ```

2. Create the corresponding expected output file:
   ```
   tests/fixtures/login/2_new_account/expected/my_new_test.out.txt
   ```

3. For multi-part tests, use the `_part_N` suffix:
   ```
   my_new_test_part_1.in.txt and my_new_test_part_1.out.txt
   my_new_test_part_2.in.txt and my_new_test_part_2.out.txt
   ```

4. Recompile and run tests to validate:
   ```sh
   cobc -x -free -o bin/main src/main.cob
   python3 tests/test_runner.py bin/main
   ```

## Continuous Integration

The project uses GitHub Actions for automated testing. On each push or pull request, the workflow:

- Compiles the COBOL program
- Runs the complete test suite
- Generates test reports
- Uploads artifacts for review

Results appear in the GitHub Actions tab with test summaries and downloadable reports.

## License

See [LICENSE](LICENSE) for details.
