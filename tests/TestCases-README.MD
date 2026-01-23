========================================
INCOLLEGE EPIC #1 TEST CASES
========================================
Project: InCollege Login System (Part 1)
Team: Montana
Tester: Aaron Fraze
Date: January 2026

========================================
OVERVIEW
========================================
This directory contains test input files for Epic #1 user stories.
Each file simulates user input to test specific functionality.

IMPORTANT NOTES:
- All tests assume menu option 1 = Log In, 2 = Create Account
- Delete the account persistence file before running most tests
- Tests for persistence (Story 3) require keeping the persistence file
- Input files contain line-by-line user input (menu choices, usernames, passwords)

========================================
TEST EXECUTION INSTRUCTIONS
========================================
1. Delete account persistence file (unless testing persistence)
2. Run COBOL program with input file:
   ./InCollege < test-cases/[TestFileName].txt > test-results/[OutputFileName].txt
3. Compare output with expected results
4. Log any failures as bug tickets in Jira

========================================
STORY 1: ACCOUNT CREATION TESTS
========================================

Epic1-Story1-ValidAccount-Input.txt
  TYPE: Positive Test
  PURPOSE: Verify successful account creation with valid credentials
  PRECONDITION: Delete persistence file
  USERNAME: TestUser1
  PASSWORD: Password1! (8 chars, has capital, digit, special char)
  EXPECTED: Success message, account created, main menu displayed

Epic1-Story1-PasswordTooShort-Input.txt
  TYPE: Negative Test
  PURPOSE: Verify password length validation (minimum 8 characters)
  PRECONDITION: Delete persistence file
  USERNAME: TestUser2
  PASSWORD: Pass1! (only 6 characters)
  EXPECTED: Error message about password length, account NOT created

Epic1-Story1-NoCapitalLetter-Input.txt
  TYPE: Negative Test
  PURPOSE: Verify password requires at least one capital letter
  PRECONDITION: Delete persistence file
  USERNAME: TestUser3
  PASSWORD: password1! (no capital letter)
  EXPECTED: Error message about capital letter requirement, account NOT created

Epic1-Story1-NoDigit-Input.txt
  TYPE: Negative Test
  PURPOSE: Verify password requires at least one digit
  PRECONDITION: Delete persistence file
  USERNAME: TestUser4
  PASSWORD: Password! (no digit)
  EXPECTED: Error message about digit requirement, account NOT created

Epic1-Story1-NoSpecialChar-Input.txt
  TYPE: Negative Test
  PURPOSE: Verify password requires at least one special character
  PRECONDITION: Delete persistence file
  USERNAME: TestUser5
  PASSWORD: Password1 (no special character)
  EXPECTED: Error message about special character requirement, account NOT created

Epic1-Story1-PasswordTooLong-Input.txt
  TYPE: Negative Test
  PURPOSE: Verify password length validation (maximum 12 characters)
  PRECONDITION: Delete persistence file
  USERNAME: TestUser6
  PASSWORD: Password123!! (13 characters)
  EXPECTED: Error message about maximum length, account NOT created

Epic1-Story1-Password8Chars-Input.txt
  TYPE: Edge Case Test (Positive)
  PURPOSE: Verify password at minimum boundary (exactly 8 characters)
  PRECONDITION: Delete persistence file
  USERNAME: TestUser7
  PASSWORD: Passw0rd! (exactly 8 characters)
  EXPECTED: Success message, account created

Epic1-Story1-Password12Chars-Input.txt
  TYPE: Edge Case Test (Positive)
  PURPOSE: Verify password at maximum boundary (exactly 12 characters)
  PRECONDITION: Delete persistence file
  USERNAME: TestUser8
  PASSWORD: Password123! (exactly 12 characters)
  EXPECTED: Success message, account created

========================================
STORY 2: LOGIN FUNCTIONALITY TESTS
========================================

Epic1-Story2-SuccessfulLogin-Input.txt
  TYPE: Positive Test
  PURPOSE: Verify successful login with existing account
  PRECONDITION: Delete persistence file
  STEPS: Create account, logout, login again with same credentials
  USERNAME: LoginUser
  PASSWORD: MyPass99!
  EXPECTED: Both logins successful, "You have successfully logged in" message

Epic1-Story2-FailedLogin-Input.txt
  TYPE: Negative Test
  PURPOSE: Verify incorrect password handling and unlimited retry attempts
  PRECONDITION: Delete persistence file
  STEPS: Create account, logout, attempt login with wrong password, retry with correct
  USERNAME: FailUser
  PASSWORDS: GoodPass1! (correct), WrongPass1! (incorrect)
  EXPECTED: First login shows error "Incorrect username/password, please try again"
           Second login successful, unlimited attempts allowed

========================================
STORY 3: ACCOUNT PERSISTENCE TESTS
========================================

Epic1-Story3-Persistence-Input-Part1.txt
  TYPE: Positive Test (Part 1 of 2)
  PURPOSE: Create accounts to test persistence
  PRECONDITION: Delete persistence file
  ACCOUNTS: PersistUser1/Persist1!, PersistUser2/Persist2!
  EXPECTED: Both accounts created successfully
  ⚠️ DO NOT DELETE PERSISTENCE FILE AFTER THIS TEST

Epic1-Story3-Persistence-Input-Part2.txt
  TYPE: Positive Test (Part 2 of 2)
  PURPOSE: Verify accounts persist after program restart
  PRECONDITION: Run Part 1 first, DO NOT delete persistence file
  STEPS: Login with both accounts created in Part 1
  EXPECTED: Both logins successful (proves accounts were saved and loaded)

========================================
STORY 4: ACCOUNT LIMIT TESTS
========================================

Epic1-Story4-AccountLimit-Input.txt
  TYPE: Negative Test
  PURPOSE: Verify system enforces 5-account limit
  PRECONDITION: Delete persistence file
  STEPS: Attempt to create 6 accounts
  ACCOUNTS: User1-User6 with Password1!-Password6!
  EXPECTED: First 5 accounts created successfully
           6th account shows: "All permitted accounts have been created, please come back later"
           6th account NOT created

========================================
STORY 6: NAVIGATION TESTS
========================================

Epic1-Story6-JobSearch-Input.txt
  TYPE: Positive Test
  PURPOSE: Verify "under construction" message for job search
  PRECONDITION: Delete persistence file
  STEPS: Create account, login, select "Search for a job"
  EXPECTED: Message: "Job search/internship is under construction"
           Return to main menu

Epic1-Story6-FindSomeone-Input.txt
  TYPE: Positive Test
  PURPOSE: Verify "under construction" message for find someone
  PRECONDITION: Delete persistence file
  STEPS: Create account, login, select "Find someone you know"
  EXPECTED: Message: "Find someone you know is under construction"
           Return to main menu

========================================
STORY 8: LEARN A SKILL TESTS
========================================

Epic1-Story8-LearnSkill-Input.txt
  TYPE: Positive Test
  PURPOSE: Verify skill list display and navigation
  PRECONDITION: Delete persistence file
  STEPS: Create account, login, select "Learn a new skill"
         Select skills 1, 2, 3, then "Go Back"
  EXPECTED: Skill list shows 5 skills + Go Back option
           Each skill selection shows "This skill is under construction"
           Returns to skill menu after each selection
           "Go Back" returns to main menu

========================================
TEST COVERAGE SUMMARY
========================================
Total Test Cases: 16

By Type:
- Positive Tests: 7
- Negative Tests: 7
- Edge Case Tests: 2

By Story:
- Story 1 (Account Creation): 8 tests
- Story 2 (Login): 2 tests
- Story 3 (Persistence): 2 tests (run as a pair)
- Story 4 (Account Limit): 1 test
- Story 6 (Navigation): 2 tests
- Story 8 (Learn Skill): 1 test

========================================
KNOWN ISSUES / NOTES
========================================
- Menu option numbers may vary based on programmer implementation
  (adjust input files if needed)
- "Logout" option number needs verification with programmers
- Skill names (Skill 1-5) are placeholders; update after implementation
- Persistence file name/location needs to be confirmed with programmers

Last Updated: January 23, 2026