# User Input Screens

This document outlines all available user input screens in the application, alongside a short description of each screen’s purpose.

This document is used to create all test-related screenings for **Epic #1: Log In, Part 1**.

---

## Global I/O Behavior (Applies to All Screens)

- **All user input is read from a predefined input file** (no interactive keyboard input during execution).
- **All program output must be displayed to the screen AND written identically to an output file.**
- **All user inputs must also be written to the output file** in the same run, to preserve a complete transcript.

---

## User Login Screen (Initial Screen)

Purpose: Entry point of the program. User chooses to log in or create a new account.

```bash
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
```

Navigation:
- Selecting **Log In** -> goes to **Log In Account Screen**
- Selecting **Create New Account** -> goes to **Create an Account Screen**
- Any other input -> (implementation-defined handling; recommended: re-display this screen)

---

## Log In Account Screen

Purpose: Prompts for existing credentials and authenticates the user. Unlimited attempts are permitted.

### Successful Login

```bash
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, [Username]!
<insert main menu screen here>
```

### Failed Login (Unlimited Retries)

```bash
Please enter your username:
Please enter your password:
Incorrect username/password, please try again.
<insert user login screen here>
```

Navigation:
- On success -> **Main Menu Screen**
- On failure -> back to **User Login Screen** (or directly re-run login flow; whichever the implementation uses—tests must match actual program flow)

---

## Create an Account Screen

Purpose: Creates a new student account (supports **up to 5** unique accounts). Enforces password rules and persists created accounts across runs.

### Prompts

```bash
Please enter your desired username:
Please enter your desired password:
```

### Password Requirements (Validation Rules)

The password must meet **all** of the following:
- Minimum **8** characters
- Maximum **12** characters
- At least **one capital letter**
- At least **one digit**
- At least **one special character**

> Note: The exact error message(s) for password validation failures are implementation-defined unless your team standardizes them. Tests must assert the exact messages your program outputs.

### Recommended Password Failure Response (If Team Chooses a Standard Message)

```bash
Password does not meet requirements. Please try again.
```

(Then re-prompt for username/password or password only—implementation-defined.)

### Account Limit Reached (6th Attempt)

Purpose: On the **6th** attempt to create an account (i.e., when 5 already exist), the system must display:

```bash
All permitted accounts have been created, please come back later
```

Navigation:
- After reaching the limit -> (recommended) return to **User Login Screen**

### Successful Account Creation (If Space Available)

Purpose: Confirms account creation and returns user to the initial screen or login flow (implementation-defined).

Recommended confirmation (implementation-defined unless standardized by the team):

```bash
Account successfully created.
<insert user login screen here>
```

---

## Main Menu Screen (Post-Login)

Purpose: Top-level navigation after successful login.

```bash
1. Search for a job
2. Find someone you know
3. Learn a new Skill
4. Logout
Enter your choice:
```

Navigation:
- **1** -> Job Search Under Construction Screen (then returns to Main Menu)
- **2** -> Find Someone Under Construction Screen (then returns to Main Menu)
- **3** -> Skill Learning Screen Selection
- **4** -> Program Termination Screen (ends program)
- Any other input -> (recommended) re-display Main Menu

---

## Job Search Under Construction Screen

Purpose: Confirms feature is not implemented.

```bash
Search for a job is under construction.
<insert main menu screen here>
```

Navigation:
- Returns to **Main Menu Screen**

---

## Find Someone Under Construction Screen

Purpose: Confirms feature is not implemented.

```bash
Find someone you know is under construction.
<insert main menu screen here>
```

Navigation:
- Returns to **Main Menu Screen**

---

## Skill Learning Screen Selection

Purpose: Allows user to select one of five skills (team-defined) or go back to the Main Menu.

```bash
Learn a New Skill:
Skill 1
Skill 2
Skill 3
Skill 4
Skill 5
Go Back
Enter your choice:
<skill screen/main menu navigation here>
```

Navigation:
- Selecting any **Skill 1–Skill 5** -> Skill Under Construction Screen, then returns to **Skill Learning Screen Selection**
- Selecting **Go Back** -> returns to **Main Menu Screen**
- Any other input -> (recommended) re-display Skill Learning Screen Selection

---

## Skill Under Construction Screen

Purpose: Confirms selected skill is not implemented.

```bash
This skill is under construction.
<insert skill learning screen selection here>
```

Navigation:
- Returns to **Skill Learning Screen Selection**

---

## Program Termination Screen

Purpose: Marks clean termination after selecting Logout.

Recommended termination marker (seen in sample outputs):

```bash
--- END_OF_PROGRAM_EXECUTION ---
```
