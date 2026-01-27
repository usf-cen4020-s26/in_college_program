IDENTIFICATION DIVISION.
PROGRAM-ID. INCOLLEGE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
     SELECT INPUT-FILE ASSIGN TO "INPUT.TXT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-INPUT-STATUS.
     SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.TXT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-OUTPUT-STATUS.
     SELECT ACCOUNTS-FILE ASSIGN TO "ACCOUNTS.DAT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-ACCOUNTS-STATUS.

DATA DIVISION.
FILE SECTION.
FD  INPUT-FILE.
01  INPUT-RECORD                PIC X(80).

FD  OUTPUT-FILE.
01  OUTPUT-RECORD               PIC X(80).

FD  ACCOUNTS-FILE.
01  ACCOUNT-RECORD.
    05  ACCT-USERNAME           PIC X(20).
    05  ACCT-PASSWORD           PIC X(12).

WORKING-STORAGE SECTION.
01  WS-USER-ACCOUNTS.
    05  WS-ACCOUNT OCCURS 5 TIMES.
        10  WS-USERNAME         PIC X(20).
        10  WS-PASSWORD         PIC X(12).

01  WS-ACCOUNT-COUNT            PIC 9 VALUE 0.
01  WS-MAX-ACCOUNTS             PIC 9 VALUE 5.

01  WS-LOGIN-USERNAME           PIC X(20).
01  WS-LOGIN-PASSWORD           PIC X(12).
01  WS-LOGIN-SUCCESS            PIC 9 VALUE 0.
01  WS-ACCOUNT-INDEX            PIC 9.

01  WS-PASSWORD-INPUT           PIC X(50).
01  WS-PASSWORD-LENGTH          PIC 99.
01  WS-HAS-CAPITAL              PIC 9 VALUE 0.
01  WS-HAS-DIGIT                PIC 9 VALUE 0.
01  WS-HAS-SPECIAL              PIC 9 VALUE 0.
01  WS-PASSWORD-VALID           PIC 9 VALUE 0.
01  WS-CHAR-INDEX               PIC 99.
01  WS-CURRENT-CHAR             PIC X.

01  WS-MENU-CHOICE              PIC X(2).
01  WS-MAIN-MENU-CHOICE         PIC X(2).
01  WS-SKILL-CHOICE             PIC X(2).

01  WS-INPUT-STATUS             PIC XX.
01  WS-OUTPUT-STATUS            PIC XX.
01  WS-ACCOUNTS-STATUS          PIC XX.
01  WS-EOF-FLAG                 PIC 9 VALUE 0.
01  WS-PROGRAM-RUNNING          PIC 9 VALUE 1.

01  WS-OUTPUT-LINE              PIC X(80).

PROCEDURE DIVISION.
0000-MAIN-PROGRAM.
    PERFORM 1000-INITIALIZE.
    PERFORM 2000-PROCESS-APPLICATION
        UNTIL WS-PROGRAM-RUNNING = 0.
    PERFORM 9000-TERMINATE.
    STOP RUN.

1000-INITIALIZE.
    OPEN INPUT INPUT-FILE.

    IF WS-INPUT-STATUS NOT = "00"
       EVALUATE WS-INPUT-STATUS
          WHEN "35"
             DISPLAY "ERROR: INPUT.TXT file not found"
             DISPLAY "Create INPUT.TXT before running"
          WHEN OTHER
             DISPLAY "ERROR opening INPUT.TXT. FILE STATUS = "
                     WS-INPUT-STATUS
       END-EVALUATE
       MOVE 0 TO WS-PROGRAM-RUNNING
       STOP RUN
    END-IF.

    OPEN OUTPUT OUTPUT-FILE.

    PERFORM 1100-LOAD-ACCOUNTS.

    MOVE "========================================"
        TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.
    MOVE "     WELCOME TO INCOLLEGE" TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.
    MOVE "========================================"
        TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.

1100-LOAD-ACCOUNTS.
    OPEN INPUT ACCOUNTS-FILE.
    IF WS-ACCOUNTS-STATUS = "00" OR WS-ACCOUNTS-STATUS = "97"
        PERFORM 1110-READ-ACCOUNT-LOOP
        CLOSE ACCOUNTS-FILE
    END-IF.

1110-READ-ACCOUNT-LOOP.
    READ ACCOUNTS-FILE
        AT END
            MOVE 1 TO WS-EOF-FLAG
        NOT AT END
            IF WS-ACCOUNT-COUNT < WS-MAX-ACCOUNTS
                ADD 1 TO WS-ACCOUNT-COUNT
                MOVE ACCT-USERNAME TO
                    WS-USERNAME(WS-ACCOUNT-COUNT)
                MOVE ACCT-PASSWORD TO
                    WS-PASSWORD(WS-ACCOUNT-COUNT)
            END-IF
    END-READ.

    IF WS-EOF-FLAG = 0
        PERFORM 1110-READ-ACCOUNT-LOOP
    ELSE
        MOVE 0 TO WS-EOF-FLAG
    END-IF.

2000-PROCESS-APPLICATION.
    MOVE " " TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.
    MOVE "Please select an option:" TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.
    MOVE "1. Login with existing account" TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.
    MOVE "2. Create new account" TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.
    MOVE "3. Exit" TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.
    MOVE "Enter choice (1-3): " TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.

    PERFORM 8100-READ-INPUT.

    IF WS-EOF-FLAG = 1
        MOVE 0 TO WS-PROGRAM-RUNNING
        EXIT PARAGRAPH
    END-IF

    MOVE INPUT-RECORD TO WS-MENU-CHOICE.
    MOVE WS-MENU-CHOICE TO WS-OUTPUT-LINE.
    PERFORM 8000-WRITE-OUTPUT.

    EVALUATE WS-MENU-CHOICE
        WHEN "1"
            PERFORM 3000-LOGIN-PROCESS
        WHEN "2"
            PERFORM 4000-CREATE-ACCOUNT
        WHEN "3"
            MOVE 0 TO WS-PROGRAM-RUNNING
        WHEN OTHER
            MOVE "Invalid choice. Please try again."
                TO WS-OUTPUT-LINE
            PERFORM 8000-WRITE-OUTPUT
    END-EVALUATE.

*> *      *>*****************************************************************
*> *      *> 3000-LOGIN-PROCESS: User Login Handler                        *
*> *      *> USER STORY 2: Login functionality                             *
*> *      *> TASK 4: Unlimited login attempts                              *
*> *      *>*****************************************************************
       3000-LOGIN-PROCESS.
           MOVE 0 TO WS-LOGIN-SUCCESS.

           PERFORM 3100-ATTEMPT-LOGIN
               UNTIL WS-LOGIN-SUCCESS = 1.

*> *      *>*****************************************************************
*> *      *> 3100-ATTEMPT-LOGIN: Single Login Attempt                      *
*> *      *> USER STORY 2, TASK 1: Display success/failure messages        *
*> *      *> USER STORY 2, TASK 2: Read from file, output to both          *
*> *      *> USER STORY 2, TASK 3: Test with valid/invalid credentials     *
*> *      *>*****************************************************************
       3100-ATTEMPT-LOGIN.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "=== LOGIN ===" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "Enter username: " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.

           IF WS-EOF-FLAG = 1
               MOVE 1 TO WS-LOGIN-SUCCESS
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF


           MOVE INPUT-RECORD TO WS-LOGIN-USERNAME.
           MOVE WS-LOGIN-USERNAME TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE "Enter password: " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           PERFORM 8100-READ-INPUT.

           IF WS-EOF-FLAG = 1
               MOVE 1 TO WS-LOGIN-SUCCESS
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           MOVE INPUT-RECORD TO WS-LOGIN-PASSWORD.
           MOVE "********" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>    Validate credentials
           PERFORM 3200-VALIDATE-LOGIN.

           IF WS-LOGIN-SUCCESS = 1
               MOVE "You have successfully logged in"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 5000-POST-LOGIN-MENU
           ELSE
               MOVE "Incorrect username/password, please try again"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 3200-VALIDATE-LOGIN: Check username and password              *
*> *      *>*****************************************************************
       3200-VALIDATE-LOGIN.
           MOVE 0 TO WS-LOGIN-SUCCESS.
           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-ACCOUNT-COUNT
                   OR WS-LOGIN-SUCCESS = 1

               IF WS-USERNAME(WS-ACCOUNT-INDEX) = WS-LOGIN-USERNAME
                   AND WS-PASSWORD(WS-ACCOUNT-INDEX) =
                       WS-LOGIN-PASSWORD
                   THEN
                       MOVE 1 TO WS-LOGIN-SUCCESS
               END-IF
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 4000-CREATE-ACCOUNT: New Account Registration                 *
*> *      *> USER STORY 1: Account creation functionality                  *
*> *      *> USER STORY 4: Inform user of account limit                    *
*> *      *>*****************************************************************
       4000-CREATE-ACCOUNT.
*> *      *>*****************************************************************
*> *      *> USER STORY 4, TASK 1: Check account limit before creation     *
*> *      *>*****************************************************************
           IF WS-ACCOUNT-COUNT >= WS-MAX-ACCOUNTS
*> *      *>*****************************************************************
*> *      *> USER STORY 4, TASK 2: Notify user of account limit            *
*> *      *>*****************************************************************
               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "All permitted accounts have been created,"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "please come back later" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               PERFORM 4100-GET-NEW-ACCOUNT-INFO
           END-IF.

*> *      *>*****************************************************************
*> *      *> 4100-GET-NEW-ACCOUNT-INFO: Collect new account details        *
*> *      *> USER STORY 1, TASK 1: New user account management setup       *
*> *      *>*****************************************************************
       4100-GET-NEW-ACCOUNT-INFO.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "=== CREATE NEW ACCOUNT ===" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "Enter username: " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.

           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           MOVE INPUT-RECORD TO WS-LOGIN-USERNAME.
           MOVE WS-LOGIN-USERNAME TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>    Check if username already exists
           PERFORM 4200-CHECK-USERNAME-EXISTS.

           IF WS-LOGIN-SUCCESS = 1
               MOVE "Username already exists. Please try another."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               PERFORM 4300-GET-VALID-PASSWORD
               IF WS-PASSWORD-VALID = 1
                   PERFORM 4500-SAVE-NEW-ACCOUNT
               END-IF
           END-IF.

*> *      *>*****************************************************************
*> *      *> 4200-CHECK-USERNAME-EXISTS: Verify username uniqueness        *
*> *      *>*****************************************************************
       4200-CHECK-USERNAME-EXISTS.
           MOVE 0 TO WS-LOGIN-SUCCESS.
           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-ACCOUNT-COUNT
                   OR WS-LOGIN-SUCCESS = 1

               IF WS-USERNAME(WS-ACCOUNT-INDEX) = WS-LOGIN-USERNAME
                   MOVE 1 TO WS-LOGIN-SUCCESS
               END-IF
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 4300-GET-VALID-PASSWORD: Password input and validation        *
*> *      *> USER STORY 1, TASK 1: Password requirements enforcement       *
*> *      *>*****************************************************************
       4300-GET-VALID-PASSWORD.
           MOVE 0 TO WS-PASSWORD-VALID.

           PERFORM UNTIL WS-PASSWORD-VALID = 1
               MOVE "Enter password (8-12 chars, 1 uppercase,"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "1 digit, 1 special character): "
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
                IF WS-EOF-FLAG = 1
                     MOVE 0 TO WS-PROGRAM-RUNNING
                     EXIT PERFORM
                END-IF

               MOVE INPUT-RECORD TO WS-PASSWORD-INPUT
               MOVE "********" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 4400-VALIDATE-PASSWORD

               IF WS-PASSWORD-VALID = 0
                   MOVE "Password does not meet requirements."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "Please try again." TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               END-IF
           END-PERFORM.

           MOVE WS-PASSWORD-INPUT TO WS-LOGIN-PASSWORD.

*> *      *>*****************************************************************
*> *      *> 4400-VALIDATE-PASSWORD: Check password requirements           *
*> *      *> USER STORY 1, TASK 1: Password validation logic               *
*> *      *>*****************************************************************
       4400-VALIDATE-PASSWORD.
           MOVE 0 TO WS-PASSWORD-VALID.
           MOVE 0 TO WS-HAS-CAPITAL.
           MOVE 0 TO WS-HAS-DIGIT.
           MOVE 0 TO WS-HAS-SPECIAL.

*> *      *>    Check length
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-PASSWORD-INPUT))
               TO WS-PASSWORD-LENGTH.

           IF WS-PASSWORD-LENGTH < 8 OR WS-PASSWORD-LENGTH > 12
               EXIT PARAGRAPH
           END-IF.

*> *      *>    Check for capital letter, digit, and special character
           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1
               UNTIL WS-CHAR-INDEX > WS-PASSWORD-LENGTH

               MOVE WS-PASSWORD-INPUT(WS-CHAR-INDEX:1)
                   TO WS-CURRENT-CHAR

*> *      *>        Check for capital letter
               IF WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "Z"
                   MOVE 1 TO WS-HAS-CAPITAL
               END-IF

*> *      *>        Check for digit
               IF WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
                   MOVE 1 TO WS-HAS-DIGIT
               END-IF

*> *      *>        Check for special character
               IF WS-CURRENT-CHAR = "!" OR WS-CURRENT-CHAR = "@"
                   OR WS-CURRENT-CHAR = "#" OR WS-CURRENT-CHAR = "$"
                   OR WS-CURRENT-CHAR = "%" OR WS-CURRENT-CHAR = "^"
                   OR WS-CURRENT-CHAR = "&" OR WS-CURRENT-CHAR = "*"
                   MOVE 1 TO WS-HAS-SPECIAL
               END-IF
           END-PERFORM.

*> *      *>    All requirements must be met
           IF WS-HAS-CAPITAL = 1 AND WS-HAS-DIGIT = 1
               AND WS-HAS-SPECIAL = 1
               MOVE 1 TO WS-PASSWORD-VALID
           END-IF.

*> *      *>*****************************************************************
*> *      *> 4500-SAVE-NEW-ACCOUNT: Store account in memory and file       *
*> *      *> USER STORY 3, TASK 1: File persistence implementation         *
*> *      *>*****************************************************************
       4500-SAVE-NEW-ACCOUNT.
           ADD 1 TO WS-ACCOUNT-COUNT.
           MOVE WS-LOGIN-USERNAME TO WS-USERNAME(WS-ACCOUNT-COUNT).
           MOVE WS-LOGIN-PASSWORD TO WS-PASSWORD(WS-ACCOUNT-COUNT).

           MOVE "Account created successfully!" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>    Save to persistence file
           PERFORM 4600-WRITE-ACCOUNTS-FILE.

*> *      *>*****************************************************************
*> *      *> 4600-WRITE-ACCOUNTS-FILE: Persist all accounts to file        *
*> *      *> USER STORY 3, TASK 1: Write accounts to persistence file      *
*> *      *>*****************************************************************
       4600-WRITE-ACCOUNTS-FILE.
           OPEN OUTPUT ACCOUNTS-FILE.

           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-ACCOUNT-COUNT

               MOVE WS-USERNAME(WS-ACCOUNT-INDEX) TO ACCT-USERNAME
               MOVE WS-PASSWORD(WS-ACCOUNT-INDEX) TO ACCT-PASSWORD
               WRITE ACCOUNT-RECORD
           END-PERFORM.

           CLOSE ACCOUNTS-FILE.

*> *      *>*****************************************************************
*> *      *> 5000-POST-LOGIN-MENU: Main menu after successful login        *
*> *      *>*****************************************************************
       5000-POST-LOGIN-MENU.
           MOVE "1" TO WS-MAIN-MENU-CHOICE.

           PERFORM UNTIL WS-MAIN-MENU-CHOICE = "4"
               OR WS-PROGRAM-RUNNING = 0
               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "=== MAIN MENU ===" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "1. Search for a job" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "2. Find someone you know" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "3. Learn a new skill" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "4. Logout" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Enter choice (1-4): " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT

               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-MAIN-MENU-CHOICE
               MOVE WS-MAIN-MENU-CHOICE TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               EVALUATE WS-MAIN-MENU-CHOICE
                   WHEN "1"
                       MOVE "Search for a job is under construction." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   WHEN "2"
                       MOVE "Find someone you know is under construction." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   WHEN "3"
                       PERFORM 6000-SKILLS-MENU
                   WHEN "4"
                       MOVE "Logging out..." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
               END-EVALUATE
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 6000-SKILLS-MENU: Learn a new skill submenu                   *
*> *      *>*****************************************************************
       6000-SKILLS-MENU.
           MOVE "1" TO WS-SKILL-CHOICE.

           PERFORM UNTIL WS-SKILL-CHOICE = "6"
               OR WS-PROGRAM-RUNNING = 0
               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "=== LEARN A NEW SKILL ===" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "1. Skill 1" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "2. Skill 2" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "3. Skill 3" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "4. Skill 4" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "5. Skill 5" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "6. Go Back" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Enter choice (1-6): " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT

               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-SKILL-CHOICE
               MOVE WS-SKILL-CHOICE TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF WS-SKILL-CHOICE = "1" OR WS-SKILL-CHOICE = "2" OR
                   WS-SKILL-CHOICE = "3" OR WS-SKILL-CHOICE = "4" OR
                   WS-SKILL-CHOICE = "5"
                   MOVE "This skill is under construction." TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               ELSE
                   IF WS-SKILL-CHOICE NOT = "6"
                       MOVE "Invalid choice. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   END-IF
               END-IF
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 8000-WRITE-OUTPUT: Output to both screen and file             *
*> *      *> Implements requirement to display AND preserve output         *
*> *      *>*****************************************************************
       8000-WRITE-OUTPUT.
           DISPLAY WS-OUTPUT-LINE.
           WRITE OUTPUT-RECORD FROM WS-OUTPUT-LINE.

*> *      *>*****************************************************************
*> *      *> 8100-READ-INPUT: Read from input file                         *
*> *      *> Implements requirement to read input from file                *
*> *      *>*****************************************************************
       8100-READ-INPUT.
           READ INPUT-FILE
               AT END
                   MOVE 1 to WS-EOF-FLAG
                   MOVE SPACES TO INPUT-RECORD
           END-READ.

*> *      *>*****************************************************************
*> *      *> 9000-TERMINATE: Cleanup and close files                       *
*> *      *>*****************************************************************
       9000-TERMINATE.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "Thank you for using InCollege!" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.


