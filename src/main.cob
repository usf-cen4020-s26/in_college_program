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
     SELECT PROFILES-FILE ASSIGN TO "PROFILES.DAT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-PROFILES-STATUS.

DATA DIVISION.
FILE SECTION.
FD  INPUT-FILE.
01  INPUT-RECORD                PIC X(200).

FD  OUTPUT-FILE.
01  OUTPUT-RECORD               PIC X(80).

FD  ACCOUNTS-FILE.
01  ACCOUNT-RECORD.
    05  ACCT-USERNAME           PIC X(20).
    05  ACCT-PASSWORD           PIC X(12).

FD  PROFILES-FILE.
01  PROFILE-RECORD.
    05  PROF-USERNAME           PIC X(20).
    05  PROF-HAS-PROFILE        PIC 9.
    05  PROF-FIRST-NAME         PIC X(30).
    05  PROF-LAST-NAME          PIC X(30).
    05  PROF-UNIVERSITY         PIC X(50).
    05  PROF-MAJOR              PIC X(50).
    05  PROF-GRAD-YEAR          PIC X(4).
    05  PROF-ABOUT-ME           PIC X(200).
    05  PROF-EXP-COUNT          PIC 9.
    05  PROF-EXPERIENCE OCCURS 3 TIMES.
        10  PROF-EXP-TITLE      PIC X(50).
        10  PROF-EXP-COMPANY    PIC X(50).
        10  PROF-EXP-DATES      PIC X(30).
        10  PROF-EXP-DESC       PIC X(100).
    05  PROF-EDU-COUNT          PIC 9.
    05  PROF-EDUCATION OCCURS 3 TIMES.
        10  PROF-EDU-DEGREE     PIC X(50).
        10  PROF-EDU-UNIVERSITY PIC X(50).
        10  PROF-EDU-YEARS      PIC X(20).

WORKING-STORAGE SECTION.
01  WS-USER-ACCOUNTS.
    05  WS-ACCOUNT OCCURS 5 TIMES.
        10  WS-USERNAME         PIC X(20).
        10  WS-PASSWORD         PIC X(12).

01  WS-USER-PROFILES.
    05  WS-PROFILE OCCURS 5 TIMES.
        10  WS-PROF-USERNAME    PIC X(20).
        10  WS-HAS-PROFILE      PIC 9.
        10  WS-FIRST-NAME       PIC X(30).
        10  WS-LAST-NAME        PIC X(30).
        10  WS-UNIVERSITY       PIC X(50).
        10  WS-MAJOR            PIC X(50).
        10  WS-GRAD-YEAR        PIC X(4).
        10  WS-ABOUT-ME         PIC X(200).
        10  WS-EXP-COUNT        PIC 9.
        10  WS-EXPERIENCE OCCURS 3 TIMES.
            15  WS-EXP-TITLE    PIC X(50).
            15  WS-EXP-COMPANY  PIC X(50).
            15  WS-EXP-DATES    PIC X(30).
            15  WS-EXP-DESC     PIC X(100).
        10  WS-EDU-COUNT        PIC 9.
        10  WS-EDUCATION OCCURS 3 TIMES.
            15  WS-EDU-DEGREE   PIC X(50).
            15  WS-EDU-UNIVERSITY PIC X(50).
            15  WS-EDU-YEARS    PIC X(20).

01  WS-ACCOUNT-COUNT            PIC 9 VALUE 0.
01  WS-MAX-ACCOUNTS             PIC 9 VALUE 5.
01  WS-PROFILE-COUNT            PIC 9 VALUE 0.

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
01  WS-PROFILES-STATUS          PIC XX.
01  WS-EOF-FLAG                 PIC 9 VALUE 0.
01  WS-PROGRAM-RUNNING          PIC 9 VALUE 1.

01  WS-CURRENT-USER-INDEX       PIC 9 VALUE 0.
01  WS-CURRENT-PROFILE-INDEX    PIC 9 VALUE 0.
01  WS-PROFILE-FOUND            PIC 9 VALUE 0.

01  WS-INPUT-VALID              PIC 9 VALUE 0.

01  WS-VALID                    PIC 9 VALUE 0.

01  WS-EOF-REACHED              PIC 9 VALUE 0.

01  WS-TEMP-FIRST-NAME          PIC X(30).
01  WS-TEMP-LAST-NAME           PIC X(30).
01  WS-TEMP-UNIVERSITY          PIC X(50).
01  WS-TEMP-MAJOR               PIC X(50).
01  WS-TEMP-GRAD-YEAR           PIC X(4).
01  WS-TEMP-ABOUT-ME            PIC X(200).

01  WS-EXP-LOOP-INDEX           PIC 9.
01  WS-EDU-LOOP-INDEX           PIC 9.
01  WS-TEMP-EXPERIENCES.
    05  WS-TEMP-EXP OCCURS 3 TIMES.
        10  WS-TEMP-EXP-TITLE       PIC X(50).
        10  WS-TEMP-EXP-COMPANY     PIC X(50).
        10  WS-TEMP-EXP-DATES       PIC X(30).
        10  WS-TEMP-EXP-DESC        PIC X(100).
01  WS-TEMP-EDUCATIONS.
    05  WS-TEMP-EDU OCCURS 3 TIMES.
        10  WS-TEMP-EDU-DEGREE      PIC X(50).
        10  WS-TEMP-EDU-UNIVERSITY  PIC X(50).
        10  WS-TEMP-EDU-YEARS       PIC X(20).
01  WS-CONTINUE-ADDING          PIC X(80).
01  WS-SAVE-INDEX               PIC 9.

01  WS-YEAR-VALID               PIC 9 VALUE 0.
01  WS-YEAR-NUMERIC             PIC 9 VALUE 0.
01  WS-TEMP-YEAR                PIC 9999.
01  WS-TEMP-CHAR                PIC X.
01  WS-YEAR-INDEX               PIC 9.

01  WS-DISPLAY-INDEX            PIC 9.
01  WS-PROFILE-EXISTS           PIC 9 VALUE 0.

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
           PERFORM 1150-LOAD-PROFILES.

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

*> *      *>*****************************************************************
*> *      *> 1150-LOAD-PROFILES: Load all profiles from PROFILES.dat       *
*> *      *> USER STORY (Epic 2): Profile persistence                      *
*> *      *>*****************************************************************
       1150-LOAD-PROFILES.
           OPEN INPUT PROFILES-FILE.
           IF WS-PROFILES-STATUS = "00" OR WS-PROFILES-STATUS = "97"
               IF WS-PROFILES-STATUS = "00"
                   PERFORM 1160-READ-PROFILE-LOOP
               END-IF
               CLOSE PROFILES-FILE
           END-IF.

*> *      *>*****************************************************************
*> *      *> 1160-READ-PROFILE-LOOP: Read profile records into memory      *
*> *      *>*****************************************************************
       1160-READ-PROFILE-LOOP.
           READ PROFILES-FILE
               AT END
                   MOVE 1 TO WS-EOF-FLAG
               NOT AT END
                   IF WS-PROFILE-COUNT < WS-MAX-ACCOUNTS
                       ADD 1 TO WS-PROFILE-COUNT
                       MOVE PROF-USERNAME TO
                           WS-PROF-USERNAME(WS-PROFILE-COUNT)
                       MOVE PROF-HAS-PROFILE TO
                           WS-HAS-PROFILE(WS-PROFILE-COUNT)
                       MOVE PROF-FIRST-NAME TO
                           WS-FIRST-NAME(WS-PROFILE-COUNT)
                       MOVE PROF-LAST-NAME TO
                           WS-LAST-NAME(WS-PROFILE-COUNT)
                       MOVE PROF-UNIVERSITY TO
                           WS-UNIVERSITY(WS-PROFILE-COUNT)
                       MOVE PROF-MAJOR TO
                           WS-MAJOR(WS-PROFILE-COUNT)
                       MOVE PROF-GRAD-YEAR TO
                           WS-GRAD-YEAR(WS-PROFILE-COUNT)
                       MOVE PROF-ABOUT-ME TO
                           WS-ABOUT-ME(WS-PROFILE-COUNT)
                       MOVE PROF-EXP-COUNT TO
                           WS-EXP-COUNT(WS-PROFILE-COUNT)
                       MOVE PROF-EDU-COUNT TO
                           WS-EDU-COUNT(WS-PROFILE-COUNT)

                       PERFORM 1161-COPY-EXPERIENCE-ENTRIES
                       PERFORM 1162-COPY-EDUCATION-ENTRIES
                   END-IF
           END-READ.

           IF WS-EOF-FLAG = 0
               PERFORM 1160-READ-PROFILE-LOOP
           ELSE
               MOVE 0 TO WS-EOF-FLAG
           END-IF.

*> *      *>*****************************************************************
*> *      *> 1161-COPY-EXPERIENCE-ENTRIES: Copy experience from file       *
*> *      *>*****************************************************************
       1161-COPY-EXPERIENCE-ENTRIES.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE PROF-EXP-TITLE(WS-DISPLAY-INDEX) TO
                   WS-EXP-TITLE(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EXP-COMPANY(WS-DISPLAY-INDEX) TO
                   WS-EXP-COMPANY(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EXP-DATES(WS-DISPLAY-INDEX) TO
                   WS-EXP-DATES(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EXP-DESC(WS-DISPLAY-INDEX) TO
                   WS-EXP-DESC(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 1162-COPY-EDUCATION-ENTRIES: Copy education from file         *
*> *      *>*****************************************************************
       1162-COPY-EDUCATION-ENTRIES.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE PROF-EDU-DEGREE(WS-DISPLAY-INDEX) TO
                   WS-EDU-DEGREE(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EDU-UNIVERSITY(WS-DISPLAY-INDEX) TO
                   WS-EDU-UNIVERSITY(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EDU-YEARS(WS-DISPLAY-INDEX) TO
                   WS-EDU-YEARS(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
           END-PERFORM.

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
                MOVE WS-ACCOUNT-INDEX TO WS-CURRENT-USER-INDEX
               END-IF
           END-PERFORM.

           IF WS-LOGIN-SUCCESS = 1
               PERFORM 3210-FIND-USER-PROFILE
           END-IF.

*> *      *>*****************************************************************
*> *      *> 3210-FIND-USER-PROFILE: Find user's profile in profiles array *
*> *      *> USER STORY (Epic 2): Link profile to logged-in user           *
*> *      *>*****************************************************************
       3210-FIND-USER-PROFILE.
           MOVE 0 TO WS-CURRENT-PROFILE-INDEX.
           MOVE 0 TO WS-PROFILE-FOUND.

           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT
                   OR WS-PROFILE-FOUND = 1

               IF WS-PROF-USERNAME(WS-ACCOUNT-INDEX) =
                   WS-USERNAME(WS-CURRENT-USER-INDEX)
                   MOVE WS-ACCOUNT-INDEX TO WS-CURRENT-PROFILE-INDEX
                   MOVE 1 TO WS-PROFILE-FOUND
               END-IF
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 4000-CREATE-ACCOUNT: Check capacity and create new account    *
*> *      *> USER STORY 4: New account creation with limits                *
*> *      *>*****************************************************************
       4000-CREATE-ACCOUNT.
           IF WS-ACCOUNT-COUNT >= 5
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
               MOVE "Enter password (8-12 chars, 1 uppercase, 1 digit, 1 special character):"
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
*> *      *> 4650-WRITE-PROFILES-FILE: Persist all profiles to file        *
*> *      *> USER STORY (Epic 2): Profile persistence                      *
*> *      *>*****************************************************************
       4650-WRITE-PROFILES-FILE.
           OPEN OUTPUT PROFILES-FILE.

           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT

               MOVE WS-PROF-USERNAME(WS-ACCOUNT-INDEX) TO PROF-USERNAME
               MOVE WS-HAS-PROFILE(WS-ACCOUNT-INDEX) TO PROF-HAS-PROFILE
               MOVE WS-FIRST-NAME(WS-ACCOUNT-INDEX) TO PROF-FIRST-NAME
               MOVE WS-LAST-NAME(WS-ACCOUNT-INDEX) TO PROF-LAST-NAME
               MOVE WS-UNIVERSITY(WS-ACCOUNT-INDEX) TO PROF-UNIVERSITY
               MOVE WS-MAJOR(WS-ACCOUNT-INDEX) TO PROF-MAJOR
               MOVE WS-GRAD-YEAR(WS-ACCOUNT-INDEX) TO PROF-GRAD-YEAR
               MOVE WS-ABOUT-ME(WS-ACCOUNT-INDEX) TO PROF-ABOUT-ME
               MOVE WS-EXP-COUNT(WS-ACCOUNT-INDEX) TO PROF-EXP-COUNT
               MOVE WS-EDU-COUNT(WS-ACCOUNT-INDEX) TO PROF-EDU-COUNT

               PERFORM 4651-COPY-EXPERIENCE-TO-FILE
               PERFORM 4652-COPY-EDUCATION-TO-FILE

               WRITE PROFILE-RECORD
           END-PERFORM.

           CLOSE PROFILES-FILE.

*> *      *>*****************************************************************
*> *      *> 4651-COPY-EXPERIENCE-TO-FILE: Copy experience to file record  *
*> *      *>*****************************************************************
       4651-COPY-EXPERIENCE-TO-FILE.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE WS-EXP-TITLE(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-TITLE(WS-DISPLAY-INDEX)
               MOVE WS-EXP-COMPANY(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-COMPANY(WS-DISPLAY-INDEX)
               MOVE WS-EXP-DATES(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-DATES(WS-DISPLAY-INDEX)
               MOVE WS-EXP-DESC(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-DESC(WS-DISPLAY-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 4652-COPY-EDUCATION-TO-FILE: Copy education to file record    *
*> *      *>*****************************************************************
       4652-COPY-EDUCATION-TO-FILE.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE WS-EDU-DEGREE(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EDU-DEGREE(WS-DISPLAY-INDEX)
               MOVE WS-EDU-UNIVERSITY(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EDU-UNIVERSITY(WS-DISPLAY-INDEX)
               MOVE WS-EDU-YEARS(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EDU-YEARS(WS-DISPLAY-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 5000-POST-LOGIN-MENU: Main menu after successful login        *
*> *      *>*****************************************************************
       5000-POST-LOGIN-MENU.
           MOVE "1" TO WS-MAIN-MENU-CHOICE.

           PERFORM UNTIL WS-MAIN-MENU-CHOICE = "6"
           OR WS-PROGRAM-RUNNING = 0
                   MOVE " " TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "=== MAIN MENU ===" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "1. Create/Edit My Profile" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "2. View My Profile" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "3. Search for a job" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "4. Find someone you know" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "5. Learn a new skill" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "6. Logout" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "Enter choice (1-6): " TO WS-OUTPUT-LINE
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
                           PERFORM 7000-CREATE-EDIT-PROFILE
                       WHEN "2"
                           PERFORM 7100-VIEW-PROFILE
                       WHEN "3"
                           MOVE "Search for a job is under construction." TO WS-OUTPUT-LINE
                           PERFORM 8000-WRITE-OUTPUT
                       WHEN "4"
                           MOVE "Find someone you know is under construction." TO WS-OUTPUT-LINE
                           PERFORM 8000-WRITE-OUTPUT
                       WHEN "5"
                           PERFORM 6000-SKILLS-MENU
                       WHEN "6"
                           EXIT PERFORM
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
*> *      *> 7000-CREATE-EDIT-PROFILE: Create or edit user profile         *
*> *      *> USER STORY (Epic 2): Profile creation and editing             *
*> *      *>*****************************************************************
       7000-CREATE-EDIT-PROFILE.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           IF WS-CURRENT-PROFILE-INDEX > 0 AND
               WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX) = 1
               MOVE "=== EDIT MY PROFILE ===" TO WS-OUTPUT-LINE
           ELSE
               MOVE "=== CREATE MY PROFILE ===" TO WS-OUTPUT-LINE
           END-IF.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 7200-GET-REQUIRED-FIELDS.

           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           PERFORM 7300-GET-OPTIONAL-FIELDS.

           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           PERFORM 7400-SAVE-PROFILE-DATA.
           PERFORM 4650-WRITE-PROFILES-FILE.

           MOVE "Profile saved successfully!" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7100-VIEW-PROFILE: Display user's profile                     *
*> *      *> USER STORY (Epic 2): View profile information                 *
*> *      *>*****************************************************************
       7100-VIEW-PROFILE.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           IF WS-CURRENT-PROFILE-INDEX = 0 OR
               WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX) = 0
               MOVE "You have not created a profile yet."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Please use 'Create/Edit My Profile' option first."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF.

           MOVE "=== YOUR PROFILE ===" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 7110-DISPLAY-BASIC-INFO.

           IF WS-ABOUT-ME(WS-CURRENT-PROFILE-INDEX) NOT = SPACES
               PERFORM 7120-DISPLAY-ABOUT-ME
           END-IF.

           IF WS-EXP-COUNT(WS-CURRENT-PROFILE-INDEX) > 0
               PERFORM 7130-DISPLAY-EXPERIENCE
           END-IF.

           IF WS-EDU-COUNT(WS-CURRENT-PROFILE-INDEX) > 0
               PERFORM 7140-DISPLAY-EDUCATION
           END-IF.

           MOVE "--------------------" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7110-DISPLAY-BASIC-INFO: Display required profile fields      *
*> *      *>*****************************************************************
       7110-DISPLAY-BASIC-INFO.
           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "Name: "
               FUNCTION TRIM(WS-FIRST-NAME(WS-CURRENT-PROFILE-INDEX))
               " "
               FUNCTION TRIM(WS-LAST-NAME(WS-CURRENT-PROFILE-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "University: "
               FUNCTION TRIM(WS-UNIVERSITY(WS-CURRENT-PROFILE-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "Major: "
               FUNCTION TRIM(WS-MAJOR(WS-CURRENT-PROFILE-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "Graduation Year: "
               WS-GRAD-YEAR(WS-CURRENT-PROFILE-INDEX)
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7120-DISPLAY-ABOUT-ME: Display About Me section               *
*> *      *>*****************************************************************
       7120-DISPLAY-ABOUT-ME.
           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "About Me: "
               FUNCTION TRIM(WS-ABOUT-ME(WS-CURRENT-PROFILE-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7130-DISPLAY-EXPERIENCE: Display all experience entries       *
*> *      *>*****************************************************************
       7130-DISPLAY-EXPERIENCE.
           MOVE "Experience:" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX >
                   WS-EXP-COUNT(WS-CURRENT-PROFILE-INDEX)

               PERFORM 7131-DISPLAY-SINGLE-EXPERIENCE
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7131-DISPLAY-SINGLE-EXPERIENCE: Display one experience entry  *
*> *      *>*****************************************************************
       7131-DISPLAY-SINGLE-EXPERIENCE.
           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Title: "
               FUNCTION TRIM(WS-EXP-TITLE(WS-CURRENT-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Company/Organization: "
               FUNCTION TRIM(WS-EXP-COMPANY(WS-CURRENT-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Dates: "
               FUNCTION TRIM(WS-EXP-DATES(WS-CURRENT-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           IF WS-EXP-DESC(WS-CURRENT-PROFILE-INDEX, WS-DISPLAY-INDEX)
               NOT = SPACES
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "  Description: "
                   FUNCTION TRIM(WS-EXP-DESC(WS-CURRENT-PROFILE-INDEX,
                       WS-DISPLAY-INDEX))
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7140-DISPLAY-EDUCATION: Display all education entries         *
*> *      *>*****************************************************************
       7140-DISPLAY-EDUCATION.
           MOVE "Education:" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX >
                   WS-EDU-COUNT(WS-CURRENT-PROFILE-INDEX)

               PERFORM 7141-DISPLAY-SINGLE-EDUCATION
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7141-DISPLAY-SINGLE-EDUCATION: Display one education entry    *
*> *      *>*****************************************************************
       7141-DISPLAY-SINGLE-EDUCATION.
           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Degree: "
               FUNCTION TRIM(WS-EDU-DEGREE(WS-CURRENT-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  University: "
               FUNCTION TRIM(WS-EDU-UNIVERSITY(WS-CURRENT-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Years: "
               FUNCTION TRIM(WS-EDU-YEARS(WS-CURRENT-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7200-GET-REQUIRED-FIELDS: Collect required profile fields     *
*> *      *>*****************************************************************
       7200-GET-REQUIRED-FIELDS.
           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter First Name: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-FIRST-NAME
                   IF WS-TEMP-FIRST-NAME = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-FIRST-NAME TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Last Name: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-LAST-NAME
                   IF WS-TEMP-LAST-NAME = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-LAST-NAME TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter University/College Attended: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-UNIVERSITY
                   IF WS-TEMP-UNIVERSITY = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-UNIVERSITY TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Major: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-MAJOR
                   IF WS-TEMP-MAJOR = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-MAJOR TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 7210-GET-VALID-GRAD-YEAR.

*> *      *>*****************************************************************
*> *      *> 7210-GET-VALID-GRAD-YEAR: Get and validate graduation year    *
*> *      *>*****************************************************************
       7210-GET-VALID-GRAD-YEAR.
           MOVE 0 TO WS-YEAR-VALID.

           PERFORM UNTIL WS-YEAR-VALID = 1
               MOVE "Enter Graduation Year (YYYY): " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-TEMP-GRAD-YEAR
               MOVE WS-TEMP-GRAD-YEAR TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 7220-VALIDATE-YEAR

               IF WS-YEAR-VALID = 0
                   MOVE "Invalid year. Must be 4-digit year (1950-2050)."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               END-IF
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7220-VALIDATE-YEAR: Validate graduation year format           *
*> *      *>*****************************************************************
       7220-VALIDATE-YEAR.
           MOVE 0 TO WS-YEAR-VALID.
           MOVE 1 TO WS-YEAR-NUMERIC.

           IF FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-GRAD-YEAR)) NOT = 4
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-YEAR-INDEX FROM 1 BY 1
               UNTIL WS-YEAR-INDEX > 4
               MOVE WS-TEMP-GRAD-YEAR(WS-YEAR-INDEX:1) TO WS-TEMP-CHAR
               IF WS-TEMP-CHAR < "0" OR WS-TEMP-CHAR > "9"
                   MOVE 0 TO WS-YEAR-NUMERIC
               END-IF
           END-PERFORM.

           IF WS-YEAR-NUMERIC = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-TEMP-GRAD-YEAR TO WS-TEMP-YEAR.
           IF WS-TEMP-YEAR >= 1950 AND WS-TEMP-YEAR <= 2050
               MOVE 1 TO WS-YEAR-VALID
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7300-GET-OPTIONAL-FIELDS: Collect optional profile fields     *
*> *      *>*****************************************************************
       7300-GET-OPTIONAL-FIELDS.
           MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):"
               TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           MOVE INPUT-RECORD TO WS-TEMP-ABOUT-ME.
           IF WS-TEMP-ABOUT-ME NOT = SPACES
               MOVE WS-TEMP-ABOUT-ME TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE "(skipped)" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

           PERFORM 7310-GET-EXPERIENCE-ENTRIES.

           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           PERFORM 7320-GET-EDUCATION-ENTRIES.

*> *      *>*****************************************************************
*> *      *> 7310-GET-EXPERIENCE-ENTRIES: Collect experience entries       *
*> *      *>*****************************************************************
       7310-GET-EXPERIENCE-ENTRIES.
           MOVE 0 TO WS-EXP-LOOP-INDEX.
           MOVE "ADD" TO WS-CONTINUE-ADDING.

           PERFORM UNTIL WS-EXP-LOOP-INDEX >= 3
               OR WS-CONTINUE-ADDING = "DONE"
               OR WS-EOF-FLAG = 1

               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-CONTINUE-ADDING
               MOVE WS-CONTINUE-ADDING TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF WS-CONTINUE-ADDING = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-EXP-LOOP-INDEX
               PERFORM 7311-GET-SINGLE-EXPERIENCE
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7311-GET-SINGLE-EXPERIENCE: Collect one experience entry      *
*> *      *>*****************************************************************
       7311-GET-SINGLE-EXPERIENCE.
           STRING "Experience #" WS-EXP-LOOP-INDEX " - Title: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-TITLE(WS-EXP-LOOP-INDEX).
           MOVE WS-TEMP-EXP-TITLE(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Experience #" WS-EXP-LOOP-INDEX
               " - Company/Organization: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-COMPANY(WS-EXP-LOOP-INDEX).
           MOVE WS-TEMP-EXP-COMPANY(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Experience #" WS-EXP-LOOP-INDEX
               " - Dates (e.g., Summer 2024): "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-DATES(WS-EXP-LOOP-INDEX).
           MOVE WS-TEMP-EXP-DATES(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Experience #" WS-EXP-LOOP-INDEX
               " - Description (optional, max 100 chars,"
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "blank to skip): " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX).
           IF WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX) NOT = SPACES
               MOVE WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE "(skipped)" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7320-GET-EDUCATION-ENTRIES: Collect education entries         *
*> *      *>*****************************************************************
       7320-GET-EDUCATION-ENTRIES.
           MOVE 0 TO WS-EDU-LOOP-INDEX.
           MOVE "ADD" TO WS-CONTINUE-ADDING.

           PERFORM UNTIL WS-EDU-LOOP-INDEX >= 3
               OR WS-CONTINUE-ADDING = "DONE"
               OR WS-EOF-FLAG = 1

               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-CONTINUE-ADDING
               MOVE WS-CONTINUE-ADDING TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF WS-CONTINUE-ADDING = "DONE"
                   EXIT PERFORM
               END-IF

      *>       If the user did not enter "DONE", proceed to get education entry -> this encompasses blank lines and other inputs.

               ADD 1 TO WS-EDU-LOOP-INDEX
               PERFORM 7321-GET-SINGLE-EDUCATION
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7321-GET-SINGLE-EDUCATION: Collect one education entry        *
*> *      *>*****************************************************************
       7321-GET-SINGLE-EDUCATION.
           STRING "Education #" WS-EDU-LOOP-INDEX " - Degree: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EDU-DEGREE(WS-EDU-LOOP-INDEX).
           MOVE WS-TEMP-EDU-DEGREE(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Education #" WS-EDU-LOOP-INDEX
               " - University/College: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EDU-UNIVERSITY(WS-EDU-LOOP-INDEX).
           MOVE WS-TEMP-EDU-UNIVERSITY(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Education #" WS-EDU-LOOP-INDEX
               " - Years Attended (e.g., 2023-2025): "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EDU-YEARS(WS-EDU-LOOP-INDEX).
           MOVE WS-TEMP-EDU-YEARS(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7400-SAVE-PROFILE-DATA: Save profile data to memory           *
*> *      *>*****************************************************************
       7400-SAVE-PROFILE-DATA.
           IF WS-CURRENT-PROFILE-INDEX = 0
               ADD 1 TO WS-PROFILE-COUNT
               MOVE WS-PROFILE-COUNT TO WS-CURRENT-PROFILE-INDEX
               MOVE WS-USERNAME(WS-CURRENT-USER-INDEX) TO
                   WS-PROF-USERNAME(WS-CURRENT-PROFILE-INDEX)
           END-IF.

           MOVE 1 TO WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX).

           MOVE WS-TEMP-FIRST-NAME TO
               WS-FIRST-NAME(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-LAST-NAME TO
               WS-LAST-NAME(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-UNIVERSITY TO
               WS-UNIVERSITY(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-MAJOR TO
               WS-MAJOR(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-GRAD-YEAR TO
               WS-GRAD-YEAR(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-ABOUT-ME TO
               WS-ABOUT-ME(WS-CURRENT-PROFILE-INDEX).

           MOVE WS-EXP-LOOP-INDEX TO
               WS-EXP-COUNT(WS-CURRENT-PROFILE-INDEX).
           PERFORM 7410-SAVE-EXPERIENCE-ENTRIES.

           MOVE WS-EDU-LOOP-INDEX TO
               WS-EDU-COUNT(WS-CURRENT-PROFILE-INDEX).
           PERFORM 7420-SAVE-EDUCATION-ENTRIES.

*> *      *>*****************************************************************
*> *      *> 7410-SAVE-EXPERIENCE-ENTRIES: Copy experience to profile      *
*> *      *>*****************************************************************
       7410-SAVE-EXPERIENCE-ENTRIES.
           PERFORM VARYING WS-SAVE-INDEX FROM 1 BY 1
               UNTIL WS-SAVE-INDEX > WS-EXP-LOOP-INDEX
               MOVE WS-TEMP-EXP-TITLE(WS-SAVE-INDEX) TO
                   WS-EXP-TITLE(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EXP-COMPANY(WS-SAVE-INDEX) TO
                   WS-EXP-COMPANY(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EXP-DATES(WS-SAVE-INDEX) TO
                   WS-EXP-DATES(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EXP-DESC(WS-SAVE-INDEX) TO
                   WS-EXP-DESC(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7420-SAVE-EDUCATION-ENTRIES: Copy education to profile        *
*> *      *>*****************************************************************
       7420-SAVE-EDUCATION-ENTRIES.
           PERFORM VARYING WS-SAVE-INDEX FROM 1 BY 1
               UNTIL WS-SAVE-INDEX > WS-EDU-LOOP-INDEX
               MOVE WS-TEMP-EDU-DEGREE(WS-SAVE-INDEX) TO
                   WS-EDU-DEGREE(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EDU-UNIVERSITY(WS-SAVE-INDEX) TO
                   WS-EDU-UNIVERSITY(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EDU-YEARS(WS-SAVE-INDEX) TO
                   WS-EDU-YEARS(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
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


