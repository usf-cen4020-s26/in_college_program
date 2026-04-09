*>*****************************************************************
      *> PROGRAM:     INCOLLEGE
      *> DESCRIPTION: LinkedIn-style college networking application.
      *>              Supports account creation, login, profiles, job
      *>              search, connections, skills, messaging, and more.
      *>              Completed through Epic 9 (Messaging).
      *>
      *> FILES:
      *>   INPUT.TXT        - Sequential input commands (test/automation)
      *>   OUTPUT.TXT       - Sequential output log (test verification)
      *>   ACCOUNTS.DAT     - User credentials (username + password)
      *>   PROFILES.DAT     - User profiles (bio, experience, education)
      *>   PENDING.DAT      - Pending connection requests
      *>   CONNECTIONS.DAT  - Established connections between users
      *>   JOBS.DAT         - Job postings
      *>   APPLICATIONS.DAT - Job applications submitted by users
      *>   MESSAGES.DAT     - Messages sent between connected users
      *>
      *> WORKING-STORAGE COPYBOOKS:
      *>   WS-CONSTANTS     - File status codes and app-wide constants
      *>   WS-IO-CONTROL    - Input/output control flags and buffers
      *>   WS-ACCOUNTS      - In-memory accounts table (up to 10 users)
      *>   WS-PROFILES      - In-memory profiles table
      *>   WS-CONNECTIONS   - In-memory connections table
      *>   WS-JOBS          - In-memory jobs table and counters
      *>   WS-MESSAGES      - Messaging state variables and flags
      *>
      *> PROCEDURE COPYBOOKS:
      *>   DATALOAD         - Load all DAT files into working storage
      *>   AUTH             - Login and account creation (3000, 4000)
      *>   PROFILE          - Create/edit/view profile (7000, 7100)
      *>   SEARCH           - Job search menu (5300)
      *>   SKILLS           - Skills menu (6000)
      *>   CONNMGMT         - Connection management (7500)
      *>   CONNWRITE        - Write connections to file
      *>   NETWORK          - View network list (7700)
      *>   SENDREQ          - Send connection requests
      *>   JOBS             - Job posting and management (5350)
      *>   SENDMESSAGE      - Messages menu and send message (7800, 7810)
      *>   VIEWMESSAGE      - View messages (7840, 7841)
      *>   APPLYJOB         - Apply to jobs
      *>   VIEWAPPS         - View applications
      *>   JOBSIO           - Jobs file I/O
      *>   VIEWREQ          - View connection requests
      *>
      *> MAIN PROGRAM FLOW (0000-MAIN-PROGRAM):
      *>   1000-INITIALIZE        - Open files, load data, show banner
      *>   2000-PROCESS-APPLICATION - Pre-login menu (Login/Create/Exit)
      *>     3000-LOGIN-PROCESS   - Authenticate user
      *>     4000-CREATE-ACCOUNT  - Register new user
      *>     5000-POST-LOGIN-MENU - Main menu (options 1-9):
      *>       1. Create/Edit My Profile  -> 7000-CREATE-EDIT-PROFILE
      *>       2. View My Profile         -> 7100-VIEW-PROFILE
      *>       3. Search for a job        -> 5300-JOB-SEARCH-MENU
      *>       4. Find someone you know   -> 7500-FIND-SOMEONE-YOU-KNOW
      *>       5. View Pending Requests   -> 7500-VIEW-PENDING-REQUESTS
      *>       6. Learn a new skill       -> 6000-SKILLS-MENU
      *>       7. View My Network         -> 7700-VIEW-NETWORK-LIST
      *>       8. Messages                -> 7800-MESSAGES-MENU
      *>       9. Logout
      *>   9000-TERMINATE         - Close files, display goodbye
      *>*****************************************************************
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
     SELECT PENDING-FILE ASSIGN TO "PENDING.DAT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-PENDING-STATUS.
     SELECT CONNECTIONS-FILE ASSIGN TO "CONNECTIONS.DAT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-CONNECTIONS-STATUS.
     SELECT JOBS-FILE ASSIGN TO "JOBS.DAT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-JOBS-STATUS.
     SELECT APPLICATIONS-FILE ASSIGN TO "APPLICATIONS.DAT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-APPS-STATUS.
     SELECT MESSAGES-FILE ASSIGN TO "MESSAGES.DAT"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-MESSAGES-STATUS.
DATA DIVISION.
FILE SECTION.
FD  INPUT-FILE.
01  INPUT-RECORD                PIC X(500).

FD  OUTPUT-FILE.
01  OUTPUT-RECORD               PIC X(500).

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
FD  PENDING-FILE.
01  PENDING-REC.
    05  PEND-SENDER-USERNAME     PIC X(20).
    05  PEND-RECIPIENT-USERNAME  PIC X(20).
    05  PEND-STATUS              PIC X(1).
FD  CONNECTIONS-FILE.
01  CONNECTION-REC.
    05  CONN-USER-A            PIC X(20).
    05  CONN-USER-B            PIC X(20).
FD  JOBS-FILE.
01  JOB-RECORD.
    05  JOB-ID                 PIC 9(5).
    05  JOB-POSTER             PIC X(20).
    05  JOB-TITLE              PIC X(50).
    05  JOB-DESCRIPTION        PIC X(200).
    05  JOB-EMPLOYER           PIC X(50).
    05  JOB-LOCATION           PIC X(50).
    05  JOB-SALARY             PIC X(20).

FD  APPLICATIONS-FILE.
01  APP-RECORD.
    05  APP-USERNAME           PIC X(20).
    05  APP-JOB-ID             PIC 9(5).
    05  APP-JOB-TITLE          PIC X(50).
    05  APP-JOB-EMPLOYER       PIC X(50).
    05  APP-JOB-LOCATION       PIC X(50).

FD  MESSAGES-FILE.
01  MSG-RECORD.
    05  MSG-ID                 PIC 9(5).
    05  MSG-SENDER             PIC X(20).
    05  MSG-RECIPIENT          PIC X(20).
    05  MSG-CONTENT            PIC X(200).
    05  MSG-TIMESTAMP          PIC X(20).

WORKING-STORAGE SECTION.
       COPY WS-CONSTANTS.
       COPY WS-IO-CONTROL.
       COPY WS-ACCOUNTS.
       COPY WS-PROFILES.
       COPY WS-CONNECTIONS.
       COPY WS-JOBS.
       COPY WS-MESSAGES.


PROCEDURE DIVISION.
       0000-MAIN-PROGRAM.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-APPLICATION
               UNTIL WS-PROGRAM-RUNNING = 0.
           PERFORM 9000-TERMINATE.
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT INPUT-FILE.

           IF WS-INPUT-STATUS NOT = WS-CONST-FS-OK
              EVALUATE WS-INPUT-STATUS
                 WHEN WS-CONST-FS-NOT-FOUND
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
           PERFORM 9200-LOAD-PENDING-REQUESTS.
           PERFORM 9250-LOAD-CONNECTIONS.
           PERFORM 5350-LOAD-JOBS.
           PERFORM 5360-LOAD-APPLICATIONS.
           PERFORM 9270-LOAD-NEXT-MSG-ID.

           MOVE "========================================"
           TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "     WELCOME TO INCOLLEGE" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "========================================"
               TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

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

           MOVE SPACES TO WS-MENU-CHOICE.
           PERFORM UNTIL FUNCTION TRIM(WS-MENU-CHOICE) NOT = SPACES
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PARAGRAPH
               END-IF
               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MENU-CHOICE
           END-PERFORM.

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
*> *      *> 5000-POST-LOGIN-MENU: Main menu after successful login        *
*> *      *>*****************************************************************
       5000-POST-LOGIN-MENU.
           MOVE "1" TO WS-MAIN-MENU-CHOICE.

           PERFORM UNTIL WS-MAIN-MENU-CHOICE = "9"
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
                   MOVE "5. View Pending Connection Requests" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "6. Learn a new skill" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "7. View My Network" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "8. Messages" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "9. Logout" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "Enter choice (1-9): " TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT

                   IF WS-SKIP-NEXT-MENU-READ = "Y"
                       MOVE "N" TO WS-SKIP-NEXT-MENU-READ
                       MOVE WS-PRELOADED-MENU-CHOICE TO WS-MAIN-MENU-CHOICE
                   ELSE
                       PERFORM 8100-READ-INPUT
                       IF WS-EOF-FLAG = 1
                           MOVE 0 TO WS-PROGRAM-RUNNING
                           EXIT PERFORM
                       END-IF
                       MOVE FUNCTION TRIM(INPUT-RECORD)
                           TO WS-MAIN-MENU-CHOICE
                   END-IF
                   MOVE WS-MAIN-MENU-CHOICE TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT

                   EVALUATE WS-MAIN-MENU-CHOICE
                       WHEN "1"
                           PERFORM 7000-CREATE-EDIT-PROFILE
                       WHEN "2"
                           PERFORM 7100-VIEW-PROFILE
                       WHEN "3"
                           PERFORM 5300-JOB-SEARCH-MENU
                       WHEN "4"
                           PERFORM 7500-FIND-SOMEONE-YOU-KNOW
                       WHEN "5"
                           PERFORM 7500-VIEW-PENDING-REQUESTS
                       WHEN "6"
                           PERFORM 6000-SKILLS-MENU
                       WHEN "7"
                           PERFORM 7700-VIEW-NETWORK-LIST
                       WHEN "8"
                           PERFORM 7800-MESSAGES-MENU
                       WHEN "9"
                           EXIT PERFORM
                       WHEN OTHER
                          MOVE "Invalid choice. Please try again."
                          TO WS-OUTPUT-LINE
                          PERFORM 8000-WRITE-OUTPUT
                   END-EVALUATE
           END-PERFORM.


*> *      *>*****************************************************************
*> *      *> 8000-WRITE-OUTPUT: Output to both screen and file             *
*> *      *> Implements requirement to display AND preserve output         *
*> *      *>*****************************************************************
       8000-WRITE-OUTPUT.
           DISPLAY FUNCTION TRIM(WS-OUTPUT-LINE TRAILING).
           WRITE OUTPUT-RECORD FROM WS-OUTPUT-LINE.

*> *      *>*****************************************************************
*> *      *> 8100-READ-INPUT: Read from input file (or pushback buffer)   *
*> *      *>*****************************************************************
       8100-READ-INPUT.
           IF WS-INPUT-PUSHBACK-FLAG = "Y"
               MOVE WS-INPUT-PUSHBACK-LINE TO INPUT-RECORD
               MOVE "N" TO WS-INPUT-PUSHBACK-FLAG
               MOVE 0 TO WS-EOF-FLAG
               EXIT
           END-IF.
           READ INPUT-FILE
               AT END
                   MOVE 1 to WS-EOF-FLAG
                   MOVE SPACES TO INPUT-RECORD
           END-READ.
           EXIT.


       COPY DATALOAD.
       COPY AUTH.
       COPY PROFILE.
       COPY SEARCH.
       COPY SKILLS.
       COPY CONNMGMT.
       COPY CONNWRITE.
       COPY NETWORK.
       COPY SENDREQ.
       COPY JOBS.
       COPY SENDMESSAGE.
       COPY VIEWMESSAGE.
       COPY APPLYJOB.
       COPY VIEWAPPS.
       COPY JOBSIO.
       COPY VIEWREQ.
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


