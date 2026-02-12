IDENTIFICATION DIVISION.
PROGRAM-ID. DATA-STORE.

*> *      *>*****************************************************************
*> *      *> DATA-STORE: Account/profile persistence handler              *
*> *      *> USER STORY 3: File persistence for accounts                   *
*> *      *> USER STORY (Epic 2): Profile persistence                      *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT ACCOUNTS-FILE ASSIGN TO "ACCOUNTS.DAT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-ACCOUNTS-STATUS-DS.
    SELECT PROFILES-FILE ASSIGN TO "PROFILES.DAT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-PROFILES-STATUS-DS.

DATA DIVISION.
FILE SECTION.
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
01  WS-ACCOUNTS-STATUS-DS       PIC XX.
01  WS-PROFILES-STATUS-DS       PIC XX.

LINKAGE SECTION.
01  LS-ACTION                   PIC X(10).
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-ACTION LS-CONTEXT.
    EVALUATE LS-ACTION
        WHEN "LOAD"
            PERFORM 1100-LOAD-ACCOUNTS
            PERFORM 1150-LOAD-PROFILES
        WHEN "SAVE-ACCT"
            PERFORM 4600-WRITE-ACCOUNTS-FILE
        WHEN "SAVE-PROF"
            PERFORM 4650-WRITE-PROFILES-FILE
        WHEN OTHER
            CONTINUE
    END-EVALUATE.
    GOBACK.

*> *      *>*****************************************************************
*> *      *> 1100-LOAD-ACCOUNTS: Load all accounts from ACCOUNTS.dat      *
*> *      *> USER STORY 3: File persistence implementation                 *
*> *      *>*****************************************************************
    1100-LOAD-ACCOUNTS.
        OPEN INPUT ACCOUNTS-FILE.
        IF WS-ACCOUNTS-STATUS-DS = "00" OR WS-ACCOUNTS-STATUS-DS = "97"
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
*> *      *> 1150-LOAD-PROFILES: Load all profiles from PROFILES.dat      *
*> *      *> USER STORY (Epic 2): Profile persistence                      *
*> *      *>*****************************************************************
    1150-LOAD-PROFILES.
        OPEN INPUT PROFILES-FILE.
        IF WS-PROFILES-STATUS-DS = "00" OR WS-PROFILES-STATUS-DS = "97"
            IF WS-PROFILES-STATUS-DS = "00"
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
