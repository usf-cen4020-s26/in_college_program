*> *      *>*****************************************************************
*> *      *> context.cpy: Shared working-storage context across modules    *
*> *      *>*****************************************************************
05  WS-USER-ACCOUNTS.
    10  WS-ACCOUNT OCCURS 5 TIMES.
        15  WS-USERNAME         PIC X(20).
        15  WS-PASSWORD         PIC X(12).

05  WS-USER-PROFILES.
    10  WS-PROFILE OCCURS 5 TIMES.
        15  WS-PROF-USERNAME    PIC X(20).
        15  WS-HAS-PROFILE      PIC 9.
        15  WS-FIRST-NAME       PIC X(30).
        15  WS-LAST-NAME        PIC X(30).
        15  WS-UNIVERSITY       PIC X(50).
        15  WS-MAJOR            PIC X(50).
        15  WS-GRAD-YEAR        PIC X(4).
        15  WS-ABOUT-ME         PIC X(200).
        15  WS-EXP-COUNT        PIC 9.
        15  WS-EXPERIENCE OCCURS 3 TIMES.
            20  WS-EXP-TITLE    PIC X(50).
            20  WS-EXP-COMPANY  PIC X(50).
            20  WS-EXP-DATES    PIC X(30).
            20  WS-EXP-DESC     PIC X(100).
        15  WS-EDU-COUNT        PIC 9.
        15  WS-EDUCATION OCCURS 3 TIMES.
            20  WS-EDU-DEGREE   PIC X(50).
            20  WS-EDU-UNIVERSITY PIC X(50).
            20  WS-EDU-YEARS    PIC X(20).

05  WS-ACCOUNT-COUNT            PIC 9 VALUE 0.
05  WS-MAX-ACCOUNTS             PIC 9 VALUE 5.
05  WS-PROFILE-COUNT            PIC 9 VALUE 0.

05  WS-LOGIN-USERNAME           PIC X(20).
05  WS-LOGIN-PASSWORD           PIC X(12).
05  WS-LOGIN-SUCCESS            PIC 9 VALUE 0.
05  WS-ACCOUNT-INDEX            PIC 9.

05  WS-PASSWORD-INPUT           PIC X(50).
05  WS-PASSWORD-LENGTH          PIC 99.
05  WS-HAS-CAPITAL              PIC 9 VALUE 0.
05  WS-HAS-DIGIT                PIC 9 VALUE 0.
05  WS-HAS-SPECIAL              PIC 9 VALUE 0.
05  WS-PASSWORD-VALID           PIC 9 VALUE 0.
05  WS-CHAR-INDEX               PIC 99.
05  WS-CURRENT-CHAR             PIC X.

05  WS-MENU-CHOICE              PIC X(2).
05  WS-MAIN-MENU-CHOICE         PIC X(2).
05  WS-SKILL-CHOICE             PIC X(2).

05  WS-INPUT-STATUS             PIC XX.
05  WS-OUTPUT-STATUS            PIC XX.
05  WS-ACCOUNTS-STATUS          PIC XX.
05  WS-PROFILES-STATUS          PIC XX.
05  WS-DATA-ACTION              PIC X(10).
05  WS-EOF-FLAG                 PIC 9 VALUE 0.
05  WS-PROGRAM-RUNNING          PIC 9 VALUE 1.

05  WS-CURRENT-USER-INDEX       PIC 9 VALUE 0.
05  WS-CURRENT-PROFILE-INDEX    PIC 9 VALUE 0.
05  WS-PROFILE-FOUND            PIC 9 VALUE 0.

05  WS-INPUT-VALID              PIC 9 VALUE 0.

05  WS-VALID                    PIC 9 VALUE 0.

05  WS-EOF-REACHED              PIC 9 VALUE 0.

05  WS-TEMP-FIRST-NAME          PIC X(30).
05  WS-TEMP-LAST-NAME           PIC X(30).
05  WS-TEMP-UNIVERSITY          PIC X(50).
05  WS-TEMP-MAJOR               PIC X(50).
05  WS-TEMP-GRAD-YEAR           PIC X(4).
05  WS-TEMP-ABOUT-ME            PIC X(200).

05  WS-EXP-LOOP-INDEX           PIC 9.
05  WS-EDU-LOOP-INDEX           PIC 9.
05  WS-TEMP-EXPERIENCES.
    10  WS-TEMP-EXP OCCURS 3 TIMES.
        15  WS-TEMP-EXP-TITLE       PIC X(50).
        15  WS-TEMP-EXP-COMPANY     PIC X(50).
        15  WS-TEMP-EXP-DATES       PIC X(30).
        15  WS-TEMP-EXP-DESC        PIC X(100).
05  WS-TEMP-EDUCATIONS.
    10  WS-TEMP-EDU OCCURS 3 TIMES.
        15  WS-TEMP-EDU-DEGREE      PIC X(50).
        15  WS-TEMP-EDU-UNIVERSITY  PIC X(50).
        15  WS-TEMP-EDU-YEARS       PIC X(20).
05  WS-CONTINUE-ADDING          PIC X(80).
05  WS-SAVE-INDEX               PIC 9.

05  WS-YEAR-VALID               PIC 9 VALUE 0.
05  WS-YEAR-NUMERIC             PIC 9 VALUE 0.
05  WS-TEMP-YEAR                PIC 9999.
05  WS-TEMP-CHAR                PIC X.
05  WS-YEAR-INDEX               PIC 9.

05  WS-DISPLAY-INDEX            PIC 9.
05  WS-PROFILE-EXISTS           PIC 9 VALUE 0.

05  WS-SEARCH-NAME              PIC X(80).
05  WS-SEARCH-FIRST-NAME        PIC X(30).
05  WS-SEARCH-LAST-NAME         PIC X(30).
05  WS-SEARCH-FOUND-INDEX       PIC 9 VALUE 0.
05  WS-USER-FOUND               PIC 9 VALUE 0.
05  WS-DISPLAY-PROFILE-INDEX    PIC 9 VALUE 0.

05  WS-INPUT-LINE               PIC X(500).
05  WS-OUTPUT-LINE              PIC X(500).
