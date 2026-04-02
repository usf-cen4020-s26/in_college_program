      *> ============================================================
      *> WS-PROFILES.cpy - Profile table, temp fields, search vars
      *> ============================================================
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

      01  WS-SEARCH-NAME              PIC X(80).
      01  WS-SEARCH-FIRST-NAME        PIC X(30).
      01  WS-SEARCH-LAST-NAME         PIC X(30).
      01  WS-SEARCH-FOUND-INDEX       PIC 9 VALUE 0.
      01  WS-USER-FOUND               PIC 9 VALUE 0.
      01  WS-DISPLAY-PROFILE-INDEX    PIC 9 VALUE 0.
