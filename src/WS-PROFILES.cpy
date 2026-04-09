*>*****************************************************************
      *> FILE:    WS-PROFILES.cpy
      *> PURPOSE: In-memory user profiles table, temporary input buffers
      *>          for profile editing, and user search variables.
      *>          Populated from PROFILES.DAT at startup by DATALOAD.cpy.
      *>
      *> VARIABLES:
      *>   WS-USER-PROFILES           - Table of up to 5 profiles (OCCURS 5)
      *>     WS-PROF-USERNAME(n)      - Username key (PIC X(20))
      *>     WS-HAS-PROFILE(n)        - 1 = filled, 0 = empty
      *>     WS-FIRST-NAME(n)         - First name (PIC X(30))
      *>     WS-LAST-NAME(n)          - Last name (PIC X(30))
      *>     WS-UNIVERSITY(n)         - University (PIC X(50))
      *>     WS-MAJOR(n)              - Major (PIC X(50))
      *>     WS-GRAD-YEAR(n)          - Graduation year (PIC X(4))
      *>     WS-ABOUT-ME(n)           - Optional bio (PIC X(200))
      *>     WS-EXP-COUNT(n)          - Experience entry count (0-3)
      *>     WS-EXPERIENCE(n,m)       - Experience sub-table (OCCURS 3):
      *>       WS-EXP-TITLE/COMPANY/DATES/DESC
      *>     WS-EDU-COUNT(n)          - Education entry count (0-3)
      *>     WS-EDUCATION(n,m)        - Education sub-table (OCCURS 3):
      *>       WS-EDU-DEGREE/UNIVERSITY/YEARS
      *>   WS-TEMP-FIRST/LAST/UNIVERSITY/MAJOR/GRAD-YEAR - Edit input buffers
      *>   WS-TEMP-ABOUT-ME           - About Me input buffer
      *>   WS-TEMP-EXP-TITLE/COMPANY/DATES/DESC - Experience input buffers
      *>   WS-TEMP-EDU-DEGREE/UNIVERSITY/YEARS  - Education input buffers
      *>   WS-SEARCH-NAME             - Full name entered in user search
      *>   WS-SEARCH-FIRST-NAME       - First name parsed from search
      *>   WS-SEARCH-LAST-NAME        - Last name parsed from search
      *>   WS-USER-FOUND              - 1 if search found a match
      *>   WS-SEARCH-FOUND-INDEX      - Profile table index of found user
      *>
      *> NOTE: OCCURS 5 is governed by WS-CONST-MAX-PROFILES in
      *>       WS-CONSTANTS.cpy — both must be kept in sync.
      *>*****************************************************************
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

      01  WS-SEARCH-NAME              PIC X(80).
      01  WS-SEARCH-FIRST-NAME        PIC X(30).
      01  WS-SEARCH-LAST-NAME         PIC X(30).
      01  WS-SEARCH-FOUND-INDEX       PIC 9 VALUE 0.
      01  WS-USER-FOUND               PIC 9 VALUE 0.
      01  WS-DISPLAY-PROFILE-INDEX    PIC 9 VALUE 0.
