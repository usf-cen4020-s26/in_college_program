*>*****************************************************************
      *> FILE:    WS-JOBS.cpy
      *> PURPOSE: In-memory job postings table, application table, and
      *>          all working-storage for job/application flows.
      *>          Populated from JOBS.DAT and APPLICATIONS.DAT at
      *>          startup by JOBSIO.cpy.
      *>
      *> VARIABLES:
      *>   --- Job Postings ---
      *>   WS-JOBS-STATUS          - File status for JOBS.DAT
      *>   WS-JOBS-EOF             - "Y" when JOBS.DAT read is done
      *>   WS-JOB-COUNT            - Number of job postings loaded
      *>   WS-JOB-ID-COUNTER       - Highest JOB-ID seen; next post = +1
      *>   WS-JOB-WRITE-SUCCESS    - 1 if last job write succeeded
      *>   WS-TEMP-JOB-TITLE/DESC/EMPLOYER/LOCATION/SALARY - Input buffers
      *>   WS-JOB-TABLE            - In-memory table (OCCURS 25)
      *>     WS-JT-ID(n)           - Job ID (PIC 9(5))
      *>     WS-JT-POSTER(n)       - Posting username (PIC X(20))
      *>     WS-JT-TITLE(n)        - Job title (PIC X(50))
      *>     WS-JT-DESCRIPTION(n)  - Description (PIC X(200))
      *>     WS-JT-EMPLOYER(n)     - Employer (PIC X(50))
      *>     WS-JT-LOCATION(n)     - Location (PIC X(50))
      *>     WS-JT-SALARY(n)       - Salary string (PIC X(20))
      *>   WS-SELECTED-JOB-IDX     - Index of job chosen in browse view
      *>   WS-BROWSE-CHOICE        - User's numeric choice in browse loop
      *>
      *>   --- Job Applications ---
      *>   WS-APPS-STATUS          - File status for APPLICATIONS.DAT
      *>   WS-APPS-EOF             - "Y" when APPLICATIONS.DAT read is done
      *>   WS-APP-COUNT            - Number of applications loaded
      *>   WS-APP-FOUND            - 1 if duplicate application detected
      *>*****************************************************************

      *> ===== Job Postings working-storage =====
      01  WS-JOBS-STATUS              PIC XX.
      01  WS-JOBS-EOF                 PIC X VALUE "N".
      01  WS-JOB-COUNT                PIC 999 VALUE 0.
      01  WS-JOB-ID-COUNTER           PIC 9(5) VALUE 0.
      01  WS-JOB-WRITE-SUCCESS        PIC 9 VALUE 0.
      01  WS-TEMP-JOB-TITLE           PIC X(50).
      01  WS-TEMP-JOB-DESC            PIC X(200).
      01  WS-TEMP-JOB-EMPLOYER        PIC X(50).
      01  WS-TEMP-JOB-LOCATION        PIC X(50).
      01  WS-TEMP-JOB-SALARY          PIC X(20).

      *> ===== In-Memory Job Table (populated by 5355-READ-JOBS-LOOP) =====
      01  WS-JOB-TABLE.
          05  WS-JOB-ENTRY OCCURS 25 TIMES.
              10  WS-JT-ID           PIC 9(5).
              10  WS-JT-POSTER       PIC X(20).
              10  WS-JT-TITLE        PIC X(50).
              10  WS-JT-DESC         PIC X(200).
              10  WS-JT-EMPLOYER     PIC X(50).
              10  WS-JT-LOCATION     PIC X(50).
              10  WS-JT-SALARY       PIC X(20).

      *> ===== Browse/Select variables =====
      01  WS-BROWSE-IDX              PIC 999 VALUE 0.
      01  WS-BROWSE-CHOICE           PIC 999 VALUE 0.
      01  WS-SELECTED-JOB-IDX        PIC 999 VALUE 0.
      01  WS-DETAIL-CHOICE           PIC X(2).

      *> ===== Applications file variables =====
      01  WS-APPS-STATUS             PIC XX.
      01  WS-APPS-EOF                PIC X VALUE "N".
      01  WS-APPS-WRITE-SUCCESS      PIC 9 VALUE 0.
      01  WS-APP-COUNT               PIC 999 VALUE 0.
      01  WS-APP-FOUND               PIC 9 VALUE 0.
      01  WS-DISPLAY-NUM             PIC Z(2)9.
      01  WS-DISP-ALPHANUM           PIC X(3) VALUE SPACES.
      01  WS-NUM-DISP-STR            PIC X(5) VALUE SPACES.
