      *> ============================================================
      *> WS-JOBS.cpy - Job table, temp job fields, application vars
      *> ============================================================

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
