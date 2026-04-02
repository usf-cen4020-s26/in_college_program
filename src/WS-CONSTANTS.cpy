      *> ============================================================
      *> WS-CONSTANTS.cpy - Named constants for InCollege application
      *> ============================================================
      01  WS-CONST-MAX-ACCOUNTS          PIC 9     VALUE 5.
      01  WS-CONST-MAX-PROFILES          PIC 9     VALUE 5.
      01  WS-CONST-MAX-PENDING           PIC 99    VALUE 50.
      01  WS-CONST-MAX-CONNECTIONS       PIC 99    VALUE 50.
      01  WS-CONST-MAX-JOBS              PIC 999   VALUE 25.
      01  WS-CONST-MAX-EXPERIENCES       PIC 9     VALUE 3.
      01  WS-CONST-MAX-EDUCATIONS        PIC 9     VALUE 3.
      01  WS-CONST-MAX-APPLICATIONS      PIC 999   VALUE 25.

      *> File status codes
      01  WS-CONST-FS-OK                 PIC XX    VALUE "00".
      01  WS-CONST-FS-NOT-FOUND          PIC XX    VALUE "35".
      01  WS-CONST-FS-OPEN-OK            PIC XX    VALUE "97".

      *> Boolean flag values
      01  WS-CONST-YES                   PIC X     VALUE "Y".
      01  WS-CONST-NO                    PIC X     VALUE "N".
      01  WS-CONST-TRUE                  PIC 9     VALUE 1.
      01  WS-CONST-FALSE                 PIC 9     VALUE 0.

      *> Status sentinel values
      01  WS-CONST-PENDING-STATUS        PIC X     VALUE "P".
