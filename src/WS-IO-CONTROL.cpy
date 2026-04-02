      *> ============================================================
      *> WS-IO-CONTROL.cpy - I/O state, menu choices, program control
      *> ============================================================
01  WS-MENU-CHOICE              PIC X(2).
01  WS-MAIN-MENU-CHOICE         PIC X(2).
01  WS-SKIP-NEXT-MENU-READ      PIC X VALUE "N".
01  WS-PRELOADED-MENU-CHOICE   PIC X(2).
01  WS-SKILL-CHOICE             PIC X(2).
01  WS-JOB-MENU-CHOICE          PIC X(10).

01  WS-INPUT-STATUS             PIC XX.
01  WS-OUTPUT-STATUS            PIC XX.
01  WS-ACCOUNTS-STATUS          PIC XX.
01  WS-PROFILES-STATUS          PIC XX.
01  WS-PENDING-STATUS           PIC XX.
01  WS-PENDING-EOF              PIC 9 VALUE 0.
01  WS-EOF-FLAG                 PIC 9 VALUE 0.
01  WS-PROGRAM-RUNNING          PIC 9 VALUE 1.
*> Input pushback: when set, 8100 returns buffered line instead of reading
01  WS-INPUT-PUSHBACK-FLAG      PIC X VALUE "N".
01  WS-INPUT-PUSHBACK-LINE      PIC X(200).
01  WS-INPUT-PUSHBACK-TEMP     PIC X(200).

01  WS-CURRENT-USER-INDEX       PIC 9 VALUE 0.
01  WS-CURRENT-PROFILE-INDEX    PIC 9 VALUE 0.
01  WS-PROFILE-FOUND            PIC 9 VALUE 0.

01  WS-INPUT-VALID              PIC 9 VALUE 0.

01  WS-VALID                    PIC 9 VALUE 0.

01  WS-EOF-REACHED              PIC 9 VALUE 0.

01  WS-OUTPUT-LINE              PIC X(500).
