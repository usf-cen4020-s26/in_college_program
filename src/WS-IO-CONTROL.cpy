*>*****************************************************************
      *> FILE:    WS-IO-CONTROL.cpy
      *> PURPOSE: I/O control flags, menu choice buffers, file status
      *>          variables, and program lifecycle state.
      *>
      *> VARIABLES:
      *>   WS-MENU-CHOICE            - Pre-login menu selection buffer
      *>   WS-MAIN-MENU-CHOICE       - Post-login main menu selection buffer
      *>   WS-SKIP-NEXT-MENU-READ    - "Y" = use WS-PRELOADED-MENU-CHOICE
      *>                               instead of reading next input line
      *>   WS-PRELOADED-MENU-CHOICE  - Saved choice for pushback use
      *>                               (set by VIEWREQ view-only mode)
      *>   WS-SKILL-CHOICE           - Skills submenu selection buffer
      *>   WS-JOB-MENU-CHOICE        - Job Search submenu selection buffer
      *>   WS-INPUT-STATUS           - File status for INPUT.TXT
      *>   WS-OUTPUT-STATUS          - File status for OUTPUT.TXT
      *>   WS-ACCOUNTS-STATUS        - File status for ACCOUNTS.DAT
      *>   WS-PROFILES-STATUS        - File status for PROFILES.DAT
      *>   WS-PENDING-STATUS         - File status for PENDING.DAT
      *>   WS-PENDING-EOF            - EOF flag for PENDING.DAT reads
      *>   WS-EOF-FLAG               - 1 = INPUT.TXT exhausted
      *>   WS-PROGRAM-RUNNING        - 1 = continue loop; 0 = terminate
      *>   WS-OUTPUT-LINE            - Single output line buffer (PIC X(500))
      *>   WS-INPUT-PUSHBACK-FLAG    - "Y" = return pushback line on next read
      *>   WS-INPUT-PUSHBACK-LINE    - Saved line for pushback buffer
      *>   WS-CURRENT-USER-INDEX     - Index of logged-in user in accounts table
      *>   WS-VALID                  - 1 = valid, 0 = invalid; used by
      *>                               SENDREQ duplicate checking
      *>
      *> USED BY: All procedure copybooks and main.cob
      *>*****************************************************************
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

      01  WS-CURRENT-USER-INDEX       PIC 9 VALUE 0.



      01  WS-VALID                    PIC 9 VALUE 0.


      01  WS-OUTPUT-LINE              PIC X(500).
