      *> ============================================================
      *> WS-MESSAGES.cpy - Messaging state and content variables
      *> ============================================================
      01  WS-MESSAGES-STATUS         PIC XX.

      *> ===== Messaging working-storage =====
      01  WS-MSG-MENU-CHOICE          PIC X VALUE SPACES.
      01  WS-MSG-RECIPIENT            PIC X(20) VALUE SPACES.
      01  WS-MSG-CONTENT              PIC X(200) VALUE SPACES.
      01  WS-MSG-TIMESTAMP            PIC X(20) VALUE SPACES.
      01  WS-MSG-CONN-FOUND           PIC 9 VALUE 0.
      01  WS-MSG-USER-EXISTS          PIC 9 VALUE 0.
      01  WS-MSG-CURRENT-DATE         PIC X(21).
      01  WS-MSG-NEXT-ID              PIC 9(5) VALUE 0.

      *> ===== View messages variables =====
      01  WS-VIEW-MSG-FOUND           PIC 9 VALUE 0.
      01  WS-VIEW-MSG-EOF             PIC X VALUE "N".
