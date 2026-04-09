*>*****************************************************************
      *> FILE:    WS-MESSAGES.cpy
      *> PURPOSE: Working-storage variables for send-message and
      *>          view-messages flows (Epics 8 and 9). No in-memory
      *>          message table — MESSAGES.DAT is read on demand.
      *>
      *> VARIABLES:
      *>   WS-MESSAGES-STATUS     - File status for MESSAGES.DAT
      *>
      *>   --- Send Message ---
      *>   WS-MSG-MENU-CHOICE     - User's choice in Messages submenu
      *>   WS-MSG-RECIPIENT       - Recipient username entered by user
      *>   WS-MSG-CONTENT         - Message body (max 200 chars)
      *>   WS-MSG-TIMESTAMP       - Formatted timestamp (YYYY-MM-DD HH:MM:SS)
      *>   WS-MSG-CONN-FOUND      - 1 if recipient is a confirmed connection
      *>   WS-MSG-USER-EXISTS     - 1 if recipient username exists in system
      *>   WS-MSG-CURRENT-DATE    - Raw FUNCTION CURRENT-DATE output (PIC X(21))
      *>   WS-MSG-NEXT-ID         - Auto-incrementing ID; set by 9270 at startup
      *>
      *>   --- View Messages ---
      *>   WS-MSG-FOUND           - 1 if at least one message was displayed
      *>   WS-VIEW-MSG-EOF        - "Y" when MESSAGES.DAT read loop is done
      *>
      *> USED BY: SENDMESSAGE.cpy, VIEWMESSAGE.cpy, DATALOAD.cpy
      *>*****************************************************************
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
      01  WS-MSG-FOUND           PIC 9 VALUE 0.
      01  WS-VIEW-MSG-EOF             PIC X VALUE "N".
