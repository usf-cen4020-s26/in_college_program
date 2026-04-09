*>*****************************************************************
      *> FILE:    WS-CONNECTIONS.cpy
      *> PURPOSE: In-memory tables and variables for pending requests,
      *>          accepted connections, and network display.
      *>          Populated from PENDING.DAT and CONNECTIONS.DAT at
      *>          startup by DATALOAD.cpy.
      *>
      *> VARIABLES:
      *>   --- Pending Requests ---
      *>   WS-PENDING-COUNT          - Number of pending entries loaded
      *>   WS-PENDING-TABLE          - Up to 50 pending requests (OCCURS 50)
      *>     WS-PEND-SENDER-USERNAME(n)    - Sender username (PIC X(20))
      *>     WS-PEND-RECIPIENT-USERNAME(n) - Recipient username (PIC X(20))
      *>     WS-PEND-STATUS(n)             - "P" = pending
      *>       PEND-STATUS-PENDING(n)       - 88: TRUE when "P"
      *>       PEND-STATUS-PENDING-OR-EMPTY - 88: TRUE when "P" or " "
      *>   WS-PEND-IDX               - Loop index into pending table
      *>
      *>   --- Send Request (SENDREQ) ---
      *>   WS-SENDREQ-CHOICE         - Menu choice in send-request submenu
      *>   WS-SENDREQ-TARGET-INDEX   - Profile index of request target
      *>
      *>   --- View Request (VIEWREQ) ---
      *>   WS-VIEWREQ-FOUND-FLAG     - "Y" if any pending requests found
      *>   WS-VIEWREQ-PEND-IDX       - Loop index while scanning pending
      *>   WS-VIEWREQ-SENDER-USERNAME - Username of sender being processed
      *>   WS-VIEWREQ-SENDER-IDX     - Profile index of sender (0 = unknown)
      *>   WS-VIEWREQ-DISP-COUNT     - Count of displayed pending entries
      *>   WS-VIEWREQ-SELECTED-PEND-IDX - Pending index of chosen entry
      *>
      *>   --- Accepted Connections ---
      *>   WS-CONNECTIONS-STATUS     - File status for CONNECTIONS.DAT
      *>   WS-CONNECTIONS-COUNT      - Number of accepted connections loaded
      *>   WS-CONNECTIONS-TABLE      - Up to 50 connection pairs (OCCURS 50)
      *>     WS-CONN-USER-A(n)       - First user (PIC X(20))
      *>     WS-CONN-USER-B(n)       - Second user (PIC X(20))
      *>   WS-CONNECTIONS-EOF        - "Y" when file read is done
      *>   WS-CONN-IDX               - Loop index into connections table
      *>
      *>   --- Network Display ---
      *>   WS-NETWORK-DISP-COUNT     - Count of connections printed
      *>   WS-NETWORK-FOUND-FLAG     - "Y" if at least one connection shown
      *>   WS-NETWORK-OTHER-USERNAME - Username of the other user in pair
      *>   WS-NETWORK-OTHER-IDX      - Profile index of that user
      *>*****************************************************************

01  WS-PENDING-COUNT            PIC 99 VALUE 0.
01  WS-PENDING-TABLE.
    05  WS-PENDING-ENTRY OCCURS 50 TIMES.
        10  WS-PEND-SENDER-USERNAME     PIC X(20).
        10  WS-PEND-RECIPIENT-USERNAME  PIC X(20).
        10  WS-PEND-STATUS              PIC X(1).
           88  PEND-STATUS-PENDING      VALUE "P".
           88  PEND-STATUS-PENDING-OR-EMPTY VALUES "P", " ".

01  WS-CONNECTIONS-STATUS      PIC XX.

01  WS-SENDREQ-CHOICE           PIC X(2).

01  WS-SENDREQ-TARGET-INDEX     PIC 9 VALUE 0.

01  WS-PEND-IDX                  PIC 99 VALUE 0.


01  WS-VIEWREQ-FOUND-FLAG        PIC X VALUE "N".
01  WS-VIEWREQ-PEND-IDX          PIC 99 VALUE 0.
01  WS-VIEWREQ-SENDER-USERNAME   PIC x(20).
01  WS-VIEWREQ-SENDER-IDX        PIC 9 VALUE 0.

*> ===== View Pending Requests (VIEWREQ_SRC) working-storage =====
01  WS-VIEWREQ-DISP-COUNT           PIC 99 VALUE 0.
01  WS-VIEWREQ-SELECTED-PEND-IDX    PIC 99 VALUE 0.

01  WS-CONNECTIONS-COUNT       PIC 99 VALUE 0.
01  WS-CONNECTIONS-TABLE.
    05  WS-CONNECTION-ENTRY OCCURS 50 TIMES.
        10  WS-CONN-USER-A     PIC X(20).
        10  WS-CONN-USER-B     PIC X(20).
01  WS-CONNECTIONS-EOF         PIC X VALUE "N".
01  WS-CONN-IDX                PIC 99 VALUE 0.

*> ===== Network List (VIEW NETWORK) working-storage =====
01  WS-NETWORK-DISP-COUNT       PIC 99 VALUE 0.
01  WS-NETWORK-FOUND-FLAG       PIC X VALUE "N".
01  WS-NETWORK-OTHER-USERNAME   PIC X(20).
01  WS-NETWORK-OTHER-IDX        PIC 9 VALUE 0.
