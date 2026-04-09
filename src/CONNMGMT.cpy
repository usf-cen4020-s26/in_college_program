*>*****************************************************************
      *> FILE:    CONNMGMT.cpy
      *> PURPOSE: Manage pending connection request state — adding new
      *>          requests, removing resolved ones, and rewriting PENDING.DAT.
      *>          Also contains echo helpers used by VIEWREQ.cpy.
      *>
      *> PARAGRAPHS:
      *>   9300-WRITE-PENDING-REQUEST    - Add new pending request to in-memory
      *>                                   table and append to PENDING.DAT
      *>   9305-REMOVE-PENDING-ENTRY     - Remove one entry from WS-PENDING-TABLE
      *>                                   by index, then calls 9310 to rewrite file
      *>   9310-REWRITE-PENDING-FILE     - Truncate and rewrite entire PENDING.DAT
      *>                                   from current WS-PENDING-TABLE
      *>
      *> DEPENDENCIES:
      *>   WS-CONNECTIONS.cpy - WS-PENDING-TABLE, WS-PENDING-COUNT, WS-PEND-IDX,
      *>                        WS-PEND-SENDER/RECIPIENT-USERNAME, WS-PEND-STATUS,
      *>                        WS-VIEWREQ-SELECTED-PEND-IDX
      *>   WS-ACCOUNTS.cpy   - WS-CURRENT-USER-INDEX, WS-USERNAME
      *>   WS-PROFILES.cpy   - WS-PROF-USERNAME
      *>   WS-CONSTANTS.cpy  - WS-CONST-FS-OK, WS-CONST-MAX-PENDING
      *>   WS-IO-CONTROL.cpy - WS-OUTPUT-LINE
      *>   main.cob          - 8000-WRITE-OUTPUT, PENDING-FILE, WS-PENDING-STATUS
      *>*****************************************************************

      *>*****************************************************************
      *> 9305-REMOVE-PENDING-ENTRY
      *>   Remove one pending request from the table and rewrite file.
      *>*****************************************************************
       9305-REMOVE-PENDING-ENTRY.
           IF WS-VIEWREQ-SELECTED-PEND-IDX < 1
              OR WS-VIEWREQ-SELECTED-PEND-IDX > WS-PENDING-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE WS-VIEWREQ-SELECTED-PEND-IDX TO WS-PEND-IDX
           ADD 1 TO WS-PEND-IDX
           PERFORM UNTIL WS-PEND-IDX > WS-PENDING-COUNT
               MOVE WS-PEND-SENDER-USERNAME(WS-PEND-IDX)
                   TO WS-PEND-SENDER-USERNAME(WS-PEND-IDX - 1)
               MOVE WS-PEND-RECIPIENT-USERNAME(WS-PEND-IDX)
                   TO WS-PEND-RECIPIENT-USERNAME(WS-PEND-IDX - 1)
               MOVE WS-PEND-STATUS(WS-PEND-IDX)
                   TO WS-PEND-STATUS(WS-PEND-IDX - 1)
               ADD 1 TO WS-PEND-IDX
           END-PERFORM
           SUBTRACT 1 FROM WS-PENDING-COUNT
           PERFORM 9310-REWRITE-PENDING-FILE
           EXIT.
      *>*****************************************************************
      *> 9310-REWRITE-PENDING-FILE
      *>   Rewrites entire PENDING.DAT from WS-PENDING-TABLE
      *>*****************************************************************
       9310-REWRITE-PENDING-FILE.
           OPEN OUTPUT PENDING-FILE
           IF WS-PENDING-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not open PENDING.DAT for rewrite. STATUS="
                   WS-PENDING-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-PEND-IDX FROM 1 BY 1
               UNTIL WS-PEND-IDX > WS-PENDING-COUNT
               MOVE WS-PEND-SENDER-USERNAME(WS-PEND-IDX)
                   TO PEND-SENDER-USERNAME
               MOVE WS-PEND-RECIPIENT-USERNAME(WS-PEND-IDX)
                   TO PEND-RECIPIENT-USERNAME
               MOVE WS-PEND-STATUS(WS-PEND-IDX)
                   TO PEND-STATUS
               WRITE PENDING-REC
           END-PERFORM
           CLOSE PENDING-FILE
           EXIT.
      *>*****************************************************************
      *> 9300-WRITE-PENDING-REQUEST
      *>   Adds a new pending request to memory + appends to PENDING.DAT
      *>*****************************************************************
       9300-WRITE-PENDING-REQUEST.
           IF WS-PENDING-COUNT >= WS-CONST-MAX-PENDING
               MOVE "ERROR: Pending requests table is full." TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-PENDING-COUNT
           MOVE WS-USERNAME(WS-CURRENT-USER-INDEX)
               TO WS-PEND-SENDER-USERNAME(WS-PENDING-COUNT)
           MOVE WS-PROF-USERNAME(WS-SENDREQ-TARGET-INDEX)
               TO WS-PEND-RECIPIENT-USERNAME(WS-PENDING-COUNT)
           SET PEND-STATUS-PENDING(WS-PENDING-COUNT) TO TRUE
           MOVE WS-PEND-SENDER-USERNAME(WS-PENDING-COUNT)
               TO PEND-SENDER-USERNAME
           MOVE WS-PEND-RECIPIENT-USERNAME(WS-PENDING-COUNT)
               TO PEND-RECIPIENT-USERNAME
           MOVE WS-PEND-STATUS(WS-PENDING-COUNT)
               TO PEND-STATUS
           OPEN EXTEND PENDING-FILE
           IF WS-PENDING-STATUS = WS-CONST-FS-NOT-FOUND
               OPEN OUTPUT PENDING-FILE
               CLOSE PENDING-FILE
               OPEN EXTEND PENDING-FILE
           END-IF
           IF WS-PENDING-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not open PENDING.DAT for append. STATUS="
                   WS-PENDING-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF
           WRITE PENDING-REC
           IF WS-PENDING-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not write to PENDING.DAT. STATUS="
                   WS-PENDING-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           END-IF
           CLOSE PENDING-FILE
           EXIT.
