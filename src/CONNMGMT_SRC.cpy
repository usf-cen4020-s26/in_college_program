      *> ============================================================
      *> CONNMGMT_SRC.cpy - Connection file writes and request status updates
      *> ============================================================
       7528-ECHO-REQUEST-INPUT.
           MOVE INPUT-RECORD TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
       7529-ECHO-ACCEPT-REJECT-INPUT.
           MOVE INPUT-RECORD TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
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
           IF WS-PENDING-COUNT >= WS-MAX-PENDING
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
