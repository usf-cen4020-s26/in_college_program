      *> ============================================================
      *> CONNWRITE_SRC.cpy - Add established connection to file
      *> ============================================================
      *>*****************************************************************
      *> 9400-ADD-CONNECTION
      *>   Adds a new connection to memory + appends to CONNECTIONS.DAT
      *>*****************************************************************
       9400-ADD-CONNECTION.
           IF WS-CONNECTIONS-COUNT >= WS-CONST-MAX-CONNECTIONS
               MOVE "ERROR: Connections table is full." TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-CONNECTIONS-COUNT
           MOVE WS-USERNAME(WS-CURRENT-USER-INDEX)
               TO WS-CONN-USER-A(WS-CONNECTIONS-COUNT)
           MOVE WS-VIEWREQ-SENDER-USERNAME
               TO WS-CONN-USER-B(WS-CONNECTIONS-COUNT)
           MOVE WS-CONN-USER-A(WS-CONNECTIONS-COUNT) TO CONN-USER-A
           MOVE WS-CONN-USER-B(WS-CONNECTIONS-COUNT) TO CONN-USER-B
           OPEN EXTEND CONNECTIONS-FILE
           IF WS-CONNECTIONS-STATUS = WS-CONST-FS-NOT-FOUND
               OPEN OUTPUT CONNECTIONS-FILE
               CLOSE CONNECTIONS-FILE
               OPEN EXTEND CONNECTIONS-FILE
           END-IF
           IF WS-CONNECTIONS-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not open CONNECTIONS.DAT for append. STATUS="
                   WS-CONNECTIONS-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF
           WRITE CONNECTION-REC
           IF WS-CONNECTIONS-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not write to CONNECTIONS.DAT. STATUS="
                   WS-CONNECTIONS-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           END-IF
           CLOSE CONNECTIONS-FILE
           EXIT.
