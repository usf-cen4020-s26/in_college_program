*>*****************************************************************
      *> FILE:    CONNWRITE.cpy
      *> PURPOSE: Persist a newly accepted connection. Appends one record
      *>          to CONNECTIONS.DAT and adds the pair to the in-memory
      *>          WS-CONNECTIONS-TABLE. Called by VIEWREQ.cpy when a user
      *>          accepts an incoming request.
      *>
      *> PARAGRAPHS:
      *>   9400-ADD-CONNECTION - Validate table capacity, set CONN-USER-A
      *>                         (current user) and CONN-USER-B (sender),
      *>                         append to CONNECTIONS.DAT, update in-memory
      *>                         WS-CONNECTIONS-TABLE and WS-CONNECTIONS-COUNT
      *>
      *> DEPENDENCIES:
      *>   WS-CONNECTIONS.cpy - WS-CONNECTIONS-TABLE, WS-CONNECTIONS-COUNT,
      *>                        WS-CONN-USER-A/B, WS-CONNECTIONS-STATUS,
      *>                        WS-VIEWREQ-SENDER-USERNAME
      *>   WS-ACCOUNTS.cpy   - WS-CURRENT-USER-INDEX, WS-USERNAME
      *>   WS-CONSTANTS.cpy  - WS-CONST-MAX-CONNECTIONS, WS-CONST-FS-OK,
      *>                        WS-CONST-FS-NOT-FOUND
      *>   WS-IO-CONTROL.cpy - WS-OUTPUT-LINE
      *>   main.cob          - 8000-WRITE-OUTPUT, CONNECTIONS-FILE
      *>*****************************************************************
       9400-ADD-CONNECTION.
           IF WS-CONNECTIONS-COUNT >= WS-CONST-MAX-CONNECTIONS
               MOVE "ERROR: Connections table is full." TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF
      *>   Set up file record BEFORE updating in-memory table
           MOVE WS-USERNAME(WS-CURRENT-USER-INDEX) TO CONN-USER-A
           MOVE WS-VIEWREQ-SENDER-USERNAME TO CONN-USER-B
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
               CLOSE CONNECTIONS-FILE
               EXIT PARAGRAPH
           END-IF
           CLOSE CONNECTIONS-FILE
      *>   Only update in-memory table after successful persist
           ADD 1 TO WS-CONNECTIONS-COUNT
           MOVE CONN-USER-A
               TO WS-CONN-USER-A(WS-CONNECTIONS-COUNT)
           MOVE CONN-USER-B
               TO WS-CONN-USER-B(WS-CONNECTIONS-COUNT)
           EXIT.
