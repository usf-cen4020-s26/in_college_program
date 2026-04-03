      *>*****************************************************************
      *> VIEWMESSAGE_SRC - View Messages Copybook
      *> Implements the View My Messages flow for the Messages menu.
      *> All output via 8000-WRITE-OUTPUT, all input via 8100-READ-INPUT.
      *>
      *> Paragraphs:
      *>   7840-VIEW-MESSAGES       - Entry point: open, iterate, close
      *>   7841-VIEW-MESSAGES-LOOP  - Read each record, filter, display
      *>*****************************************************************

      *>*****************************************************************
      *> 7840-VIEW-MESSAGES
      *>   Entry point from 7800-MESSAGES-MENU option 2.
      *>   Opens MESSAGES.DAT, iterates all MSG-RECORDs filtering for
      *>   the logged-in user as recipient, displays matching messages
      *>   in chronological order (oldest first), then closes the file.
      *>*****************************************************************
       7840-VIEW-MESSAGES.
           MOVE "--- Your Messages ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE 0 TO WS-VIEW-MSG-FOUND
           MOVE "N" TO WS-VIEW-MSG-EOF

           OPEN INPUT MESSAGES-FILE

           EVALUATE WS-MESSAGES-STATUS
               WHEN WS-CONST-FS-OK
                   PERFORM 7841-VIEW-MESSAGES-LOOP
                   CLOSE MESSAGES-FILE
               WHEN WS-CONST-FS-NOT-FOUND
                   CONTINUE
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "ERROR: Could not open MESSAGES.DAT. STATUS="
                       WS-MESSAGES-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE

           IF WS-VIEW-MSG-FOUND = 0
               MOVE "You have no messages at this time."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF

           MOVE "---------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT.
           EXIT.

      *>*****************************************************************
      *> 7841-VIEW-MESSAGES-LOOP
      *>   Reads records one at a time. For each record where
      *>   MSG-RECIPIENT matches the logged-in user, formats and
      *>   displays sender, content, and timestamp. Recurses until EOF.
      *>*****************************************************************
       7841-VIEW-MESSAGES-LOOP.
           READ MESSAGES-FILE
               AT END
                   MOVE "Y" TO WS-VIEW-MSG-EOF
               NOT AT END
                   IF FUNCTION TRIM(MSG-RECIPIENT)
                       = FUNCTION TRIM(
                           WS-USERNAME(WS-CURRENT-USER-INDEX))

                       MOVE 1 TO WS-VIEW-MSG-FOUND

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "From: "
                           FUNCTION TRIM(MSG-SENDER)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Message: "
                           FUNCTION TRIM(MSG-CONTENT)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Sent: "
                           FUNCTION TRIM(MSG-TIMESTAMP)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE "---" TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   END-IF
           END-READ

           IF WS-VIEW-MSG-EOF = "N"
               PERFORM 7841-VIEW-MESSAGES-LOOP
           END-IF.
           EXIT.
