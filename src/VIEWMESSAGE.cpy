*>*****************************************************************
      *> FILE:    VIEWMESSAGE.cpy
      *> PURPOSE: View received messages for the logged-in user (Epic 9).
      *>          Opens MESSAGES.DAT and displays all records where
      *>          MSG-RECIPIENT matches the current user, in file order
      *>          (oldest first). Gracefully handles missing file.
      *>
      *> PARAGRAPHS:
      *>   7840-VIEW-MESSAGES      - Entry point; open MESSAGES.DAT, call
      *>                             7841 loop, close file; print "no messages"
      *>                             if WS-MSG-FOUND = 0; handles status 35
      *>   7841-VIEW-MESSAGES-LOOP - Recursive read; for each record where
      *>                             MSG-RECIPIENT = current user, print
      *>                             "From:", "Message:", "Sent:", "---";
      *>                             recurse until EOF
      *>
      *> DEPENDENCIES:
      *>   WS-MESSAGES.cpy   - WS-MSG-FOUND, WS-VIEW-MSG-EOF,
      *>                        WS-MESSAGES-STATUS
      *>   WS-ACCOUNTS.cpy   - WS-CURRENT-USER-INDEX, WS-USERNAME
      *>   WS-CONSTANTS.cpy  - WS-CONST-FS-OK, WS-CONST-FS-NOT-FOUND
      *>   WS-IO-CONTROL.cpy - WS-OUTPUT-LINE
      *>   main.cob          - 8000-WRITE-OUTPUT, MESSAGES-FILE,
      *>                        MSG-RECORD (MSG-RECIPIENT, MSG-SENDER,
      *>                        MSG-CONTENT, MSG-TIMESTAMP)
      *>*****************************************************************

      *>*****************************************************************
      *> 7840-VIEW-MESSAGES
      *>   Entry point from 7800-MESSAGES-MENU option 2.
      *>   Opens MESSAGES.DAT, iterates all MSG-RECORDs filtering for
      *>   the logged-in user as recipient, displays matching messages
      *>   in chronological order (oldest first), then closes the file.
      *>*****************************************************************
       7840-VIEW-MESSAGES.

           MOVE 0 TO WS-MSG-FOUND
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

           IF WS-MSG-FOUND = 0
               MOVE "You have no messages at this time."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF

           IF WS-MSG-FOUND = 1
               MOVE "---------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.
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

                       IF WS-MSG-FOUND = 0
                           MOVE "--- Your Messages ---"
                               TO WS-OUTPUT-LINE
                           PERFORM 8000-WRITE-OUTPUT
                       END-IF
                       MOVE 1 TO WS-MSG-FOUND

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
