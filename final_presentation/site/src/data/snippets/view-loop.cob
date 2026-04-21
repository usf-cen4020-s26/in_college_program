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
