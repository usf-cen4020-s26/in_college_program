      *>*****************************************************************
      *> VIEWREQ_SRC - View Pending Connection Requests
      *> All output must go through 8000-WRITE-OUTPUT
      *> Uses WS-PENDING-TABLE / WS-PENDING-COUNT loaded at startup
      *>*****************************************************************

       7500-VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "N" TO WS-VIEWREQ-FOUND-FLAG

           PERFORM VARYING WS-VIEWREQ-PEND-IDX FROM 1 BY 1
               UNTIL WS-VIEWREQ-PEND-IDX > WS-PENDING-COUNT

               *> Match: recipient is current logged-in user, status pending
               IF FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-VIEWREQ-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND (WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = "P"
                       OR WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = SPACE)

                   MOVE "Y" TO WS-VIEWREQ-FOUND-FLAG

                   MOVE WS-PEND-SENDER-USERNAME(WS-VIEWREQ-PEND-IDX)
                       TO WS-VIEWREQ-SENDER-USERNAME

                   PERFORM 7510-LOOKUP-SENDER-NAME

               END-IF
           END-PERFORM

           IF WS-VIEWREQ-FOUND-FLAG = "N"
               MOVE "You have no pending connection requests at this time."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF

           MOVE "-----------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

      *> Finds sender name in profiles by username and prints it
       7510-LOOKUP-SENDER-NAME.
           MOVE 0 TO WS-VIEWREQ-SENDER-IDX

           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT
                   OR WS-VIEWREQ-SENDER-IDX > 0

               IF FUNCTION TRIM(WS-PROF-USERNAME(WS-ACCOUNT-INDEX))
                    = FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                  AND WS-HAS-PROFILE(WS-ACCOUNT-INDEX) = 1

                   MOVE WS-ACCOUNT-INDEX TO WS-VIEWREQ-SENDER-IDX
               END-IF
           END-PERFORM

           IF WS-VIEWREQ-SENDER-IDX > 0
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING FUNCTION TRIM(WS-FIRST-NAME(WS-VIEWREQ-SENDER-IDX))
                      " "
                      FUNCTION TRIM(WS-LAST-NAME(WS-VIEWREQ-SENDER-IDX))
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "(Unknown user: "
                      FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                      ")"
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           END-IF
           EXIT.
