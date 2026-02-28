      *>*****************************************************************
      *> VIEWREQ_SRC - View Pending Connection Requests
      *> All output must go through 8000-WRITE-OUTPUT
      *> Uses WS-PENDING-TABLE / WS-PENDING-COUNT loaded at startup
      *>*****************************************************************

              7500-VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "N" TO WS-VIEWREQ-FOUND-FLAG
           MOVE 0   TO WS-VIEWREQ-DISP-COUNT

           PERFORM VARYING WS-VIEWREQ-PEND-IDX FROM 1 BY 1
               UNTIL WS-VIEWREQ-PEND-IDX > WS-PENDING-COUNT

               *> Match: recipient is current logged-in user, status pending
               IF FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-VIEWREQ-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND (WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = "P"
                       OR WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = SPACE)

                   MOVE "Y" TO WS-VIEWREQ-FOUND-FLAG

                   ADD 1 TO WS-VIEWREQ-DISP-COUNT
                   MOVE WS-VIEWREQ-PEND-IDX
                       TO WS-VIEWREQ-MAP-IDX(WS-VIEWREQ-DISP-COUNT)

                   *> lookup sender name (prints it) but we want it on same line
                   *> so we build one line: "1) First Last"
                   MOVE WS-PEND-SENDER-USERNAME(WS-VIEWREQ-PEND-IDX)
                       TO WS-VIEWREQ-SENDER-USERNAME

                   PERFORM 7515-PRINT-ONE-PENDING-LINE

               END-IF
           END-PERFORM

           IF WS-VIEWREQ-FOUND-FLAG = "N"
               MOVE "You have no pending connection requests at this time."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> Ask what to do
           MOVE "Enter request number to Accept/Reject (0 to go back):"
               TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           *> basic numeric parse for 0-99
           MOVE 0 TO WS-VIEWREQ-SELECTION
           IF FUNCTION TRIM(INPUT-RECORD) = "0"
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(FUNCTION TRIM(INPUT-RECORD))
               TO WS-VIEWREQ-SELECTION

           IF WS-VIEWREQ-SELECTION < 1
              OR WS-VIEWREQ-SELECTION > WS-VIEWREQ-DISP-COUNT
               MOVE "Invalid selection." TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           MOVE WS-VIEWREQ-MAP-IDX(WS-VIEWREQ-SELECTION)
               TO WS-VIEWREQ-SELECTED-PEND-IDX

           MOVE "Enter A to accept or R to reject:" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           MOVE INPUT-RECORD(1:1) TO WS-VIEWREQ-ACTION

           EVALUATE WS-VIEWREQ-ACTION
           WHEN "A"
               MOVE "A" TO WS-PEND-STATUS(WS-VIEWREQ-SELECTED-PEND-IDX)
               PERFORM 9310-REWRITE-PENDING-FILE

               *>  DEV: show accepted confirmation message (with sender name)
               MOVE WS-PEND-SENDER-USERNAME(WS-VIEWREQ-SELECTED-PEND-IDX)
                    TO WS-VIEWREQ-SENDER-USERNAME
               PERFORM 9400-ADD-CONNECTION
               
               PERFORM 7525-PRINT-ACCEPTED-CONFIRMATION

               WHEN "R"
                   MOVE "R" TO WS-PEND-STATUS(WS-VIEWREQ-SELECTED-PEND-IDX)
                   PERFORM 9310-REWRITE-PENDING-FILE

                   *> ✅ DEV requirement: rejection confirmation message
                   MOVE "Connection request rejected." TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT

               WHEN OTHER
                   MOVE "Invalid choice." TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE

           MOVE "-----------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
      *> Prints one pending request line like: "1) First Last"
       7515-PRINT-ONE-PENDING-LINE.
           *> Find sender name index
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

           MOVE SPACES TO WS-OUTPUT-LINE

           IF WS-VIEWREQ-SENDER-IDX > 0
               STRING WS-VIEWREQ-DISP-COUNT
                      ") "
                      FUNCTION TRIM(WS-FIRST-NAME(WS-VIEWREQ-SENDER-IDX))
                      " "
                      FUNCTION TRIM(WS-LAST-NAME(WS-VIEWREQ-SENDER-IDX))
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           ELSE
               STRING WS-VIEWREQ-DISP-COUNT
                      ") (Unknown user: "
                      FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                      ")"
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           END-IF

           PERFORM 8000-WRITE-OUTPUT
           EXIT.

              *> Prints accepted confirmation like:
      *> "You have accepted John Doe's connection request."
       7525-PRINT-ACCEPTED-CONFIRMATION.
           *> Find sender profile index
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

           MOVE SPACES TO WS-OUTPUT-LINE

           IF WS-VIEWREQ-SENDER-IDX > 0
               STRING "You have accepted "
                      FUNCTION TRIM(WS-FIRST-NAME(WS-VIEWREQ-SENDER-IDX))
                      " "
                      FUNCTION TRIM(WS-LAST-NAME(WS-VIEWREQ-SENDER-IDX))
                      "'s connection request."
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           ELSE
               STRING "You have accepted the connection request from "
                      FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                      "."
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           END-IF

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
