      *>*****************************************************************
      *> VIEWREQ_SRC - View Pending Connection Requests (Epic 5)
      *> Show ONE pending request at a time (no numbered list selection)
      *>*****************************************************************

       7500-VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "N" TO WS-VIEWREQ-FOUND-FLAG

           *> Iterate through pending table; show requests one-by-one
           MOVE 1 TO WS-VIEWREQ-PEND-IDX
           PERFORM UNTIL WS-VIEWREQ-PEND-IDX > WS-PENDING-COUNT

               *> Match: recipient is current logged-in user, status pending
               IF FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-VIEWREQ-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND (WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = "P"
                       OR WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = SPACE)

                   MOVE "Y" TO WS-VIEWREQ-FOUND-FLAG

                   *> Save selected pending index for accept/reject flow
                   MOVE WS-VIEWREQ-PEND-IDX TO WS-VIEWREQ-SELECTED-PEND-IDX

                   *> Sender username for prompts/confirmation
                   MOVE WS-PEND-SENDER-USERNAME(WS-VIEWREQ-SELECTED-PEND-IDX)
                       TO WS-VIEWREQ-SENDER-USERNAME

                   *> Print: Request from: <Sender>
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "Request from: "
                          FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                          DELIMITED BY SIZE
                          INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT

                   *> Options
                   MOVE "1. Accept" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "2. Reject" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT

                   *> Prompt: Enter your choice for <Sender>:
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "Enter your choice for "
                          FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                          ":"
                          DELIMITED BY SIZE
                          INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT

                   *> Read choice (must echo)
                   PERFORM 8100-READ-INPUT
                   IF WS-EOF-FLAG = 1
                       MOVE 0 TO WS-PROGRAM-RUNNING
                       EXIT PARAGRAPH
                   END-IF
                   MOVE INPUT-RECORD TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT

                   MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MENU-CHOICE

                   EVALUATE WS-MENU-CHOICE
                       WHEN "1"
                           *> Accept: remove pending + add connection + confirmation
                           PERFORM 9305-REMOVE-PENDING-ENTRY
                           PERFORM 9400-ADD-CONNECTION
                           PERFORM 7525-PRINT-ACCEPTED-CONFIRMATION

                           *> IMPORTANT:
                           *> Since we removed current entry, DO NOT increment index.
                           *> Next request is now at the same WS-VIEWREQ-PEND-IDX.

                       WHEN "2"
                           *> Reject: remove pending + rejection confirmation
                           PERFORM 9305-REMOVE-PENDING-ENTRY
                           PERFORM 7526-PRINT-REJECTED-CONFIRMATION
                           *> Same note: do not increment index after removal.

                       WHEN OTHER
                           MOVE "Invalid choice." TO WS-OUTPUT-LINE
                           PERFORM 8000-WRITE-OUTPUT
                           *> No removal -> move on to next request
                           ADD 1 TO WS-VIEWREQ-PEND-IDX
                   END-EVALUATE

                   MOVE "-----------------------------------" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT

               ELSE
                   *> Not a match -> next record
                   ADD 1 TO WS-VIEWREQ-PEND-IDX
               END-IF

           END-PERFORM

           IF WS-VIEWREQ-FOUND-FLAG = "N"
               MOVE "You have no pending connection requests at this time."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF

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
      *> Prints rejected confirmation like:
      *> "Connection request from OtherUser rejected!"
       7526-PRINT-REJECTED-CONFIRMATION.
           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Connection request from "
                  FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                  " rejected!"
                  DELIMITED BY SIZE
                  INTO WS-OUTPUT-LINE
           END-STRING
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
