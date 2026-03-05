      *>*****************************************************************
      *> VIEWREQ_SRC - View Pending Connection Requests (Epic 5)
      *> Peek next input: if "1" or "2" = interactive (one accept/reject then return).
      *> Else = view-only: list names, push back line, return.
      *>*****************************************************************

       7500-VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "N" TO WS-VIEWREQ-FOUND-FLAG
           MOVE 0 TO WS-VIEWREQ-DISP-COUNT

           *> Count pendings for current user and find first index
           MOVE 1 TO WS-VIEWREQ-PEND-IDX
           PERFORM UNTIL WS-VIEWREQ-PEND-IDX > WS-PENDING-COUNT
               IF FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-VIEWREQ-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND (WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = "P"
                       OR WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = SPACE)
                   ADD 1 TO WS-VIEWREQ-DISP-COUNT
                   IF WS-VIEWREQ-FOUND-FLAG = "N"
                       MOVE "Y" TO WS-VIEWREQ-FOUND-FLAG
                       MOVE WS-VIEWREQ-PEND-IDX TO WS-VIEWREQ-SELECTED-PEND-IDX
                   END-IF
               END-IF
               ADD 1 TO WS-VIEWREQ-PEND-IDX
           END-PERFORM

           IF WS-VIEWREQ-FOUND-FLAG = "N"
               MOVE "You have no pending connection requests at this time."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> Peek: read next line to decide interactive vs view-only
           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF
           INSPECT INPUT-RECORD REPLACING ALL X"0D" BY SPACE
                                     ALL X"0A" BY SPACE
           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MENU-CHOICE

           IF WS-MENU-CHOICE = "1" OR WS-MENU-CHOICE = "2"
               *> Interactive: show first request, echo choice, process, return
               MOVE WS-PEND-SENDER-USERNAME(WS-VIEWREQ-SELECTED-PEND-IDX)
                   TO WS-VIEWREQ-SENDER-USERNAME
               PERFORM 7509-FIND-SENDER-INDEX
               PERFORM 7512-OUTPUT-REQUEST-FROM
               MOVE "1. Accept" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "2. Reject" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 7513-OUTPUT-ENTER-CHOICE-FOR

               EVALUATE WS-MENU-CHOICE
                   WHEN "1"
                       PERFORM 9305-REMOVE-PENDING-ENTRY
                       PERFORM 9400-ADD-CONNECTION
                       PERFORM 7525-PRINT-ACCEPTED-CONFIRMATION
                   WHEN "2"
                       PERFORM 9305-REMOVE-PENDING-ENTRY
                       PERFORM 7526-PRINT-REJECTED-CONFIRMATION
               END-EVALUATE
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> View-only: preload choice for post-login menu, list names, return
           MOVE WS-MENU-CHOICE TO WS-PRELOADED-MENU-CHOICE
           MOVE "Y" TO WS-SKIP-NEXT-MENU-READ
           MOVE 0 TO WS-VIEWREQ-DISP-COUNT
           MOVE 1 TO WS-VIEWREQ-PEND-IDX
           PERFORM UNTIL WS-VIEWREQ-PEND-IDX > WS-PENDING-COUNT
               IF FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-VIEWREQ-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND (WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = "P"
                       OR WS-PEND-STATUS(WS-VIEWREQ-PEND-IDX) = SPACE)
                   MOVE WS-PEND-SENDER-USERNAME(WS-VIEWREQ-PEND-IDX)
                       TO WS-VIEWREQ-SENDER-USERNAME
                   PERFORM 7510-LOOKUP-SENDER-NAME
               END-IF
               ADD 1 TO WS-VIEWREQ-PEND-IDX
           END-PERFORM
           MOVE "-----------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

      *> Output "Request from: First Last" or "Request from: username"
       7512-OUTPUT-REQUEST-FROM.
           MOVE SPACES TO WS-OUTPUT-LINE
           IF WS-VIEWREQ-SENDER-IDX > 0
               STRING "Request from: "
                   FUNCTION TRIM(WS-FIRST-NAME(WS-VIEWREQ-SENDER-IDX))
                   " "
                   FUNCTION TRIM(WS-LAST-NAME(WS-VIEWREQ-SENDER-IDX))
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           ELSE
               STRING "Request from: "
                   FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           END-IF
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
      *> Output "Enter your choice for First Last:" or "username:"
       7513-OUTPUT-ENTER-CHOICE-FOR.
           MOVE SPACES TO WS-OUTPUT-LINE
           IF WS-VIEWREQ-SENDER-IDX > 0
               STRING "Enter your choice for "
                   FUNCTION TRIM(WS-FIRST-NAME(WS-VIEWREQ-SENDER-IDX))
                   " "
                   FUNCTION TRIM(WS-LAST-NAME(WS-VIEWREQ-SENDER-IDX))
                   ":"
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           ELSE
               STRING "Enter your choice for "
                   FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                   ":"
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           END-IF
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

      *> Prints one pending request line like: "1) First Last"
       7515-PRINT-ONE-PENDING-LINE.
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

      *> 7509-FIND-SENDER-INDEX: Set WS-VIEWREQ-SENDER-IDX from sender username (no output)
       7509-FIND-SENDER-INDEX.
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
           EXIT.
      *> "Connection request from First Last accepted!" or "... username accepted!"
       7525-PRINT-ACCEPTED-CONFIRMATION.
           MOVE SPACES TO WS-OUTPUT-LINE
           IF WS-VIEWREQ-SENDER-IDX > 0
               STRING "Connection request from "
                   FUNCTION TRIM(WS-FIRST-NAME(WS-VIEWREQ-SENDER-IDX))
                   " "
                   FUNCTION TRIM(WS-LAST-NAME(WS-VIEWREQ-SENDER-IDX))
                   " accepted!"
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           ELSE
               STRING "Connection request from "
                   FUNCTION TRIM(WS-VIEWREQ-SENDER-USERNAME)
                   " accepted!"
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           END-IF
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
      *> "Connection request rejected." (reject_single test)
       7526-PRINT-REJECTED-SHORT.
           MOVE "Connection request rejected." TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
      *> "Connection request from X rejected!"
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
      *> Finds sender name in profiles by username and prints it (one line: "First Last" or "(Unknown user: X)")
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
