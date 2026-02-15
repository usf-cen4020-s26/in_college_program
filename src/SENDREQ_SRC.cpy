      *>*****************************************************************
      *> SENDREQ_SRC - Send Connection Request Sub-Menu (Scaffold)
      *> All output must go through 8000-WRITE-OUTPUT
      *> All input reads must use 8100-READ-INPUT (INPUT.TXT)
      *>*****************************************************************


      *> Entry point paragraph to PERFORM from main.cob
       7600-SEND-REQUEST-MENU.
           PERFORM 7610-DISPLAY-SENDREQ-MENU
           PERFORM 7620-READ-SENDREQ-CHOICE

           EVALUATE WS-SENDREQ-CHOICE
               WHEN "1"
                   PERFORM 7630-SEND-REQUEST-LOGIC
               WHEN "2"
                   *> Back to main menu: just return to caller
                   CONTINUE
               WHEN OTHER
                   IF WS-EOF-FLAG = 1
                       EXIT PARAGRAPH
                   END-IF
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE.
           EXIT.

       7610-DISPLAY-SENDREQ-MENU.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "=== SEND CONNECTION REQUEST ===" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "1. Send Connection Request" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "2. Back to Main Menu" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT.
           EXIT.

       7620-READ-SENDREQ-CHOICE.
           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           MOVE INPUT-RECORD TO WS-SENDREQ-CHOICE
           MOVE WS-SENDREQ-CHOICE TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT.
           EXIT.

       7630-SEND-REQUEST-LOGIC.
           *> Validate: target user exists and has profile
           IF WS-SENDREQ-TARGET-INDEX = 0
               MOVE "Error: No user selected." TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> Check if request already exists in either direction
           PERFORM 7640-CHECK-EXISTING-REQUEST

           IF WS-VALID = 0
               *> Error message already in 7640
               EXIT PARAGRAPH
           END-IF

           *> All validations passed - send the request
           PERFORM 9300-WRITE-PENDING-REQUEST
           MOVE "Connection request sent!" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

       7640-CHECK-EXISTING-REQUEST.
           MOVE 1 TO WS-VALID

           PERFORM VARYING WS-PEND-IDX FROM 1 BY 1
               UNTIL WS-PEND-IDX > WS-PENDING-COUNT

               *> Check if current user already sent request to target
               IF FUNCTION TRIM(WS-PEND-SENDER-USERNAME(WS-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-PEND-IDX))
                    = FUNCTION TRIM(WS-PROF-USERNAME(WS-SENDREQ-TARGET-INDEX))
                  AND WS-PEND-STATUS(WS-PEND-IDX) = "P"

                   MOVE "You have already sent a connection request to this user."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 0 TO WS-VALID
                   EXIT PERFORM
               END-IF

               *> Check if target already sent request to current user
               IF FUNCTION TRIM(WS-PEND-SENDER-USERNAME(WS-PEND-IDX))
                    = FUNCTION TRIM(WS-PROF-USERNAME(WS-SENDREQ-TARGET-INDEX))
                  AND FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND WS-PEND-STATUS(WS-PEND-IDX) = "P"

                   MOVE "This user has already sent you a connection request."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 0 TO WS-VALID
                   EXIT PERFORM
               END-IF
           END-PERFORM
           EXIT.


