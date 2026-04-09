*>*****************************************************************
      *> FILE:    SENDREQ.cpy
      *> PURPOSE: Send a connection request to a user found via search.
      *>          Validates that no duplicate request or existing connection
      *>          exists before writing to PENDING.DAT via CONNMGMT.cpy.
      *>
      *> PARAGRAPHS:
      *>   7600-SEND-REQUEST-MENU    - Entry point; display menu, read choice,
      *>                               dispatch to send or return to caller
      *>   7610-DISPLAY-SENDREQ-MENU - Print "Send Connection Request" submenu
      *>   7620-READ-SENDREQ-CHOICE  - Read and echo user's menu choice
      *>   7630-SEND-REQUEST-LOGIC   - Validate target index, check duplicates,
      *>                               call 9300-WRITE-PENDING-REQUEST on success
      *>   7640-CHECK-EXISTING-REQUEST - Scan CONNECTIONS and PENDING tables;
      *>                               set WS-VALID = 0 and display error if found
      *>
      *> DEPENDENCIES:
      *>   WS-CONNECTIONS.cpy - WS-SENDREQ-TARGET-INDEX, WS-SENDREQ-CHOICE,
      *>                        WS-PENDING-TABLE, WS-PENDING-COUNT, WS-PEND-IDX,
      *>                        WS-CONNECTIONS-TABLE, WS-CONNECTIONS-COUNT,
      *>                        WS-CONN-IDX
      *>   WS-ACCOUNTS.cpy   - WS-CURRENT-USER-INDEX, WS-USERNAME
      *>   WS-PROFILES.cpy   - WS-PROF-USERNAME
      *>   WS-IO-CONTROL.cpy - WS-EOF-FLAG, WS-PROGRAM-RUNNING, WS-OUTPUT-LINE,
      *>                        WS-VALID
      *>   CONNMGMT.cpy      - 9300-WRITE-PENDING-REQUEST
      *>   main.cob          - 8000-WRITE-OUTPUT, 8100-READ-INPUT
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

           *> Check if already connected via CONNECTIONS table
           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONNECTIONS-COUNT

               IF (FUNCTION TRIM(WS-CONN-USER-A(WS-CONN-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                   AND FUNCTION TRIM(WS-CONN-USER-B(WS-CONN-IDX))
                    = FUNCTION TRIM(
                        WS-PROF-USERNAME(WS-SENDREQ-TARGET-INDEX)))
                  OR
                  (FUNCTION TRIM(WS-CONN-USER-A(WS-CONN-IDX))
                    = FUNCTION TRIM(
                        WS-PROF-USERNAME(WS-SENDREQ-TARGET-INDEX))
                   AND FUNCTION TRIM(WS-CONN-USER-B(WS-CONN-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX)))

                   MOVE "You are already connected with this user."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 0 TO WS-VALID
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-VALID = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-PEND-IDX FROM 1 BY 1
               UNTIL WS-PEND-IDX > WS-PENDING-COUNT

               *> Check if current user already sent request to target
               IF FUNCTION TRIM(WS-PEND-SENDER-USERNAME(WS-PEND-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND FUNCTION TRIM(WS-PEND-RECIPIENT-USERNAME(WS-PEND-IDX))
                    = FUNCTION TRIM(WS-PROF-USERNAME(WS-SENDREQ-TARGET-INDEX))
                  AND PEND-STATUS-PENDING(WS-PEND-IDX)

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
                  AND PEND-STATUS-PENDING(WS-PEND-IDX)

                   MOVE "This user has already sent you a connection request."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 0 TO WS-VALID
                   EXIT PERFORM
               END-IF

               *> (Connection check now handled above via
               *>  WS-CONNECTIONS-TABLE instead of pending "A" status)
               CONTINUE
           END-PERFORM
           EXIT.


