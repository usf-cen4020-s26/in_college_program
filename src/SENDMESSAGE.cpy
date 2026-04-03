      *>*****************************************************************
      *> SENDMESSAGE_SRC - Send Message Copybook
      *> Implements the Messages menu and Send a New Message flow.
      *> All output via 8000-WRITE-OUTPUT, all input via 8100-READ-INPUT.
      *>
      *> Paragraphs:
      *>   7800-MESSAGES-MENU       - Messages sub-menu (Send/View/Back)
      *>   7810-SEND-MESSAGE        - Orchestrate send message flow
      *>   7820-VALIDATE-RECIPIENT  - Check recipient is a connection
      *>   7830-WRITE-MESSAGE       - Persist message to MESSAGES.DAT
      *>*****************************************************************

      *>*****************************************************************
      *> 7800-MESSAGES-MENU
      *>   Entry point from main menu option 8.
      *>   Loops presenting Send/View/Back until user picks Back.
      *>*****************************************************************
       7800-MESSAGES-MENU.
           MOVE "1" TO WS-MSG-MENU-CHOICE

           PERFORM UNTIL WS-MSG-MENU-CHOICE = "3"
               OR WS-PROGRAM-RUNNING = 0

               MOVE "--- Messages Menu ---" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               MOVE "1. Send a New Message" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               MOVE "2. View My Messages" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               MOVE "3. Back to Main Menu" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               MOVE "Enter your choice:" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-RECORD)
                   TO WS-MSG-MENU-CHOICE
               MOVE WS-MSG-MENU-CHOICE TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               EVALUATE WS-MSG-MENU-CHOICE
                   WHEN "1"
                       PERFORM 7810-SEND-MESSAGE
                   WHEN "2"
                       PERFORM 7840-VIEW-MESSAGES
                   WHEN "3"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
               END-EVALUATE
           END-PERFORM.
           EXIT.

      *>*****************************************************************
      *> 7810-SEND-MESSAGE
      *>   Prompts for recipient username, validates connection,
      *>   captures message content, and writes to MESSAGES.DAT.
      *>*****************************************************************
       7810-SEND-MESSAGE.
           MOVE "Enter recipient's username (must be a connection):"
               TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MSG-RECIPIENT
           MOVE WS-MSG-RECIPIENT TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

      *>   Check if the recipient exists in the system (MSW-454)
           MOVE 0 TO WS-MSG-USER-EXISTS
           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-ACCOUNT-COUNT
               OR WS-MSG-USER-EXISTS = 1
               IF FUNCTION TRIM(WS-USERNAME(WS-ACCOUNT-INDEX))
                   = FUNCTION TRIM(WS-MSG-RECIPIENT)
                   MOVE 1 TO WS-MSG-USER-EXISTS
               END-IF
           END-PERFORM

           IF WS-MSG-USER-EXISTS = 0
               MOVE "You can only message users you are connected with."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM 7820-VALIDATE-RECIPIENT

           IF WS-MSG-CONN-FOUND = 0
               MOVE
               "You can only message users you are connected with."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

      *>   Message content validation loop (MSW-451 / MSW-453)
           PERFORM FOREVER
               MOVE "Enter your message (max 200 chars):"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-RECORD TRAILING)
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF FUNCTION TRIM(INPUT-RECORD) = SPACES
                   MOVE "Message cannot be empty. Please try again."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               ELSE
                   IF FUNCTION LENGTH(
                       FUNCTION TRIM(INPUT-RECORD TRAILING))
                       > 200
                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Message is too long. "
                           "Please enter 200 "
                           "characters or fewer."
                           DELIMITED BY SIZE
                           INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE INPUT-RECORD TO WS-MSG-CONTENT
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM

           IF WS-PROGRAM-RUNNING = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM 7830-WRITE-MESSAGE

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Message sent to "
               FUNCTION TRIM(WS-MSG-RECIPIENT)
               " successfully!"
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE "---------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT.
           EXIT.

      *>*****************************************************************
      *> 7820-VALIDATE-RECIPIENT
      *>   Checks the in-memory WS-CONNECTIONS-TABLE to verify the
      *>   recipient is an established connection of the current user.
      *>   Sets WS-MSG-CONN-FOUND = 1 if valid, 0 otherwise.
      *>*****************************************************************
       7820-VALIDATE-RECIPIENT.
           MOVE 0 TO WS-MSG-CONN-FOUND

           IF WS-CONNECTIONS-COUNT = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONNECTIONS-COUNT
               OR WS-MSG-CONN-FOUND = 1

               IF FUNCTION TRIM(WS-CONN-USER-A(WS-CONN-IDX))
                   = FUNCTION TRIM(
                       WS-USERNAME(WS-CURRENT-USER-INDEX))
                  AND FUNCTION TRIM(WS-CONN-USER-B(WS-CONN-IDX))
                   = FUNCTION TRIM(WS-MSG-RECIPIENT)
                   MOVE 1 TO WS-MSG-CONN-FOUND
               ELSE
                   IF FUNCTION TRIM(WS-CONN-USER-B(WS-CONN-IDX))
                       = FUNCTION TRIM(
                           WS-USERNAME(WS-CURRENT-USER-INDEX))
                      AND FUNCTION TRIM(WS-CONN-USER-A(WS-CONN-IDX))
                       = FUNCTION TRIM(WS-MSG-RECIPIENT)
                       MOVE 1 TO WS-MSG-CONN-FOUND
                   END-IF
               END-IF
           END-PERFORM.
           EXIT.

      *>*****************************************************************
      *> 7830-WRITE-MESSAGE
      *>   Generates a timestamp via FUNCTION CURRENT-DATE and appends
      *>   a new message record to MESSAGES.DAT.
      *>*****************************************************************
       7830-WRITE-MESSAGE.
           MOVE FUNCTION CURRENT-DATE TO WS-MSG-CURRENT-DATE

           MOVE SPACES TO WS-MSG-TIMESTAMP
           STRING WS-MSG-CURRENT-DATE(1:4) "-"
                  WS-MSG-CURRENT-DATE(5:2) "-"
                  WS-MSG-CURRENT-DATE(7:2) " "
                  WS-MSG-CURRENT-DATE(9:2) ":"
                  WS-MSG-CURRENT-DATE(11:2) ":"
                  WS-MSG-CURRENT-DATE(13:2)
                  DELIMITED BY SIZE INTO WS-MSG-TIMESTAMP
           END-STRING

           MOVE WS-MSG-NEXT-ID TO MSG-ID
           MOVE WS-USERNAME(WS-CURRENT-USER-INDEX) TO MSG-SENDER
           MOVE WS-MSG-RECIPIENT TO MSG-RECIPIENT
           MOVE WS-MSG-CONTENT TO MSG-CONTENT
           MOVE WS-MSG-TIMESTAMP TO MSG-TIMESTAMP

           OPEN EXTEND MESSAGES-FILE

           IF WS-MESSAGES-STATUS = WS-CONST-FS-NOT-FOUND
               CLOSE MESSAGES-FILE
               OPEN OUTPUT MESSAGES-FILE
               CLOSE MESSAGES-FILE
               OPEN EXTEND MESSAGES-FILE
           END-IF

           IF WS-MESSAGES-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not open MESSAGES.DAT. STATUS="
                   WS-MESSAGES-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           WRITE MSG-RECORD

           IF WS-MESSAGES-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not write to MESSAGES.DAT. STATUS="
                   WS-MESSAGES-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           END-IF

           ADD 1 TO WS-MSG-NEXT-ID

           CLOSE MESSAGES-FILE.
           EXIT.

