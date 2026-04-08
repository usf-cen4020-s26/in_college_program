*>*****************************************************************
      *> FILE:    NETWORK.cpy
      *> PURPOSE: Display the logged-in user's network of accepted
      *>          connections. Shows "First Last" if the connected user
      *>          has a profile, otherwise falls back to their username.
      *>
      *> PARAGRAPHS:
      *>   7700-VIEW-NETWORK-LIST  - Entry point; iterate WS-CONNECTIONS-TABLE,
      *>                             print each connection where current user
      *>                             appears as CONN-USER-A or CONN-USER-B
      *>   7710-PRINT-NETWORK-ENTRY - Format and print one connection line
      *>                              as "First Last" or username fallback
      *>
      *> DEPENDENCIES:
      *>   WS-CONNECTIONS.cpy - WS-CONNECTIONS-TABLE, WS-CONNECTIONS-COUNT,
      *>                        WS-CONN-USER-A/B, WS-NETWORK-* vars, WS-CONN-IDX
      *>   WS-ACCOUNTS.cpy   - WS-CURRENT-USER-INDEX, WS-USERNAME
      *>   WS-PROFILES.cpy   - WS-PROF-USERNAME, WS-FIRST-NAME, WS-LAST-NAME,
      *>                        WS-HAS-PROFILE, WS-PROFILE-COUNT
      *>   WS-IO-CONTROL.cpy - WS-OUTPUT-LINE
      *>   main.cob          - 8000-WRITE-OUTPUT
      *>*****************************************************************
*>*****************************************************************
*> 7700-VIEW-NETWORK-LIST
*>   - Displays all accepted connections for the current user
*>   - Uses WS-CONNECTIONS-TABLE loaded at startup (9250)
*>   - Prints First/Last if profile exists, otherwise prints username
*>   - All output via 8000-WRITE-OUTPUT, input via 8100-READ-INPUT
*>*****************************************************************
       7700-VIEW-NETWORK-LIST.
           MOVE "=== MY NETWORK ===" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE 0   TO WS-NETWORK-DISP-COUNT
           MOVE "N" TO WS-NETWORK-FOUND-FLAG

           IF WS-CONNECTIONS-COUNT = 0
               MOVE "You have no connections in your network yet."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONNECTIONS-COUNT
               IF FUNCTION TRIM(WS-CONN-USER-A(WS-CONN-IDX))
                    = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                   MOVE WS-CONN-USER-B(WS-CONN-IDX)
                       TO WS-NETWORK-OTHER-USERNAME
                   PERFORM 7710-PRINT-ONE-NETWORK-LINE
               ELSE
                   IF FUNCTION TRIM(WS-CONN-USER-B(WS-CONN-IDX))
                        = FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
                       MOVE WS-CONN-USER-A(WS-CONN-IDX)
                           TO WS-NETWORK-OTHER-USERNAME
                       PERFORM 7710-PRINT-ONE-NETWORK-LINE
                   END-IF
               END-IF
           END-PERFORM

           IF WS-NETWORK-FOUND-FLAG = "N"
               MOVE "You have no connections in your network yet."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> One connection: 35 dashes. Multiple: 20 dashes.
           IF WS-NETWORK-DISP-COUNT = 1
               MOVE "-----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE "--------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF
           EXIT.

*>*****************************************************************
*> 7710-PRINT-ONE-NETWORK-LINE
*>   - "Connected with: First Last (University: X, Major: Y)" or "Connected with: username"
*>*****************************************************************
       7710-PRINT-ONE-NETWORK-LINE.
           MOVE "Y" TO WS-NETWORK-FOUND-FLAG
           ADD 1 TO WS-NETWORK-DISP-COUNT
           MOVE 0 TO WS-NETWORK-OTHER-IDX
           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT
                  OR WS-NETWORK-OTHER-IDX > 0
               IF FUNCTION TRIM(WS-PROF-USERNAME(WS-ACCOUNT-INDEX))
                    = FUNCTION TRIM(WS-NETWORK-OTHER-USERNAME)
                  AND WS-HAS-PROFILE(WS-ACCOUNT-INDEX) = 1
                   MOVE WS-ACCOUNT-INDEX TO WS-NETWORK-OTHER-IDX
               END-IF
           END-PERFORM
           MOVE SPACES TO WS-OUTPUT-LINE
           IF WS-NETWORK-OTHER-IDX > 0
               STRING "Connected with: "
                      FUNCTION TRIM(WS-FIRST-NAME(WS-NETWORK-OTHER-IDX))
                      " "
                      FUNCTION TRIM(WS-LAST-NAME(WS-NETWORK-OTHER-IDX))
                      " (University: "
                      FUNCTION TRIM(WS-UNIVERSITY(WS-NETWORK-OTHER-IDX))
                      ", Major: "
                      FUNCTION TRIM(WS-MAJOR(WS-NETWORK-OTHER-IDX))
                      ")"
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           ELSE
               STRING "Connected with: "
                      FUNCTION TRIM(WS-NETWORK-OTHER-USERNAME)
                      DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
           END-IF
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
