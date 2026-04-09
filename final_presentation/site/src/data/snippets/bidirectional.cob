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
