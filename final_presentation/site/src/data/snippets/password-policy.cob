       4400-VALIDATE-PASSWORD.
           MOVE 0 TO WS-PASSWORD-VALID.
           MOVE 0 TO WS-HAS-CAPITAL.
           MOVE 0 TO WS-HAS-DIGIT.
           MOVE 0 TO WS-HAS-SPECIAL.

*> *      *>    Check length
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-PASSWORD-INPUT))
               TO WS-PASSWORD-LENGTH.

           IF WS-PASSWORD-LENGTH < 8 OR WS-PASSWORD-LENGTH > 12
               EXIT PARAGRAPH
           END-IF.

*> *      *>    Check for capital letter, digit, and special character
           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1
               UNTIL WS-CHAR-INDEX > WS-PASSWORD-LENGTH

               MOVE WS-PASSWORD-INPUT(WS-CHAR-INDEX:1)
                   TO WS-CURRENT-CHAR

*> *      *>        Check for capital letter
               IF WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "Z"
                   MOVE 1 TO WS-HAS-CAPITAL
               END-IF

*> *      *>        Check for digit
               IF WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
                   MOVE 1 TO WS-HAS-DIGIT
               END-IF

*> *      *>        Check for special character
               IF WS-CURRENT-CHAR = "!" OR WS-CURRENT-CHAR = "@"
                   OR WS-CURRENT-CHAR = "#" OR WS-CURRENT-CHAR = "$"
                   OR WS-CURRENT-CHAR = "%" OR WS-CURRENT-CHAR = "^"
                   OR WS-CURRENT-CHAR = "&" OR WS-CURRENT-CHAR = "*"
                   MOVE 1 TO WS-HAS-SPECIAL
               END-IF
           END-PERFORM.

*> *      *>    All requirements must be met
           IF WS-HAS-CAPITAL = 1 AND WS-HAS-DIGIT = 1
               AND WS-HAS-SPECIAL = 1
               MOVE 1 TO WS-PASSWORD-VALID
           END-IF.
