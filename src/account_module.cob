IDENTIFICATION DIVISION.
PROGRAM-ID. ACCOUNT-MODULE.

*> *      *>*****************************************************************
*> *      *> ACCOUNT-MODULE: Account creation and password validation     *
*> *      *> USER STORY 1: Account creation requirements                   *
*> *      *> USER STORY 3: Account persistence                             *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    PERFORM 4000-CREATE-ACCOUNT.
    GOBACK.

*> *      *>*****************************************************************
*> *      *> 4000-CREATE-ACCOUNT: Check capacity and create new account    *
*> *      *> USER STORY 4: New account creation with limits                *
*> *      *>*****************************************************************
    4000-CREATE-ACCOUNT.
        IF WS-ACCOUNT-COUNT >= 5
*> *      *> USER STORY 4, TASK 2: Notify user of account limit            *
*> *      *>*****************************************************************
            MOVE " " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "All permitted accounts have been created,"
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "please come back later" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        ELSE
            PERFORM 4100-GET-NEW-ACCOUNT-INFO
        END-IF.

*> *      *>*****************************************************************
*> *      *> 4100-GET-NEW-ACCOUNT-INFO: Collect new account details        *
*> *      *> USER STORY 1, TASK 1: New user account management setup       *
*> *      *>*****************************************************************
    4100-GET-NEW-ACCOUNT-INFO.
        MOVE " " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
        MOVE "=== CREATE NEW ACCOUNT ===" TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
        MOVE "Enter username: " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.

        IF WS-EOF-FLAG = 1
            MOVE 0 TO WS-PROGRAM-RUNNING
            EXIT PARAGRAPH
        END-IF

        MOVE WS-INPUT-LINE TO WS-LOGIN-USERNAME.
        MOVE WS-LOGIN-USERNAME TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

*> *      *>    Check if username already exists
        PERFORM 4200-CHECK-USERNAME-EXISTS.

        IF WS-LOGIN-SUCCESS = 1
            MOVE "Username already exists. Please try another."
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        ELSE
            PERFORM 4300-GET-VALID-PASSWORD
            IF WS-PASSWORD-VALID = 1
                PERFORM 4500-SAVE-NEW-ACCOUNT
            END-IF
        END-IF.

*> *      *>*****************************************************************
*> *      *> 4200-CHECK-USERNAME-EXISTS: Verify username uniqueness        *
*> *      *>*****************************************************************
    4200-CHECK-USERNAME-EXISTS.
        MOVE 0 TO WS-LOGIN-SUCCESS.
        PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
            UNTIL WS-ACCOUNT-INDEX > WS-ACCOUNT-COUNT
                OR WS-LOGIN-SUCCESS = 1

            IF WS-USERNAME(WS-ACCOUNT-INDEX) = WS-LOGIN-USERNAME
                MOVE 1 TO WS-LOGIN-SUCCESS
            END-IF
        END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 4300-GET-VALID-PASSWORD: Password input and validation        *
*> *      *> USER STORY 1, TASK 1: Password requirements enforcement       *
*> *      *>*****************************************************************
    4300-GET-VALID-PASSWORD.
        MOVE 0 TO WS-PASSWORD-VALID.

        PERFORM UNTIL WS-PASSWORD-VALID = 1
            MOVE "Enter password (8-12 chars, 1 uppercase, 1 digit, 1 special character):"
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                MOVE 0 TO WS-PROGRAM-RUNNING
                EXIT PERFORM
            END-IF

            MOVE WS-INPUT-LINE TO WS-PASSWORD-INPUT
            MOVE "********" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            PERFORM 4400-VALIDATE-PASSWORD

            IF WS-PASSWORD-VALID = 0
                MOVE "Password does not meet requirements."
                    TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "Please try again." TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            END-IF
        END-PERFORM.

        MOVE WS-PASSWORD-INPUT TO WS-LOGIN-PASSWORD.

*> *      *>*****************************************************************
*> *      *> 4400-VALIDATE-PASSWORD: Check password requirements           *
*> *      *> USER STORY 1, TASK 1: Password validation logic               *
*> *      *>*****************************************************************
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

*> *      *>*****************************************************************
*> *      *> 4500-SAVE-NEW-ACCOUNT: Store account in memory and file       *
*> *      *> USER STORY 3, TASK 1: File persistence implementation         *
*> *      *>*****************************************************************
    4500-SAVE-NEW-ACCOUNT.
        ADD 1 TO WS-ACCOUNT-COUNT.
        MOVE WS-LOGIN-USERNAME TO WS-USERNAME(WS-ACCOUNT-COUNT).
        MOVE WS-LOGIN-PASSWORD TO WS-PASSWORD(WS-ACCOUNT-COUNT).

        MOVE "Account created successfully!" TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

*> *      *>    Save to persistence file
        MOVE "SAVE-ACCT" TO WS-DATA-ACTION
        CALL "DATA-STORE" USING WS-DATA-ACTION LS-CONTEXT.
