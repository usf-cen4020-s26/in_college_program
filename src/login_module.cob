IDENTIFICATION DIVISION.
PROGRAM-ID. LOGIN-MODULE.

*> *      *>*****************************************************************
*> *      *> LOGIN-MODULE: User login flow                                *
*> *      *> USER STORY 2: Login functionality                             *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    PERFORM 3000-LOGIN-PROCESS.
    GOBACK.

*> *      *>*****************************************************************
*> *      *> 3000-LOGIN-PROCESS: User Login Handler                        *
*> *      *> USER STORY 2: Login functionality                             *
*> *      *> TASK 4: Unlimited login attempts                              *
*> *      *>*****************************************************************
    3000-LOGIN-PROCESS.
        MOVE 0 TO WS-LOGIN-SUCCESS.

        PERFORM 3100-ATTEMPT-LOGIN
            UNTIL WS-LOGIN-SUCCESS = 1.

*> *      *>*****************************************************************
*> *      *> 3100-ATTEMPT-LOGIN: Single Login Attempt                      *
*> *      *> USER STORY 2, TASK 1: Display success/failure messages        *
*> *      *> USER STORY 2, TASK 2: Read from file, output to both          *
*> *      *> USER STORY 2, TASK 3: Test with valid/invalid credentials     *
*> *      *>*****************************************************************
    3100-ATTEMPT-LOGIN.
        MOVE " " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
        MOVE "=== LOGIN ===" TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
        MOVE "Enter username: " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.

        IF WS-EOF-FLAG = 1
            MOVE 1 TO WS-LOGIN-SUCCESS
            MOVE 0 TO WS-PROGRAM-RUNNING
            EXIT PARAGRAPH
        END-IF

        MOVE WS-INPUT-LINE TO WS-LOGIN-USERNAME.
        MOVE WS-LOGIN-USERNAME TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE "Enter password: " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.

        IF WS-EOF-FLAG = 1
            MOVE 1 TO WS-LOGIN-SUCCESS
            MOVE 0 TO WS-PROGRAM-RUNNING
            EXIT PARAGRAPH
        END-IF

        MOVE WS-INPUT-LINE TO WS-LOGIN-PASSWORD.
        MOVE "********" TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

*> *      *>    Validate credentials
        PERFORM 3200-VALIDATE-LOGIN.

        IF WS-LOGIN-SUCCESS = 1
            MOVE "You have successfully logged in"
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            CALL "POST-LOGIN-MENU" USING LS-CONTEXT
        ELSE
            MOVE "Incorrect username/password, please try again"
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        END-IF.

*> *      *>*****************************************************************
*> *      *> 3200-VALIDATE-LOGIN: Check username and password              *
*> *      *>*****************************************************************
    3200-VALIDATE-LOGIN.
        MOVE 0 TO WS-LOGIN-SUCCESS.
        PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
            UNTIL WS-ACCOUNT-INDEX > WS-ACCOUNT-COUNT
                OR WS-LOGIN-SUCCESS = 1

            IF WS-USERNAME(WS-ACCOUNT-INDEX) = WS-LOGIN-USERNAME
                AND WS-PASSWORD(WS-ACCOUNT-INDEX) =
                    WS-LOGIN-PASSWORD
                THEN
                    MOVE 1 TO WS-LOGIN-SUCCESS
                    MOVE WS-ACCOUNT-INDEX TO WS-CURRENT-USER-INDEX
            END-IF
        END-PERFORM.

        IF WS-LOGIN-SUCCESS = 1
            PERFORM 3210-FIND-USER-PROFILE
        END-IF.

*> *      *>*****************************************************************
*> *      *> 3210-FIND-USER-PROFILE: Find user's profile in profiles array *
*> *      *> USER STORY (Epic 2): Link profile to logged-in user           *
*> *      *>*****************************************************************
    3210-FIND-USER-PROFILE.
        MOVE 0 TO WS-CURRENT-PROFILE-INDEX.
        MOVE 0 TO WS-PROFILE-FOUND.

        PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
            UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT
                OR WS-PROFILE-FOUND = 1

            IF WS-PROF-USERNAME(WS-ACCOUNT-INDEX) =
                WS-USERNAME(WS-CURRENT-USER-INDEX)
                MOVE WS-ACCOUNT-INDEX TO WS-CURRENT-PROFILE-INDEX
                MOVE 1 TO WS-PROFILE-FOUND
            END-IF
        END-PERFORM.
