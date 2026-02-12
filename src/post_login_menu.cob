IDENTIFICATION DIVISION.
PROGRAM-ID. POST-LOGIN-MENU.

*> *      *>*****************************************************************
*> *      *> POST-LOGIN-MENU: Main menu after successful login            *
*> *      *> USER STORY (Epic 2): Post-login navigation                    *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    PERFORM 5000-POST-LOGIN-MENU.
    GOBACK.

*> *      *>*****************************************************************
*> *      *> 5000-POST-LOGIN-MENU: Main menu after successful login        *
*> *      *>*****************************************************************
    5000-POST-LOGIN-MENU.
        MOVE "1" TO WS-MAIN-MENU-CHOICE.

        PERFORM UNTIL WS-MAIN-MENU-CHOICE = "6"
            OR WS-PROGRAM-RUNNING = 0
                MOVE " " TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "=== MAIN MENU ===" TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "1. Create/Edit My Profile" TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "2. View My Profile" TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "3. Search for a job" TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "4. Find someone you know" TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "5. Learn a new skill" TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "6. Logout" TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                MOVE "Enter choice (1-6): " TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

                CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG

                IF WS-EOF-FLAG = 1
                    MOVE 0 TO WS-PROGRAM-RUNNING
                    EXIT PERFORM
                END-IF

                MOVE WS-INPUT-LINE TO WS-MAIN-MENU-CHOICE
                MOVE WS-MAIN-MENU-CHOICE TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

                EVALUATE WS-MAIN-MENU-CHOICE
                    WHEN "1"
                        CALL "PROFILE-MODULE" USING LS-CONTEXT
                    WHEN "2"
                        CALL "PROFILE-VIEW" USING LS-CONTEXT
                    WHEN "3"
                        MOVE "Search for a job is under construction." TO WS-OUTPUT-LINE
                        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                    WHEN "4"
                        CALL "SEARCH-MODULE" USING LS-CONTEXT
                    WHEN "5"
                        CALL "SKILLS-MENU" USING LS-CONTEXT
                    WHEN "6"
                        EXIT PERFORM
                    WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                       TO WS-OUTPUT-LINE
                       CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                END-EVALUATE
        END-PERFORM.
