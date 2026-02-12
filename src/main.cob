IDENTIFICATION DIVISION.
PROGRAM-ID. INCOLLEGE.

*> *      *>*****************************************************************
*> *      *> INCOLLEGE: Main menu entry point (dispatch only)              *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION.
       0000-MAIN-PROGRAM.
           CALL "APP-INIT" USING WS-CONTEXT.
           PERFORM 2000-PROCESS-APPLICATION
               UNTIL WS-PROGRAM-RUNNING = 0.
           CALL "APP-TERM" USING WS-CONTEXT.
           STOP RUN.

       2000-PROCESS-APPLICATION.
           MOVE " " TO WS-OUTPUT-LINE.
           CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
           MOVE "Please select an option:" TO WS-OUTPUT-LINE.
           CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
           MOVE "1. Login with existing account" TO WS-OUTPUT-LINE.
           CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
           MOVE "2. Create new account" TO WS-OUTPUT-LINE.
           CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
           MOVE "3. Exit" TO WS-OUTPUT-LINE.
           CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
           MOVE "Enter choice (1-3): " TO WS-OUTPUT-LINE.
           CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

           CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.

           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           MOVE WS-INPUT-LINE TO WS-MENU-CHOICE.
           MOVE WS-MENU-CHOICE TO WS-OUTPUT-LINE.
           CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

            EVALUATE WS-MENU-CHOICE
                WHEN "1"
                    CALL "LOGIN-MODULE" USING WS-CONTEXT
                WHEN "2"
                    CALL "ACCOUNT-MODULE" USING WS-CONTEXT
                WHEN "3"
                    MOVE 0 TO WS-PROGRAM-RUNNING
               WHEN OTHER
                   MOVE "Invalid choice. Please try again."
                       TO WS-OUTPUT-LINE
                   CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
           END-EVALUATE.
