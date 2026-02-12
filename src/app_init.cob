IDENTIFICATION DIVISION.
PROGRAM-ID. APP-INIT.

*> *      *>*****************************************************************
*> *      *> APP-INIT: Startup initialization and welcome banner          *
*> *      *> USER STORY 2: Input/output setup                              *
*> *      *> USER STORY 3: Load persisted accounts/profiles                *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    CALL "IO-SERVICE" USING "INIT " WS-OUTPUT-LINE WS-EOF-FLAG.

    MOVE "LOAD" TO WS-DATA-ACTION.
    CALL "DATA-STORE" USING WS-DATA-ACTION LS-CONTEXT.

    MOVE "========================================" TO WS-OUTPUT-LINE.
    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
    MOVE "     WELCOME TO INCOLLEGE" TO WS-OUTPUT-LINE.
    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
    MOVE "========================================" TO WS-OUTPUT-LINE.
    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

    GOBACK.
