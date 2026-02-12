IDENTIFICATION DIVISION.
PROGRAM-ID. APP-TERM.

*> *      *>*****************************************************************
*> *      *> APP-TERM: Shutdown and cleanup                               *
*> *      *> USER STORY 2: Output final message and close files            *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    MOVE " " TO WS-OUTPUT-LINE.
    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
    MOVE "Thank you for using InCollege!" TO WS-OUTPUT-LINE.
    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
    CALL "IO-SERVICE" USING "CLOSE" WS-OUTPUT-LINE WS-EOF-FLAG.
    GOBACK.
