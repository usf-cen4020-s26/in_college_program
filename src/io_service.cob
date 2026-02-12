IDENTIFICATION DIVISION.
PROGRAM-ID. IO-SERVICE.

*> *      *>*****************************************************************
*> *      *> IO-SERVICE: Centralized input/output handler                 *
*> *      *> USER STORY 2: Read from INPUT.TXT, write to OUTPUT.TXT        *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "INPUT.TXT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-INPUT-STATUS.
    SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.TXT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-OUTPUT-STATUS.

DATA DIVISION.
FILE SECTION.
FD  INPUT-FILE.
01  INPUT-RECORD                PIC X(200).

FD  OUTPUT-FILE.
01  OUTPUT-RECORD               PIC X(500).

WORKING-STORAGE SECTION.
01  WS-IO-OPENED                PIC 9 VALUE 0.
01  WS-INPUT-STATUS             PIC XX.
01  WS-OUTPUT-STATUS            PIC XX.

LINKAGE SECTION.
01  LS-IO-OP                    PIC X(5).
01  LS-IO-LINE                  PIC X(500).
01  LS-EOF-FLAG                 PIC 9.

PROCEDURE DIVISION USING LS-IO-OP LS-IO-LINE LS-EOF-FLAG.
    EVALUATE LS-IO-OP
        WHEN "INIT"
            PERFORM 1000-IO-INIT
        WHEN "READ"
            PERFORM 2000-IO-READ
        WHEN "WRITE"
            PERFORM 3000-IO-WRITE
        WHEN "CLOSE"
            PERFORM 9000-IO-CLOSE
        WHEN OTHER
            CONTINUE
    END-EVALUATE.
    GOBACK.

*> *      *>*****************************************************************
*> *      *> 1000-IO-INIT: Open input/output files once                   *
*> *      *>*****************************************************************
    1000-IO-INIT.
        IF WS-IO-OPENED = 1
            EXIT PARAGRAPH
        END-IF.

        OPEN INPUT INPUT-FILE.
        IF WS-INPUT-STATUS NOT = "00"
            EVALUATE WS-INPUT-STATUS
                WHEN "35"
                    DISPLAY "ERROR: INPUT.TXT file not found"
                    DISPLAY "Create INPUT.TXT before running"
                WHEN OTHER
                    DISPLAY "ERROR opening INPUT.TXT. FILE STATUS = "
                            WS-INPUT-STATUS
            END-EVALUATE
            STOP RUN
        END-IF.

        OPEN OUTPUT OUTPUT-FILE.
        MOVE 1 TO WS-IO-OPENED.

*> *      *>*****************************************************************
*> *      *> 2000-IO-READ: Read next input line from INPUT.TXT            *
*> *      *>*****************************************************************
    2000-IO-READ.
        READ INPUT-FILE
            AT END
                MOVE 1 TO LS-EOF-FLAG
                MOVE SPACES TO LS-IO-LINE
            NOT AT END
                MOVE INPUT-RECORD TO LS-IO-LINE
                MOVE 0 TO LS-EOF-FLAG
        END-READ.

*> *      *>*****************************************************************
*> *      *> 3000-IO-WRITE: Output to screen and OUTPUT.TXT               *
*> *      *>*****************************************************************
    3000-IO-WRITE.
        DISPLAY LS-IO-LINE.
        WRITE OUTPUT-RECORD FROM LS-IO-LINE.

*> *      *>*****************************************************************
*> *      *> 9000-IO-CLOSE: Close input/output files                      *
*> *      *>*****************************************************************
    9000-IO-CLOSE.
        IF WS-IO-OPENED = 1
            CLOSE INPUT-FILE
            CLOSE OUTPUT-FILE
            MOVE 0 TO WS-IO-OPENED
        END-IF.
