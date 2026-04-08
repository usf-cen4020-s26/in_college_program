*>*****************************************************************
      *> FILE:    JOBSIO.cpy
      *> PURPOSE: Startup I/O for jobs and applications data files.
      *>          Loads JOBS.DAT and APPLICATIONS.DAT into working-storage
      *>          at program initialization.
      *>
      *> PARAGRAPHS:
      *>   5350-LOAD-JOBS              - Open JOBS.DAT, call read loop, close;
      *>                                 sets WS-JOB-COUNT and WS-JOB-ID-COUNTER
      *>   5355-READ-JOBS-LOOP         - Recursive read; populate WS-JOB-TABLE
      *>   5360-LOAD-APPLICATIONS      - Open APPLICATIONS.DAT, call read loop
      *>   5365-READ-APPLICATIONS-LOOP - Recursive read; populate WS-APP-TABLE
      *>
      *> DEPENDENCIES:
      *>   WS-JOBS.cpy       - WS-JOB-COUNT, WS-JOB-ID-COUNTER, WS-JOBS-EOF,
      *>                        WS-JOB-TABLE, WS-APP-COUNT, WS-APPS-EOF,
      *>                        WS-APP-TABLE, WS-JOBS-STATUS, WS-APPS-STATUS
      *>   WS-CONSTANTS.cpy  - WS-CONST-MAX-JOBS, WS-CONST-MAX-APPLICATIONS,
      *>                        WS-CONST-FS-OK, WS-CONST-FS-NOT-FOUND
      *>   WS-IO-CONTROL.cpy - WS-OUTPUT-LINE
      *>   main.cob          - 8000-WRITE-OUTPUT, JOBS-FILE, APPLICATIONS-FILE
      *>*****************************************************************
       5350-LOAD-JOBS.
           MOVE 0   TO WS-JOB-COUNT
           MOVE "N" TO WS-JOBS-EOF
           MOVE 0   TO WS-JOB-ID-COUNTER

           OPEN INPUT JOBS-FILE

           EVALUATE WS-JOBS-STATUS
               WHEN WS-CONST-FS-OK
                   PERFORM 5355-READ-JOBS-LOOP
                   CLOSE JOBS-FILE
               WHEN WS-CONST-FS-NOT-FOUND
                   CONTINUE
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open JOBS.DAT. STATUS="
                       WS-JOBS-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE
           EXIT.

      *>*****************************************************************
      *> 5355-READ-JOBS-LOOP: Read all records, track highest job ID,
      *> and populate WS-JOB-TABLE for in-session browse feature.
      *>*****************************************************************
       5355-READ-JOBS-LOOP.
           READ JOBS-FILE
               AT END
                   MOVE "Y" TO WS-JOBS-EOF
               NOT AT END
                   IF WS-JOB-COUNT < WS-CONST-MAX-JOBS
                       ADD 1 TO WS-JOB-COUNT
                       MOVE JOB-ID          TO WS-JT-ID(WS-JOB-COUNT)
                       MOVE JOB-POSTER      TO WS-JT-POSTER(WS-JOB-COUNT)
                       MOVE JOB-TITLE       TO WS-JT-TITLE(WS-JOB-COUNT)
                       MOVE JOB-DESCRIPTION TO WS-JT-DESC(WS-JOB-COUNT)
                       MOVE JOB-EMPLOYER    TO WS-JT-EMPLOYER(WS-JOB-COUNT)
                       MOVE JOB-LOCATION    TO WS-JT-LOCATION(WS-JOB-COUNT)
                       MOVE JOB-SALARY      TO WS-JT-SALARY(WS-JOB-COUNT)
                   END-IF
                   IF JOB-ID > WS-JOB-ID-COUNTER
                       MOVE JOB-ID TO WS-JOB-ID-COUNTER
                   END-IF
           END-READ

           IF WS-JOBS-EOF = "N"
               PERFORM 5355-READ-JOBS-LOOP
           END-IF
           EXIT.

      *>*****************************************************************
      *> 5330-WRITE-JOB-TO-FILE: Append one job record to JOBS.DAT
      *> Called after user completes posting flow
      *>*****************************************************************
       5330-WRITE-JOB-TO-FILE.
           MOVE 0 TO WS-JOB-WRITE-SUCCESS
           OPEN EXTEND JOBS-FILE

           IF WS-JOBS-STATUS = WS-CONST-FS-NOT-FOUND
               OPEN OUTPUT JOBS-FILE
               CLOSE JOBS-FILE
               OPEN EXTEND JOBS-FILE
           END-IF

           IF WS-JOBS-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not open JOBS.DAT. STATUS="
                   WS-JOBS-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           WRITE JOB-RECORD

           IF WS-JOBS-STATUS NOT = WS-CONST-FS-OK
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not write to JOBS.DAT. STATUS="
                   WS-JOBS-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE 1 TO WS-JOB-WRITE-SUCCESS
           END-IF

           CLOSE JOBS-FILE
           EXIT.


