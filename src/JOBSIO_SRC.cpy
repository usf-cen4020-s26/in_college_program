      *>*****************************************************************
      *> 5350-LOAD-JOBS: Load existing jobs from JOBS.DAT at startup
      *> 1) Reads file so WS-JOB-COUNT reflects existing data
      *> 2) Sets WS-JOB-ID-COUNTER to highest ID found
      *>*****************************************************************
       5350-LOAD-JOBS.
           MOVE 0   TO WS-JOB-COUNT
           MOVE "N" TO WS-JOBS-EOF
           MOVE 0   TO WS-JOB-ID-COUNTER

           OPEN INPUT JOBS-FILE

           EVALUATE WS-JOBS-STATUS
               WHEN "00"
                   PERFORM 5355-READ-JOBS-LOOP
                   CLOSE JOBS-FILE
               WHEN "35"
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
                   IF WS-JOB-COUNT < WS-MAX-JOBS
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

           IF WS-JOBS-STATUS = "35"
               OPEN OUTPUT JOBS-FILE
               CLOSE JOBS-FILE
               OPEN EXTEND JOBS-FILE
           END-IF

           IF WS-JOBS-STATUS NOT = "00"
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not open JOBS.DAT. STATUS="
                   WS-JOBS-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           WRITE JOB-RECORD

           IF WS-JOBS-STATUS NOT = "00"
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


