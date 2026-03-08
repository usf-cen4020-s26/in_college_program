*> *      *>*****************************************************************
*> *      *> 5300-JOB-SEARCH-MENU: Job Search/Internship submenu loop
*> *      *> Displays submenu and handles user input for job search/internship options
*> *      *>*****************************************************************
       5300-JOB-SEARCH-MENU.
           MOVE "1" TO WS-JOB-MENU-CHOICE

           PERFORM UNTIL WS-JOB-MENU-CHOICE = "3"
               OR WS-PROGRAM-RUNNING = 0

               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "--- Job Search/Internship Menu ---"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "1. Post a Job/Internship" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "2. Browse Jobs/Internships" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "3. Back to Main Menu" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Enter your choice (1-3): " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-MENU-CHOICE
               MOVE WS-JOB-MENU-CHOICE TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               EVALUATE WS-JOB-MENU-CHOICE
                   WHEN "1"
                       PERFORM 5310-POST-JOB-STUB
                   WHEN "2"
                       PERFORM 5320-BROWSE-JOBS-STUB
                   WHEN "3"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
               END-EVALUATE

           END-PERFORM.
           EXIT.

*> *      *>*****************************************************************
*> *      *> 5310-POST-JOB-STUB: Post a Job/Internship placeholder                   *
*> *      *> Submenu is empty (will be implemented soon)                             *
*> *      *>*****************************************************************
       5310-POST-JOB-STUB.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "--- Post a New Job/Internship ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "Post a Job/Internship is under construction."
               TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

*> *      *>*****************************************************************
*> *      *> 5320-BROWSE-JOBS-STUB: Browse Jobs/Internships placeholder                   *
*> *      *> Remains under construction per Epic 6 spec.                                  *
*> *      *>*****************************************************************
       5320-BROWSE-JOBS-STUB.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "Browse Jobs/Internships is under construction."
               TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

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
      *> 5355-READ-JOBS-LOOP: Read all records, track highest job ID
      *> Sequential ID counter = max ID found in file
      *>*****************************************************************
       5355-READ-JOBS-LOOP.
           READ JOBS-FILE
               AT END
                   MOVE "Y" TO WS-JOBS-EOF
               NOT AT END
                   ADD 1 TO WS-JOB-COUNT
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
           END-IF

           CLOSE JOBS-FILE
           EXIT.