      *>*****************************************************************
      *> 5340-VIEW-MY-APPLICATIONS: Generate job application summary report
      *> for the currently logged-in user.
      *> Reads APPLICATIONS.DAT, filters by username, prints report.
      *>*****************************************************************
       5340-VIEW-MY-APPLICATIONS.
           MOVE 0   TO WS-APP-COUNT
           MOVE "N" TO WS-APPS-EOF

           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "--- Your Job Applications ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Application Summary for "
               FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE "------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           OPEN INPUT APPLICATIONS-FILE

           EVALUATE WS-APPS-STATUS
               WHEN WS-CONST-FS-OK
                   PERFORM 5341-READ-APPS-LOOP
                   CLOSE APPLICATIONS-FILE
               WHEN WS-CONST-FS-NOT-FOUND
                   CONTINUE
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open APPLICATIONS.DAT. STATUS="
                       WS-APPS-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE

           IF WS-APP-COUNT = 0
               MOVE "No job applications found" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF

           MOVE "------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE WS-APP-COUNT TO WS-DISPLAY-NUM
           MOVE WS-DISPLAY-NUM TO WS-DISP-ALPHANUM
           MOVE FUNCTION TRIM(WS-DISP-ALPHANUM) TO WS-NUM-DISP-STR
           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Total Applications: " WS-NUM-DISP-STR
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE "------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

      *>*****************************************************************
      *> 5341-READ-APPS-LOOP: Recursively read APPLICATIONS.DAT,
      *> printing records belonging to the current logged-in user.
      *>*****************************************************************
       5341-READ-APPS-LOOP.
           READ APPLICATIONS-FILE
               AT END
                   MOVE "Y" TO WS-APPS-EOF
               NOT AT END
                   IF FUNCTION TRIM(APP-USERNAME)
                        = FUNCTION TRIM(
                            WS-USERNAME(WS-CURRENT-USER-INDEX))
                       ADD 1 TO WS-APP-COUNT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Job Title: "
                           FUNCTION TRIM(APP-JOB-TITLE)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Employer: "
                           FUNCTION TRIM(APP-JOB-EMPLOYER)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Location: "
                           FUNCTION TRIM(APP-JOB-LOCATION)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE "---" TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   END-IF
           END-READ

           IF WS-APPS-EOF = "N"
               PERFORM 5341-READ-APPS-LOOP
           END-IF
           EXIT.

      *>*****************************************************************
      *> 5360-LOAD-APPLICATIONS: Startup check for APPLICATIONS.DAT.
      *> Verifies file accessibility; warns on unexpected status codes.
      *>*****************************************************************
       5360-LOAD-APPLICATIONS.
           MOVE "N" TO WS-APPS-EOF

           OPEN INPUT APPLICATIONS-FILE

           EVALUATE WS-APPS-STATUS
               WHEN WS-CONST-FS-OK
                   CLOSE APPLICATIONS-FILE
               WHEN WS-CONST-FS-NOT-FOUND
                   CONTINUE
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open APPLICATIONS.DAT. STATUS="
                       WS-APPS-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE
           EXIT.

